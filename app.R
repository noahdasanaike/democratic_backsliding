library(shiny)
library(tidyverse)
library(readxl)
library(stats)
library(janitor)
library(rstanarm)
library(rsample)
library(ggthemes)
library(modeest)
library(countrycode)
library(forecast)
library(ggrepel)
library(binr)
library(gghighlight)
library(shinythemes)
library(wesanderson)

library("randomForestSRC")
library("ggRandomForests")

# Maps

library(rdrop2)
library(leaflet)
library(rgdal)
library(countrycode)
library(compare)
library(ggimage)

world_spdf <- readOGR( 
    dsn = paste0("map_files") , 
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)

world_spdf@data$id <- seq.int(nrow(world_spdf@data))

## Functions

demean.mat <- function(xmat) {
    apply(xmat, 2, function(z) z - mean(z))
}

eui_subset <- readRDS("data/eui_subset.rds") %>%
    mutate(ISO3 = if_else(country == "Korea, Dem. Rep.", "PRK", ISO3)) %>%
    filter(!year == 1996)

random_forests <- readRDS("data/forests.rds") %>%
    filter(!year == 1996) %>%
    arrange(year) %>%
    slice(-c(22))

### Predictions

predictions <- readRDS("predictions.RDS")

### PCA Lists

pca_eui <- eui_subset %>%
    rowwise() %>%
    mutate(pca = list(prcomp(demean.mat(as.matrix(c(va, pv, ge, rq, rl, cc)))))) %>%
    rowwise() %>%
    mutate(prc1 = unlist(pca)["x1"][[1]],
           prc2 = unlist(pca)["x2"][[1]],
           year = as.numeric(year))

### Yearly Comparison

forest_annual <- tibble()
years <- unique(random_forests$year)
    
for (i in 1:length(years)){
    year <- years[i]
    temp <- tibble(year = year,
                   actual = (eui_subset[which(eui_subset$year == year),])$standard,
                   predicted = unlist(random_forests[i, 2]),
                   region = (eui_subset[which(eui_subset$year == year),])$region)
    
    forest_annual <- rbind(forest_annual, temp)
}

chosenFlag <- NULL

### Map Graph Data

# Regions

regional_data <- tibble()
regions <- unique(eui_subset$region)

for (i in 1:length(regions)){
    for (y in 1:length(years)){
        temp <- tibble(region = regions[i],
                       year = years[y],
                       standard = mean(subset(eui_subset, year == years[y] & region == regions[i])$standard, na.rm = TRUE))
        
        regional_data <- rbind(regional_data, temp)
    }
}

world_mean <- eui_subset %>%
    group_by(year) %>%
    summarize(standard = mean(standard, na.rm = TRUE))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    tags$head(HTML("<title>Democracy Index</title>")),
    tags$head(tags$style(type = "text/css", paste0(".selectize-dropdown {
                                                     bottom: 100% !important;
                                                     top:auto!important;
                                                 }}"))),

    # Application title
    titlePanel(h1("How is Democracy Changing?", align = "center")),
    
    hr(),
    tabsetPanel(
        selected = "Global Map",
        tabPanel("Global Map", 
                 leafletOutput("globalPlot"),
                 fluidRow(column(3,
                                 br(),
                                  selectInput(multiple = FALSE, selected = 2018,
                                              "mapyear", "Map Year:",
                                              sort(as.numeric(paste(unique(eui_subset$year))),
                                                   decreasing = FALSE)),
                                 plotOutput("flag")),
                          column(9,
                                 br(),
                          plotOutput("timeGraph")))),
        tabPanel("Democracy Indices",
             plotOutput("distPlot"),
             fluidRow(column(3,
                             selectInput(multiple = FALSE, selected = 2018,
                                         "year1", "From:",
                                         sort(as.numeric(paste(unique(eui_subset$year))),
                                              decreasing = FALSE))),
                      column(3,
                             selectInput(multiple = FALSE, selected = 2019,
                                         "year2", "To:",
                                         sort(as.numeric(paste(unique(eui_subset$year))),
                                              decreasing = TRUE))),
                      column(3,
                             selectInput(multiple = TRUE, selected = "Europe & Central Asia", 
                                         "region", "Regions:",
                                         paste(unique(eui_subset$region)))))
        ),
        tabPanel("Time Forecasting",
             plotOutput("predictPlot"),
             fluidRow(column(3,
                             selectInput(multiple = TRUE, 
                                         selected = c("China", "Russian Federation", "United States"),
                                         "country", "Countries:",
                                        paste(unique(eui_subset$country)))),
                      column(3, selected = 4,
                             sliderInput("future", "n years in the future:",
                                         min = 0, max = 15,
                                         value = 2)),
                      column(3, selected = 2,
                             sliderInput("poly", "y polynomial regression:",
                                         min = 1, max = 4,
                                         value = 2))
        )),
        tabPanel("Yearly Comparison",
                 plotOutput("forestPlot"),
                 fluidRow(column(3,
                                 selectInput(multiple = TRUE, "forestyear", "Years:",
                                             selected = c(2018, 2019),
                                             sort(as.numeric(paste(unique(eui_subset$year))),
                                                  decreasing = FALSE))),
                          column(3,
                                 selectInput(multiple = TRUE, "forestregion", "Regions:",
                                             selected = c(unlist(paste(unique(eui_subset$region)))),
                                             paste(unique(eui_subset$region)))),
                          column(3,
                                 selectInput(multiple = FALSE, "type", "Graph Type:",
                                             selected = "Horizontal",
                                             c("Sloped", "Horizontal")))
                          # ,column(5, plotOutput("miniForest"))
                 )),
        tabPanel("n+1 Predictive Modeling",
                 plotOutput("predictModel")),
        tabPanel("About",
                 br(),
                 p("This project is intended to measure and track changes in
                   democracy over-time. Variables are taken from the Economist
                   Intelligence Unit, then SVD is performed in order to get a
                   unidimensional index score. Then, the values are standardized
                   in order to get a final value between 0 and 1.")
        ))
)

server <- function(input, output) {
    output$predictModel <- renderPlot({
        mean <- predictions %>%
            group_by(year) %>%
            summarize(standard = mean(standard)) %>%
            mutate(lag = lag(standard)) %>%
            mutate(lag_year = lag(year)) %>%
            mutate(change = ifelse(lag(standard) > standard, "less", "more")) %>%
            slice(2:21)
        
        predictions_graph <- predictions %>%
            mutate(jitter_year = jitter(year, factor = 1, amount = NULL))
        
        ggplot() +
            geom_point(data = predictions_graph, aes(x = jitter_year, y = standard), color = "red") +
            gghighlight(year > 2019) +
            geom_segment(data = mean, aes(x = lag_year, y = lag, xend = year, yend = standard, color = change),
                         size = 1) +
            scale_color_discrete(name = "Change", labels = c("Decrease", "Increase")) +
            labs(x = "Year", y = "Democratic Index Score")
    })
    
    observeEvent(input$globalPlot_shape_click, {
        p <- input$globalPlot_shape_click
        print(p)
        chosenFlag <<- tolower(p$id)
    })
    
    output$flag <- renderPlot({
        req(input$globalPlot_shape_click)
        data.frame(image = paste("https://flagcdn.com/w640/",
                                 chosenFlag,
                                 ".jpg",
                                 sep = "")) %>%
            ggplot(aes(0, 0)) + 
            geom_image(aes(image=image), nudge_y = 0.1, size = 0.8) +
            annotate("text", label = countrycode(chosenFlag, origin = "iso2c", 
                                                 destination = "country.name"), 
                     x = 0, y = 0.18, size = 10) +
            annotate("text", label = unique(subset(eui_subset, ISO3 == countrycode(chosenFlag, origin = "iso2c", 
                                                                            destination = "iso3c"))$region), 
                     x = 0, y = 0.165, size = 6) +
            theme_void()
    })
    
    output$timeGraph <- renderPlot({
        req(input$globalPlot_shape_click)
        
        region_choice <- unique(subset(eui_subset, ISO3 == countrycode(chosenFlag, origin = "iso2c", 
                                                                destination = "iso3c"))$region)
        
        name <- countrycode(chosenFlag, origin = "iso2c", 
                            destination = "country.name")
        
        cols <- c("country" = "#B10026", "world" = "#FEB24C", "region" = "#FC4E2A")
        
        region_mean <- regional_data %>%
            filter(region == region_choice)
        
        filtered <- eui_subset %>%
            filter(ISO3 == countrycode(chosenFlag, origin = "iso2c", 
                                       destination = "iso3c"))
        
            ggplot() +
            stat_smooth(geom = "line",  alpha = 0.5, data = filtered, size = 1.8, aes(x = year, y = standard, color = "country"), se = FALSE, span = 1) +
                geom_point(data = filtered, size = 2, aes(x = year, y = standard)) +
            stat_smooth(geom = "line", alpha = 0.5, data = region_mean, size = 1.8, aes(x = year, y = standard, color = "region"), se = FALSE, span = 1) +
                geom_point(data = region_mean, size = 2, aes(x = year, y = standard)) +
            stat_smooth(geom = "line", alpha = 0.5, data = world_mean, size = 1.8, aes(x = year, y = standard, color = "world"), se = FALSE, span = 1) +
                geom_point(data = world_mean, size = 2, aes(x = year, y = standard)) +
            labs(x = "Year", y = "Democratic Index") +
            geom_vline(xintercept = as.numeric(input$mapyear), size = 1) +
            scale_colour_manual(name = "", values = cols, labels = c(paste(unlist(name)), paste(unlist(region_choice)), "World"))
    })
    
    output$globalPlot <- renderLeaflet({
        filtered_map <- world_spdf
        
        filtered_map@data <- merge((subset(eui_subset, year == input$mapyear) %>% 
                                        select(year, ISO3, standard)), world_spdf@data)
        
        filtered_map@data <- filtered_map@data %>% arrange(id)
        
        filtered_map@polygons <- filtered_map@polygons[c(as.numeric(unlist(paste(filtered_map@data$id))))]
        filtered_map@plotOrder <- filtered_map@plotOrder[c(as.numeric(unlist(paste(filtered_map@data$id))))]
        
        mybins <- c(as.numeric(unlist(paste(attr(bins.getvals(bins(filtered_map@data$standard, target.bins = 7, 
                                                                      minpts = 10)), "binlo")))), 1)
        
        mypalette <- colorBin("YlOrRd", domain = filtered_map@data$standard, 
                              na.color = "transparent", bins = mybins)
        
        mytext <- paste(
            "Country: ",  filtered_map@data$NAME,"<br/>", 
            "Democratic Index Score: ", round(filtered_map@data$standard, 3), "<br/>",
            sep="") %>%
            lapply(htmltools::HTML)
        
        leaflet(filtered_map) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
            setView(lat = 10, lng = 0, zoom = 2) %>%
            addPolygons( 
                fillColor = ~mypalette(standard), 
                stroke = TRUE,
                color = "black",
                opacity = 1,
                fillOpacity = 0.5, 
                label = mytext,
                layerId = ~ISO2,
                weight = 1,
                smoothFactor = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                )
            ) %>%
            addLegend(pal = mypalette, values = ~standard, opacity = 0.9, 
                      title = "Democratic Index", position = "bottomright")
    })

    output$distPlot <- renderPlot({
        eui_subset %>%
            filter(year %in% c(paste(unlist(input$year1)),
                               paste(unlist(input$year2))),
                   region %in% c(paste(unlist(input$region)))) %>%
            group_by(country) %>%
            pivot_wider(names_from = year, values_from = standard) %>%
            group_by(country) %>% 
            summarise_all(funs(first(na.omit(.)))) %>%
            mutate(change = as.factor(ifelse(get(input$year2) > get(input$year1), 
                                             1, 0))) %>%
            ggplot(aes(x = get(input$year1), y = get(input$year2))) +
            geom_text_repel(aes(label = country, color = change)) +
            labs(x = paste(unlist(input$year1)), y = paste(unlist(input$year2))) +
            geom_abline(slope = 1, intercept = 0, aes(alpha = 0.5)) +
            scale_color_discrete(name = "Change", labels = c("Negative", "Positive"))
    })
    
    output$predictPlot <- renderPlot({
        eui_subset %>%
            filter(country %in% c(paste(unlist(input$country)))) %>%
            ggplot(aes(x = year, y = standard, group = country, color = country)) +
            geom_line(size = 1.1) +
            xlim(1996, 2020 + input$future) +
            stat_smooth(method = "lm", formula = y~poly(x, input$poly), fullrange = TRUE, 
                        se = FALSE, lty = 2) +
            labs(x = "Year", y = "Democratic Index") +
            scale_color_brewer(palette = "Dark2", name = "Countries") +
            geom_vline(xintercept = 2019, size = 1.2)
    })
    
    output$forestPlot <- renderPlot({
        if (input$type == "Sloped"){
            forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear))),
                       region %in% c(paste(unlist(input$forestregion)))) %>%
                mutate(year = as.character(year)) %>%
                ggplot(aes(x = actual, y = predicted, color = year)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y~x, se = F) +
                scale_color_discrete(name = "Year") +
                labs(y = "Predicted Democratic Index", x = "Actual Democratic Index") +
                facet_wrap(~ region, nrow = 1)
        }
        else
        {
            forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear))),
                       region %in% c(paste(unlist(input$forestregion)))) %>%
                mutate(year = as.character(year)) %>%
                group_by(year, region) %>%
                mutate(mean_pred = mean(predicted)) %>%
                ggplot(aes(x = 0, y = predicted, color = year)) +
                geom_jitter(height = 0) +
                geom_hline(aes(yintercept = mean_pred, color = year)) +
                labs(y = "Predicted Democratic Index", x = "") +
                facet_wrap(~ region, nrow = 1)
        }
    })
    
    # output$miniForest<- renderPlot({
    #     if (input$type == "Sloped"){
    #         temp <- forest_annual %>%
    #             filter(year %in% c(paste(unlist(input$forestyear))),
    #                    region %in% c(paste(unlist(input$forestregion)))) %>%
    #             mutate(year = as.character(year))
    #         
    #         temp %>%
    #             ggplot(aes(y = predicted, x = actual)) +
    #             geom_smooth(formula = y~x, aes(group = interaction(year, region), 
    #                                            color =  interaction(year, region)), se = F,
    #                         method = "lm") + 
    #             theme_bw() +
    #             scale_color_discrete(name = "Year") +
    #             labs(y = "Mean Predicted Democratic Index", x = "") +
    #             coord_cartesian(xlim = c(input$zoom - (0.5 * (1 - input$zoomamt)), input$zoom + (0.5 * (1 - input$zoomamt))),
    #                             ylim = c(input$zoom - (0.5 * (1 - input$zoomamt)), input$zoom + (0.5 * (1 - input$zoomamt))))
    #     }
    #     else
    #     {
    #         temp <- forest_annual %>%
    #             filter(year %in% c(paste(unlist(input$forestyear))),
    #                    region %in% c(paste(unlist(input$forestregion)))) %>%
    #             mutate(year = as.character(year)) %>%
    #             group_by(year, region) %>%
    #             mutate(mean_pred = mean(predicted))
    # 
    #         temp %>%
    #             ggplot(aes(y = predicted)) +
    #             geom_hline(aes(yintercept = mean_pred, color = interaction(year, region))) + 
    #             theme_bw() +
    #             scale_color_discrete(name = "Year") +
    #             labs(y = "Mean Predicted Democratic Index", x = "") +
    #             coord_cartesian(ylim = c(input$zoom - (0.5 * (1 - input$zoomamt)), input$zoom + (0.5 * (1 - input$zoomamt))))
    #     }
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
