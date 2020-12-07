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
library(WDI)
library(gtsummary)
library(gt)

library("randomForestSRC")
library("ggRandomForests")

# Libraries used for map and flag generation

library(rdrop2)
library(leaflet)
library(rgdal)
library(countrycode)
library(compare)
library(ggimage)

# Note: more detailed comments are available further below, in the server_ui 
# call; this is because much of the data is pre-generated for speed purposes.

### Project Setup

# Set flag to null for homepage initial load

chosenFlag <- NULL

# Read in the map file in order to generate polygons

world_spdf <- readOGR( 
    dsn = paste0("map_files") , 
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)

# Create an ID in order to reduce the data to only countries that match with
# my dataset.

world_spdf@data$id <- seq.int(nrow(world_spdf@data))

## Functions

# Create a demean function to standardize means

demean.mat <- function(xmat) {
    apply(xmat, 2, function(z) z - mean(z))
}

# Read in EIU data (spelling error)

eui_subset <- readRDS("data/eui_subset.rds") %>%
    mutate(ISO3 = if_else(country == "Korea, Dem. Rep.", "PRK", ISO3)) %>%
    filter(!year == 1996)

# Read in pre-generated Random Forests

random_forests <- readRDS("data/forests.rds") %>%
    filter(!year == 1996) %>%
    arrange(year) %>%
    slice(-c(22))

## Predictions for 2020

predictions <- readRDS("predictions.RDS")

# Categorize predictions according to EIU subsets

pred_categorization <- predictions %>%
    rowwise() %>%
    mutate(type = case_when(standard > .8 ~ "Full Democracy",
                            standard >= .6 & standard < .8 ~ "Flawed Democracy",
                            standard > .4 & standard < .6 ~ "Hybrid Regime",
                            standard < .4 & standard ~ "Authoritarian",
                            TRUE ~ "None"))

# Create data for the table on the models paage

pred_2019 <- pred_categorization %>%
    filter(year %in% c(2019)) %>%
    count(type) %>%
    mutate(year = 2019)

pred_2020 <- pred_categorization %>%
    filter(year %in% c(2020)) %>%
    count(type) %>%
    mutate(year = 2020)

pred_table <- pred_2019

pred_table <- rbind(pred_table, pred_2020)

## PCA Lists

# Run prcomp (this isn't used anymore)

pca_eui <- eui_subset %>%
    rowwise() %>%
    mutate(pca = list(prcomp(demean.mat(as.matrix(c(va, pv, ge, rq, rl, cc)))))) %>%
    rowwise() %>%
    mutate(prc1 = unlist(pca)["x1"][[1]],
           prc2 = unlist(pca)["x2"][[1]],
           year = as.numeric(year))

## Yearly Comparison Data

forest_annual <- tibble()
years <- unique(random_forests$year)

# Create dataset from which year and region may be selected
    
for (i in 1:length(years)){
    year <- years[i]
    temp <- tibble(year = year,
                   actual = (eui_subset[which(eui_subset$year == year),])$standard,
                   predicted = unlist(random_forests[i, 2]),
                   region = (eui_subset[which(eui_subset$year == year),])$region)
    
    forest_annual <- rbind(forest_annual, temp)
}

### Map Graph Data

# Create mean for each region for line on home page graph

regional_data <- tibble()
regions <- unique(eui_subset$region)

for (i in 1:length(regions)){
    for (y in 1:length(years)){
        temp <- tibble(region = regions[i],
                       year = years[y],
                       standard = mean(subset(eui_subset, year == years[y] & 
                                                region == regions[i])$standard, 
                                       na.rm = TRUE))
        
        regional_data <- rbind(regional_data, temp)
    }
}

# Create mean for global line on home page

world_mean <- eui_subset %>%
    group_by(year) %>%
    summarize(standard = mean(standard, na.rm = TRUE))

## Random Forest Fit

# Read in names for importance plot

names <- WDIsearch() %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(indicator = tolower(indicator))

names$indicator <- gsub('\\.', '_', names$indicator)

# Read in pre-generated plot

fit <- readRDS("ranger_fit.RDS") 

# Generate importance plot 

importance <- fit$variable.importance %>%
    tibble() %>%
    rename(var = ".") %>%
    rowwise() %>%
    mutate(indicator = attributes(var)) %>%
    mutate(indicator = sapply(indicator, toString)) %>%
    tibble() %>%
    left_join(names, by = "indicator")

filtered_importance <- importance %>%
    arrange(desc(var)) %>%
    rowwise() %>%
    mutate(sqrt_var = sqrt(var))

# Remove variables that are co-linear for presentation
    
filtered_importance <- filtered_importance[-c(2, 5, 10, 11),]

## Predictions Graph

# Create color of each line for each year on predictive modeling page

mean <- predictions %>%
    group_by(year) %>%
    summarize(standard = mean(standard)) %>%
    mutate(lag = lag(standard)) %>%
    mutate(lag_year = lag(year)) %>%
    mutate(change = ifelse(lag(standard) > standard, "less", "more")) %>%
    slice(2:21)

predictions_graph <- predictions %>%
    mutate(jitter_year = jitter(year, factor = 1, amount = NULL))

# Generate the table of change in regime

pred_table_done <- pred_table[rep(row.names(pred_table), pred_table$n), 1:3][, c(1, 3)] %>%
  rename(Type = type) %>%
  tbl_summary(by = year)%>% 
  as_gt()

ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    # Create tab title and make drop-downs open upwards
    
    tags$head(HTML("<title>Democracy Index</title>")),
    tags$head(tags$style(type = "text/css", paste0(".selectize-dropdown {
                                                     bottom: 100% !important;
                                                     top:auto!important;
                                                 }}"))),

    # Top header title
    titlePanel(h1("How is Democracy Changing?", align = "center")),
    
    hr(),
    tabsetPanel(
        selected = "Global Map",
        tabPanel("Global Map", 
                 leafletOutput("globalPlot"),
                 fluidRow(column(3,
                                 br(),
                                 p("Select a country to view more details (this 
                                   may take a couple of seconds)."),
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
                                         selected = c("China", "Russian Federation", 
                                                      "United States"),
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
                                             c("Sloped", "Horizontal"))),
                          column(3,
                                 br(),
                                 p("Note that the x-position of the dots is irrelevant 
                                 and has been added exclusively for viewability."),
                                 br(),
                                 p("These points are predictions generated using a Random
                                   Forest model."))
                          # ,column(5, plotOutput("miniForest"))
                 )),
        tabPanel("n+1 Predictive Modeling",
                 plotOutput("predictModel"),
                 fluidRow(column(6,
                                 br(),
                                 p(strong("Time-Series Cross-Validation and Block Partitioning")),
                                 br(),
                                 p("In order to construct the predicted democratic index values for 2020
                                   as seen above, I first downloaded all of the World Bank Development Indicators across
                                   all available years, then merged with my democratic index values by country and year in order to create
                                   a time-series of each variable. From there, I dropped each variable with all NA values, then removed any
                                   constant and double variables as well as exact bijections. From there, I ran manual imputation by mean, grouping
                                   by year and then again, for NaN values, by year and country. I then used modified k-fold cross-validation such that the
                                   data is split in sequential order as follows:"),
                                 br(),
                                 img(src = "blocked_split.png"),
                                 br(),
                                 br(),
                                 p("After creating the timefolds and determining the best mtry, or number of
                                   variables available for splitting at each node, I create my model using
                                   the ranger() function, then predict() using my testing split. Because the
                                   outcome variable is the lagged democratic index value for each year, the outcome
                                   for 2019 should be the actual predicted standard for 2020."),
                                 br(),
                                 p("On the right, we can see a table of predicted regime types for 2020 compared to the 
                                   actual values for 2019. These categories are derived from the official EIU categorizations.
                                   It appears as though 2020, according to this model, is forecasted to become less democratic as a whole.
                                   This includes a reduction in full democracies, and an increase in flawed democracies, hybrid regimes,
                                   and authoritarian regimes. The n-size of 179 for each category is the total number of countries
                                   in the data set.")),
                          column(6,
                                 plotOutput("predictImportance"),
                                 br(),
                                 br(),
                                 br(),
                                 gt_output("predictTable")))),
        tabPanel("About",
                 fluidRow(column(6,
                 br(),
                 p("This project is intended to measure and track changes in
                   democracy over-time. Variables are taken from the Economist
                   Intelligence Unit, then SVD is performed in order to get a
                   unidimensional index score. Finally, the values are standardized
                   in order to produce a value range of 0 to 1. For my n+1 time-series
                   modeling, I downloaded all 539 World Bank Development Indicators across
                   all available years by using the WDI R package."),
                 br(),
                 p("The EIU dataset can be obtained from the following link. Additionally, in
                   order to clean the data as a consequence of strange column formatting, I had to run the code attached below that."),
                 br(),
                 a("World Bank EIU Upload (warning: direct download)", href = "https://info.worldbank.org/governance/wgi/Home/downLoadFile?fileName=EIU.xlsx",
                   target = "_blank"),
                 br(),
                 a("Cleaning Script (my Github)", href = "https://github.com/noahdasanaike/democratic_backsliding/blob/master/create_data.R",
                   target = "_blank"),
                 br(),
                 br(),
                 p("My name is Noah Dasanaike. I'm a current Junior at Harvard
                   College studying Government and Data Science, and you can find my 
                   Github here:"),
                 a("https://github.com/noahdasanaike", href = "https://github.com/noahdasanaike",
                   target = "_blank")))
        ))
)

server <- function(input, output) {
  
    # Any function pertaining to the front page uses a req in order to generate only after
    # a user has selected something.
  
    output$flag <- renderPlot({
        req(input$globalPlot_shape_click)
      
        # Read in id (iso2) and pass to flagcdn in order to download flag image
        
        p <- input$globalPlot_shape_click
        chosenFlag <<- tolower(p$id)
        
        # Generate flag with the country name and region above it; in order
        # to set the black outline, make a slightly bigger flag behind it
        # and set the coloring to black.
        
        data.frame(image = paste("https://flagcdn.com/w640/",
                                 chosenFlag,
                                 ".jpg",
                                 sep = "")) %>%
            ggplot(aes(0, 0)) + 
            geom_image(aes(image = image), nudge_y = 0.09, size = 0.82, 
                       color = "black") +
            geom_image(aes(image = image), nudge_y = 0.09, size = 0.8) +
            annotate("text", label = countrycode(chosenFlag, origin = "iso2c", 
                                                 destination = "country.name"), 
                     x = 0, y = 0.18, size = 10) +
            annotate("text", label = unique(subset(eui_subset, 
                                                   ISO3 == countrycode(chosenFlag, 
                                                                       origin = "iso2c", 
                                                                       destination = "iso3c"))$region), 
                     x = 0, y = 0.165, size = 6) +
            theme_void()
    })
    
    output$timeGraph <- renderPlot({
        req(input$globalPlot_shape_click)
        
        p <- input$globalPlot_shape_click
        chosenFlag <<- tolower(p$id)
        
        # Read in the region of the country for the relevant regional line
        
        region_choice <- unique(subset(eui_subset, 
                                       ISO3 == countrycode(chosenFlag, 
                                                           origin = "iso2c", 
                                                           destination = "iso3c"))$region)
        
        name <- countrycode(chosenFlag, origin = "iso2c", 
                            destination = "country.name")
        
        cols <- c("country" = "#B10026", "world" = "#FEB24C", "region" = "#FC4E2A")
        
        region_mean <- regional_data %>%
            filter(region == region_choice)
        
        # Create the line plot with the country, region, and world change over
        # time; use geom_vline in order to show the selection of the map year
        # respective to total change over time.
        
        filtered <- eui_subset %>%
            filter(ISO3 == countrycode(chosenFlag, origin = "iso2c", 
                                       destination = "iso3c"))
        
            ggplot() +
            stat_smooth(geom = "line",  alpha = 0.5, data = filtered, size = 1.8, 
                        aes(x = year, y = standard, color = "country"), 
                        se = FALSE, span = 1) +
                geom_point(data = filtered, size = 2, aes(x = year, y = standard, 
                                                          color = "country")) +
            stat_smooth(geom = "line", alpha = 0.5, data = region_mean, size = 1.8, 
                        aes(x = year, y = standard, color = "region"), 
                        se = FALSE, span = 1) +
                geom_point(data = region_mean, size = 2, aes(x = year, y = standard, 
                                                             color = "region")) +
            stat_smooth(geom = "line", alpha = 0.5, data = world_mean, size = 1.8, 
                        aes(x = year, y = standard, color = "world"), 
                        se = FALSE, span = 1) +
                geom_point(data = world_mean, size = 2, aes(x = year, y = standard, 
                                                            color = "world")) +
            theme_bw() +
            labs(x = "Year", y = "Democratic Index") +
            geom_vline(xintercept = as.numeric(input$mapyear), size = 1) +
            scale_colour_manual(name = "", values = cols, labels = c(paste(unlist(name)), paste(unlist(region_choice)), "World"))
    })
    
    output$globalPlot <- renderLeaflet({
        filtered_map <- world_spdf
        
        # Filter the map according to the year selected
        
        filtered_map@data <- merge((subset(eui_subset, year == input$mapyear) %>% 
                                        select(year, ISO3, standard)), world_spdf@data)
        
        filtered_map@data <- filtered_map@data %>% arrange(id)
        
        # Only select polygons of countries which have data in that given year in the 
        # democracy dataset; this is in order to prevent mis-matching of polygons
        # and color fills.
        
        filtered_map@polygons <- filtered_map@polygons[c(as.numeric(unlist(paste(filtered_map@data$id))))]
        filtered_map@plotOrder <- filtered_map@plotOrder[c(as.numeric(unlist(paste(filtered_map@data$id))))]
        
        mybins <- c(as.numeric(unlist(paste(attr(bins.getvals(bins(filtered_map@data$standard, target.bins = 7, 
                                                                      minpts = 10)), "binlo")))), 1)
        
        # Add the most aesthetically pleasing colorbin, which is dynamically generated
        # according to the standard selected from that year.
        
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
      
        # This uses geom_text_repel in order to show where a country lies when two
        # years are selected. Every possible combination of country-year_one-year_two
        # are pre-generated for the sake of speed on the web-page.
      
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
            geom_text_repel(aes(label = country, color = change, fontface = 2), 
                            show.legend = FALSE) +
            geom_point(aes(color = change), alpha = 0, size = 2) +
            labs(x = paste(unlist(input$year1)), y = paste(unlist(input$year2))) +
            theme_bw() +
            geom_abline(slope = 1, intercept = 0, aes(alpha = 0.5)) +
            scale_color_discrete(name = "Change", labels = c("Negative", "Positive")) +
            guides(colour = guide_legend(override.aes = list(alpha = 1)))
    })
    
    output$predictPlot <- renderPlot({
      
        # Produce linear regression predictions using stat_smooth, then add
        # a line at the last point with real data. User inputs are time (xlim)
        # and the polynomial exponent (input$poly)
      
        eui_subset %>%
            filter(country %in% c(paste(unlist(input$country)))) %>%
            ggplot(aes(x = year, y = standard, group = country, color = country)) +
            geom_line(size = 1.1) +
            theme_bw() +
            xlim(1996, 2020 + input$future) +
            stat_smooth(method = "lm", formula = y~poly(x, input$poly), fullrange = TRUE, 
                        se = FALSE, lty = 2) +
            labs(x = "Year", y = "Democratic Index") +
            scale_color_brewer(palette = "Dark2", name = "Countries") +
            geom_vline(xintercept = 2019, size = 1.2)
    })
    
    output$forestPlot <- renderPlot({
      
        # Present the pre-generated Random Forest data using either horizontal
        # or sloped; sloped is largely useless because the change-over-time is
        # usually very minimal.
      
        if (input$type == "Sloped"){
            forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear))),
                       region %in% c(paste(unlist(input$forestregion)))) %>%
                mutate(year = as.character(year)) %>%
                ggplot(aes(x = actual, y = predicted, color = year)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y~x, se = F) +
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Predicted Democratic Index", x = "") +
                facet_wrap(~ region, nrow = 1) +
                theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
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
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Predicted Democratic Index", x = "") +
                facet_wrap(~ region, nrow = 1) +
                theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
        }
    })
    
    output$predictModel <- renderPlot({
      
      # Show the actual democratic index score by year, then highlight the 
      # prediction. geom_segment is used in order to color each line by the change.
      
      ggplot() +
        geom_point(data = predictions_graph, aes(x = jitter_year, y = standard), 
                   color = "red") +
        gghighlight(year > 2019) +
        geom_segment(data = mean, aes(x = lag_year, y = lag, xend = year, 
                                      yend = standard, color = change),
                     size = 1.3) +
        scale_color_discrete(name = "Change", labels = c("Decrease", "Increase")) +
        labs(x = "Year", y = "Democratic Index Score") +
        theme_bw()
    })

    output$predictImportance <- renderPlot({
      
      # Display the importance scores of each predictor.
      
      filtered_importance %>%
        arrange(desc(var)) %>%
        head(10) %>%
        ggplot(aes(sqrt_var, x = reorder(name, sqrt_var))) +
        geom_point(size = 3, colour = "#ff6767") +
        coord_flip() +
        labs(x = "Predictors", y = "Importance Scores (Square Root)") +
        theme_bw()
    })
    
    output$predictTable <- render_gt(
      
      # Render plot of regime change (pre-generated)
      
      pred_table_done
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
