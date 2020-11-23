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

library("randomForestSRC")
library("ggRandomForests")

# Maps

library(rdrop2)
library(leaflet)
library(rgdal)
library(countrycode)

## Functions

demean.mat <- function(xmat) {
    apply(xmat, 2, function(z) z - mean(z))
}

eui_subset <- readRDS("data/eui_subset.rds")

random_forests <- readRDS("data/forests.rds") %>%
    arrange(year) %>%
    slice(-c(22))

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
                   predicted = unlist(random_forests[i, 2]))
    
    forest_annual <- rbind(forest_annual, temp)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(HTML("<title>Democracy Index</title>")),
    tags$head(tags$style(type = "text/css", paste0(".selectize-dropdown {
                                                     bottom: 100% !important;
                                                     top:auto!important;
                                                 }}"))),

    # Application title
    titlePanel(h1("How is Democracy Changing?", align = "center")),
    
    hr(),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Democracy Indices",
             plotOutput("distPlot"),
             fluidRow(column(3,
                             selectInput(multiple = FALSE, selected = 2018,
                                         "year1", "From:",
                                         sort(as.numeric(paste(unique(eui_subset$year))),
                                              decreasing = FALSE))),
                      column(4,
                             selectInput(multiple = FALSE, selected = 2019,
                                         "year2", "To:",
                                         sort(as.numeric(paste(unique(eui_subset$year))),
                                              decreasing = TRUE))),
                      column(5,
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
                      column(4, selected = 4,
                             sliderInput("future", "n years in the future:",
                                         min = 0, max = 15,
                                         value = 2)),
                      column(5, selected = 2,
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
                          column(4,
                                 selectInput(multiple = FALSE, "type", "Graph Type:",
                                             selected = "Horizontal",
                                             c("Sloped", "Horizontal")),
                                 conditionalPanel(condition = "input.type == Sloped",
                                                  sliderInput("zoom", "Zoom to Mean:",
                                                              value = 0.5,
                                                              min = 0.2, max = 0.9))),
                          column(5, plotOutput("miniForest"))
                 )),
        tabPanel("About",
                 br(),
                 p("This project is intended to measure and track changes in
                   democracy over-time. Variables are taken from the Economist
                   Intelligence Unit, then SVD is performed in order to get a
                   unidimensional index score. Then, the values are standardized
                   in order to get a final value between 0 and 1.")
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # pca_eui %>%
    #     filter(year %in% c(paste(unlist(input$year1)),
    #                        paste(unlist(input$year2)))) %>%
    #     ggplot(aes(x = prc1, y = prc2, color = year)) +
    #     geom_text(aes(label = country)) +
    #     geom_path(aes(group = country), 
    #               arrow = arrow(length=unit(0.15,"cm")),
    #               colour="black", size=1) +
    #     theme(axis.title.y = element_blank(),
    #           axis.text.y = element_blank(), 
    #           axis.ticks.y = element_blank(),
    #           plot.background = element_blank(),
    #           panel.background = element_blank())
    
    # eui_subset %>%
    #     filter(country %in% c(paste(unlist(input$countries)))) %>%
    #     ggplot(aes(x = year, y = standard, group = country, color = country)) +
    #     geom_line(size = 1.5) +
    #     theme(plot.background = element_blank(),
    #           panel.background = element_blank()) +
    #     scale_y_continuous(position = "right")

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
            theme_bw() +
            geom_abline(slope = 1, intercept = 0, aes(alpha = 0.5)) +
            scale_color_discrete(name = "Change", labels = c("Negative", "Positive"))
    })
    
    output$predictPlot <- renderPlot({
        eui_subset %>%
            filter(country %in% c(paste(unlist(input$country)))) %>%
            ggplot(aes(x = year, y = standard, group = country, color = country)) +
            geom_line(size = 1.1) +
            theme_bw() +
            xlim(1996, 2020 + input$future) +
            stat_smooth(method = "lm", formula = y~poly(x, input$poly), fullrange = TRUE, 
                        se = FALSE, lty = 2) +
            labs(x = "Year", y = "Democratic Index") +
            scale_color_discrete(name = "Countries") +
            geom_vline(xintercept = 2019, size = 1.2)
    })
    
    output$forestPlot <- renderPlot({
        if (input$type == "Sloped"){
            forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear)))) %>%
                mutate(year = as.character(year)) %>%
                ggplot(aes(x = actual, y = predicted, group = year, color = year)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y~x, se = F) +
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Predicted Democratic Index", x = "Actual Democratic Index")
        }
        else
        {
            forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear)))) %>%
                mutate(year = as.character(year)) %>%
                group_by(year) %>%
                mutate(mean_pred = mean(predicted)) %>%
                ggplot(aes(x = 0, y = predicted, group = year, color = year)) +
                geom_jitter(height = 0) +
                geom_hline(aes(yintercept = mean_pred, color = year)) + 
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Predicted Democratic Index", x = "")
        }
    })
    
    output$miniForest<- renderPlot({
        if (input$type == "Sloped"){
            temp <- forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear)))) %>%
                mutate(year = as.character(year))
            
            temp %>%
                ggplot(aes(y = predicted, x = actual)) +
                geom_smooth(formula = y~x, aes(group = year, color = year), se = F,
                            method = "lm") + 
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Mean Predicted Democratic Index", x = "") +
                coord_cartesian(xlim = c(input$zoom - 0.01, input$zoom + 0.01),
                                ylim = c(input$zoom - 0.01, input$zoom + 0.01))
        }
        else
        {
            temp <- forest_annual %>%
                filter(year %in% c(paste(unlist(input$forestyear)))) %>%
                mutate(year = as.character(year)) %>%
                group_by(year) %>%
                mutate(mean_pred = mean(predicted))

            temp %>%
                ggplot(aes(y = predicted)) +
                geom_hline(aes(yintercept = mean_pred, color = year)) + 
                theme_bw() +
                scale_color_discrete(name = "Year") +
                labs(y = "Mean Predicted Democratic Index", x = "") +
                coord_cartesian(ylim = c(input$zoom - 0.05, input$zoom + 0.05))
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
