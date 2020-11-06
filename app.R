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

### PCA Lists

pca_eui <- eui_subset %>%
    rowwise() %>%
    mutate(pca = list(prcomp(demean.mat(as.matrix(c(va, pv, ge, rq, rl, cc)))))) %>%
    rowwise() %>%
    mutate(prc1 = unlist(pca)["x1"][[1]],
           prc2 = unlist(pca)["x2"][[1]],
           year = as.numeric(year))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("How is Democracy Changing?", align = "center")),
    
    hr(),
    
    plotOutput("distPlot"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
            h3("Index Distribution"),
            selectInput(multiple = FALSE, "year1", "Year One:",
                        paste(unique(eui_subset$year))),
            selectInput(multiple = FALSE, "year2", "Year Two:",
                        paste(unique(eui_subset$year))),
            selectInput(multiple = TRUE, "region", "Regions:",
                        paste(unique(eui_subset$region)))
        ),
        column(3,
               h3("Time Series"),
               selectInput(multiple = TRUE, "country", "Countries:",
                           paste(unique(eui_subset$year))),
        )
    )
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
            ggplot(aes(x = get(input$year1), y = get(input$year2))) +
            geom_text_repel(aes(label = country)) +
            labs(x = paste(unlist(input$year1)), y = paste(unlist(input$year2))) +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
