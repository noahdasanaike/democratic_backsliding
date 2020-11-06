library(shiny)
library(tidyverse)
library(readxl)
library(stats)
library(janitor)
library(rstanarm)
library(rsample)
library(ggthemes)
library(modeest)

# Maps

library(rdrop2)
library(leaflet)
library(rgdal)
library(countrycode)


demean.mat <- function(xmat) {
    apply(xmat, 2, function(z) z - mean(z))
}

eui <- read_xlsx("data/EIU.xlsx", sheet = "MERGEPublic") %>%
    rename(country = ...1)

variables <- substr(colnames(eui)[3:8], start = 6, stop = 7)
years <- as.character(1996:2019)

eui_fixed <- tibble(country = 0, year = 0)
for(i in 1:length(variables)){
    eui_fixed[, ncol(eui_fixed) + 1] <- rnorm(nrow(eui_fixed))
    names(eui_fixed)[ncol(eui_fixed)] <- paste0(variables[i])
}


country_index = 2
inner_index = 0
insert_vector <- NULL

for (y in 2:nrow(eui)){
    insert_vector["country"] = eui[[1]][y]
    for (i in 3:length(colnames(eui))){
        if (inner_index == 6){
            eui_fixed <- rbind(eui_fixed, insert_vector)
            insert_vector <- NULL
            insert_vector["country"] = eui[[1]][y]
            inner_index = 0
        }
        
        insert_vector["year"] = eui[1, i]
        
        for (p in 1:length(variables)){
            if (grepl(variables[p], colnames(eui)[i])){
                insert_vector[variables[p]] = eui[y, i]
                inner_index = inner_index + 1
            }
        }
    }
}

eui_subset <- eui_fixed %>% 
    slice(-1) %>%
    drop_na() %>%
    clean_names() %>%
    mutate(va = as.numeric(va),
           pv = as.numeric(pv),
           ge = as.numeric(ge),
           rq = as.numeric(rq),
           rl = as.numeric(rl),
           cc = as.numeric(cc))

eui_subset <- eui_subset[complete.cases(eui_subset), ]

eui_matrix <- eui_subset %>%
    select(-c(country, year)) %>%
    as.matrix() %>%
    demean.mat()

udv <- svd(eui_matrix)
v1 <- matrix(udv$v[,1], ncol = 1); e1 <- eui_matrix%*%v1 * -1

eui_subset$index <- e1

min = min(eui_subset$index)
max = max(eui_subset$index)

eui_subset <- eui_subset %>%
    rowwise() %>%
    mutate(standard = (index - min) / (max - min)) %>%
    mutate(year = as.numeric(year))

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

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(multiple = FALSE, "year1", "Year One:",
                        paste(unique(eui_subset$year))),
            selectInput(multiple = FALSE, "year2", "Year Two:",
                        paste(unique(eui_subset$year)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
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
                               paste(unlist(input$year2)))) %>%
            group_by(country) %>%
            pivot_wider(names_from = year, values_from = standard) %>%
            group_by(country) %>% 
            summarise_all(funs(first(na.omit(.)))) %>%
            ggplot(aes_string(x = input$year1, y = input$year2)) +
            geom_text(aes(label = country))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
