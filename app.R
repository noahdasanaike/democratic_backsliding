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

#################
# Precalculate

# years = paste(unique(eui_subset$year))

# precalculated = tibble(year_one = 0, year_two = 0, year_one_index = 0,
#                        year_two_index = 0, year_one_mean = 0, year_two_mean = 0,
#                        height_one = 0, height_two = 0)
# 
# for (i in 1:length(years)){
#     for (y in 1:length(years)){
#         if (!(years[y] == years[i])){
#             insert_vector = NULL
#             
#             min_year = as.character(min(as.numeric(years[y]), as.numeric(years[i])))
#             max_year = as.character(min(as.numeric(years[y]), as.numeric(years[i])))
#             
#             
#             diff <- eui_subset %>%
#                 filter(year %in% c(paste(unlist(min_year)), paste(unlist(max_year))))
#             
#             # year one is later year
#             # year two is earlier year
#             
#             min = min(diff$index)
#             max = max(diff$index)
#             
#             diff_one <- diff %>%
#                 filter(year %in% c(paste(unlist(max_year)))) %>%
#                 rowwise() %>%
#                 mutate(standard = (index - min) / (max - min)) %>%
#                 bootstraps(times = 1000) %>%
#                 mutate(boot = map(splits, ~ analysis(.))) %>%
#                 mutate(standard = map(boot, ~ pull(., standard))) %>% 
#                 mutate(index_mean = map_dbl(standard, ~ mean(.)))
#             
#             year_one <- stan_glm(data = diff_one, 
#                                  index_mean ~ 1, 
#                                  family = gaussian(), 
#                                  refresh = 0)
#             
#             diff_two <- diff %>%
#                 filter(year %in% c(paste(unlist(min_year)))) %>%
#                 rowwise() %>%
#                 mutate(standard = (index - min) / (max - min)) %>%
#                 bootstraps(times = 1000) %>%
#                 mutate(boot = map(splits, ~ analysis(.))) %>%
#                 mutate(standard = map(boot, ~ pull(., standard))) %>% 
#                 mutate(index_mean = map_dbl(standard, ~ mean(.)))
#             
#             year_two <- stan_glm(data = diff_two, 
#                                  index_mean ~ 1, 
#                                  family = gaussian(), 
#                                  refresh = 0)
#             
#             year_one <- year_one %>%
#                 as_tibble() %>%
#                 select(-sigma) %>%
#                 rename(index = `(Intercept)`)
#             
#             year_two <- year_two %>%
#                 as_tibble() %>%
#                 select(-sigma) %>%
#                 rename(index = `(Intercept)`)
#             
#             year_one_mean = mean(year_one$index)
#             year_two_mean = mean(year_two$index)
#             
#             height_one = which.max(hist(year_one$index, 
#                                         n = nrow(year_one))$density) / nrow(year_one)
#             height_two = which.max(hist(year_one$index, 
#                                         n = nrow(year_two))$density) / nrow(year_two)
#             
#             insert_vector["year_one"] = years[y] 
#             insert_vector["year_two"] = years[i] 
#             insert_vector["year_one_index"] = as.tibble(year_one$index)
#             insert_vector["year_two_index"] = as.tibble(year_two$index)
#             insert_vector["year_one_mean"] =  year_one_mean
#             insert_vector["year_two_mean"] = year_two_mean
#             insert_vector["height_one"] = height_one
#             insert_vector["height_two"] = height_two
#             
#             precalculated <- rbind(precalculated, data.frame(insert_vector))
#         }
#     }
# }
# 
# write.csv(precalculated, "data/precalculated.csv")

precalculated <- read_csv("data/precalculated.csv") %>%
    slice(-1)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("How is Democracy Changing?", align = "center")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(multiple = FALSE, "year1", "From:",
                        paste(unique(eui_subset$year))),
            selectInput(multiple = FALSE, "year2", "To:",
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

    output$distPlot <- renderPlot({
        earlier = min(c(as.numeric(paste(unlist(input$year1))), 
                             as.numeric(paste(unlist(input$year2)))))
        later = max(c(as.numeric(paste(unlist(input$year1))), 
                 as.numeric(paste(unlist(input$year2)))))
        
        data <- precalculated %>% 
            filter(year_one == later, year_two == earlier)
        
        # year one is later year
        # year two is earlier year
        
        cols <- c("bar_1" = "#f54242", "bar_2" = "#b8b2ad")
        
        ggplot() +
            geom_histogram(aes(x = data$year_one_index, 
                               y = after_stat(count/sum(count)),
                               fill = "bar_1"), 
                           color = "black", alpha = 0.8) +
            geom_histogram(aes(x = data$year_two_index,
                               y = after_stat(count/sum(count)),
                               fill = "bar_2"), 
                           color = "black", alpha = 0.8) +
            scale_fill_manual(name = "Year", values = cols,
                              labels = c()) +
            annotate("text", x = (data$year_one_mean[[1]] -.002), y = data$height_one[[1]] / 2, 
                     size = 6, fontface = 2,
                     label = paste(unlist(input$year1))) +
            annotate("text", x = (data$year_two_mean[[1]] + .002), y = data$height_two[[1]] / 2, 
                     size = 6, fontface = 2,
                     label = paste(unlist(input$year2))) + 
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(),
                  plot.background = element_blank(),
                  panel.background = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
