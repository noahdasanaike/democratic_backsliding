library(tidyverse)
library(readxl)
library(stats)
library(janitor)
library(rstanarm)
library(rsample)
library(ggthemes)
library(modeest)
library(countrycode)

## Functions

demean.mat <- function(xmat) {
  apply(xmat, 2, function(z) z - mean(z))
}

## Code

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

# Assign regions

eui_subset <- eui_subset %>%
  rowwise() %>%
  mutate(region = ifelse(country == "Jersey, Channel Islands",
                         "Europe & Central Asia",
                         countrycode(country, "country.name", "region")))

saveRDS(eui_subset, "data/eui_subset.rds")