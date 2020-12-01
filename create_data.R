library(tidyverse)
library(readxl)
library(stats)
library(janitor)
library(modeest)
library(countrycode)

## Functions

demean.mat <- function(xmat) {
  apply(xmat, 2, function(z) z - mean(z))
}

## Code

eiu <- read_xlsx("data/EIU.xlsx", sheet = "MERGEPublic") %>%
  rename(country = ...1)

variables <- substr(colnames(eiu)[3:8], start = 6, stop = 7)
years <- as.character(1996:2019)

eiu_fixed <- tibble(country = 0, year = 0)
for(i in 1:length(variables)){
  eiu_fixed[, ncol(eiu_fixed) + 1] <- rnorm(nrow(eiu_fixed))
  names(eiu_fixed)[ncol(eiu_fixed)] <- paste0(variables[i])
}


country_index = 2
inner_index = 0
insert_vector <- NULL

for (y in 2:nrow(eiu)){
  insert_vector["country"] = eiu[[1]][y]
  for (i in 3:length(colnames(eiu))){
    if (inner_index == 6){
      eiu_fixed <- rbind(eiu_fixed, insert_vector)
      insert_vector <- NULL
      insert_vector["country"] = eiu[[1]][y]
      inner_index = 0
    }
    
    insert_vector["year"] = eiu[1, i]
    
    for (p in 1:length(variables)){
      if (grepl(variables[p], colnames(eiu)[i])){
        insert_vector[variables[p]] = eiu[y, i]
        inner_index = inner_index + 1
      }
    }
  }
}

eiu_subset <- eiu_fixed %>% 
  slice(-1) %>%
  drop_na() %>%
  clean_names() %>%
  mutate(va = as.numeric(va),
         pv = as.numeric(pv),
         ge = as.numeric(ge),
         rq = as.numeric(rq),
         rl = as.numeric(rl),
         cc = as.numeric(cc))

eiu_subset <- eiu_subset[complete.cases(eiu_subset), ]

eiu_matrix <- eiu_subset %>%
  select(-c(country, year)) %>%
  as.matrix() %>%
  demean.mat()

udv <- svd(eiu_matrix)
v1 <- matrix(udv$v[,1], ncol = 1); e1 <- eiu_matrix%*%v1 * -1

eiu_subset$index <- e1

min = min(eiu_subset$index)
max = max(eiu_subset$index)

eiu_subset <- eiu_subset %>%
  rowwise() %>%
  mutate(standard = (index - min) / (max - min)) %>%
  mutate(year = as.numeric(year))

# Assign regions

eiu_subset <- eiu_subset %>%
  rowwise() %>%
  mutate(region = ifelse(country == "Jersey, Channel Islands",
                         "Europe & Central Asia",
                         countrycode(country, "country.name", "region")))

saveRDS(eiu_subset, "data/eiu_subset.rds")