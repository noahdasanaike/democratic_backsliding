library(tidyverse)
library("randomForestSRC")
library("ggRandomForests")

eui_subset <- readRDS("data/eui_subset.rds")

forests <- tibble(year = 0, rf = 0)

years <- unique(eui_subset$year)

for (i in 1:length(years)){
  subset <- eui_subset %>%
    filter(year == years[i]) %>%
    mutate(country = as.factor(country),
           region = as.factor(region))
  
  rfsrc_eui <- rfsrc(standard~., 
                     data = as.data.frame(subset))
  
  forests$year[nrow(forests)] = years[i]
  forests$rf[nrow(forests)] = list(rfsrc_eui$predicted)
  forests[nrow(forests) + 1, ] <- NA
  
}

saveRDS(forests, "data/forests.rds")