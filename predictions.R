library(tidyverse)
library(countrycode)
library(mice)
library(dataPreparation)
library(glmnet)
library(splitTools)
library(ranger)
library(Metrics)
library(randomForest)
library(party)

## This code is used in order to produce predictions for 2020

standardization <- function(x){(x-min(x))/(max(x)-min(x))}

# Read in data from the world bank, which is about 450 different indicator
# variables. Then, add the ISO3 code and remove constants, doubles, and bijections.

dat <- readRDS("wdi_data.RDS")

dat <- dat %>%
  clean_names() %>% 
  mutate(iso3 = countrycode(iso2c, origin = "iso2c", destination = "iso3c")) %>%
  dplyr::select(-c(country, iso2c)) %>%
  drop_na(iso3)

filtered <- dat[, !which_are_constant(dat, verbose = FALSE)]
filtered <- dat[, !which_are_constant(dat, verbose = FALSE), with = FALSE]
filtered <- filtered[, !which_are_in_double(filtered, verbose = FALSE), with = FALSE]
filtered <- filtered[, !which_are_bijection(filtered, verbose = FALSE), with = FALSE]

# Filter out 1996 because of an abundance of missing values, then create two
# lagged variables in accordance with pre-existing forecasting literature.


eui <- readRDS("data/eui_subset.rds") %>%
  rename(iso3 = ISO3) %>%
  dplyr::select(iso3, year, standard) %>%
  group_by(iso3) %>%
  arrange(year) %>%
  group_by(iso3) %>%
  filter(!year == 1996) %>%
  mutate(lag1 = lead(standard),
         lag2 = lead(standard, 2)) %>%
  rowwise() %>%
  mutate(region = countrycode(iso3, origin = "iso3c", destination = "region"))

merged <- eui %>%
  merge(filtered, by = c("iso3", "year"))

# Remove any variables with completely missing data in order not to impact
# the imputation.

cleaned <- merged[, -which(colMeans(!is.na(merged)) == 0)]

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

# Impute when grouped by year and ISO3; ideally this would be using an imputation
# package such as MICE or Amelia, but due to computation limitations, I opted
# for this somewhat faster method.
  
cleaned <- cleaned %>%
  group_by(year) %>%
  mutate_each(funs(replace(., which(is.na(.)),
                           mean(., na.rm = TRUE)))) %>%
  group_by(iso3) %>%
  mutate_each(funs(replace(., which(is.na(.)),
                           mean(., na.rm = TRUE)))) %>%
  mutate_each(funs(replace(., which(is.nan(.)),
                           mean(., na.rm = TRUE))))

data <- cleaned[,2:150] %>%
  arrange(year) %>%
  dplyr::select(-c(standard, lag2))

## Create splits of data

# These two splits are for adding year and ISO3 values later on.
keep_real <- cleaned[,2:150] %>%
  arrange(year) %>%
  dplyr::select(standard, year)

test_iso <- cleaned %>%
  arrange(year)
#

# Train the data on all years leading up to 2019 in order to create time-series
# blocking. 

train <- subset(data, year < 2019) %>%
  rename(standard = lag1) %>%
  dplyr::select(-c(year, region))

test <- subset(data, year == 2019) %>%
  rename(standard = lag1) %>%
  dplyr::select(-c(year, region))

# Create time folds and determine the best mtry value for the random forest model;
# ideally, there would be more timefolds but again this is a limitation of computing
# power available.

folds <- create_timefolds(train$standard, k = 5)

valid_mtry <- numeric(ncol(train) - 1)

for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(standard ~ ., data = train[fold$insample, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[fold$outsample, "standard"]$standard, 
                      predict(fit, train[fold$outsample, ])$predictions))
  }
  print(i)
  print(mean(cv_mtry))
  valid_mtry[i] <- mean(cv_mtry)
}

best_mtry <- which.min(valid_mtry)

# Create the random forest model using the best mtry determined and fit
# using ranger() for speed and memory efficiency.

final_fit <- ranger(lag1 ~ ., data = train, mtry = best_mtry, 
                    importance = "impurity")
# saveRDS(final_fit, "ranger_fit.RDS")


# Create 2020 predictions and add the year and iso3 values for presentation
# on the ShinyApp. Finally, export the resulting data.

test_pred <- predict(final_fit, test)$predictions

predictions <- tibble(data <- keep_real$standard)
predictions$year <- keep_real$year

for (i in 1:length(test_pred)){
  predictions <- rbind(predictions, c(test_pred[i], 2020)) 
}

iso_2019 <- cleaned %>%
  arrange(year)

get_iso <- cleaned$iso3
get_iso <- append(get_iso, iso_2019[3014:3192,]$iso3)

predictions$iso3 <- get_iso

predictions <- predictions %>%
  rename(standard = "data <- keep_real$standard")

# saveRDS(predictions, "predictions.RDS")