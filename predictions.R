library(tidyverse)
library(countrycode)
library(mice)
library(dataPreparation)
library(glmnet)
library(countrycode)
library(splitTools)
library(ranger)
library(Metrics)

standardization <- function(x){(x-min(x))/(max(x)-min(x))}

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


eui <- readRDS("data/eui_subset.rds") %>%
  rename(iso3 = ISO3) %>%
  select(iso3, year, standard) %>%
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

cleaned <- merged[, -which(colMeans(!is.na(merged)) == 0)]

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
  
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
  select(-c(standard, lag2))

inds <- partition(data$lag1, p = c(train = 0.9, test = 0.1), type = "blocked")

# for later
keep_real <- cleaned[,2:150] %>%
  arrange(year) %>%
  select(standard, year)
#

train <- subset(data, year < 2019) %>%
  rename(standard = lag1) %>%
  select(-c(year, region))
test <- subset(data, year == 2019) %>%
  rename(standard = lag1) %>%
  select(-c(year, region))

## get iso

test_iso <- cleaned %>%
  arrange(year)



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

final_fit <- ranger(standard ~ ., data = train, mtry = best_mtry)
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

test <- subset(predictions, year > 2015)
x <- seq_along(predictions$standard)
plot(x, predictions$standard, pch = ".", cex = 2)
points(tail(x, length(test$standard)), test$standard, col = "red", pch = ".", cex = 2)

predictions %>%
  mutate(jitter_year = jitter(year, factor = 1, amount = NULL)) %>%
  ggplot(aes(x = jitter_year, y = standard)) +
  geom_point(color = "red") +
  gghighlight(year > 2019)
