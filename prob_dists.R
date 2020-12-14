#################
# Precalculate

# These probability distributions did not end up in the final product due to 
# brevity requirements and limitations on ShinyApp memory usage.

# years = paste(unique(eui_subset$year))
# 
# precalculated = tibble(year = as.numeric(0),
#                        year_index = as.list(0),
#                        year_mean = as.numeric(0),
#                        height = as.numeric(0))
# 
# for (y in 1:length(years)){
#   diff <- eui_subset %>%
#     filter(year == years[y])
#   
#   min = min(diff$index)
#   max = max(diff$index)
#   
#   diff <- diff %>%
#     rowwise() %>%
#     mutate(standard = (index - min) / (max - min)) %>%
#     bootstraps(times = 1000) %>%
#     mutate(boot = map(splits, ~ analysis(.))) %>%
#     mutate(standard = map(boot, ~ pull(., standard))) %>%
#     mutate(index_mean = map_dbl(standard, ~ mean(.)))
#   
#   year_model <- stan_glm(data = diff_one,
#                          index_mean ~ 1,
#                          family = gaussian(),
#                          refresh = 0)
#   
#   year_model <- year_model %>%
#     as_tibble() %>%
#     select(-sigma) %>%
#     rename(index = `(Intercept)`)
#   
#   year_mean = mean(year_model$index)
#   
#   height = which.max(hist(year_model$index,
#                           n = nrow(year_model))$density) / nrow(year_model)
#   
#   precalculated$year[nrow(precalculated)] <- years[y]
#   precalculated$year_index[nrow(precalculated)] = list(year_model$index)
#   precalculated$year_mean[nrow(precalculated)] =  year_mean
#   precalculated$height[nrow(precalculated)] = height
#   
#   precalculated[nrow(precalculated) + 1, ] <- NA
# }
# 
#  for (i in 1:length(years)){
#      for (y in 1:length(years)){
#          if (!(years[y] == years[i])){
#              insert_vector = NULL
# 
#              min_year = as.character(min(as.numeric(years[y]), as.numeric(years[i])))
#              max_year = as.character(min(as.numeric(years[y]), as.numeric(years[i])))
# 
# 
#              diff <- eui_subset %>%
#                  filter(year %in% c(paste(unlist(min_year)), paste(unlist(max_year))))
# 
#              # year one is later year
#              # year two is earlier year
# 
#              diff_one <- diff %>%
#                  filter(year %in% c(paste(unlist(max_year))))
# 
#              min = min(diff_one$index)
#              max = max(diff_one$index)
# 
#              diff_one <- diff_one %>%
#                  rowwise() %>%
#                  mutate(standard = (index - min) / (max - min)) %>%
#                  bootstraps(times = 1000) %>%
#                  mutate(boot = map(splits, ~ analysis(.))) %>%
#                  mutate(standard = map(boot, ~ pull(., standard))) %>%
#                  mutate(index_mean = map_dbl(standard, ~ mean(.)))
# 
#              year_one <- stan_glm(data = diff_one,
#                                   index_mean ~ 1,
#                                   family = gaussian(),
#                                   refresh = 0)
# 
#              diff_two <- diff %>%
#                  filter(year %in% c(paste(unlist(min_year))))
# 
#              min = min(diff_two$index)
#              max = max(diff_two$index)
# 
#              diff_two <- diff_two %>%
#                  rowwise() %>%
#                  mutate(standard = (index - min) / (max - min)) %>%
#                  bootstraps(times = 1000) %>%
#                  mutate(boot = map(splits, ~ analysis(.))) %>%
#                  mutate(standard = map(boot, ~ pull(., standard))) %>%
#                  mutate(index_mean = map_dbl(standard, ~ mean(.)))
# 
#              year_two <- stan_glm(data = diff_two,
#                                   index_mean ~ 1,
#                                   family = gaussian(),
#                                   refresh = 0)
# 
#              year_one <- year_one %>%
#                  as_tibble() %>%
#                  select(-sigma) %>%
#                  rename(index = `(Intercept)`)
# 
#              year_two <- year_two %>%
#                  as_tibble() %>%
#                  select(-sigma) %>%
#                  rename(index = `(Intercept)`)
# 
#              year_one_mean = mean(year_one$index)
#              year_two_mean = mean(year_two$index)
# 
#              height_one = which.max(hist(year_one$index,
#                                          n = nrow(year_one))$density) / nrow(year_one)
#              height_two = which.max(hist(year_one$index,
#                                          n = nrow(year_two))$density) / nrow(year_two)
# 
#              precalculated$year_one[nrow(precalculated)] <- years[y]
#              precalculated$year_two[nrow(precalculated)] = years[i]
#              precalculated$year_one_index[nrow(precalculated)] = list(year_one$index)
#              precalculated$year_two_index[nrow(precalculated)] = list(year_two$index)
#              precalculated$year_one_mean[nrow(precalculated)] =  year_one_mean
#              precalculated$year_two_mean[nrow(precalculated)] = year_two_mean
#              precalculated$height_one[nrow(precalculated)] = height_one
#              precalculated$height_two[nrow(precalculated)] = height_two
# 
#              precalculated[nrow(precalculated) + 1, ] <- NA
#          }
#      }
#  }

# saveRDS(precalculated, "data/precalculated.rds")