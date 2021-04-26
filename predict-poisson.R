# Poisson model for predicting number of new species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-04-26

rm(list = ls())

################################################################################
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

results <- readr::read_csv(file = "data/target-results.csv")

results <- results %>%
  mutate(days_ago = as.integer(date - last_seen)) %>%
  filter(!is.na(count))

# Our model will end up looking like this:
# # New species seen = # seen 1 day ago + # seen 2 days agon + ... + julian day
new_results <- results %>%
  group_by(location, date) %>%
  summarize(new_seen = sum(seen),
            back_1 = sum(days_ago == 1), # better way?
            back_2 = sum(days_ago == 2),
            back_3 = sum(days_ago == 3),
            back_4 = sum(days_ago == 4)) %>%
  mutate(julian_day = lubridate::yday(date)) %>%
  arrange(julian_day) %>%
  ungroup() # Critical for testing/training split

train <- new_results %>%
  filter(date != max(date))

pois_model <- glm(new_seen ~ back_1 + back_2 + back_3 + back_4 + julian_day,
                  family = "poisson",
                  data = train)

test <- new_results %>%
  filter(date == max(date))

predict(pois_model, newdata = test)

# A linear regression model, but not the best (or appropriate, really)
# lr_model <- lm(new_seen ~ back_1 + back_2 + back_3 + back_4 + julian_day, 
#                data = train)
