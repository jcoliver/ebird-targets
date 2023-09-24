# Poisson model for predicting number of new species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-04-26

################################################################################
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

results <- readr::read_csv(file = "data/target-results.csv")

# Create days ago column
results <- results %>%
  mutate(days_ago = as.integer(date - last_seen))

# Drop any records without a count
# results <- results %>%
#   filter(!is.na(count))

# Our model will end up looking like this:
# # New species seen = # seen 1 day ago + # seen 2 days ago + ... + julian day
# Start by counting how many new species seen for each "days ago" category
new_results <- results %>%
  group_by(location, date, days_ago) %>%
  summarize(num_species = n()) %>%
  ungroup()

# Transform to wide so one column for each "days ago" category and replace any 
# NAs in those columns with zeros (using lambda function!)
results_wide <- new_results %>%
  mutate(days_ago = paste0("da_", days_ago)) %>%
  pivot_wider(id_cols = c(location, date), 
              names_from = days_ago, 
              values_from = num_species) %>%
  mutate(across(starts_with("da_"), ~replace_na(data = ., replace = 0)))

# Split into testing/training, start by assigning folds
results_wide <- results_wide %>%
  rowwise() %>%
  mutate(fold = sample(x = 1:5, size = 1))

# Testing fold is 1
results_test <- results_wide %>%
  filter(fold == 1)

# Training fold is != 1
results_train <- results_wide %>%
  filter(fold != 1)

# Build the model
pred_index <- grep(x = names(results_train), 
                   pattern = "da_")

preds <- paste0(names(results_train)[pred_index], collapse = " + ")
form <- 


### OLD BELOW
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
