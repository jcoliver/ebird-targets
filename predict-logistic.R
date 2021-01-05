# Try logistic regression for predictive model
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-01-05

rm(list = ls())

################################################################################
library(readr)
library(dplyr)

results <- readr::read_csv(file = "data/target-results.csv")

results <- results %>%
  mutate(days_ago = as.integer(date - last_seen)) %>%
  filter(!is.na(count))

model_1 <- glm(seen ~ days_ago, 
               family = "binomial",
               data = results)

model_2 <- glm(seen ~ count + days_ago, 
                  family = "binomial",
                  data = results)

anova(model_1, model_2, test = "Chisq")
