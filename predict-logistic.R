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

predicted_probs <- predict(model_2, newdata = results, type = "response")

predicted_species <- sum(predicted_probs)
# 23
observed_species <- sum(results$seen)
# 23

# Probability of seeing a species:
# p = 1/(1 - e^-(b0 + b1x1 + b2x2))
# OR, 
# p = 1/(1 - e^-(l))
# where l is the log-odds