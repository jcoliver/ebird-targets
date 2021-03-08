# Try logistic regression for predictive model
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-01-05

rm(list = ls())

################################################################################
library(readr)
library(dplyr)
library(lubridate)

results <- readr::read_csv(file = "data/target-results.csv")

results <- results %>%
  mutate(days_ago = as.integer(date - last_seen),
         julian_day = lubridate::yday(date)) %>%
  filter(!is.na(count))

train <- results %>%
  filter(date != lubridate::today(tzone = "MST"))

model_1 <- glm(seen ~ days_ago, 
               family = "binomial",
               data = train)

model_2 <- glm(seen ~ count + days_ago, 
                  family = "binomial",
                  data = train)

model_3 <- glm(seen ~ sqrt(count) + days_ago, 
               family = "binomial",
               data = train)

model_4 <- glm(seen ~ I(count - 1) + days_ago, 
               family = "binomial",
               data = train)

model_5 <- glm(seen ~ count + days_ago + julian_day, 
               family = "binomial",
               data = train)

model_6 <- glm(seen ~ days_ago + julian_day, 
               family = "binomial",
               data = train)

anova(model_1, model_2, test = "Chisq")
anova(model_1, model_3, test = "Chisq")
anova(model_1, model_4, test = "Chisq")
anova(model_1, model_5, test = "Chisq")
anova(model_1, model_6, test = "Chisq")

# predicted_probs <- predict(model_2, newdata = results, type = "response")

test <- results %>%
  filter(date == lubridate::today(tzone = "MST"))

predicted_probs <- predict(model_6, newdata = test, type = "response")

predicted_species <- sum(predicted_probs)
# 23
observed_species <- sum(today$seen)
# 23

# Probability of seeing a species:
# p = 1/(1 - e^-(b0 + b1x1 + b2x2))
# OR, 
# p = 1/(1 - e^-(l))
# where l is the log-odds