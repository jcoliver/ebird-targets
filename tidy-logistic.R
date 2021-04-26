# Use tidymodels approach for logistic regression model selection
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-04-19

# See materials on lessons 10 & 11
# 10 https://www.tmwr.org/resampling.html
# 11 https://www.tmwr.org/compare.html

library(tidymodels)
library(workflowsets)
library(tidyposterior)
library(rstanarm)

# Read in data
results <- readr::read_csv(file = "data/target-results.csv")

# Calculate how many days since survey date species was last seen
results <- results %>%
  mutate(days_ago = as.integer(date - last_seen),
         julian_day = lubridate::yday(date)) %>%
  filter(!is.na(count))

# Calculate number of visits to site
visits <- results %>%
  select(c(location, date)) %>%
  distinct() %>%
  group_by(location) %>%
  arrange(date) %>%
  mutate(visit = 1:n()) %>%
  ungroup()

# Add visits back to data frame
full_results <- results %>%
  inner_join(visits, 
             by = c("location", "date"))

# Split testing from training
full_split <- initial_split(data = full_results,
                            prop = 0.8)
full_train <- training(full_split)
full_test <- testing(full_split)

# Create folds for cross-validation
full_folds <- vfold_cv(data = full_train,
                       v = 10)

# Create a simple logistic model
recipe_one <- full_train %>%
  recipe(seen ~ days_ago) %>%
  step_bin2factor(seen)

# Model with julian day also included
recipe_two <- full_train %>%
  recipe(seen ~ days_ago + julian_day) %>%
  step_bin2factor(seen)

# Third parameter is number of visits to that site
recipe_three <- full_train %>%
  recipe(seen ~ days_ago + julian_day + visit) %>%
  step_bin2factor(seen)

# Bundle all the recipes for application with workflows
model_set <- list(days_only = recipe_one,
                  days_julian = recipe_two,
                  visit_num = recipe_three)

# Create the engine that will do the model fitting
log_model <- logistic_reg() %>% 
  set_engine("glm", family = "binomial")

# Create workflow set, allowing automated resampling
wflws <- workflow_set(preproc = model_set,
                      models = list(logits = log_model),
                      cross = FALSE)

### Test on one of the models
# simple_wflow <- workflow() %>%
#   add_model(log_model) %>%
#   add_recipe(recipe_one)
# glm_fit <- fit(simple_wflow, full_train)

# Settings for resampling that we will use
keep_pred <- control_resamples(save_pred = TRUE, 
                               save_workflow = TRUE)

# Use workflows to fit each model with 10-fold cross-validation
logit_models <- wflws %>%
  workflow_map("fit_resamples", 
               # Passed to workflow_map():
               verbose = TRUE, 
               # Passed to fit_resamples()
               resamples = full_folds,
               control = keep_pred)

# Can look at accuracy
collect_metrics(logit_models) %>% 
  filter(.metric == "accuracy")
autoplot(logit_models, metric = "accuracy")

# Or look at area under the curve
collect_metrics(logit_models) %>% 
  filter(.metric == "roc_auc")
autoplot(logit_models, metric = "roc_auc")

# For now, focus on accuracy and compare models
accu_anova <- perf_mod(logit_models, metric = "accuracy",
                       prior_intercept = rstanarm::student_t(df = 1),
                       chains = 4, 
                       iter = 4000)

# Take a random sample of the posterior distribution of accuracy
models_post <- accu_anova %>%
  tidy(seed = 20210426)

# And plot the histograms
models_post %>%
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(mapping = aes(x = posterior)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ model, ncol = 1)

model_post %>% 
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, col = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1) + 
  labs(x = expression(paste("Posterior for mean ", R^2)))

