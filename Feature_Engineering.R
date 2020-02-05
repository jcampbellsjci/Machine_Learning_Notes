#### Data Load ####

library(rvest)
library(tidyverse)

pacers <- "http://www.basketball-reference.com/teams/IND/2017/gamelog/" %>%
  read_html('#tgl_basic') %>%
  html_table() %>%
  .[[1]] %>%
  .[, 2:23] %>%
  `colnames<-` (make.names(.[1, ], unique = T)) %>%
  filter(Date != "Date" & Date != "") %>%
  mutate(X = ifelse(X == "@", "A", "H")) %>%
  mutate_at(.vars = vars(Tm:TOV), .funs = funs(as.numeric)) %>%
  rename(H.A = X)


#### Feature Engineering ####

library(recipes)
library(tidyverse)

# Creating a recipe in which we'll add onto throughout this section
pacers_recipe <- recipe(W.L ~ ., data = pacers)

# We can transform continuous variables to have a more normal distribution
# Taking the natural log will fix a right skew
# We could also use a Box-Cox transformation to find the ideal transformation
# Looks over all possible exponent transformations from -5 to 5
# Finds the one that is the closest to normality
pacers %>%
  mutate(STL_LOG = log(STL)) %>%
  gather(STL, STL_LOG, key = METRIC, value = VALUE) %>%
  ggplot(aes(x = VALUE)) +
  geom_density() +
  facet_wrap(~ METRIC, scales = "free")

pacers_recipe %>%
  step_BoxCox(STL)


# There are two types of missing values
# Informative missingness: structure related to the missingness
# Missing at random: missingness is unrelated to data collection process

# Imputation is the process of replacing a missing value with a best guess
# Should be done in the resampling process to prevent data leakage

# Estimated statistic: using something like the mean or median to replace
# missing values
# Doesn't take any other data into consideration
# Could be improved by being a grouped statistic
pacers_recipe %>%
  step_medianimpute(STL)

# KNN: using K nearest neighbors to impute values
# Becomes computationally burdonsome with larger data sets
pacers_recipe %>%
  step_knnimpute(STL, neighbors = 5)

# Bagged decision trees: modeling technique used to predict missing values
pacers_recipe %>%
  step_bagimpute(STL)


# Most of the time, we'll run into features that are unimportant
# We can use different methods of feature filtering to remove them

# Zero variance variables have only one unique value
# nzv can be used for near zero variance
pacers_recipe %>%
  step_nzv(all_predictors())
# zv can be used for zero variance
pacers_recipe %>%
  step_zv(all_predictors())


# Models that employ linear functions might require standardization
# Standardization puts all variables on the same scale
# Includes centering and scaling
# Numeric variables have zero mean and unit variance
pacers_recipe %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())


# Categorical feature engineering

# For variables that have sparse levels, we can lump those levels into one
# Have to specify the threshhold to determine what levels get lumped
pacers_recipe %>%
  step_other(H.A, threshold = .1)

# Most models require numeric input
# In that case, we have to encode categorical features
# One hot encoding creates a column for each level
# Creates perfect colinearity
pacers_recipe %>%
  step_dummy(all_nominal(), one_hot = T)
# Dummy variables removes one of the levels to solve collinearity issue
pacers_recipe %>%
  step_dummy(all_nominal(), one_hot = F)


# Dimension reduction can be used to filter out uninformative information
# We can use step_pca to perform PCA while maintaining x% of variability
pacers_recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric(), threshold = .95)


# We can put this all together using other recipes functions
preprocessed_pacers <- pacers_recipe %>%
  step_nzv(all_predictors()) %>%
  step_other(all_nominal(), threshold = .1) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  # Train the blueprint on the data
  # This should be done on the training data to prevent leakage
  prep(pacers) %>%
  # Apply the preprocessing to the data set
  bake(pacers)