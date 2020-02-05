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


#### Sampling ####

library(rsample)
library(tidyverse)

# We want a model that fits well to past data AND future data
# This is called generalizability

# We split our data into a training set to train
# And a testing set to provide an unbiased estimate of our error
# This is our generalization error

# Simple random sampling does not take any data attribute into account
set.seed(123)
random_sample <- initial_split(pacers, prop = .7)
data_training <- training(random_sample)
data_testing <- testing(random_sample)

# With enough data, we'll see similar distributions to the actual
data_training %>%
  mutate(split = "training") %>%
  bind_rows(data_testing %>%
              mutate(split = "testing")) %>%
  ggplot(aes(x = FG., col = split)) +
  geom_density()

# Stratified random sampling controls sampling so that training and
# testing have similar Y distributions
# More common for imbalanced classification problems
# Could be used with skewed regression problems
# Response is split into quantiles and sampled from there
set.seed(123)
stratified_sample <- initial_split(pacers, prop = .7, strata = W.L)
data_training <- training(stratified_sample)
data_testing <- testing(stratified_sample)
table(data_training$W.L) %>%
  prop.table()
table(data_testing$W.L) %>%
  prop.table()

# We can also create bootstrap samples
# These samples are the same size as the OG data set
# They are sampled with replacement
# Samples not included are considered OOB
set.seed(123)
data_bootstrap <- bootstraps(pacers, times = 10)
data_bootstrap

