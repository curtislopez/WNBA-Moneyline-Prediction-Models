library(readxl)
library(dplyr)
library(zoo)
library(tidyverse)
library(lightgbm)

source("wnba clean v4.R")

log.mod1 <- glm(Result ~., data = train1, family = binomial)
log.mod1 <- step(log.mod1, direction = "both",
                 scope = list(lower = ~ Team + Opp + stadium, 
                              upper = ~ . - Team - Opp - stadium + Team + Opp + stadium))

log.mod2 <- glm(Result ~., data = train2, family = binomial)
log.mod2 <- step(log.mod2, direction = "both", 
                 scope = list(lower = ~ Team + Opp + stadium, 
                              upper = ~ . - Team - Opp - stadium + Team + Opp + stadium))

# Set parameters for the LightGBM model
params <- list(
  objective = "binary",
  metric = "binary_logloss",
  boosting_type = "gbdt",
  num_leaves = 25,
  learning_rate = 0.05,
  feature_fraction = 0.9
)

# Train the model
tree.mod1 <- lgb.train(
  params = params,
  data = dtrain1,
  nrounds = 150,
  valids = list(test = dtest1),
  early_stopping_rounds = 10,
  verbose = 1
)

tree.mod2 <- lgb.train(
  params = params,
  data = dtrain2,
  nrounds = 150,
  valids = list(test = dtest2),
  early_stopping_rounds = 10,
  verbose = 1
)


