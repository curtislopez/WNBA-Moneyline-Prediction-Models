library(tidyverse)

source("wnba model analysis.R")

pred_games$log.mod1_pred <- (predict(log.mod1,pred_games,type = 'response'))
pred_games$log.mod2_pred <- round(predict(log.mod2,pred_games,type = 'response'))
pred_games$tree.mod1_pred <- predict(tree.mod1, pred_xmat)
pred_games$tree.mod2_pred <- predict(tree.mod2, pred_xmat)


pred_games %>%
  select(Team,Opp,log.mod1_pred)%>%
  mutate(conf = abs(log.mod1_pred - 0.5)*2)