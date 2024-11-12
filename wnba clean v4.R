library(readxl)
library(zoo)
library(tidyverse)
library(lubridate)
library(data.table)
library(lightgbm)

random_number <- sample(1:10000, 1)
set.seed(352)

#import data
data <- read.csv("WNBA_Game_Data - Sheet1.csv")
season_data <- as.data.frame(read_excel("Season_Breakdown.xlsx"))
time_zone_change <- as.matrix(read.csv("time_zone_transition.csv", row.names = 1))
travel_dist <- as.matrix(read.csv("travel_dist.csv", row.names = 1))
prediction_games <- read.csv("Upcoming Schedule - Sheet1.csv")
df <- data


#PREPARE PAST AND FUTURE DATA FOR BIND...THEN BIND...(yuh yuh)
df <- df %>% 
  mutate(
    Home = ifelse(grepl("@", Match.Up), 0, 1),
    Opp = substr(Match.Up, nchar(Match.Up) - 2, nchar(Match.Up)),
    Date = mdy(Game.Date),
    future = 0,
    Result = (Result == 'W')*1
         )

prediction_games <- prediction_games %>%
  mutate(
        Date = mdy(Date),
        future = 1,
  )%>%
  filter(Date >= Sys.Date())%>%
  group_by(Team)%>%
  arrange(Date)%>%
  slice_min(Date)
  
df <- bind_rows(df,prediction_games)


#add season and regular season
season_data$Start <- ymd(season_data$Start)
season_data$End <- ymd(season_data$End)

season_data <- season_data %>%
  mutate(
    Start = ymd(Start),
    End = ymd(End),
    GameType = (GameType == 'Regular')*1
  )

#assign season and gameType (regular season vs playoff)
expanded_season_data <- season_data %>%
  rowwise() %>%
  do(data.frame(Date = seq(.$Start, .$End, by = "day"), Season = .$Season, GameType = .$GameType)) %>%
  ungroup()

# Merge df with expanded_season_data to get Season and gametype
df <- df %>%
  left_join(expanded_season_data, by = "Date")

#make a unique faceoff factor
df <- df %>%
  rowwise() %>%
  mutate(game_id = paste(paste(sort(c(Team, Opp)), collapse = "_"),"_",Date)) %>%
  ungroup()

lag_cols = c("stadium","Result","Date","TOV","AST","OREB",
             "FTA","FGA","Opp_PTS","PTS","OREB","PlusMinus")
mav_cols = c("lag_PTS","lag_TO_Ratio","lag_Possessions","lag_AST_TO_Ratio","lag_off_rtg")

#adding and mutating Team & Season based predictors
df <- df %>%
  group_by(Team, Season) %>%
  arrange(Date) %>% 
  
  #new predictors
  mutate(
    games_into_season = row_number(), #games into season
    stadium = factor(ifelse(Home, Team, Opp)),
    Opp_PTS = PTS + PlusMinus
    )%>%
  
  #lag columns
  mutate(across(all_of(lag_cols), 
                ~lag(.x), 
                .names = "lag_{.col}"))%>%
  mutate(
    #calculated values
    lag_TO_Ratio = lag_TOV / (lag_FGA + 0.44 * lag_FTA + lag_TOV),
    lag_AST_TO_Ratio = lag_AST / lag_TOV,
    lag_Possessions = lag_FGA - lag_OREB + lag_TOV + 0.44 * lag_FTA,
    lag_off_rtg = (lag_PTS / lag_Possessions) * 100,
    Rest_Days = as.numeric(difftime(Date, lag(Date), units = "days")),
    lag_def_rtg = (lag_Opp_PTS/lag_Possessions) * 100
    
  )%>%
  ungroup()%>%
  #filter(complete.cases(.)) %>% #add a clause that will select(.,All columns of post game info)
  
  group_by(Team, Season) %>%
  mutate(
    across(all_of(mav_cols), 
           ~rollapply(.x,width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE), 
           .names = "MAV3_{.col}")
  )%>%
  
  filter(!is.na(lag_stadium))%>%

  
  #win streak and loss streak
  mutate(
    # Grouping variable increments on each loss (for win streak calculation)
    win_group = cumsum(lag_Result == 0),
    win_streak_bef = ave(lag_Result, win_group, FUN = function(x) {
      cumsum(x) - cummax((x == 0) * cumsum(x))}),
    # Grouping variable increments on each win (for loss streak calculation)
    lag_Result_L = as.integer(lag_Result == 0),
    loss_group = cumsum(lag_Result == 1),
    loss_streak_bef = ave(lag_Result_L, loss_group, FUN = function(x){
      cumsum(x) - cummax((x == 0) * cumsum(x))})
  )%>%
  select(-win_group, -loss_group, -lag_Result_L)%>%
  
  #cumulative column baselines
  mutate(
    bl_PTS = cummean(lag_PTS), #use overall avg as season progresses as baseline
    bl_TO_Ratio = cummean(lag_TO_Ratio),
    bl_AST_TO_Ratio = cummean(lag_AST_TO_Ratio),
    bl_Possessions = cummean(lag_Possessions),
    bl_off_rtg = cummean(lag_off_rtg),
    bl_df_rtg = cummean(lag_def_rtg)
  )%>%
  
  filter(!is.na(MAV3_lag_off_rtg))%>%
  #rolling averages

  ungroup()

# df <- df %>%
#   group_by(Team,Season)%>%
#   mutate(
#     better_PTS = as.integer(MAV3_lag_PTS > bl_PTS),
#     better_TO = as.integer(MAV3_lag_TO_Ratio > bl_TO_Ratio),
#     better_POS = as.integer(MAV3_lag_Possessions > bl_Possessions),
#     better_AST = as.integer(MAV3_lag_AST_TO_Ratio > bl_AST_TO_Ratio),
#     better_off_rtg = as.integer(MAV3_lag_off_rtg > bl_off_rtg)
#   )
#travel fatigue columns
df <- df %>%
  group_by(Team, Season) %>%
  mutate(stadium = factor(ifelse(Home, Team, Opp)))%>% 
  rowwise() %>%                                          
  mutate(dist_traveled = travel_dist[lag_stadium,stadium],
         time_changed = time_zone_change[lag_stadium,stadium])%>%
  mutate(log_dist = log(dist_traveled+1))%>%
  ungroup()

#make opp columns
opp_cols <- c("games_into_season","lag_PlusMinus","lag_Possessions","Rest_Days","win_streak_bef",
              "loss_streak_bef","bl_PTS","bl_TO_Ratio","bl_AST_TO_Ratio","bl_Possessions","bl_off_rtg",
              "bl_df_rtg","MAV3_lag_PTS","MAV3_lag_TO_Ratio","MAV3_lag_Possessions","MAV3_lag_AST_TO_Ratio",
              "MAV3_lag_off_rtg","dist_traveled","time_changed")

df <- df %>%
  arrange(game_id, Team) %>%  # Adjust sorting as needed, e.g., by Game.Date
  group_by(game_id) %>%
  mutate(across(all_of(opp_cols), 
                ~lag(.x, 1), 
                .names = "Opp_{.col}")) %>%
  ungroup()

# Second pass: Arrange in descending order
df <- df %>%
  arrange(game_id, desc(Team)) %>%  # Adjust sorting as needed
  group_by(game_id) %>%
  mutate(across(all_of(opp_cols), 
                ~ifelse(is.na(get(paste0("Opp_", cur_column()))), lag(.x, 1), get(paste0("Opp_", cur_column()))), 
                .names = "Opp_{.col}")) %>%
  ungroup()%>%
  filter(!is.na(Opp_games_into_season))
  #filter(complete.cases(.)) #add a clause that will select(.,All columns of post game info)

analysis.df <- df %>%
  select(-c(Match.Up, Game.Date, MIN, PTS, FGM, FGA, FG., 
            X3PTM, X3PA, X3P., FTM, FTA, FT., OREB, DREB, REB, 
            AST, TOV, STL, BLK, PF, PlusMinus, Opp_PTS,Date,lag_Date,lag_stadium))%>%
  mutate(across(where(~ is.numeric(.) && all(!is.na(.))), 
                ~ (. - min(.)) / (max(.) - min(.)))) %>%
  mutate(across(where(is.character), as.factor))


#remove future games
pred_games <- analysis.df %>%
  filter(future == 1)%>%
  select(-future, - game_id)

analysis.df <- analysis.df %>%
  filter(future == 0)%>%
  select(-future)

#create separate df's for different models?
df_unique_1 <- analysis.df %>%
  group_by(game_id) %>%
  slice_sample(n = 1) %>%
  ungroup()%>%
  select(-game_id)

# Remove the selected rows from the original dataframe to get the second sample
df_unique_2 <- analysis.df %>%
  filter(!row_number() %in% row_number(df_unique_1))%>%
  select(-game_id)

#train/test split
temp <- sort(sample(nrow(df_unique_1),nrow(df_unique_1)*0.2))
test1 <- df_unique_1[temp,]
train1 <- df_unique_1[-temp,]

test2 <- df_unique_2[temp,]
train2 <- df_unique_2[-temp,]

# Define the target variable name
target_column <- "Result"

# Extract features and labels from train1
train1_data <- as.matrix(train1[ , !names(train1) %in% target_column])  # Exclude the target column
train1_label <- train1[[target_column]]  # Extract the target column as a vector

train2_data <- as.matrix(train2[ , !names(train2) %in% target_column])  # Exclude the target column
train2_label <- train2[[target_column]]  # Extract the target column as a vector

# Extract features and labels from test1
test1_data <- as.matrix(test1[ , !names(test1) %in% target_column])  # Exclude the target column
test1_label <- test1[[target_column]]  # Extract the target column as a vector

test2_data <- as.matrix(test2[ , !names(test2) %in% target_column])  # Exclude the target column
test2_label <- test2[[target_column]]  # Extract the target column as a vector

# Convert the data to LightGBM dataset format
dtrain1 <- lgb.Dataset(data = train1_data, label = train1_label)
dtest1 <- lgb.Dataset(data = test1_data, label = test1_label, reference = dtrain1)

dtrain2 <- lgb.Dataset(data = train2_data, label = train2_label)
dtest2 <- lgb.Dataset(data = test2_data, label = test2_label, reference = dtrain2)


# pred_games_data <- as.matrix(pred_games[ , !names(pred_games) %in% target_column])
# 
# pred_games_label <- pred_games[[target_column]]  
# dpred_games <- lgb.Dataset(data = pred_games_data, label = pred_games_label)

