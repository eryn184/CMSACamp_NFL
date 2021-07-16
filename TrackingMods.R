#PURPOSE: Predicting Run vs Pass Incorporating NFL Tracking Data
#AUTHOR:Nicole Tucker and Tej Seth



# Load Packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vip)
library(data.table)
library(ggthemes)
library(caret)
library(randomForest)
library(nflfastR)
library(ggimage)


# Load Data ---------------------------------------------------------------

#Load Tabular Data
plays <- read_csv("Data/plays.csv")
players <- read_csv("Data/players.csv")
games <- read_csv("Data/games.csv")

#FastR Data
pbp_17 <- nflfastR::load_pbp(2017)

#Single Tracking File
tracking_game_2017090700 <- read_csv("Data/tracking_gameId_2017090700.csv")
  
##Reading All tracking data

#weeks of NFL season'
ID <- c("090700", "091000", "091001", "091002", "091003", "091004", "091005", "091007", "091008", "091009", "091010",
        "091011", "091012", "091100", "091101", "091400", "091700", "091701", "091702", "091703", "091704","091705",
        "091706", "091707", "091708", "091709","091710", "091711", "091712", "091713", "091800", "092100", "092401",
        "092402", "092403", "092404", "092405", "092406", "092407","092408", "092409", "092410", "092411", "092412",
        "092413", "092500", "092800", "100100", "100101", "100102", "100103", "100104", "100105", "100106","100107",
        "100108", "100109", "100110", "100111", "100112", "100113", "100200", "100500", "100800", "100801","100802", 
        "100803", "100804", "100805", "100806", "100807", "100808", "100809", "100810", "100811", "100900", "101200",
        "101500","101501", "101502", "101503", "101504", "101505", "101506", "101507", "101508", "101509", "101510",
        "101511", "101600")

#blank dataframe to store tracking data
df_tracking <- list()

#iterating through all weeks
for(i in 1:length(ID)){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("Data/tracking_gameId_2017",ID[i],".csv"),
                               col_types = cols())
  
  df_tracking[[i]] <- df_tracking_temp
  
}

tracking <- rbindlist(df_tracking)

# Pass or Run- Response Var ---------------------------------------------------
plays <- plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(pass_or_run = ifelse(is.na(PassLength), "Run", "Pass")) %>%
  select(gameId, playId, quarter, GameClock, down, yardsToGo, possessionTeam, yardlineSide, 
         yardlineNumber, offenseFormation, personnel.offense,pass_or_run, playDescription, defendersInTheBox, playDescription)


# Adding Positions to Tracking Data ---------------------------------------

#create df of player positions and corresponding nflid
player_positions <- players %>%
  select(nflId, PositionAbbr)

#join player positions to tracking data by nflid
tracking <- right_join(player_positions, tracking,
                       by = c("nflId"))

# Adding Week to Plays Data --------------------------------------------


games_select <- games %>%
  select(gameId, week)

games_select$gameId <- as.character(games_select$gameId)
plays$gameId <- as.character(plays$gameId)

plays <- plays %>%
  left_join(games_select, by = c("gameId"))

#Adding Sacked? Column

plays <- plays %>%
  mutate(sacked = ifelse(grepl("sacked", plays$playDescription), TRUE, FALSE))

plays <- plays %>%
  mutate(pass_or_run = ifelse(sacked == TRUE, "Pass", pass_or_run))


# Offense Personnel Vars --------------------------------------------------------------

plays <- plays %>%
  mutate(len = nchar(personnel.offense)) %>%
  filter(len < 31) 

plays <- plays %>%
  mutate(num_rb = case_when(
    len == 27 ~ substring(plays$personnel.offense, 7, 7),
    len == 22 ~ substring(plays$personnel.offense, 7, 7),
    len == 16 ~ substring(plays$personnel.offense, 1, 1)))

plays <- plays %>%
  mutate(num_wr = case_when(
    len == 27 ~ substring(plays$personnel.offense, 19, 19),
    len == 22 ~ substring(plays$personnel.offense, 19, 19),
    len == 16 ~ substring(plays$personnel.offense, 13, 13)))

plays <- plays %>%
  mutate(num_te = case_when(
    len == 27 ~ substring(plays$personnel.offense, 7, 7),
    len == 22 ~ substring(plays$personnel.offense, 7, 7),
    len == 16 ~ substring(plays$personnel.offense, 7, 7)))

plays <- plays %>%
  select(-personnel.offense, -len)

plays$num_rb <- as.numeric(plays$num_rb)
plays$num_te <- as.numeric(plays$num_te)
plays$num_wr <- as.numeric(plays$num_wr)

plays <- plays %>%
  filter(!is.na(offenseFormation)) %>%
  filter(!is.na(num_rb)) %>%
  filter(!is.na(yardlineNumber))


# QB Position Vars --------------------------------------------------------

plays <- plays %>%
  mutate(is_shotgun = case_when(
    offenseFormation == "SHOTGUN" | offenseFormation == "EMPTY" ~ 1,
    TRUE ~ 0
  ),
  is_under_center = case_when(
    offenseFormation == "SINGLEBACK" | offenseFormation == "I_FORM" |
      offenseFormation == "JUMBO" | offenseFormation == "ACE" ~ 1,
    TRUE ~ 0
  ),
  is_pistol = ifelse(offenseFormation == "PISTOL", 1, 0),
  is_wildcat = ifelse(offenseFormation == "WILDCAT", 1, 0))



# Line of Scrimmage/Center Location at Snap -------------------------------------------------------

plays$gameId <- as.character(plays$gameId)
tracking$gameId <- as.character(tracking$gameId)

#create df containing the x coords of Football at Snap
los <- tracking %>%
  filter(displayName == "football", frame.id == 1)%>%
  select(gameId, playId, x)
names(los)[3] <- c("los")


#join LOS x coords to plays data
plays <- plays %>%
  left_join(los, by = c("gameId", "playId")) 

#join Los x coord to tracking data
tracking <- tracking %>%
  left_join(los, by = c("gameId", "playId"))



# Distance from LOS X coord/Vertically (Yards Downfield) ----------------------------------------

tracking <- tracking %>%
  mutate(VerticalYdsFromLos = abs(x - los))


# Distance from Center Y-Coord/Horizontally (sideline to sideline) -------------------------------

#create df containing the y coords of Football at Snap
ball_y <- tracking %>%
  filter(displayName == "football", frame.id == 1) %>%
  select(gameId, playId, y)
names(ball_y)[3] <- c("snap_ball_y")


#join Los y coord to tracking data
tracking <- tracking %>%
  left_join(ball_y, by = c("gameId", "playId"))

#distance from ball y
tracking <- tracking %>%
  mutate(HorizontalYdsFromLos = abs(y - snap_ball_y))


# Width of Formation at Snap ----------------------------------------------

offense_positions <- c("C", "FB", "G", "QB", "RB", "TE", "WR", "T")

plays$gameId <- as.character(plays$gameId)
tracking$gameId <- as.character(tracking$gameId) 

formation_width <- tracking %>%
  filter(PositionAbbr %in% offense_positions) %>%
    filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(width = max(y)- min(y))

plays <- plays %>%
  left_join(formation_width, by = c("gameId", "playId"))

#formation width sd
formation_width_sd <- tracking %>%
  filter(PositionAbbr %in% offense_positions) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(width_sd = sd(y))

plays <- plays %>%
  left_join(formation_width_sd, by = c("gameId", "playId"))


# Width of Linemen and Height of Linemen ----------------------------------

tracking$gameId <- as.character(tracking$gameId)
plays$gameId <- as.character(plays$gameId) 

linemen_positioning <- tracking %>%
  filter(PositionAbbr %in% c("T", "G", "C")) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(linemen_width = max(y)- min(y), 
            linemen_depth = max(x) - min(x),
            linemen_width_sd = sd(y))

plays <- plays %>%
  left_join(linemen_positioning, by = c("gameId", "playId"))


# Previous Play Var -------------------------------------------------------

plays <- plays %>%
  mutate(prev_play = lag(pass_or_run)) %>%
  mutate(prev_pass = ifelse(prev_play == "Pass", 1, 0)) %>%
  select(-prev_play)

plays$prev_pass[is.na(plays$prev_pass)] <- 0


# Deep RB ----------------------------------------------------

running_back_deep <- tracking %>%
  filter(PositionAbbr %in% c("QB", "RB")) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarize(rb_depth = max(x) - min(x))

plays <- plays %>%
  left_join(running_back_deep, by = c("gameId", "playId"))

plays <- plays %>%
  mutate(rb_deep = as.factor(ifelse(rb_depth >= 4, 1, 0))) %>%
  select(-rb_depth)


# Fullback Indicator ------------------------------------------------------

fullbacks <- tracking %>%
  filter(PositionAbbr == "FB") %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarize(is_fullback = 1)

plays <- plays %>%
  left_join(fullbacks, by = c("gameId", "playId"))

plays$is_fullback[is.na(plays$is_fullback)] <- 0


# Exploring TE Distance ----------------------------------------------------------------
#Distance from center or distance from QB?
# Use Euclidean Dist???

TE_dist <- tracking %>%
  filter(PositionAbbr %in% c("C", "TE")) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(TE_dist = max(y) - min(y))

TE_dist %>%
  filter(TE_dist < 25) %>%
  ggplot(aes(x = TE_dist)) +
  geom_histogram() +
  theme_bw()


# Hash Side- # Players on Open Field Side ---------------------------------------------------------------

ball_los <- tracking %>%
  filter(frame.id == 1) %>%
  select(gameId, playId, y) 
names(ball_los)[3] <- c("los")

ball_los <- ball_los %>%
  mutate(hash = case_when(
    los <= 24.5 ~ "L",
    los >= 29 ~ "R",
    TRUE ~ "C"
  ))



# Max Distance Receiver Starts Off Line-------------------------------------

Receivers <- c("WR", "TE")

los <- tracking %>%
  filter(frame.id == 1, displayName == "football") %>% 
  select(gameId, playId, x) 
names(los)[3] <- c("los")

tracking <- tracking %>% 
  left_join(los, by = c("gameId", "playId"))

#Distance from LOS column on TRACKING DATA
tracking <- tracking %>%
  mutate(distFromLos = abs(x - los))

#Finding Receiver Furthest From line at Snap on each play
deepest_players <- tracking %>%
  filter(frame.id == 1)%>%
  filter(PositionAbbr %in% Receivers) %>%
  group_by(gameId, playId)%>%
  filter(distFromLos == max(distFromLos))


#Distance from LOS of player farthest from LOS
max_depth_at_snap <- deepest_players %>%
  select(gameId, playId, distFromLos)
names(max_depth_at_snap)[3] <- c("receiver_offline")

plays <- plays %>%
  left_join(max_depth_at_snap, by = c("gameId", "playId"))


# In Motion ---------------------------------------------------------------

#Define- 


  

# Adding Data to Plays from FastR -----------------------------------------

pbp_17_select <- pbp_17 %>%
  select(old_game_id, play_id, half_seconds_remaining, score_differential)

plays$gameId <- as.character(plays$gameId)
pbp_17_select$old_game_id <- as.character(pbp_17_select$old_game_id)

plays <- plays %>%
  left_join(pbp_17_select, by = c("gameId" = "old_game_id", "playId" = "play_id"))



# Changing to Factors -----------------------------------------------------

plays$is_fullback <- as.factor(plays$is_fullback)
plays$is_shotgun <- as.factor(plays$is_shotgun)
plays$is_under_center <- as.factor(plays$is_under_center)
plays$is_pistol <- as.factor(plays$is_pistol)
plays$is_wildcat <- as.factor(plays$is_wildcat)
plays$prev_pass <- as.factor(plays$prev_pass)
plays$down <- as.factor(plays$down)


# Remove NAs --------------------------------------------------------------

plays <- plays %>%
  filter(!is.na(offenseFormation)) %>%
  filter(!is.na(num_rb)) %>%
  filter(!is.na(yardlineNumber)) %>%
  filter(!is.na(defendersInTheBox)) %>%
  filter(!is.na(width)) %>%
  filter(!is.na(width_sd)) %>%
  filter(!is.na(linemen_width)) %>%
  filter(!is.na(linemen_depth)) %>%
  filter(!is.na(linemen_width_sd)) %>%
  filter(!is.na(rb_deep))

colSums(is.na(plays))



# Situational Variables Model Using GLM ---------------------------------

set.seed(123)

simple_data_model <- plays %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber,
         is_shotgun, half_seconds_remaining, score_differential)

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

training.samples <- simple_data_model$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- simple_data_model[training.samples, ]
test.data <- simple_data_model[-training.samples, ]

model <- glm(pass_or_run ~., data = train.data, family = binomial)

summary(model)

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test.data$pass_or_run)


# Only Tracking Variables Model Using GLM -------------------------------

set.seed(234)

plays_model_data <- plays %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_height, rb_deep, is_fullback)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

training.samples <- plays_model_data$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- plays_model_data[training.samples, ]
test.data <- plays_model_data[-training.samples, ]

model <- glm(pass_or_run ~ num_rb + num_wr + num_te + is_shotgun + 
               is_under_center + is_pistol + is_wildcat + defendersInTheBox + width + 
               linemen_width + prev_pass + linemen_height + rb_deep + is_fullback +
               yardsToGo + yardlineNumber + score_differential, data = train.data, family = binomial)

summary(model)

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test.data$pass_or_run)


# Mix of Tracking and Situational Data Model- Caret -----------------------

model <- glm(pass_or_run ~ ., data = train.data, family = binomial)

summary(model)

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test.data$pass_or_run)





###################################################################################################

# Building Simple Predictive Model Using Tidy Models ----------------------------------------

set.seed(2014) 

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

simple_split_pbp <- initial_split(simple_data_model, 0.75, strata = pass_or_run)

simple_train_data <- training(simple_split_pbp)

simple_test_data <- testing(simple_split_pbp)

simple_pbp_rec <- recipe(pass_or_run ~ ., data = simple_train_data) %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

simple_lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

simple_lr_wflow <- workflow() %>% 
  add_model(simple_lr_mod) %>% # parsnip model
  add_recipe(simple_pbp_rec)   # recipe from recipes

simple_pbp_fit_lr <- simple_lr_wflow %>% 
  fit(data = simple_train_data) # fit the model against the training data

simple_pbp_pred_lr <- predict(simple_pbp_fit_lr, simple_test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(simple_pbp_fit_lr, simple_test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(simple_test_data %>% select(pass_or_run))

simple_pbp_pred_lr %>% 
  group_by(.pred_class, pass_or_run) %>%
  summarize(perc = n() / nrow(simple_pbp_pred_lr)) %>%
  arrange(-perc)

table("Predictions" = simple_pbp_pred_lr$.pred_class, "Observed" = simple_pbp_pred_lr$pass_or_run)

`simple_pbp_pred_lr` %>% # Simple Logistic Regression predictions
  metrics(truth = pass_or_run, .pred_class)


# Building Model with Situational and Tracking Data Using TidyModels ---------------------------------------

set.seed(2016) #go lions

plays_model_data <- plays %>%
  filter(!is.na(formation_width)) %>%
  select(pass_or_run, quarter, week, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, formation_width, linemen_width, prev_pass, oline_curve_radius)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

split_pbp <- initial_split(plays_model_data, 0.75, strata = pass_or_run)

train_data <- training(split_pbp)

test_data <- testing(split_pbp)

pbp_rec <- recipe(pass_or_run ~ ., data = train_data) %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)   # recipe from recipes

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data) # fit the model against the training data

pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(pass_or_run))

pbp_pred_lr %>% 
  group_by(.pred_class, pass_or_run) %>%
  summarize(perc = n() / nrow(pbp_pred_lr)) %>%
  arrange(-perc)

table("Predictions" = pbp_pred_lr$.pred_class, "Observed" = pbp_pred_lr$pass_or_run)

model_stats <- lr_wflow %>% 
  finalize_workflow(pbp_fit_lr) %>%
  fit(train_data) %>%
  pull_workflow_fit() %>%
  tidy()

model_stats %>%
  mutate(ord_term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = ord_term)) +
  geom_bar(aes(fill = ifelse(estimate > 0, "darkblue", "darkred")), stat = "identity") +
  scale_color_identity(aesthetics = c("fill")) +
  theme_bw() +
  theme(legend.position = "none")

rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe

pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics

pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(pass_or_run)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

`pbp_pred_rf` %>% # Random Forest predictions
  metrics(truth = pass_or_run, .pred_class)
#73.2

`pbp_pred_lr` %>% # Logistic Regression predictions
  metrics(truth = pass_or_run, .pred_class)
#73.0%

roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Random Forest")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Logistic Regression")

bind_rows(roc_rf, roc_lr) %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  theme_bw() +
  scale_color_manual(values = c("#374785", "#E98074")) +
  theme(legend.position = "top")

############################################################################
set.seed(1991) 

xg_model_data <- plays_model_data %>%
  mutate(label = ifelse(pass_or_run == "Pass", 1, 0)) %>%
  select(-pass_or_run)

nrounds <- 1121
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("error", "logloss"),
    eta = .015,
    gamma = 2,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 7,
    min_child_weight = 0.9
  )

cv_results <- map_dfr(1:6, function(x) {
  test_data <- xg_model_data %>%
    filter(week == x) %>%
    select(-week)
  train_data <- xg_model_data %>%
    filter(week != x) %>%
    select(-week)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-label)),
                                     label = train_data$label
  )
  xp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(xp_model, as.matrix(test_data %>% select(-label))))
  ) %>%
    dplyr::rename(xp = V1)
  
  cv_data <- bind_cols(test_data, preds) %>% mutate(week = x)
  return(cv_data)
})

cv_results <- cv_results %>%
  mutate(actual_result = ifelse(label == 1, "Pass", "Run"),
         pred = ifelse(xp >= 0.50, "Pass", "Run"),
         is_right = ifelse(actual_result == pred, 1, 0))

mean(cv_results$is_right)








