#  Purpose: Updating the Model Every 3 Seconds

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

#FastR Data
pbp_17 <- nflfastR::load_pbp(2017)

#Tabular Data
frame_games <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/games.csv"))
frame_players <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/players.csv"))
frame_plays <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/plays.csv"))
probs_and_preds <- read_csv("Data/probs_and_preds.csv")

#Tracking Data
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




# Add Week to Tracking Data -----------------------------------------------

week <- frame_games %>%
  select(week, gameId)

frame_tracking <- tracking %>%
  left_join(week, by = c("gameId"))

# Adding Positions to Tracking Data ---------------------------------------

#create df of player positions and corresponding nflid
player_positions <- frame_players %>%
  select(nflId, PositionAbbr)

#join player positions to tracking data by nflid
frame_tracking <- right_join(player_positions, frame_tracking,
                       by = c("nflId"))


# Adding Is Pass? ---------------------------------------------------------


frame_plays_select <- frame_plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(is_pass = ifelse(is.na(PassLength), 0, 1)) %>%
  select(gameId, playId, playDescription, is_pass)

frame_plays_select <- frame_plays_select %>%
  mutate(sacked = ifelse(grepl("sacked", frame_plays_select$playDescription), TRUE, FALSE))

frame_plays_select <- frame_plays_select %>%
  mutate(is_pass = ifelse(sacked == TRUE, 1, is_pass)) %>%
  select(-sacked, -playDescription)

frame_plays_select$gameId <- as.character(frame_plays_select$gameId)
tracking$gameId <- as.character(tracking$gameId)

#add is_pass to tracking data
frame_tracking <- tracking %>%
  left_join(frame_plays_select, by = c("gameId", "playId")) %>%
  filter(!is.na(is_pass))



# Removing Pre Snap Data --------------------------------------------------

#ball snap
ball_snaps <- frame_tracking %>%
  filter(event == "ball_snap")

snap_frame <- ball_snaps %>%
  group_by(gameId, playId) %>%
  summarise(snap_frame = round(mean(frame.id), 0))

frame_tracking <- frame_tracking %>%
  left_join(snap_frame, by = c("gameId", "playId"))

frame_tracking <- frame_tracking %>%
  filter(frame.id >= snap_frame)

#Remove NAs

frame_tracking <- frame_tracking %>%
  filter(!is.na(is_pass))


#join xp from probs and preds------------------------------------

probs_and_preds <- probs_and_preds %>%
  select(-X1)

probs_select <- probs_and_preds %>%
  select(gameId, playId, xp)

probs_select$gameId <- as.character(probs_select$gameId)

frame_tracking <- frame_tracking %>%
  left_join(probs_select, by = c("gameId", "playId"))


# Orientation of the Play -------------------------------------------------

Offense <- c("T", "C", "G", "WR","QB", "TE", "WR", "FB")
Defense <- c("CB", "OLB", "SS", "ILB", "DE", "NT", "MLB", "FS", "DT", "LB", "DB" )

offense_orientation <- frame_tracking %>%
  filter(event == "ball_snap", PositionAbbr %in% Offense) %>%
  group_by(gameId, playId) %>%
  summarise(offense_x = median(x, na.rm = T))

defense_orientation <- frame_tracking %>%
  filter(event == "ball_snap", PositionAbbr %in% Defense) %>%
  group_by(gameId, playId) %>%
  summarise(defense_x = median(x, na.rm = T))

frame_tracking <- frame_tracking %>%
  left_join(offense_orientation, by = c( "gameId", "playId"))

frame_tracking <- frame_tracking %>%
  left_join(defense_orientation, by = c( "gameId", "playId"))

frame_tracking <- frame_tracking %>%
  mutate(endzone = ifelse(offense_x > defense_x , "H", "V"))

frame_tracking <- frame_tracking %>%
  select(-offense_x, -defense_x)



# Change Coords -----------------------------------------------------------

frame_tracking <- frame_tracking %>%
  mutate(x = ifelse(endzone == "H", 120-x, x)) %>%
  mutate(y = ifelse(endzone == "H", 53.3-y, y)) %>%
  mutate(dir = ifelse(endzone == "H", (dir + 180) %% 360, dir))



# Oline Speed -------------------------------------------------------------

changes <- probs_and_preds %>%
  select(gameId, playId, is_pass, xp, week)

oline <- c("T", "C", "G")
avgspeed <- frame_tracking %>%
  filter(PositionAbbr %in% oline) %>%
  group_by(gameId, playId, frame.id) %>%
  summarise(avgspeed = mean(s))

changes$gameId <- as.character(changes$gameId)

changes <- avgspeed %>%
left_join(changes, by = c("gameId", "playId")) 
names(changes)[4] <- c("avgspeedoline")


# WR Speed ----------------------------------------------------------------

wravgspeed <- frame_tracking %>%
  filter(PositionAbbr == "WR") %>%
  group_by(gameId, playId, frame.id) %>%
  summarise(avgspeedwr = mean(s))

changes <- changes %>%
  left_join(wravgspeed, by = c("gameId", "playId", "frame.id"))



# Oline Distance Traveled -------------------------------------------------

los <- frame_tracking %>%
  filter(PositionAbbr == "C", event == "ball_snap") %>%
  select(x, gameId, playId) 
names(los)[1] <- c("los")


frame_tracking <- frame_tracking %>%
  left_join(los, by = c("gameId", "playId"))


olinedist <- frame_tracking %>%
  filter(PositionAbbr %in% oline) %>%
  group_by(gameId, playId, frame.id) %>%
  summarise(olinedist = (mean(x)-los)) %>%
  unique()


changes <- changes %>%
  left_join(olinedist, by = c("gameId", "playId", "frame.id"))


# WR Distance Traveled ----------------------------------------------------

wrdist <- frame_tracking %>%
  filter(PositionAbbr == "WR") %>%
  group_by(gameId, playId, frame.id) %>%
  summarise(wrdist = (mean(x)-los)) %>%
  unique()

changes <- changes %>%
  left_join(wrdist, by = c("gameId", "playId", "frame.id"))


#remove NAs
changes <- changes %>%
  filter(!is.na(wrdist))

changes <- changes %>%
  filter(!is.na(is_pass))

changes <- changes %>%
  filter(!is.na(avgspeedoline))

colSums(is.na(changes))



# Build Model -------------------------------------------------------------


changes <- changes %>%
  arrange(gameId, playId, frame.id) %>%
  group_by(gameId, playId) %>%
  mutate(seconds_snap = row_number()/10)

# 0-2.5 seconds

changes <- changes %>%
  filter(seconds_snap <= 2.5)

changes <- changes %>%
  ungroup() %>%
  mutate(ID = row_number())

#CHANGES CSV
write_csv(changes, "changes.csv")

#change to leave one week out
set.seed(1985)
week_fold_table <- tibble(week = unique(changes$week)) %>%
  mutate(week_fold = sample(rep(1:6, length.out = n()), n()))
plays <- changes %>% dplyr::left_join(game_fold_table, by = "gameId")



logit_cv_preds <- 
  map_dfr(unique(plays$game_fold), 
          function(test_fold) {
            # Separate test and training data:
            test_data <- plays %>%
              filter(game_fold == test_fold)
            train_data <- plays %>%
              filter(game_fold != test_fold)
            # Train model:
            logit_model <- glm(is_pass ~ xp + avgspeedoline + avgspeedwr + olinedist + wrdist, 
                               data = train_data,family = "binomial")
            # Return tibble of holdout results:
            tibble(test_pred_probs = predict(logit_model, newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$is_pass,
                   game_fold = test_fold) 
          })

logit_cv_preds %>%
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) %>%
  group_by(game_fold) %>%
  summarize(mcr = mean(test_pred != test_actual))

changes_preds <- cbind(changes, logit_cv_preds)


changes_preds <- changes_preds %>%
  mutate(pred_pass = ifelse(test_pred_probs > 0.5, 1, 0)) %>%
  mutate(is_right = ifelse(pred_pass == is_pass, 1, 0))

accuracy_sec <- changes_preds %>% 
  group_by(seconds_snap) %>% 
  summarize(count = n(), acc = mean(is_right))



library(plotROC)
logit_cv_preds %>%
  ggplot() + 
  geom_roc(aes(d = test_actual,
               m = test_pred_probs),
           labelround = 4) + 
  style_roc() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(color = "Test fold")
with(logit_cv_preds, 
     MLmetrics::AUC(test_pred_probs, test_actual))

logit_cv_preds[1:10000,] %>%
  group_by(game_fold) %>%
  summarise(auc = MLmetrics::AUC(test_pred_probs, test_actual))


# XGBoost -----------------------------------------------------------------

library(xgboost)

model_data <- changes %>%
  mutate(is_pass = as.factor(is_pass))

game_fold_table <- tibble(gameId = unique(model_data$gameId)) %>%
  mutate(game_fold = sample(rep(1:5, length.out = n()), n()))
model_data <- model_data %>% dplyr::left_join(game_fold_table, by = "gameId")
         
xgb_cv_preds <- 
  map_dfr(unique(model_data$game_fold), 
          function(test_fold) {
            # Separate test and training data - scale variables:
            test_data <- model_data %>% filter(game_fold == test_fold)
            test_data_x <- as.matrix(dplyr::select(test_data, -is_pass, -game_fold, -frame.id, -gameId, -playId))
            train_data <- model_data %>% filter(game_fold != test_fold)
            train_data_x <- as.matrix(dplyr::select(train_data, -complete_pass, -game_fold))
            train_data_y <- as.numeric(train_data$complete_pass) - 1
            xgb_model <- xgboost(data = train_data_x, label = train_data_y,
                                 nrounds = 100, max_depth = 3, eta = 0.3, 
                                 gamma = 0, colsample_bytree = 1, min_child_weight = 1,
                                 subsample = 1, nthread = 1,
                                 objective = 'binary:logistic', eval_metric = 'auc', 
                                 verbose = 0)
            # Return tibble of holdout results:
            tibble(test_pred_probs = 
                     as.numeric(predict(xgb_model, newdata = test_data_x, type = "response")),
                   test_actual = as.numeric(test_data$complete_pass) - 1,
                   game_fold = test_fold) 
          })

