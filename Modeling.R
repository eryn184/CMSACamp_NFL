#PURPOSE: Random Independent Model Work



# rf all vars, all data ---------------------------------------------------


plays_model_data <- plays %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_height, rb_deep, is_fullback)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

et.seed(998)
# create a testing and training set
in_training <- createDataPartition(plays_model_data$pass_or_run, p = .75, list = FALSE)
training_all <- plays_model_data[ in_training,]
testing_all  <- plays_model_data[-in_training,]


# specify that the resampling method is 
fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  classProbs = TRUE)

# run a random forest model
set.seed(825)
rf_fit <- train(pass_or_run ~ ., 
                data = training_all, 
                method = "ranger",
                trControl = fit_control,
                preProcess(Center, Scale))
rf_fit

# predict the outcome on a test set
all_rf_pred <- predict(rf_fit, testing_all)
# compare predicted outcome and true outcome
confusionMatrix(all_rf_pred, as.factor(testing_all$pass_or_run))





# Logistic Regression Using GLM -------------------------------------------


simple_data_model <- plays %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber,
         is_shotgun, half_seconds_remaining, score_differential)

set.seed(123)

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

training.samples <- simple_data_model$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train_simple  <- simple_data_model[training.samples, ]
test_simple <- simple_data_model[-training.samples, ]

glmmodel_simple <- glm(pass_or_run ~ quarter + down + yardsToGo + yardlineNumber +
                       is_shotgun + half_seconds_remaining + score_differential + down * yardsToGo,
                       data = train_simple, family = binomial)

summary(glmmodel_simple)

probabilities <- glmmodel_simple %>% predict(test_simple, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test_simple$pass_or_run)

#simple, no interactions: 0.7265306 Accuracy
#simple, interaction between down and yards to go





set.seed(234)

plays_model_data <- plays %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_depth, rb_deep, is_fullback, linemen_width_sd, width_sd)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

training.samples <- plays_model_data$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train_tracking  <- plays_model_data[training.samples, ]
test_tracking <- plays_model_data[-training.samples, ]

glmmodel_tracking1 <- glm(pass_or_run ~ quarter + down + yardsToGo + yardlineNumber + 
                            num_rb + num_wr + num_te +
                            is_shotgun + is_under_center + is_pistol + defendersInTheBox +
                            half_seconds_remaining + score_differential + width + linemen_width + prev_pass +
                            linemen_depth + rb_deep + is_fullback + linemen_width_sd + width_sd 
                          + linemen_width_sd * num_rb + down * yardsToGo,
                          data = train_tracking, family = binomial)

glmmodel_tracking2 <- glm(pass_or_run ~ quarter + down + yardsToGo + yardlineNumber + 
                         num_rb + num_wr + num_te +
                        is_shotgun + is_under_center + is_pistol + defendersInTheBox +
                         half_seconds_remaining + score_differential + width + linemen_width + prev_pass +
                         linemen_depth + rb_deep + is_fullback + linemen_width_sd + width_sd 
                        + linemen_width_sd * num_rb + rb_deep * is_under_center + 
                          score_differential * half_seconds_remaining + down * yardsToGo
                        + is_under_center * linemen_depth,
                         data = train_tracking, family = binomial)

summary(glmmodel_tracking1)

probabilities <- glmmodel_tracking1 %>% predict(test_tracking, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test_tracking$pass_or_run)
#tracking, no interactions: 0.7219955 accuracy
#tracking, all plus down * yardsToGo + linemen_width_sd * num_rb: 0.7378685

library(DescTools)
BrierScore(glmmodel_tracking)

# trying glmnet -----------------------------------------------------------

library(glmnet)

plays_model_data <- plays %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_depth, rb_deep, is_fullback, linemen_width_sd, width_sd)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

training.samples <- plays_model_data$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train_tracking  <- plays_model_data[training.samples, ]
test_tracking <- plays_model_data[-training.samples, ]

train_tracking_x <- train_tracking %>%
  dplyr:: select(-pass_or_run)

train_tracking_x <- data.matrix(train_tracking_x)

test_tracking_x <- test_tracking %>%
  dplyr:: select(-pass_or_run)
test_tracking_x <- data.matrix(test_tracking_x)


fit <- cv.glmnet(train_tracking_x, train_tracking$pass_or_run, family = "binomial")

probabilities <- predict(fit, newx = test_tracking_x, type = "response", s = "lambda.1se")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

mean(predicted.classes == test_tracking$pass_or_run)

