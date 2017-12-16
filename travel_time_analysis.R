library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(glmnet)
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(earth)
library(leaps)
library(boot)
library(kernlab)
library(hydroGOF)
library(rpart)
library(neuralnet)

setwd("~/Desktop/CS 229 Project/New Data/")
data <- read_csv("~/Desktop/CS 229 Project/New Data/data.csv")

all <- data


### For this model, only need a few columns:

all <- all[,c(2,3,4,13,15,16,17,18,19,20,21,23,24,25,26,27)]
set.seed(111)
sample_all <- all[sample(nrow(all), nrow(all)),]
colnames(sample_all)[4] <- "travel_time"


### Dividing into test and train sets

na.omit(sample_all)
samplesize = floor(0.8 * nrow(sample_all))
set.seed(111)
train_ind = sample(seq_len(nrow(sample_all)), size = samplesize)
train = sample_all[train_ind, ]
test = sample_all[-train_ind, ]


## create test sets for 10/1

include132230 = (test$location_in == 132 & test$location_out == 230 & test$month == 10 & test$day == 1)
test_132230 = test[include132230,]

include13248 = (test$location_in == 132 & test$location_out == 48 & test$month == 10 & test$day == 1)
test_13248 = test[include13248,]

write_csv(test_132230, 'test_132230.csv')
write_csv(test_13248, 'test_13248.csv')

## create october test for two districts

include230 = (test$location_in == 132 & test$location_out == 230 & test$month == 10)
test_230 = test[include230,]

average_travel <- test_230 %>%
  group_by(hour) %>%
  summarize_at(c(4), mean, na.rm = TRUE)

test_230 = full_join(test_230, average_travel, by = "hour") 

include48 = (test$location_in == 132 & test$location_out == 48 & test$month == 10)
test_48 = test[include48,]

average_travel2 <- test_48 %>%
  group_by(hour) %>%
  summarize_at(c(4), mean, na.rm = TRUE)

test_48 = full_join(test_48, average_travel2, by = "hour") 


#### filter for airport rides going to airport and delete loc in and out variable for models

train <- train %>%
  filter(location_out == 132)


test <- test %>%
  filter(location_out == 132)


### 2 hour before train

train$hour_id = (train$hour + train$day*100 + train$month*10000)
train$hour_location_id = (train$hour_id + train$location_in*1000000)

train_mean <- train %>%
  group_by(hour_location_id) %>%
  summarize_at(c(4), mean, na.rm = TRUE)
names(train_mean)[names(train_mean) == 'hour_location_id'] <- 'location_hour_before_in'

train$two_hour_before_id = NA
train$two_hour_before_id = ifelse(train$hour_id == 010100, 123122,
                                   ifelse(train$hour_id == 010101, 123123,
                                          ifelse(train$hour_id == 020100, 013122,
                                                 ifelse(train$hour_id == 020101, 013123,
                                                        ifelse(train$hour_id == 030100, 022822,
                                                               ifelse(train$hour_id == 030101, 022823,
                                                                      ifelse(train$hour_id == 040100, 033122,
                                                                             ifelse(train$hour_id == 040101, 033123,
                                                                                    ifelse(train$hour_id == 050100, 043022,
                                                                                           ifelse(train$hour_id == 050101, 043023,
                                                                                                  ifelse(train$hour_id == 060100, 053122,
                                                                                                         ifelse(train$hour_id == 060101, 053123,
                                                                                                                ifelse(train$hour_id == 070100, 063022,
                                                                                                                       ifelse(train$hour_id == 070101, 063023,
                                                                                                                              ifelse(train$hour_id == 080100, 073122,
                                                                                                                                     ifelse(train$hour_id == 080101, 073123,
                                                                                                                                            ifelse(train$hour_id == 090100, 083122,
                                                                                                                                                   ifelse(train$hour_id == 090101, 083123,
                                                                                                                                                          ifelse(train$hour_id == 100100, 093022,
                                                                                                                                                                 ifelse(train$hour_id == 100101, 093023,
                                                                                                                                                                        ifelse(train$hour_id == 110100, 103122,
                                                                                                                                                                               ifelse(train$hour_id == 110101, 103123,
                                                                                                                                                                                      ifelse(train$hour_id == 120100, 113022,
                                                                                                                                                                                             ifelse(train$hour_id == 120101, 113023,
                                                                                                                                                                                                    train$hour_id - 2))))))))))))))))))))))))



train$location_two_hour_before_in = (train$two_hour_before_id + train$location_in*1000000)

names(train_mean)[names(train_mean) == 'location_hour_before_in'] <- 'location_two_hour_before_in'
train = full_join(train, train_mean, by = "location_two_hour_before_in")

train = train[!is.na(train$location_in),]

### 2 hour before test

test$hour_id = (test$hour + test$day*100 + test$month*10000)
test$hour_location_id = (test$hour_id + test$location_in*1000000)

test_mean <- test %>%
  group_by(hour_location_id) %>%
  summarize_at(c(4), mean, na.rm = TRUE)
names(test_mean)[names(test_mean) == 'hour_location_id'] <- 'location_hour_before_in'

test$two_hour_before_id = NA
test$two_hour_before_id = ifelse(test$hour_id == 010100, 123122,
                                  ifelse(test$hour_id == 010101, 123123,
                                         ifelse(test$hour_id == 020100, 013122,
                                                ifelse(test$hour_id == 020101, 013123,
                                                       ifelse(test$hour_id == 030100, 022822,
                                                              ifelse(test$hour_id == 030101, 022823,
                                                                     ifelse(test$hour_id == 040100, 033122,
                                                                            ifelse(test$hour_id == 040101, 033123,
                                                                                   ifelse(test$hour_id == 050100, 043022,
                                                                                          ifelse(test$hour_id == 050101, 043023,
                                                                                                 ifelse(test$hour_id == 060100, 053122,
                                                                                                        ifelse(test$hour_id == 060101, 053123,
                                                                                                               ifelse(test$hour_id == 070100, 063022,
                                                                                                                      ifelse(test$hour_id == 070101, 063023,
                                                                                                                             ifelse(test$hour_id == 080100, 073122,
                                                                                                                                    ifelse(test$hour_id == 080101, 073123,
                                                                                                                                           ifelse(test$hour_id == 090100, 083122,
                                                                                                                                                  ifelse(test$hour_id == 090101, 083123,
                                                                                                                                                         ifelse(test$hour_id == 100100, 093022,
                                                                                                                                                                ifelse(test$hour_id == 100101, 093023,
                                                                                                                                                                       ifelse(test$hour_id == 110100, 103122,
                                                                                                                                                                              ifelse(test$hour_id == 110101, 103123,
                                                                                                                                                                                     ifelse(test$hour_id == 120100, 113022,
                                                                                                                                                                                            ifelse(test$hour_id == 120101, 113023,
                                                                                                                                                                                                   test$hour_id - 2))))))))))))))))))))))))



test$location_two_hour_before_in = (test$two_hour_before_id + test$location_in*1000000)

names(test_mean)[names(test_mean) == 'location_hour_before_in'] <- 'location_two_hour_before_in'
test = full_join(test, test_mean, by = "location_two_hour_before_in")

test = test[!is.na(test$location_in),]

## imputation

train$travel_time.y = ifelse(is.na(train$travel_time.y), train$hour_before_time, train$travel_time.y)
test$travel_time.y = ifelse(is.na(test$travel_time.y), test$hour_before_time, test$travel_time.y)


## factors

train$location_in = factor(train$location_in)
train$cond = factor(train$cond)
train$hour = factor(train$hour)
train$day = factor(train$day)
train$month = factor(train$month)
train$weekday = factor(train$weekday)
train$holiday = factor(train$holiday)
train$holiday_range_short = factor(train$holiday_range_short)

test$location_in = factor(test$location_in)
test$cond = factor(test$cond)
test$hour = factor(test$hour)
test$day = factor(test$day)
test$month = factor(test$month)
test$weekday = factor(test$weekday)
test$holiday = factor(test$holiday)
test$holiday_range_short = factor(test$holiday_range_short)


## delete location variable

train <- train[,-c(2,3,17,18,19,20)]
test <- test[,-c(2,3,17,18,19,20)]

names(train)[names(train) == 'travel_time.x'] <- 'travel_time'
names(test)[names(test) == 'travel_time.x'] <- 'travel_time'
names(train)[names(train) == 'travel_time.y'] <- 'two_hour_before_time'
names(test)[names(test) == 'travel_time.y'] <- 'two_hour_before_time'

## create smaller sample for computational reasons
percentage = 75 ## 2% 

set.seed(11111)
train_sample = train[sample(nrow(train), nrow(train)/percentage),]
test_sample = test[sample(nrow(test), nrow(test)/percentage),]

train_sample <- train_sample[complete.cases(train_sample),]
test_sample <- test_sample[complete.cases(test_sample),]


#### models

# tuning

fitControl <- trainControl(
   method = "repeatedcv",
   number = 10,
   repeats = 10)


## linear regression

formula = formula(travel_time ~ . - travel_time)
regFit = glm(formula, data = train_sample)

test_sample$prediction_reg <- as.numeric(predict(regFit,test_sample))
train_sample$prediction_reg <- as.numeric(predict(regFit,train_sample))


## gradient boosting

gbmFit <- train(travel_time ~ . - travel_time, data = train_sample, 
                 method = "gbm", 
                 trControl = fitControl,
                 na.action = na.omit,
                 verbose = FALSE)

test_sample$prediction_gbm <- as.numeric(predict(gbmFit,test_sample))
train_sample$prediction_gbm <- as.numeric(predict(gbmFit,train_sample))


## neural net

elmFit <- train(travel_time ~ . - travel_time, data = train_sample, 
                method = "elm", 
                trControl = fitControl,
                na.action = na.omit,
                verbose = FALSE)

test_sample$prediction_elm <- as.numeric(predict(elmFit,test_sample))
train_sample$prediction_elm <- as.numeric(predict(elmFit,train_sample))


## pcr

pcrFit <- train(travel_time ~ . - travel_time, data = train_sample, 
                method = "pcr", 
                trControl = fitControl,
                na.action = na.omit,
                verbose = FALSE)

test_sample$prediction_pcr <- as.numeric(predict(pcrFit,test_sample))
train_sample$prediction_pcr <- as.numeric(predict(pcrFit,train_sample))



#### error metrics

# regression
reg_test_rmse = sqrt(sum((test_sample$travel_time - test_sample$prediction_reg)^2 ) / nrow(test_sample))
reg_test_rmse # 10.87
reg_test_average = (sum(sqrt((test_sample$travel_time - test_sample$prediction_reg)^2))) / nrow(test_sample)
reg_test_average # 7.41

reg_train_rmse = sqrt(sum((train_sample$travel_time - train_sample$prediction_reg)^2 ) / nrow(train_sample))
reg_train_rmse # 9.95
reg_train_average = (sum(sqrt((train_sample$travel_time - train_sample$prediction_reg)^2))) / nrow(train_sample)
reg_train_average # 7.17


# gbm
gbm_test_rmse = sqrt(sum((test_sample$travel_time - test_sample$prediction_gbm)^2 ) / nrow(test_sample))
gbm_test_rmse # 10.43
gbm_test_average = (sum(sqrt((test_sample$travel_time - test_sample$prediction_gbm)^2))) / nrow(test_sample)
gbm_test_average # 6.95

gbm_train_rmse = sqrt(sum((train_sample$travel_time - train_sample$prediction_gbm)^2 ) / nrow(train_sample))
gbm_train_rmse # 9.11
gbm_train_average = (sum(sqrt((train_sample$travel_time - train_sample$prediction_gbm)^2))) / nrow(train_sample)
gbm_train_average # 6.48


# neural net
elm_test_rmse = sqrt(sum((test_sample$travel_time - test_sample$prediction_elm)^2 ) / nrow(test_sample))
elm_test_rmse  # 11.27
elm_test_average = (sum(sqrt((test_sample$travel_time - test_sample$prediction_elm)^2))) / nrow(test_sample)
elm_test_average # 7.72

elm_train_rmse = sqrt(sum((train_sample$travel_time - train_sample$prediction_elm)^2 ) / nrow(train_sample))
elm_train_rmse # 10.45
elm_train_average = (sum(sqrt((train_sample$travel_time - train_sample$prediction_elm)^2))) / nrow(train_sample)
elm_train_average # 7.59


# pcr
pcr_test_rmse = sqrt(sum((test_sample$travel_time - test_sample$prediction_pcr)^2 ) / nrow(test_sample))
pcr_test_rmse # 10.80
pcr_test_average = (sum(sqrt((test_sample$travel_time - test_sample$prediction_pcr)^2))) / nrow(test_sample)
pcr_test_average # 7.24

pcr_train_rmse = sqrt(sum((train_sample$travel_time - train_sample$prediction_pcr)^2 ) / nrow(train_sample))
pcr_train_rmse # 9.69
pcr_train_average = (sum(sqrt((train_sample$travel_time - train_sample$prediction_pcr)^2))) / nrow(train_sample)
pcr_train_average # 6.91


##### weather test sets

include = (test_sample$cond == "Overcast" |
                 test_sample$cond == "Scattered Clouds" |
                 test_sample$cond == "Clear" |
                 test_sample$cond == "Mostly Cloudy" |
                 test_sample$cond == "Partly Cloudy")
test_sun_cloud = test_sample[include,]

include = (test_sample$cond == "Shallow Fog" |
             test_sample$cond == "Fog" |
           test_sample$cond == "Haze")
test_fog = test_sample[include,]

include = (test_sample$cond == "Shallow Fog" |
             test_sample$cond == "Light Snow" |
             test_sample$cond == "Light Ice Pellets" |
             test_sample$cond == "Heavy Snow" |
             test_sample$cond == "Ice Pellets" |
             test_sample$cond == "Light Freezing Rain")
test_snow = test_sample[include,]

include = (test_sample$cond == "Heavy Thunderstorms and Rain" |
             test_sample$cond == "Light Thunderstorms and Rain" |
             test_sample$cond == "Rain" |
             test_sample$cond == "Light Drizzle" |
             test_sample$cond == "Heavy Rain" |
             test_sample$cond == "Thunderstorms and Rain")
test_rain = test_sample[include,]


## regression test on weather

fog_reg_test_rmse = sqrt(sum((test_fog$travel_time - test_fog$prediction_reg)^2 ) / nrow(test_fog))
fog_reg_test_rmse # 4.95
fog_reg_test_average = (sum(sqrt((test_fog$travel_time - test_fog$prediction_reg)^2))) / nrow(test_fog)
fog_reg_test_average # 3.98

snow_reg_test_rmse = sqrt(sum((test_snow$travel_time - test_snow$prediction_reg)^2 ) / nrow(test_snow))
snow_reg_test_rmse # 10.05
snow_reg_test_average = (sum(sqrt((test_snow$travel_time - test_snow$prediction_reg)^2))) / nrow(test_snow)
snow_reg_test_average # 7.81

rain_reg_test_rmse = sqrt(sum((test_rain$travel_time - test_rain$prediction_reg)^2 ) / nrow(test_rain))
rain_reg_test_rmse # 10.04
rain_reg_test_average = (sum(sqrt((test_rain$travel_time - test_rain$prediction_reg)^2))) / nrow(test_rain)
rain_reg_test_average # 8.04

sun_cloud_reg_test_rmse = sqrt(sum((test_sun_cloud$travel_time - test_sun_cloud$prediction_reg)^2 ) / nrow(test_sun_cloud))
sun_cloud_reg_test_rmse # 11.02
sun_cloud_reg_test_average = (sum(sqrt((test_sun_cloud$travel_time - test_sun_cloud$prediction_reg)^2))) / nrow(test_sun_cloud)
sun_cloud_reg_test_average # 7.44


## gbm test on weather

fog_gbm_test_rmse = sqrt(sum((test_fog$travel_time - test_fog$prediction_gbm)^2 ) / nrow(test_fog))
fog_gbm_test_rmse # 4.95
fog_gbm_test_average = (sum(sqrt((test_fog$travel_time - test_fog$prediction_gbm)^2))) / nrow(test_fog)
fog_gbm_test_average # 4.03

snow_gbm_test_rmse = sqrt(sum((test_snow$travel_time - test_snow$prediction_gbm)^2 ) / nrow(test_snow))
snow_gbm_test_rmse # 8.87
snow_gbm_test_average = (sum(sqrt((test_snow$travel_time - test_snow$prediction_gbm)^2))) / nrow(test_snow)
snow_gbm_test_average # 6.96

rain_gbm_test_rmse = sqrt(sum((test_rain$travel_time - test_rain$prediction_gbm)^2 ) / nrow(test_rain))
rain_gbm_test_rmse # 10.24
rain_gbm_test_average = (sum(sqrt((test_rain$travel_time - test_rain$prediction_gbm)^2))) / nrow(test_rain)
rain_gbm_test_average # 7.80

sun_cloud_gbm_test_rmse = sqrt(sum((test_sun_cloud$travel_time - test_sun_cloud$prediction_gbm)^2 ) / nrow(test_sun_cloud))
sun_cloud_gbm_test_rmse # 10.63
sun_cloud_gbm_test_average = (sum(sqrt((test_sun_cloud$travel_time - test_sun_cloud$prediction_gbm)^2))) / nrow(test_sun_cloud)
sun_cloud_gbm_test_average # 6.99


## difference for standard deviation

test_sample$sd_reg = sd(test_sample$travel_time - test_sample$prediction_reg)
test_sample$sd_gbm = sd(test_sample$travel_time - test_sample$prediction_gbm)
test_sample$sd_elm = sd(test_sample$travel_time - test_sample$prediction_elm)
test_sample$sd_pcr = sd(test_sample$travel_time - test_sample$prediction_pcr)


### morning hour and afternoon test sets

include = (test_sample$hour == 8 |
             test_sample$hour == 9 |
             test_sample$hour == 10 |
             test_sample$hour == 11)
test_morning = test_sample[include,]

include = (test_sample$hour == 16 |
             test_sample$hour == 17 |
             test_sample$hour == 18 |
             test_sample$hour == 19)
test_evening = test_sample[include,]

include = (test_sample$hour == 20 |
             test_sample$hour == 21 |
             test_sample$hour == 22 |
             test_sample$hour == 23)
test_night = test_sample[include,]


morning_test_rmse = sqrt(sum((test_morning$travel_time - test_morning$prediction_gbm)^2 ) / nrow(test_morning))
morning_test_rmse # 10.11
morning_test_average = (sum(sqrt((test_morning$travel_time - test_morning$prediction_gbm)^2))) / nrow(test_morning)
morning_test_average # 5.90


evening_test_rmse = sqrt(sum((test_evening$travel_time - test_evening$prediction_gbm)^2 ) / nrow(test_evening))
evening_test_rmse # 11.92
evening_test_average = (sum(sqrt((test_evening$travel_time - test_evening$prediction_gbm)^2))) / nrow(test_evening)
evening_test_average # 8.33


night_test_rmse = sqrt(sum((test_night$travel_time - test_night$prediction_gbm)^2 ) / nrow(test_night))
night_test_rmse # 7.33
night_test_average = (sum(sqrt((test_night$travel_time - test_night$prediction_gbm)^2))) / nrow(test_night)
night_test_average # 5.83
