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


# filter taxis that are going to JFK
all <- all %>% 
  filter(location_in != 132)

all$density_id = (all$day + all$month*100 + all$location_in*10000)
all$count_id = 1

# sum rides per day and per district
all_sum <- all %>%
  group_by(density_id) %>%
  summarize_at(c(29), sum, na.rm = TRUE)

# match density with fields

all = full_join(all, all_sum, by = "density_id")

all = all[, c(c(1:28),30)]
names(all)[names(all) == 'count_id.y'] <- 'density'

# day before density

all$day_id = (all$month*100 + all$day)
all$day_before_id = NA
all$day_before_id = ifelse(all$day_id == 0101, 1231,
                               ifelse (all$day_id == 0201, 0131,
                                       ifelse (all$day_id == 0301, 0228,
                                               ifelse(all$day_id == 0401, 0331,
                                                      ifelse(all$day_id == 0501, 0430,
                                                             ifelse(all$day_id == 0601, 0531,
                                                                    ifelse(all$day_id == 0701, 0630,
                                                                           ifelse(all$day_id == 0801, 0731,
                                                                                  ifelse(all$day_id == 0901, 0831,
                                                                                         ifelse(all$day_id == 1001, 0930,
                                                                                                ifelse(all$day_id == 1101, 1031,
                                                                                                       ifelse(all$day_id == 1201, 1130,
                                                                                                              all$day_id - 1))))))))))))

all$day_location_id = (all$day_id + all$location_in*10000)
all$day_before_location_id = (all$day_before_id + all$location_in*10000)

all$count_id = 1

all_mean <- all %>%
  group_by(day_location_id) %>%
  summarize_at(c(34), sum, na.rm = TRUE)
names(all_mean)[names(all_mean) == 'day_location_id'] <- 'day_before_location_id'

all = full_join(all, all_mean, by = "day_before_location_id")


# delete redundant variables

all = all[, c(3,15,16,17,18,19,20,21,23,24,25,29,35)]
names(all)[names(all) == 'count_id.y'] <- 'density_day_before'

all = all[!is.na(all$location_in),]

# density hour id

all$id = all$hour + all$day*100 + all$month*10000 + all$location_in*1000000

all$count = 1

all_sum_hr <- all %>%
  group_by(id) %>%
  summarize_at(c(15), sum, na.rm = TRUE)

all = full_join(all, all_sum_hr, by = "id") 
names(all)[names(all) == 'count.y'] <- 'density_hour'

# factors

all$location_in = factor(all$location_in)
all$cond = factor(all$cond)
all$hour = factor(all$hour)
all$day = factor(all$day)
all$month = factor(all$month)
all$weekday = factor(all$weekday)
all$holiday = factor(all$holiday)
all$holiday_range_short = factor(all$holiday_range_short)



# imputation

all_av <- all %>%
group_by(location_in) %>%
summarize_at(c(12), mean, na.rm = TRUE)

all = full_join(all, all_av, by = "location_in")  
all$density_day_before = ifelse(is.na(all$density_day_before), all$density.y, all$density_day_before)

all = all[!is.na(all$density_day_before),]

all = all[,-c(14,15,17)]
names(all)[names(all) == 'count.y'] <- 'density_hour'
names(all)[names(all) == 'density.x'] <- 'density_day'

all = all[!is.na(all$location_in),]


## delete July 1

include = all$day == 1 & all$month == 7
all = all[!include,]




# ## remove duplicates
# 
# all$day = as.numeric(all$day)
# all$month = as.numeric(all$month)
# all$location_in = as.numeric(all$location_in)
# 
# all$id = (all$location_in*10000 + all$day + all$month*100)
# 
# all = all[!duplicated(all$id),]


######### models

percentage = 1 

set.seed(111)
data1 = all[sample(nrow(all), nrow(all)/percentage),]


# test and train

samplesize = floor(0.8 * nrow(data1))
set.seed(1)
train_ind = sample(seq_len(nrow(data1)), size = samplesize)
train = data1[train_ind, ]
test = data1[-train_ind, ]

test = semi_join(test, train, by = "location_in")



## adjust train set to our demands

include = test$location_in == 230 & test$month == 10 & test$day == 5 & test$hour == 15
test230 = test[include,]
include = test$location_in == 161 & test$month == 10 & test$day == 5 & test$hour == 15
test161 = test[include,]
include = test$location_in == 233 & test$month == 10 & test$day == 5 & test$hour == 15
test233 = test[include,]
include = test$location_in == 186 & test$month == 10 & test$day == 5 & test$hour == 15
test186 = test[include,]
include = test$location_in == 170 & test$month == 10 & test$day == 5 & test$hour == 15
test170 = test[include,]
include = test$location_in == 48 & test$month == 10 & test$day == 5 & test$hour == 15
test48 = test[include,]
include = test$location_in == 255 & test$month == 10 & test$day == 5 & test$hour == 15
test255 = test[include,]
include = test$location_in == 138 & test$month == 10 & test$day == 5 & test$hour == 15
test138 = test[include,]


###### train

# 1) Regression

f2 = formula(density_hour ~ . - density_hour - density_day)

fit1 = glm(f2, data = train)
summary(fit1)

test$prediction = as.numeric(predict(fit1,test))

include = test$location_in == 230 & test$month == 10 & test$day == 5 & test$hour == 15
test1 = test[include,]
include = test$location_in == 161 & test$month == 10 & test$day == 5 & test$hour == 15
test2 = test[include,]
include = test$location_in == 233 & test$month == 10 & test$day == 5 & test$hour == 15
test3 = test[include,]
include = test$location_in == 170 & test$month == 10 & test$day == 5 & test$hour == 15
test4 = test[include,]
include = test$location_in == 48 & test$month == 10 & test$day == 5 & test$hour == 15
test5 = test[include,]
include = test$location_in == 255 & test$month == 10 & test$day == 5 & test$hour == 15
test6 = test[include,]
include = test$location_in == 138 & test$month == 10 & test$day == 5 & test$hour == 15
test7 = test[include,]

test_final = rbind.data.frame(test1,test2,test3,test4,test5,test6,test7)


pred.fit1 = predict(fit1, test)
fit1test1 = sqrt(sum((test$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 3.4
fit1test2 = (sum(sqrt((test$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 2.47


pred.fit1 = predict(fit1, test138)
fit1test1 = sqrt(sum((test138$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 0.69
fit1test2 = (sum(sqrt((test138$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.09


pred.fit1 = predict(fit1, test161)
fit1test1 = sqrt(sum((test161$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 1.2
fit1test2 = (sum(sqrt((test161$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.26

pred.fit1 = predict(fit1, test170)
fit1test1 = sqrt(sum((test170$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 4.97
fit1test2 = (sum(sqrt((test170$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.91

pred.fit1 = predict(fit1, test186)
fit1test1 = sqrt(sum((test186$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 2.09
fit1test2 = (sum(sqrt((test186$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.26

pred.fit1 = predict(fit1, test230)
fit1test1 = sqrt(sum((test230$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 9.60
fit1test2 = (sum(sqrt((test230$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 2.13

pred.fit1 = predict(fit1, test233)
fit1test1 = sqrt(sum((test233$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 2.23
fit1test2 = (sum(sqrt((test233$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.31

pred.fit1 = predict(fit1, test255)
fit1test1 = sqrt(sum((test255$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 0.31
fit1test2 = (sum(sqrt((test255$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.01

pred.fit1 = predict(fit1, test48)
fit1test1 = sqrt(sum((test48$density_hour - pred.fit1)^2 ) / nrow(test))
fit1test1 # 2.72
fit1test2 = (sum(sqrt((test48$density_hour - pred.fit1)^2))) / nrow(test)
fit1test2 # 0.46

## naive predictor

all_naive <- all %>%
  group_by(location_in) %>%
  summarize_at(c(12), mean, na.rm = TRUE)




# # 2) MARS
# 
# marsGrid = expand.grid(.degree=1:4,.nprune=2:10)
# marsTuned = train(density~. -density, # use all features except name to make predictions
#                   data = train, # use just the training data
#                   method = "earth", # the MARS model
#                   tuneGrid = marsGrid, # pass it the grid of tuning parameters to try
#                   trControl = trainControl(method='cv',returnResamp='all'))
# 
# marsGrid = expand.grid(.degree=1:4,.nprune=2:10)
# marsTuned = train(f,
#                   data = train, 
#                   method = "earth",
#                   tuneGrid = marsGrid, 
#                   trControl = trainControl(method='cv',returnResamp='all'))
# 
# 
# # 3) Poly
# 
# polyTuned = train(density~poly(weight,5), # let's predict using weight, up to a 5th order polynomial term
#                   data = train,
#                   method = "leapBackward", # linear regression using backward stepping for variable selection
#                   tuneGrid = data.frame(nvmax=1:5), # check how many variables we should keep, from 1 to 5
#                   trControl = trainControl(method='repeatedcv',repeats=5,returnResamp='all'),
#                   selectionFunction=oneSE) # use the one standard error rule for picking the best model
# 


# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# 
# # 
# # neuralnetFit1 <- train(density ~ ., data = train, 
# #                  method = "nnet", 
# #                  trControl = fitControl,
# #                  ## This last option is actually one
# #                  ## for gbm() that passes through
# #                  verbose = FALSE)
# # 
# 
# glmFit2 <- train(f, data = train,
#                  method = "glm",
#                  trControl = fitControl,
#                  verbose = FALSE)
# 
# nn <- neuralnet(density~. -density, data = train, hidden=c(3))




