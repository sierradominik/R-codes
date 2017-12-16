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

setwd("~/Desktop/CS 229 Project/New Data/")

#### weather data

january_weather <- read_excel("january_weather.xlsx")
february_weather <- read_excel("february_weather.xlsx")
march_weather <- read_excel("march_weather.xlsx")
april_weather <- read_excel("april_weather.xlsx")
may_weather <- read_excel("may_weather.xlsx")
june_weather <- read_excel("june_weather.xlsx")
july_weather <- read_excel("july_weather.xlsx")
august_weather <- read_excel("august_weather.xlsx")
september_weather <- read_excel("september_weather.xlsx")
october_weather <- read_excel("october_weather.xlsx")
november_weather <- read_excel("november_weather.xlsx")
december_weather <- read_excel("december_weather.xlsx")

# weather filtering
january_weather <- january_weather %>% 
  filter(str_detect(january_weather$`Time (EST)`, "51"))
february_weather <- february_weather %>% 
  filter(str_detect(february_weather$`Time (EST)`, "51"))
march_weather <- march_weather %>% 
  filter(str_detect(march_weather$`Time (EST)`, "51"))
april_weather <- april_weather %>% 
  filter(str_detect(april_weather$`Time (EDT)`, "51"))
may_weather <- may_weather %>% 
  filter(str_detect(may_weather$`Time (EDT)`, "51"))
june_weather <- june_weather %>% 
  filter(str_detect(june_weather$`Time (EDT)`, "51"))
july_weather <- july_weather %>% 
  filter(str_detect(july_weather$`Time (EST)`, "51"))
august_weather <- august_weather %>% 
  filter(str_detect(august_weather$`Time (EDT)`, "51"))
september_weather <- september_weather %>% 
  filter(str_detect(september_weather$`Time (EDT)`, "51"))
october_weather <- october_weather %>% 
  filter(str_detect(october_weather$`Time (EDT)`, "51"))
november_weather <- november_weather %>% 
  filter(str_detect(november_weather$`Time (EDT)`, "51"))
december_weather <- december_weather %>% 
  filter(str_detect(december_weather$`Time (EST)`, "51"))

# timestamp hour
month28 = as.vector(matrix(data = c(0:23), ncol = 1, nrow = 28*24))
month30 = as.vector(matrix(data = c(0:23), ncol = 1, nrow = 30*24))
month31 = as.vector(matrix(data = c(0:23), ncol = 1, nrow = 31*24))

january_weather$hour = month31
february_weather$hour = month28
march_weather$hour = month31
april_weather$hour = month30
may_weather$hour = month31
june_weather$hour = month30
july_weather$hour = month31
august_weather$hour = month31
september_weather$hour = month30
october_weather$hour = month31
november_weather$hour = month30
december_weather$hour = month31

# timestamp day
day28 = as.vector(matrix(data = c(rep(1,24), rep(2,24), rep(3,24), rep(4,24), rep(5,24), rep(6,24), rep(7,24), rep(8,24), rep(9,24), rep(10,24), rep(11,24), rep(12,24), rep(13,24), rep(14,24), rep(15,24), rep(16,24), rep(17,24), rep(18,24), rep(19,24), rep(20,24), rep(21,24), rep(22,24), rep(23,24), rep(24,24), rep(25,24), rep(26,24), rep(27,24), rep(28,24)) , ncol = 1, nrow = 28*24))
names(day28) = c('day')
day30 = as.vector(matrix(data = c(rep(1,24), rep(2,24), rep(3,24), rep(4,24), rep(5,24), rep(6,24), rep(7,24), rep(8,24), rep(9,24), rep(10,24), rep(11,24), rep(12,24), rep(13,24), rep(14,24), rep(15,24), rep(16,24), rep(17,24), rep(18,24), rep(19,24), rep(20,24), rep(21,24), rep(22,24), rep(23,24), rep(24,24), rep(25,24), rep(26,24), rep(27,24), rep(28,24), rep(29,24), rep(30,24)) , ncol = 1, nrow = 30*24))
names(day28) = c('day')
day31 = as.vector(matrix(data = c(rep(1,24), rep(2,24), rep(3,24), rep(4,24), rep(5,24), rep(6,24), rep(7,24), rep(8,24), rep(9,24), rep(10,24), rep(11,24), rep(12,24), rep(13,24), rep(14,24), rep(15,24), rep(16,24), rep(17,24), rep(18,24), rep(19,24), rep(20,24), rep(21,24), rep(22,24), rep(23,24), rep(24,24), rep(25,24), rep(26,24), rep(27,24), rep(28,24), rep(29,24), rep(30,24), rep(31,24)) , ncol = 1, nrow = 31*24))
names(day28) = c('day')

january_weather$day = day31
february_weather$day = day28
march_weather$day = day31
april_weather$day = day30
may_weather$day = day31
june_weather$day = day30
july_weather$day = day31
august_weather$day = day31
september_weather$day = day30
october_weather$day = day31
november_weather$day = day30
december_weather$day = day31

# numeric
cols.num <- c(2:4)
january_weather[cols.num] <- sapply(january_weather[cols.num], as.numeric)
february_weather[cols.num] <- sapply(february_weather[cols.num], as.numeric)
march_weather[cols.num] <- sapply(march_weather[cols.num], as.numeric)
april_weather[cols.num] <- sapply(april_weather[cols.num], as.numeric)
may_weather[cols.num] <- sapply(may_weather[cols.num], as.numeric)
june_weather[cols.num] <- sapply(june_weather[cols.num], as.numeric)
july_weather[cols.num] <- sapply(july_weather[cols.num], as.numeric)
august_weather[cols.num] <- sapply(august_weather[cols.num], as.numeric)
september_weather[cols.num] <- sapply(september_weather[cols.num], as.numeric)
october_weather[cols.num] <- sapply(october_weather[cols.num], as.numeric)
november_weather[cols.num] <- sapply(november_weather[cols.num], as.numeric)
december_weather[cols.num] <- sapply(december_weather[cols.num], as.numeric)

january_weather$Preci = ifelse(is.na(january_weather$Preci), 0, january_weather$Preci)
january_weather$Wind = ifelse(january_weather$Wind == 'Cal', 0, january_weather$Wind)
january_weather$Wind = ifelse(is.na(january_weather$Wind), 0, january_weather$Wind)
february_weather$Preci = ifelse(is.na(february_weather$Preci), 0, february_weather$Preci)
february_weather$Wind = ifelse(february_weather$Wind == 'Cal', 0, february_weather$Wind)
february_weather$Wind = ifelse(is.na(february_weather$Wind), 0, february_weather$Wind)
march_weather$Preci = ifelse(is.na(march_weather$Preci), 0, march_weather$Preci)
march_weather$Wind = ifelse(march_weather$Wind == 'Cal', 0, march_weather$Wind)
march_weather$Wind = ifelse(is.na(march_weather$Wind), 0, march_weather$Wind)
april_weather$Preci = ifelse(is.na(april_weather$Preci), 0, april_weather$Preci)
april_weather$Wind = ifelse(april_weather$Wind == 'Cal', 0, april_weather$Wind)
april_weather$Wind = ifelse(is.na(april_weather$Wind), 0, april_weather$Wind)
may_weather$Preci = ifelse(is.na(may_weather$Preci), 0, may_weather$Preci)
may_weather$Wind = ifelse(may_weather$Wind == 'Cal', 0, may_weather$Wind)
may_weather$Wind = ifelse(is.na(may_weather$Wind), 0, may_weather$Wind)
june_weather$Preci = ifelse(is.na(june_weather$Preci), 0, june_weather$Preci)
june_weather$Wind = ifelse(june_weather$Wind == 'Cal', 0, june_weather$Wind)
june_weather$Wind = ifelse(is.na(june_weather$Wind), 0, june_weather$Wind)
july_weather$Preci = ifelse(is.na(july_weather$Preci), 0, july_weather$Preci)
july_weather$Wind = ifelse(july_weather$Wind == 'Cal', 0, july_weather$Wind)
july_weather$Wind = ifelse(is.na(july_weather$Wind), 0, july_weather$Wind)
august_weather$Preci = ifelse(is.na(august_weather$Preci), 0, august_weather$Preci)
august_weather$Wind = ifelse(august_weather$Wind == 'Cal', 0, august_weather$Wind)
august_weather$Wind = ifelse(is.na(august_weather$Wind), 0, august_weather$Wind)
september_weather$Preci = ifelse(is.na(september_weather$Preci), 0, september_weather$Preci)
september_weather$Wind = ifelse(september_weather$Wind == 'Cal', 0, september_weather$Wind)
september_weather$Wind = ifelse(is.na(september_weather$Wind), 0, september_weather$Wind)
october_weather$Preci = ifelse(is.na(october_weather$Preci), 0, october_weather$Preci)
october_weather$Wind = ifelse(october_weather$Wind == 'Cal', 0, october_weather$Wind)
october_weather$Wind = ifelse(is.na(october_weather$Wind), 0, october_weather$Wind)
november_weather$Preci = ifelse(is.na(november_weather$Preci), 0, november_weather$Preci)
november_weather$Wind = ifelse(november_weather$Wind == 'Cal', 0, november_weather$Wind)
november_weather$Wind = ifelse(is.na(november_weather$Wind), 0, november_weather$Wind)
december_weather$Preci = ifelse(is.na(december_weather$Preci), 0, december_weather$Preci)
december_weather$Wind = ifelse(december_weather$Wind == 'Cal', 0, december_weather$Wind)
december_weather$Wind = ifelse(is.na(december_weather$Wind), 0, december_weather$Wind)

# ids
january_weather$id = (january_weather$day*100 + january_weather$hour)
february_weather$id = (february_weather$day*100 + february_weather$hour)
march_weather$id = (march_weather$day*100 + march_weather$hour)
april_weather$id = (april_weather$day*100 + april_weather$hour)
may_weather$id = (may_weather$day*100 + may_weather$hour)
june_weather$id = (june_weather$day*100 + june_weather$hour)
july_weather$id = (july_weather$day*100 + july_weather$hour)
august_weather$id = (august_weather$day*100 + august_weather$hour)
september_weather$id = (september_weather$day*100 + september_weather$hour)
october_weather$id = (october_weather$day*100 + october_weather$hour)
november_weather$id = (november_weather$day*100 + november_weather$hour)
december_weather$id = (december_weather$day*100 + december_weather$hour)

# deleting variables
january_weather = january_weather[,c(2,3,4,6,7,8,9)]
february_weather = february_weather[,c(2,3,4,6,7,8,9)]
march_weather = march_weather[,c(2,3,4,6,7,8,9)]
april_weather = april_weather[,c(2,3,4,6,7,8,9)]
may_weather = may_weather[,c(2,3,4,6,7,8,9)]
june_weather = june_weather[,c(2,3,4,6,7,8,9)]
july_weather = july_weather[,c(2,3,4,6,7,8,9)]
august_weather = august_weather[,c(2,3,4,6,7,8,9)]
september_weather = september_weather[,c(2,3,4,6,7,8,9)]
october_weather = october_weather[,c(2,3,4,6,7,8,9)]
november_weather = november_weather[,c(2,3,4,6,7,8,9)]
december_weather = december_weather[,c(2,3,4,6,7,8,9)]

# adding month variable
january_weather$month = as.vector(matrix(data = 1, ncol = 1, nrow = nrow(january_weather)))
february_weather$month = as.vector(matrix(data = 2, ncol = 1, nrow = nrow(february_weather)))
march_weather$month = as.vector(matrix(data = 3, ncol = 1, nrow = nrow(march_weather)))
april_weather$month = as.vector(matrix(data = 4, ncol = 1, nrow = nrow(april_weather)))
may_weather$month = as.vector(matrix(data = 5, ncol = 1, nrow = nrow(may_weather)))
june_weather$month = as.vector(matrix(data = 6, ncol = 1, nrow = nrow(june_weather)))
july_weather$month = as.vector(matrix(data = 7, ncol = 1, nrow = nrow(july_weather)))
august_weather$month = as.vector(matrix(data = 8, ncol = 1, nrow = nrow(august_weather)))
september_weather$month = as.vector(matrix(data = 9, ncol = 1, nrow = nrow(september_weather)))
october_weather$month = as.vector(matrix(data = 10, ncol = 1, nrow = nrow(october_weather)))
november_weather$month = as.vector(matrix(data = 11, ncol = 1, nrow = nrow(november_weather)))
december_weather$month = as.vector(matrix(data = 12, ncol = 1, nrow = nrow(december_weather)))

# adding day ID
january_weather$idday = (january_weather$month*10 + january_weather$day)
february_weather$idday = (february_weather$month*10 + february_weather$day)
march_weather$idday = (march_weather$month*10 + march_weather$day)
april_weather$idday = (april_weather$month*10 + april_weather$day)
may_weather$idday = (may_weather$month*10 + may_weather$day)
june_weather$idday = (june_weather$month*10 + june_weather$day)
july_weather$idday = (july_weather$month*10 + july_weather$day)
august_weather$idday = (august_weather$month*10 + august_weather$day)
september_weather$idday = (september_weather$month*10 + september_weather$day)
october_weather$idday = (october_weather$month*10 + october_weather$day)
november_weather$idday = (november_weather$month*10 + november_weather$day)
december_weather$idday = (december_weather$month*10 + december_weather$day)

# names
label = c('temp', 'wind', 'prec', 'cond', 'hour', 'day', 'id', 'month', 'idday')
names(january_weather) = label
names(february_weather) = label
names(march_weather) = label
names(april_weather) = label
names(may_weather) = label
names(june_weather) = label
names(july_weather) = label
names(august_weather) = label
names(september_weather) = label
names(october_weather) = label
names(november_weather) = label
names(december_weather) = label




######### taxi data #########

january_taxi <- read_csv("yellow_tripdata_2017-01.csv")
february_taxi <- read_csv("yellow_tripdata_2017-02.csv")
march_taxi <- read_csv("yellow_tripdata_2017-03.csv")
april_taxi <- read_csv("yellow_tripdata_2017-04.csv")
may_taxi <- read_csv("yellow_tripdata_2017-05.csv")
june_taxi <- read_csv("yellow_tripdata_2017-06.csv")
july_taxi <- read_csv("yellow_tripdata_2016-07.csv")
august_taxi <- read_csv("yellow_tripdata_2016-08.csv")
september_taxi <- read_csv("yellow_tripdata_2016-09.csv")
october_taxi <- read_csv("yellow_tripdata_2016-10.csv")
november_taxi <- read_csv("yellow_tripdata_2016-11.csv")
december_taxi <- read_csv("yellow_tripdata_2016-12.csv")

# airport filtering -- only from and to JFK

airport_january <- january_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_february <- february_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_march <- march_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_april <- april_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_may <- may_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_june <- june_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_july <- july_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_august <- august_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_september <- september_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_october <- october_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_november <- november_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)
airport_december <- december_taxi %>% 
  filter(PULocationID == 132 | DOLocationID == 132)


######### create samples #########

percentage = 1 #100%

set.seed(111)
sample_jan = airport_january[sample(nrow(airport_january), nrow(airport_january)/percentage),]
sample_feb = airport_february[sample(nrow(airport_february), nrow(airport_february)/percentage),]
sample_mar = airport_january[sample(nrow(airport_march), nrow(airport_march)/percentage),]
sample_apr = airport_april[sample(nrow(airport_april), nrow(airport_april)/percentage),]
sample_may = airport_may[sample(nrow(airport_may), nrow(airport_may)/percentage),]
sample_jun = airport_june[sample(nrow(airport_june), nrow(airport_june)/percentage),]
sample_jul = airport_july[sample(nrow(airport_july), nrow(airport_july)/percentage),]
sample_aug = airport_august[sample(nrow(airport_august), nrow(airport_august)/percentage),]
sample_sep = airport_september[sample(nrow(airport_september), nrow(airport_september)/percentage),]
sample_oct = airport_october[sample(nrow(airport_october), nrow(airport_october)/percentage),]
sample_nov = airport_november[sample(nrow(airport_november), nrow(airport_november)/percentage),]
sample_dec = airport_december[sample(nrow(airport_december), nrow(airport_december)/percentage),]


# date and travel time
sample_jan$tpep_pickup_datetime = as.character(sample_jan$tpep_pickup_datetime)
sample_jan$hour = strftime(sample_jan$tpep_pickup_datetime, format="%H")
sample_jan$day = strftime(sample_jan$tpep_pickup_datetime, format="%d")
sample_jan$day = as.numeric(sample_jan$day)
sample_jan$hour = as.numeric(sample_jan$hour)

sample_jan$start <- as.POSIXct(sample_jan$tpep_pickup_datetime,
                                      format='%Y-%m-%d %H:%M:%S')
sample_jan$end <- as.POSIXct(sample_jan$tpep_dropoff_datetime,
                                      format='%Y-%m-%d %H:%M:%S')
sample_jan$travel_time = (sample_jan$end - sample_jan$start)/60

sample_feb$tpep_pickup_datetime = as.character(sample_feb$tpep_pickup_datetime) 
sample_feb$hour = strftime(sample_feb $tpep_pickup_datetime, format="%H")
sample_feb$day = strftime(sample_feb$tpep_pickup_datetime, format="%d")
sample_feb$day = as.numeric(sample_feb$day)
sample_feb$hour = as.numeric(sample_feb$hour)

sample_feb$start <- as.POSIXct(sample_feb$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_feb$end <- as.POSIXct(sample_feb$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_feb$travel_time = (sample_feb$end - sample_feb$start)/60

sample_mar$tpep_pickup_datetime = as.character(sample_mar$tpep_pickup_datetime)
sample_mar$hour = strftime(sample_mar$tpep_pickup_datetime, format="%H")
sample_mar$day = strftime(sample_mar$tpep_pickup_datetime, format="%d")
sample_mar$day = as.numeric(sample_mar$day)
sample_mar$hour = as.numeric(sample_mar$hour)

sample_mar$start <- as.POSIXct(sample_mar$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_mar$end <- as.POSIXct(sample_mar$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_mar$travel_time = (sample_mar$end - sample_mar$start)/60

sample_apr$tpep_pickup_datetime = as.character(sample_apr$tpep_pickup_datetime)
sample_apr$hour = strftime(sample_apr$tpep_pickup_datetime, format="%H")
sample_apr$day = strftime(sample_apr$tpep_pickup_datetime, format="%d")
sample_apr$day = as.numeric(sample_apr$day)
sample_apr$hour = as.numeric(sample_apr$hour)

sample_apr$start <- as.POSIXct(sample_apr$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_apr$end <- as.POSIXct(sample_apr$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_apr$travel_time = (sample_apr$end - sample_apr$start)/60

sample_may$tpep_pickup_datetime = as.character(sample_may$tpep_pickup_datetime)
sample_may$hour = strftime(sample_may$tpep_pickup_datetime, format="%H")
sample_may$day = strftime(sample_may$tpep_pickup_datetime, format="%d")
sample_may$day = as.numeric(sample_may$day)
sample_may$hour = as.numeric(sample_may$hour)

sample_may$start <- as.POSIXct(sample_may$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_may$end <- as.POSIXct(sample_may$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_may$travel_time = (sample_may$end - sample_may$start)/60

sample_jun$tpep_pickup_datetime = as.character(sample_jun$tpep_pickup_datetime)
sample_jun$hour = strftime(sample_jun$tpep_pickup_datetime, format="%H")
sample_jun$day = strftime(sample_jun$tpep_pickup_datetime, format="%d")
sample_jun$day = as.numeric(sample_jun$day)
sample_jun$hour = as.numeric(sample_jun$hour)

sample_jun$start <- as.POSIXct(sample_jun$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_jun$end <- as.POSIXct(sample_jun$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_jun$travel_time = (sample_jun$end - sample_jun$start)/60

sample_jul$tpep_pickup_datetime = as.character(sample_jul$tpep_pickup_datetime)
sample_jul$hour = strftime(sample_jul$tpep_pickup_datetime, format="%H")
sample_jul$day = strftime(sample_jul$tpep_pickup_datetime, format="%d")
sample_jul$day = as.numeric(sample_jul$day)
sample_jul$hour = as.numeric(sample_jul$hour)

sample_jul$start <- as.POSIXct(sample_jul$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_jul$end <- as.POSIXct(sample_jul$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_jul$travel_time = (sample_jul$end - sample_jul$start)/60

sample_aug$tpep_pickup_datetime = as.character(sample_aug$tpep_pickup_datetime)
sample_aug$hour = strftime(sample_aug$tpep_pickup_datetime, format="%H")
sample_aug$day = strftime(sample_aug$tpep_pickup_datetime, format="%d")
sample_aug$day = as.numeric(sample_aug$day)
sample_aug$hour = as.numeric(sample_aug$hour)

sample_aug$start <- as.POSIXct(sample_aug$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_aug$end <- as.POSIXct(sample_aug$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_aug$travel_time = (sample_aug$end - sample_aug$start)/60

sample_sep$tpep_pickup_datetime = as.character(sample_sep$tpep_pickup_datetime)
sample_sep$hour = strftime(sample_sep$tpep_pickup_datetime, format="%H")
sample_sep$day = strftime(sample_sep$tpep_pickup_datetime, format="%d")
sample_sep$day = as.numeric(sample_sep$day)
sample_sep$hour = as.numeric(sample_sep$hour)

sample_sep$start <- as.POSIXct(sample_sep$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_sep$end <- as.POSIXct(sample_sep$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_sep$travel_time = (sample_sep$end - sample_sep$start)/60

sample_oct$tpep_pickup_datetime = as.character(sample_oct$tpep_pickup_datetime)
sample_oct$hour = strftime(sample_oct$tpep_pickup_datetime, format="%H")
sample_oct$day = strftime(sample_oct$tpep_pickup_datetime, format="%d")
sample_oct$day = as.numeric(sample_oct$day)
sample_oct$hour = as.numeric(sample_oct$hour)

sample_oct$start <- as.POSIXct(sample_oct$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_oct$end <- as.POSIXct(sample_oct$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_oct$travel_time = (sample_oct$end - sample_oct$start)/60

sample_nov$tpep_pickup_datetime = as.character(sample_nov$tpep_pickup_datetime)
sample_nov$hour = strftime(sample_nov$tpep_pickup_datetime, format="%H")
sample_nov$day = strftime(sample_nov$tpep_pickup_datetime, format="%d")
sample_nov$day = as.numeric(sample_nov$day)
sample_nov$hour = as.numeric(sample_nov$hour)

sample_nov$start <- as.POSIXct(sample_nov$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_nov$end <- as.POSIXct(sample_nov$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_nov$travel_time = (sample_nov$end - sample_nov$start)/60

sample_dec$tpep_pickup_datetime = as.character(sample_dec$tpep_pickup_datetime)
sample_dec$hour = strftime(sample_dec$tpep_pickup_datetime, format="%H")
sample_dec$day = strftime(sample_dec$tpep_pickup_datetime, format="%d")
sample_dec$day = as.numeric(sample_dec$day)
sample_dec$hour = as.numeric(sample_dec$hour)

sample_dec$start <- as.POSIXct(sample_dec$tpep_pickup_datetime,
                               format='%Y-%m-%d %H:%M:%S')
sample_dec$end <- as.POSIXct(sample_dec$tpep_dropoff_datetime,
                             format='%Y-%m-%d %H:%M:%S')
sample_dec$travel_time = (sample_dec$end - sample_dec$start)/60

# id for hour
sample_jan$id = (sample_jan$day*100 + sample_jan$hour)
sample_feb$id = (sample_feb$day*100 + sample_feb$hour)
sample_mar$id = (sample_mar$day*100 + sample_mar$hour)
sample_apr$id = (sample_apr$day*100 + sample_apr$hour)
sample_may$id = (sample_may$day*100 + sample_may$hour)
sample_jun$id = (sample_jun$day*100 + sample_jun$hour)
sample_jul$id = (sample_jul$day*100 + sample_jul$hour)
sample_aug$id = (sample_aug$day*100 + sample_aug$hour)
sample_sep$id = (sample_sep$day*100 + sample_sep$hour)
sample_oct$id = (sample_oct$day*100 + sample_oct$hour)
sample_nov$id = (sample_nov$day*100 + sample_nov$hour)
sample_dec$id = (sample_dec$day*100 + sample_dec$hour)



#### matching data ####

jan = full_join(sample_jan, january_weather, by = "id")
jan = jan[!is.na(jan$VendorID),]
feb = full_join(sample_feb, february_weather, by = "id")
feb = feb[!is.na(feb$VendorID),]
mar = full_join(sample_mar, march_weather, by = "id")
mar = mar[!is.na(mar$VendorID),]
apr = full_join(sample_apr, april_weather, by = "id")
apr = apr[!is.na(apr$VendorID),]
may = full_join(sample_may, may_weather, by = "id")
may = may[!is.na(may$VendorID),]
jun = full_join(sample_jun, june_weather, by = "id")
jun = jun[!is.na(jun$VendorID),]
jul = full_join(sample_jul, july_weather, by = "id")
jul = jul[!is.na(jul$VendorID),]
aug = full_join(sample_aug, august_weather, by = "id")
aug = aug[!is.na(aug$VendorID),]
sep = full_join(sample_sep, september_weather, by = "id")
sep = sep[!is.na(sep$VendorID),]
oct = full_join(sample_oct, october_weather, by = "id")
oct = oct[!is.na(oct$VendorID),]
nov = full_join(sample_nov, november_weather, by = "id")
nov = nov[!is.na(nov$VendorID),]
dec = full_join(sample_dec, december_weather, by = "id")
dec = dec[!is.na(dec$VendorID),]


#### ALL dataframe starts

all = rbind.data.frame(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

### delete unknown destination
all <- all %>% 
  filter(PULocationID < 264 | DOLocationID < 264)


# add weekday
all$weekday <- weekdays(as.Date(all$tpep_pickup_datetime))
all$weekday = ifelse(all$weekday == 'Monday', 1,
                     ifelse(all$weekday == 'Tuesday', 2,
                            ifelse(all$weekday == 'Wednesday', 3,
                                   ifelse(all$weekday == 'Thursday', 4,
                                          ifelse(all$weekday == 'Friday', 5,
                                                 ifelse(all$weekday == 'Saturday', 6,
                                                        ifelse(all$weekday == 'Sunday', 7, 
                                                               all$weekday)))))))


all = all[, c(4,5, c(8:17), c(22:32))]

new_label = c('passenger_count', 'trip_distance', 'location_in', 'location_out', 'payment_type', 'fare_amount', 'extra', 'mta_tax', 'tip_amount', 'toll_amount', 'improvement_surcharge', 'total_amount', 'travel_time', 'id_hour', 'temp', 'wind', 'prec', 'cond', 'hour', 'day', 'month', 'id_day', 'weekday')
names(all) = new_label

cols.num <- c(c(1:12), 14,15,16,17, c(19:23))
all[cols.num] <- sapply(all[cols.num], as.numeric)
all$travel_time = as.integer(all$travel_time)
all$travel_time = as.numeric(all$travel_time)

# factors
cols.num <- c(3,4,5,14,19,20,21,22,23)
all[cols.num] <- sapply(all[cols.num], as.numeric)


### exclude outliers
all <- all %>% 
  filter(travel_time < 240)
all <- all %>% 
  filter(travel_time > 6)
all <- all %>% 
  filter(trip_distance < 50)
all <- all %>% 
  filter(trip_distance > 1)


### add holidays

all$holiday = NA
all$holiday = ifelse(all$month == 7 & all$day == 4, 1,
                     ifelse(all$month == 9 & all$day == 5, 1,
                            ifelse(all$month == 10 & all$day == 10, 1,
                                   ifelse(all$month == 11 & all$day == 11, 1,
                                          ifelse(all$month == 11 & all$day == 24, 1,
                                                 ifelse(all$month == 12 & all$day == 25, 1,
                                                        ifelse(all$month == 12 & all$day == 26, 1,
                                                               ifelse(all$month == 1 & all$day == 1, 1,
                                                                      ifelse(all$month == 1 & all$day == 2, 1,
                                                                             ifelse(all$month == 1 & all$day == 16, 1,
                                                                                    ifelse(all$month == 2 & all$day == 12, 1,
                                                                                           ifelse(all$month == 2 & all$day == 13, 1,
                                                                                                  ifelse(all$month == 2 & all$day == 20, 1,
                                                                                                         ifelse(all$month == 5 & all$day == 29, 1, 0))))))))))))))

all$holiday_range_short = NA
all$holiday_range_short = ifelse(all$month == 7 & all$day == 4, 1,
                                 ifelse(all$month == 7 & all$day == 3, 1,
                                        ifelse(all$month == 7 & all$day == 2, 1,
                                               ifelse(all$month == 7 & all$day == 5, 1,
                                                      ifelse(all$month == 7 & all$day == 6, 1, 0)))))
                                                             
all$holiday_range_short = ifelse(all$month == 9 & all$day == 5, 1,
                            ifelse(all$month == 9 & all$day == 4, 1,
                                   ifelse(all$month == 9 & all$day == 3, 1,
                                          ifelse(all$month == 9 & all$day == 6, 1,
                                                 ifelse(all$month == 9 & all$day == 7, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 10 & all$day == 10, 1,
                                   ifelse(all$month == 10 & all$day == 9, 1,
                                          ifelse(all$month == 10 & all$day == 8, 1,
                                                 ifelse(all$month == 10 & all$day == 11, 1,
                                                        ifelse(all$month == 10 & all$day == 12, 1, all$holiday_range_short)))))
                                                               
all$holiday_range_short = ifelse(all$month == 11 & all$day == 11, 1,
                                          ifelse(all$month == 11 & all$day == 10, 1,
                                                 ifelse(all$month == 11 & all$day == 9, 1,
                                                        ifelse(all$month == 11 & all$day == 12, 1,
                                                               ifelse(all$month == 11 & all$day == 13, 1, all$holiday_range_short)))))
                                                                      
all$holiday_range_short = ifelse(all$month == 11 & all$day == 24, 1,
                                                 ifelse(all$month == 11 & all$day == 23, 1,
                                                        ifelse(all$month == 11 & all$day == 22, 1,
                                                               ifelse(all$month == 11 & all$day == 25, 1,
                                                                      ifelse(all$month == 11 & all$day == 26, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 12 & all$day == 25, 1,
                                                        ifelse(all$month == 12 & all$day == 24, 1,
                                                               ifelse(all$month == 12 & all$day == 23, 1,
                                                                      ifelse(all$month == 12 & all$day == 26, 1,
                                                                             ifelse(all$month == 12 & all$day == 27, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 12 & all$day == 26, 1,
                                                               ifelse(all$month == 12 & all$day == 25, 1,
                                                                      ifelse(all$month == 12 & all$day == 24, 1,
                                                                             ifelse(all$month == 12 & all$day == 27, 1,
                                                                                    ifelse(all$month == 12 & all$day == 28, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 1 & all$day == 1, 1,
                                                 ifelse(all$month == 12 & all$day == 31, 1,
                                                                             ifelse(all$month == 12 & all$day == 30, 1,
                                                                                    ifelse(all$month == 1 & all$day == 2, 1,
                                                                                           ifelse(all$month == 1 & all$day == 3, 1,all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 1 & all$day == 2, 1,
                                                           ifelse(all$month == 1 & all$day == 1, 1,
                                                                                    ifelse(all$month == 12 & all$day == 31, 1,
                                                                                           ifelse(all$month == 1 & all$day == 3, 1,
                                                                                                  ifelse(all$month == 1 & all$day == 4, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 1 & all$day == 16, 1,
                                                            ifelse(all$month == 1 & all$day == 15, 1,
                                                                                           ifelse(all$month == 1 & all$day == 14, 1,
                                                                                                  ifelse(all$month == 1 & all$day == 17, 1,
                                                                                                         ifelse(all$month == 1 & all$day == 18, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 2 & all$day == 12, 1,
                                                     ifelse(all$month == 2 & all$day == 13, 1,
                                                                                          ifelse(all$month == 2 & all$day == 14, 1,
                                                                                                         ifelse(all$month == 2 & all$day == 11, 1,
                                                                                                                ifelse(all$month == 2 & all$day == 10, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 2 & all$day == 13, 1,
                                                                ifelse(all$month == 2 & all$day == 12, 1,
                                                                                                ifelse(all$month == 2 & all$day == 11, 1,
                                                                                                                ifelse(all$month == 2 & all$day == 14, 1,
                                                                                                                       ifelse(all$month == 2 & all$day == 15, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 2 & all$day == 20, 1,
                                                          ifelse(all$month == 2 & all$day == 19, 1,
                                                                                  ifelse(all$month == 2 & all$day == 18, 1,
                                                                                                                       ifelse(all$month == 2 & all$day == 21, 1,
                                                                                                                              ifelse(all$month == 2 & all$day == 22, 1, all$holiday_range_short)))))

all$holiday_range_short = ifelse(all$month == 5 & all$day == 29, 1, 
                                                               ifelse(all$month == 5 & all$day == 28, 1,
                                                                                            ifelse(all$month == 5 & all$day == 27, 1,
                                                                                                                              ifelse(all$month == 5 & all$day == 30, 1,
                                                                                                                                     ifelse(all$month == 5 & all$day == 31, 1, all$holiday_range_short)))))




###### hand-crafted features

#### Hour before

### in airport

all_in <- all %>% 
  filter(location_in != 132)

all_in$hour_id = (all_in$hour + all_in$day*100 + all_in$month*10000)
all_in$hour_location_id = (all_in$hour_id + all_in$location_in*1000000)

all_in_mean <- all_in %>%
  group_by(hour_location_id) %>%
  summarize_at(c(13), mean, na.rm = TRUE)
names(all_in_mean)[names(all_in_mean) == 'hour_location_id'] <- 'location_hour_before_in'

# For day ahead indexing

all_in$hour_before_id = NA
all_in$hour_before_id = ifelse(all_in$hour_id == 010100, 123123,
                               ifelse(all_in$hour_id == 020100, 013123,
                                      ifelse(all_in$hour_id == 030100, 022823,
                                             ifelse(all_in$hour_id == 040100, 033123,
                                                    ifelse(all_in$hour_id == 050100, 043023,
                                                           ifelse(all_in$hour_id == 060100, 053123,
                                                                  ifelse(all_in$hour_id == 070100, 063023,
                                                                         ifelse(all_in$hour_id == 080100, 073123,
                                                                                ifelse(all_in$hour_id == 090100, 083123,
                                                                                       ifelse(all_in$hour_id == 100100,093023,
                                                                                              ifelse(all_in$hour_id == 110100,103123,
                                                                                                     ifelse(all_in$hour_id == 120100, 113023,
                                                                                                            all_in$hour_id - 1))))))))))))


all_in$location_hour_before_in = (all_in$hour_before_id + all_in$location_in*1000000)


all_in = full_join(all_in, all_in_mean, by = "location_hour_before_in")

all_in = all_in[!is.na(all_in$location_in),]


## out airport

all_out <- all %>% 
  filter(location_out != 132)

all_out$hour_id = (all_out$hour + all_out$day*100 + all_out$month*10000)
all_out$hour_location_id = (all_out$hour_id + all_out$location_out*1000000)

all_out_mean <- all_out %>%
  group_by(hour_location_id) %>%
  summarize_at(c(13), mean, na.rm = TRUE)
names(all_out_mean)[names(all_out_mean) == 'hour_location_id'] <- 'location_hour_before_out'

# For day ahead indexing

all_out$hour_before_id = NA
all_out$hour_before_id = ifelse(all_out$hour_id == 010100, 123123,
                               ifelse(all_out$hour_id == 020100, 013123,
                                      ifelse(all_out$hour_id == 030100, 022823,
                                             ifelse(all_out$hour_id == 040100, 033123,
                                                    ifelse(all_out$hour_id == 050100, 043023,
                                                           ifelse(all_out$hour_id == 060100, 053123,
                                                                  ifelse(all_out$hour_id == 070100, 063023,
                                                                         ifelse(all_out$hour_id == 080100, 073123,
                                                                                ifelse(all_out$hour_id == 090100, 083123,
                                                                                       ifelse(all_out$hour_id == 100100,093023,
                                                                                              ifelse(all_out$hour_id == 110100,103123,
                                                                                                     ifelse(all_out$hour_id == 120100, 113023,
                                                                                                            all_out$hour_id - 1))))))))))))


all_out$location_hour_before_out = (all_out$hour_before_id + all_out$location_out*1000000)


all_out = full_join(all_out, all_out_mean, by = "location_hour_before_out")

all_out = all_out[!is.na(all_out$location_out),]


#### Day prior

### all_in

all_in$day_id = (all_in$month*100 + all_in$day)
all_in$day_before_id = NA
all_in$day_before_id = ifelse(all_in$day_id == 0101, 1231,
                           ifelse (all_in$day_id == 0201, 0131,
                           ifelse (all_in$day_id == 0301, 0228,
                                   ifelse(all_in$day_id == 0401, 0331,
                                          ifelse(all_in$day_id == 0501, 0430,
                                                 ifelse(all_in$day_id == 0601, 0531,
                                                        ifelse(all_in$day_id == 0701, 0630,
                                                               ifelse(all_in$day_id == 0801, 0731,
                                                                      ifelse(all_in$day_id == 0901, 0831,
                                                                             ifelse(all_in$day_id == 1001, 0930,
                                                                                    ifelse(all_in$day_id == 1101, 1031,
                                                                                           ifelse(all_in$day_id == 1201, 1130,
                                                                                                  all_in$day_id - 1))))))))))))


all_in$day_location_id = (all_in$day_id + all_in$location_in*10000)
all_in$day_before_location_id = (all_in$day_before_id + all_in$location_in*10000)

all_in_mean <- all_in %>%
  group_by(day_location_id) %>%
  summarize_at(c(13), mean, na.rm = TRUE)
names(all_in_mean)[names(all_in_mean) == 'day_location_id'] <- 'day_before_location_id'

all_in = full_join(all_in, all_in_mean, by = "day_before_location_id")

names(all_out_mean)[names(all_out_mean) == 'hour_location_id'] <- 'location_hour_before_out'

all_in = all_in[!is.na(all_in$location_in),]


### all_out

all_out$day_id = (all_out$month*100 + all_out$day)
all_out$day_before_id = NA
all_out$day_before_id = ifelse(all_out$day_id == 0101, 1231,
                              ifelse (all_out$day_id == 0201, 0131,
                                      ifelse (all_out$day_id == 0301, 0228,
                                              ifelse(all_out$day_id == 0401, 0331,
                                                     ifelse(all_out$day_id == 0501, 0430,
                                                            ifelse(all_out$day_id == 0601, 0531,
                                                                   ifelse(all_out$day_id == 0701, 0630,
                                                                          ifelse(all_out$day_id == 0801, 0731,
                                                                                 ifelse(all_out$day_id == 0901, 0831,
                                                                                        ifelse(all_out$day_id == 1001, 0930,
                                                                                               ifelse(all_out$day_id == 1101, 1031,
                                                                                                      ifelse(all_out$day_id == 1201, 1130,
                                                                                                             all_out$day_id - 1))))))))))))

all_out$day_location_id = (all_out$day_id + all_out$location_out*10000)
all_out$day_before_location_id = (all_out$day_before_id + all_out$location_out*10000)

all_out_mean <- all_out %>%
  group_by(day_location_id) %>%
  summarize_at(c(13), mean, na.rm = TRUE)
names(all_out_mean)[names(all_out_mean) == 'day_location_id'] <- 'day_before_location_id'

all_out = full_join(all_out, all_out_mean, by = "day_before_location_id")

names(all_out_mean)[names(all_out_mean) == 'hour_location_id'] <- 'location_hour_before_out'

all_out = all_out[!is.na(all_out$location_in),]


##### merge

all_in = all_in[, c(c(1:25),30,35)]
all_out = all_out[, c(c(1:25),30,35)]
all = rbind.data.frame(all_in, all_out)

### imputation

all$travel_time.y = ifelse(is.na(all$travel_time.y), all$travel_time.x.y, all$travel_time.y)

# delete remaining NAs

all = all[!is.na(all$travel_time.y),]

# rename

names(all)[names(all) == 'travel_time.y'] <- 'hour_before_time'
names(all)[names(all) == 'travel_time.x.y'] <- 'day_before_time'


#### delete July 1

include = all$day == 1 & all$month == 7
all_test = all_test[!include,]

