setwd("/Users/zhaoyuguo/Desktop/Poker Project/new/Poker")
library(holdem)
source("convert_functions.R")
library(dplyr)

#Reference data set, containing some important informations
all_hands = read.csv("~/Desktop/Poker Project/new/Poker/data/all_hands.csv")

ds_data = read.csv("~/Desktop/Poker Project/new/Poker/data/all_hands2.csv")
names(ds_data) = as.matrix(ds_data[1,])
#remove first row
ds_data = ds_data[-1,]

#Implement betting functions
