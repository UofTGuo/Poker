setwd("/Users/zhaoyuguo/Desktop/Poker Project/new/Poker")
library(holdem)
source("convert_functions.R")
library(dplyr)

#Reference data set, containing some important informations
all_hands = read.csv("~/Desktop/Poker Project/new/Poker/data/all_hands.csv")

# Small blind goes first pre-flop
# Big blind goes first post-flop
# f: fold
# c: check/call
# cc: both check
# rX: raise to X(player's total chips in pot)
# Initial blinds: 50/100
# Card dealt legend: Player1|Player2/Flop/Turn/River
# d/c/h/s: diamond/club/heart/spade

ds_data = read.csv("~/Desktop/Poker Project/new/Poker/data/all_hands2.csv")
names(ds_data) = as.matrix(ds_data[1,])
#remove first row
ds_data = ds_data[-1,]

#Implement betting functions
ds_betting = function(data){
  result = c()
  for(i in 1:(dim(data)[1])){
    rb = matrix(nrow = 2, ncol = 4)
  }
  return(result)
}
  
