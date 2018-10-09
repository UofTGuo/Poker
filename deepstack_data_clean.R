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

#Take first 10K data points
ds_data = ds_data[1:10000,]

ds_data$`Big|Small` = as.character(ds_data$`Big|Small`)
split_blind = strsplit(ds_data$`Big|Small`,split="|",fixed = T)

for(i in 1:(dim(ds_data)[1])){
  ds_data$big_blind[i] = split_blind[[i]][1]
  ds_data$small_blind[i] = split_blind[[i]][2]
}

ds_data$big_blind = gsub(" ","",ds_data$big_blind)
ds_data$small_blind = gsub(" ","",ds_data$small_blind)

#Implement betting functions
ds_betting = function(ds_data){
  result = c()
  
  for(i in 1:(dim(ds_data)[1])){
    # first column is DeepStack, and the second column is the professional player
    rb = matrix(nrow = 2, ncol = 4)
    if (ds_data$big_blind[i] = "DeepStack"){
      rb[1,1] = 100
      rb[2,1] = 50
    }
    if (ds_data$small_blind[i] = "DeepStack"){
      rb[1,1] = 50
      rb[2,1] = 100
    }
    
    if (ds_data$PreFlop[i] == "f"){
      #rb[1,1] =+
      #rb[2,1] =+
    }
    
    if (ds_data$Flop[i] == ""){
      #rb[1,2] = 
      #rb[2,2] = 
    }
    
    if (ds_data$Turn[i] == ""){
      #rb[1,3] = 
      #rb[2,3] = 
    }
    
    if (ds_data$River[i] == ""){
      #rb[1,4] = 
      #rb[2,4] =
    }
    
  }
  
  return(result)
}
  
