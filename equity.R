getwd()
setwd("C:/Users/kenny/Desktop/poker/Poker")
library("holdem")
source("new_players.R")
options(digits=6)

# calculating equity due to luck and equity due to skill
equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1, iters){
  
  p1_luck_equity = 0
  p2_luck_equity = 0
  p1_skill_equity = 0
  p2_skill_equity = 0
  
  dealt_index = sample_cards(numattable1)
  player_index = dealt_index[1:(2*numattable1)]
  player_info = switch2(player_index)
  board_index = dealt_index[(2*numattable1+1):(2*numattable1+5)]
  board_info = switch2(board_index)
  
  player1cards = player_info$num[1:2]
  player2cards = player_info$num[3:4]
  player1suits = player_info$st[1:2]
  player2suits = player_info$st[3:4]
  
  # Modify b3
  b3 = deal1(numattable1)
  b3$plnum1[1,] = player1cards
  b3$plnum1[2,] = player2cards
  b3$plsuit1[1,] = player1suits
  b3$plsuit1[2,] = player2suits
  b3$brdnum1 =  board_info$num
  b3$brdsuit1 = board_info$st
  
  # Pre-flop equity
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1) 
  pre_flop_win_prob = win_prob(dealt_index,"pre_flop", iters)
  
  # case of small blind directly folding
  if(b4$rb[1,1] == 100 && b4$rb[2,1] == 50){
    
    #Schoenberg's way
    #p1_luck_equity = p1_luck_equity + min((2*b4$rb[1,1]*pre_flop_win_prob[1]-b4$rb[1,1]),rb[2,1])
    #p2_luck_equity = p2_luck_equity + (2*b4$rb[1,1]*pre_flop_win_prob[2] - rb[1,1])
    #p1_skill_equity = p1_skill_equity + b4$rb[2,1] - p1_luck_equity
    #p2_skill_equity = p2_skill_equity - b4$rb[2,1] - p2_luck_equity
    
    #New Purposed way
    p1_luck_equity = p1_luck_equity + (2*b4$rb[1,1]*pre_flop_win_prob[1] - b4$rb[1,1])
    p2_luck_equity = p2_luck_equity + max((2*b4$rb[1,1]*pre_flop_win_prob[2]-b4$rb[1,1]),-b4$rb[2,1])
    p1_skill_equity = p1_skill_equity + b4$rb[2,1] - p1_luck_equity
    p2_skill_equity = p2_skill_equity - b4$rb[2,1] - p2_luck_equity
    
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
  }
  
  # case of small blind calling the big blind or raising a blind
  p1_luck_equity = p1_luck_equity + pre_flop_win_prob[1]*(2*blinds1[2]) - blinds1[2]
  p2_luck_equity = p2_luck_equity + pre_flop_win_prob[2]*(2*blinds1[2]) - blinds1[2]
  p1_skill_equity = p1_skill_equity + pre_flop_win_prob[1]*(b4$p1-2*blinds1[2]) - (b4$rb[1,1] - blinds1[2])
  p2_skill_equity = p2_skill_equity + pre_flop_win_prob[2]*(b4$p1-2*blinds1[2]) - (b4$rb[2,1] - blinds1[2])
  
  # Flop equity
  if(b4$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntable1, decision1) 
  flop_win_prob = win_prob(dealt_index,"flop", iters)
  p1_luck_equity = p1_luck_equity + (flop_win_prob[1]-pre_flop_win_prob[1])*b4$p1
  p2_luck_equity = p2_luck_equity + (flop_win_prob[2]-pre_flop_win_prob[2])*b4$p1
  p1_skill_equity = p1_skill_equity + (flop_win_prob[1])*(b5$p1-b4$p1) - b5$rb[1,2]
  p2_skill_equity = p2_skill_equity + (flop_win_prob[2])*(b5$p1-b4$p1) - b5$rb[2,2]
  
  # Turn equity
  if(b5$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3, b5, 3, ntable1, decision1)
  turn_win_prob = win_prob(dealt_index, "turn", iters)
  p1_luck_equity = p1_luck_equity + (turn_win_prob[1]-pre_flop_win_prob[1])*b5$p1
  p2_luck_equity = p2_luck_equity + (turn_win_prob[2]-pre_flop_win_prob[2])*b5$p1
  p1_skill_equity = p1_skill_equity + (turn_win_prob[1])*(b6$p1-b5$p1) - b6$rb[1,3]
  p2_skill_equity = p2_skill_equity + (turn_win_prob[2])*(b6$p1-b5$p1) - b6$rb[2,3]
  
  # River equity
  if(b6$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  
  b7 = bid2(numattable1, playerseats1, blinds1, dealer1, b3, b6, 4, ntable1, decision1)
  river_win_prob = win_prob(dealt_index,"river", iters)
  p1_luck_equity = p1_luck_equity + (river_win_prob[1]-pre_flop_win_prob[1])*b6$p1
  p2_luck_equity = p2_luck_equity + (river_win_prob[2]-pre_flop_win_prob[2])*b6$p1
  p1_skill_equity = p1_skill_equity + (river_win_prob[1])*(b7$p1-b6$p1) - b7$rb[1,4]
  p2_skill_equity = p2_skill_equity + (river_win_prob[2])*(b7$p1-b6$p1) - b7$rb[2,4]
  
  return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
}

# Sampling index of cards
sample_cards = function(numattable1){
  all_cards = order(runif(52))
  index = sample(52,size=numattable1*2+5,replace=FALSE)
  return(index)
}

# Calculate winning probability using Monte Carlo method and exact method.
win_prob = function(dealt_index, round, iters){
  all_index = order(runif(52))
  pre_flop_index = dealt_index[1:(2*numattable1)]
  flop_index = dealt_index[1:(2*numattable1+3)]
  turn_index = dealt_index[1:(2*numattable1+4)]
  river_index = dealt_index[1:(2*numattable1+5)]
  
  pre_flop_left = all_index[-pre_flop_index]
  flop_left = all_index[-flop_index]
  turn_left = all_index[-turn_index]
  river_left = all_index[-river_index]
  
  player_info = switch2(pre_flop_index)
  player1cards = player_info$num[1:2]
  player2cards = player_info$num[3:4]
  player1suits = player_info$st[1:2]
  player2suits = player_info$st[3:4]
  
  #monte carlo method
  winprob = c()
  temp = 0
  if(round == "pre_flop"){
    for(i in 1:iters){
      board_index = sample(pre_flop_left,5)
      board_info = switch2(board_index)
      boardcards = board_info$num
      boardsuits = board_info$st
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1
        }
    }
  }
  #exact calculation
  if(round == "flop"){
    temp_flop = combn(flop_left,2)
    flop_comb = length(temp_flop[1,])
    for (j in 1:flop_comb){
      dealt_board = switch2(flop_index[5:7])
      board_index = temp_flop[,j]
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
    }
  }
  #exact calculation
  if(round == "turn"){
    temp_turn = combn(turn_left,1)
    turn_comb = length(temp_turn)
    for (k in 1:turn_comb){
      dealt_board = switch2(turn_index[5:8])
      board_index = temp_turn[k]
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
      }
    }
  if(round == "river"){
    dealt_board = switch2(river_index[5:8])
    boardcards = c(dealt_board$num)
    boardsuits = c(dealt_board$st)
    p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
    p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
    if(p1_value > p2_value){
      return(c(1,0))
      break
    }
    if(p1_value == p2_value){
      return(c(0.5,0.5))
      break
    }
    else{
      return(c(0,1)) 
      break
    }
  }
  winprob[1] = temp/iters 
  winprob[2] = 1-winprob[1]
  return(winprob)
}

# Calculating average equity
avg_equity = function(numattable, playerseats, chips, blinds, dealer, chipstart, decision, num_hand, iters){
  p1_luck = p2_luck = p1_skill = p2_skill = p1_chip = p2_chip = c()
  for(i in 1:num_hand){
    if((i%%2) == 0){
      temp = equity(numattable, c(1,2), chips, blinds, dealer, chipstart, decision,iters)
    }
    if((i%%2) == 1){
      temp = equity(numattable, c(2,1), chips, blinds, dealer, chipstart, decision,iters)
    }
    print(temp)
    p1_luck = c(p1_luck, temp[1])
    p2_luck = c(p2_luck, temp[2])
    p1_skill = c(p1_skill, temp[3])
    p2_skill = c(p2_skill, temp[4])
    p1_chip = c(p1_chip,(chips+temp[1]+temp[3]))
    p2_chip = c(p2_chip,(chips+temp[2]+temp[4]))
  }
  cat("final output", c(mean(p1_luck),mean(p2_luck),mean(p1_skill),mean(p2_skill),mean(p1_chip),mean(p2_chip)))
  output = c(mean(p1_luck),mean(p2_luck),mean(p1_skill),mean(p2_skill),mean(p1_chip),mean(p2_chip))
  return(output)
}

# Example
numattable1 = 2
chips1 = c(20000,20000)
blinds1 = c(50,100)
dealer1 = 1
chipstart1 = 20000

decision_one = list(marlon, marly) 
decision_two = list(marlon, martin)
decision_three = list(marlon, marty)
decision_four = list(marly, martin)
decision_five = list(marly, marty)
decision_six = list(martin, marty)

# Sanity check
# decision_one = list(marly, marlon)

num_hand = 1000
iters = 1000
M=10

dec1_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
 dec1_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_one,num_hand,iters)
}
mean(dec1_result_list[,1])
mean(dec1_result_list[,2])
mean(dec1_result_list[,3])
mean(dec1_result_list[,4])
mean(dec1_result_list[,5])
mean(dec1_result_list[,6])
sd(dec1_result_list[,1])/sqrt(M)
sd(dec1_result_list[,2])/sqrt(M)
sd(dec1_result_list[,3])/sqrt(M)
sd(dec1_result_list[,4])/sqrt(M)
sd(dec1_result_list[,5])/sqrt(M)
sd(dec1_result_list[,6])/sqrt(M)


dec2_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
  dec2_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_two,num_hand,iters)
}
mean(dec2_result_list[,1])
mean(dec2_result_list[,2])
mean(dec2_result_list[,3])
mean(dec2_result_list[,4])
mean(dec2_result_list[,5])
mean(dec2_result_list[,6])
sd(dec2_result_list[,1])/sqrt(M)
sd(dec2_result_list[,2])/sqrt(M)
sd(dec2_result_list[,3])/sqrt(M)
sd(dec2_result_list[,4])/sqrt(M)
sd(dec2_result_list[,5])/sqrt(M)
sd(dec2_result_list[,6])/sqrt(M)

dec3_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
  dec3_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_three,num_hand,iters)
}
mean(dec3_result_list[,1])
mean(dec3_result_list[,2])
mean(dec3_result_list[,3])
mean(dec3_result_list[,4])
mean(dec3_result_list[,5])
mean(dec3_result_list[,6])
sd(dec3_result_list[,1])/sqrt(M)
sd(dec3_result_list[,2])/sqrt(M)
sd(dec3_result_list[,3])/sqrt(M)
sd(dec3_result_list[,4])/sqrt(M)
sd(dec3_result_list[,5])/sqrt(M)
sd(dec3_result_list[,6])/sqrt(M)

dec4_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
  dec4_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_four,num_hand,iters)
}
mean(dec4_result_list[,1])
mean(dec4_result_list[,2])
mean(dec4_result_list[,3])
mean(dec4_result_list[,4])
mean(dec4_result_list[,5])
mean(dec4_result_list[,6])
sd(dec4_result_list[,1])/sqrt(M)
sd(dec4_result_list[,2])/sqrt(M)
sd(dec4_result_list[,3])/sqrt(M)
sd(dec4_result_list[,4])/sqrt(M)
sd(dec4_result_list[,5])/sqrt(M)
sd(dec4_result_list[,6])/sqrt(M)

dec5_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
  dec5_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_five,num_hand,iters)
}
mean(dec5_result_list[,1])
mean(dec5_result_list[,2])
mean(dec5_result_list[,3])
mean(dec5_result_list[,4])
mean(dec5_result_list[,5])
mean(dec5_result_list[,6])
sd(dec5_result_list[,1])/sqrt(M)
sd(dec5_result_list[,2])/sqrt(M)
sd(dec5_result_list[,3])/sqrt(M)
sd(dec5_result_list[,4])/sqrt(M)
sd(dec5_result_list[,5])/sqrt(M)
sd(dec5_result_list[,6])/sqrt(M)

dec6_result_list = matrix(nrow=M,ncol=4)
for(i in 1:M){
  dec6_result_list[i,] = avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision_six,num_hand,iters)
}
mean(dec6_result_list[,1])
mean(dec6_result_list[,2])
mean(dec6_result_list[,3])
mean(dec6_result_list[,4])
mean(dec6_result_list[,5])
mean(dec6_result_list[,6])
sd(dec6_result_list[,1])/sqrt(M)
sd(dec6_result_list[,2])/sqrt(M)
sd(dec6_result_list[,3])/sqrt(M)
sd(dec6_result_list[,4])/sqrt(M)
sd(dec6_result_list[,5])/sqrt(M)
sd(dec6_result_list[,6])/sqrt(M)
