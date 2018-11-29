library("holdem")
source("new_players.R")
options(digits=6)

# calculating equity due to luck and equity due to skill
equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1, iters){
  
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
  
  ### Pre-flop equity

  p1_luck_equity = 0
  p2_luck_equity = 0
  p1_skill_equity = 0
  p2_skill_equity = 0
  
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntables1, decision1) 
  pre_flop_win_prob = win_prob(numattable1,dealt_index,"pre_flop", iters)
  
  # case of small blind directly folding
  if(b4$rb[1,1] == blinds1[2] && b4$rb[2,1] == blinds1[1]){

  #New Proposed pre-flop luck (skill changes depending on this value)
	#p1_luck_equity = p1_luck_equity + min((2*blinds1[2]*pre_flop_win_prob[1]-blinds1[2]),blinds1[1])
	#p2_luck_equity = p2_luck_equity + (2*blinds1[2]*pre_flop_win_prob[2] - blinds1[2])
  #Schoenberg's Way
	p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
 	p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])
    p1_skill_equity = p1_skill_equity + blinds1[1] - p1_luck_equity
    p2_skill_equity = p2_skill_equity - blinds1[1] - p2_luck_equity
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break

  #Only needed for new proposed way
  } else {

    p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
    p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])

  }

  if(b4$all1 == 2){
  #case of small blind matching blinds then folding
    if(b4$rb[1,1] > b4$rb[2,1]){

      p1_skill_equity = p1_skill_equity + 1*(b4$p1) - b4$rb[1,1] - p1_luck_equity
      p2_skill_equity = p2_skill_equity + 0*(b4$p1) - b4$rb[2,1] - p2_luck_equity
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break

    } else {

   #case of small blind raising and big blind folding
      p1_skill_equity = p1_skill_equity + 0*(b4$p1) - b4$rb[1,1] - p1_luck_equity
      p2_skill_equity = p2_skill_equity + 1*(b4$p1) - b4$rb[2,1] - p2_luck_equity
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break
      }
  }
  
  #case where no one folds
  p1_skill_equity = p1_skill_equity + pre_flop_win_prob[1]*(b4$p1) - b4$rb[1,1] - p1_luck_equity
  p2_skill_equity = p2_skill_equity + pre_flop_win_prob[2]*(b4$p1) - b4$rb[2,1] - p2_luck_equity

  ### Flop equity  
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntables1, decision1) 
  flop_win_prob = win_prob(numattable1,dealt_index,"flop", iters)
  
  p1_luck_equity2 = (flop_win_prob[1]-pre_flop_win_prob[1])*b4$p1
  p2_luck_equity2 = (flop_win_prob[2]-pre_flop_win_prob[2])*b4$p1
  p1_luck_equity <- p1_luck_equity + p1_luck_equity2
  p2_luck_equity <- p2_luck_equity + p2_luck_equity2

  if(b5$all1 == 2){
  #player 2 folding
    if(b5$rb[1,2] > b5$rb[2,2]){

      p1_skill_equity = p1_skill_equity + (1-flop_win_prob[1])*b4$p1
      p2_skill_equity = p2_skill_equity - (1-flop_win_prob[1])*b4$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break

    } else {

   #player 1 folding
      p1_skill_equity = p1_skill_equity - (1-flop_win_prob[2])*b4$p1
      p2_skill_equity = p2_skill_equity + (1-flop_win_prob[2])*b4$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break
      }
  }
  
  #no one folding
  p1_skill_equity = p1_skill_equity + (flop_win_prob[1])*(b5$p1-b4$p1) - b5$rb[1,2]
  p2_skill_equity = p2_skill_equity + (flop_win_prob[2])*(b5$p1-b4$p1) - b5$rb[2,2]

  ### Turn equity
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3, b5, 3, ntables1, decision1)
  turn_win_prob = win_prob(numattable1, dealt_index, "turn", iters)

  p1_luck_equity2 = (turn_win_prob[1]-flop_win_prob[1])*b5$p1
  p2_luck_equity2 = (turn_win_prob[2]-flop_win_prob[2])*b5$p1
  p1_luck_equity <- p1_luck_equity + p1_luck_equity2
  p2_luck_equity <- p2_luck_equity + p2_luck_equity2

  if(b6$all1 == 2){
  #player 2 folding
    if(b6$rb[1,3] > b6$rb[2,3]){

      p1_skill_equity = p1_skill_equity + (1-turn_win_prob[1])*b5$p1
      p2_skill_equity = p2_skill_equity - (1-turn_win_prob[1])*b5$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break

    } else {

   #player 1 folding
      p1_skill_equity = p1_skill_equity - (1-turn_win_prob[2])*b5$p1
      p2_skill_equity = p2_skill_equity + (1-turn_win_prob[2])*b5$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break
      }
  }

  #no folds
  p1_skill_equity = p1_skill_equity + (turn_win_prob[1])*(b6$p1-b5$p1) - b6$rb[1,3]
  p2_skill_equity = p2_skill_equity + (turn_win_prob[2])*(b6$p1-b5$p1) - b6$rb[2,3]
 
  ### River equity
  b7 = bid2(numattable1, playerseats1, blinds1, dealer1, b3, b6, 4, ntables1, decision1)
  river_win_prob = win_prob(numattable1, dealt_index,"river", iters)

  p1_luck_equity2 = (river_win_prob[1]-turn_win_prob[1])*b6$p1
  p2_luck_equity2 = (river_win_prob[2]-turn_win_prob[2])*b6$p1
  p1_luck_equity <- p1_luck_equity + p1_luck_equity2
  p2_luck_equity <- p2_luck_equity + p2_luck_equity2

  if(b7$all1 == 2){
  #player 2 folding
    if(b7$rb[1,4] > b7$rb[2,4]){

      p1_skill_equity = p1_skill_equity + (1-river_win_prob[1])*b6$p1
      p2_skill_equity = p2_skill_equity - (1-river_win_prob[1])*b6$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break

    } else {

   #player 1 folding
      p1_skill_equity = p1_skill_equity - (1-river_win_prob[2])*b6$p1
      p2_skill_equity = p2_skill_equity + (1-river_win_prob[2])*b6$p1
      return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
      break
      }
  }

  p1_skill_equity = p1_skill_equity + (river_win_prob[1])*(b7$p1-b6$p1) - b7$rb[1,4]
  p2_skill_equity = p2_skill_equity + (river_win_prob[2])*(b7$p1-b6$p1) - b7$rb[2,4]
  
  return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
}

# Sampling index of cards
sample_cards = function(numattable1){
  all_cards = sample(52)
  index = sample(52,size=numattable1*2+5,replace=FALSE)
  return(index)
}

# Calculate winning probability using Monte Carlo method and exact method.
win_prob = function(numattable1,dealt_index, round, iters){
  all_index = sample(52)
  pre_flop_index = dealt_index[1:(2*numattable1)]
  flop_index = dealt_index[1:(2*numattable1+3)]
  turn_index = dealt_index[1:(2*numattable1+4)]
  river_index = dealt_index[1:(2*numattable1+5)]
  
  pre_flop_left = all_index[-which(all_index %in% pre_flop_index)]
  flop_left = all_index[-which(all_index %in% flop_index)]
  turn_left = all_index[-which(all_index %in% turn_index)]
  river_left = all_index[-which(all_index %in% river_index)]
  
  player_info = switch2(pre_flop_index)
  player1cards = player_info$num[1:2]
  player2cards = player_info$num[3:4]
  player1suits = player_info$st[1:2]
  player2suits = player_info$st[3:4]

  winprob <- numeric(3)
  board_index <- numeric(5)
  temp = 0
  tie = 0

  #monte carlo method
  if(round == "pre_flop"){
    for(i in 1:iters){
      board_index = sample(pre_flop_left,5)
      board_info = switch2(board_index)
      boardcards = board_info$num
      boardsuits = board_info$st
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value > p2_value){
        temp = temp + 1
        } else if(p1_value == p2_value){
	  tie = tie + 1
	  }
     }
    winprob[3] <- tie/iters
    winprob[1] <- temp/iters+winprob[3]/2
    winprob[2] <- 1-winprob[1]
    return(winprob)
    break
  }

  #exact calculation
  if(round == "flop"){
    temp_flop = t(combn(flop_left,2))
    flop_comb = nrow(temp_flop)
    flop_info <- matrix(rep(flop_index[5:7],flop_comb),flop_comb,3,byrow=T)
    board <- cbind(flop_info,temp_flop)
    board_info <- switch2(board)
    for(j in 1:flop_comb){
	boardcards <- board_info$num[j,]
      boardsuits <- board_info$st[c(j,j+flop_comb,j+2*flop_comb,j+3*flop_comb,j+4*flop_comb)]
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value > p2_value){
        temp = temp + 1
      } else if(p1_value == p2_value){
	  tie = tie + 1
	}
    }
    winprob[3] <- tie/flop_comb
    winprob[1] <- temp/flop_comb+winprob[3]/2
    winprob[2] <- 1-winprob[1]
    return(winprob)
    break
  }

  #exact calculation
  if(round == "turn"){
    temp_turn = turn_left
    turn_comb = length(temp_turn)
    board <- matrix(c(rep(turn_index[5:8],each=turn_comb),temp_turn),turn_comb,5)
    board_info <- switch2(board)
    for (k in 1:turn_comb){
	boardcards <- board_info$num[k,]
      boardsuits <- board_info$st[c(k,k+turn_comb,k+2*turn_comb,k+3*turn_comb,k+4*turn_comb)]
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value > p2_value){
        temp <- temp + 1
      } else if(p1_value == p2_value){
	  tie <- tie + 1
	}
    }
    winprob[3] <- tie/turn_comb
    winprob[1] <- temp/turn_comb+winprob[3]/2
    winprob[2] <- 1-winprob[1]
    return(winprob)
    break
  }

  if(round == "river"){
    dealt_board = switch2(river_index[5:9])
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
}

# Calculating average equity
avg_equity = function(numattable, chips, blinds, dealer, chipstart, decision, num_hand, iters){
  p1_luck = p2_luck = p1_skill = p2_skill = p1_chip = p2_chip = numeric(num_hand)
  cond <- ((1:num_hand)%%2) == 0
  for(i in 1:num_hand){
    if(cond[i]){
      temp = equity(numattable, c(1,2), chips, blinds, dealer, chipstart, decision,iters)
      p1_luck[i] = temp[1]
      p2_luck[i] = temp[2]
      p1_skill[i] = temp[3]
      p2_skill[i] = temp[4]
      p1_chip[i] = chips[1]+temp[1]+temp[3]
      p2_chip[i] = chips[2]+temp[2]+temp[4]
      cat(c(temp,(chips[1]+temp[1]+temp[3]),(chips[2]+temp[2]+temp[4])),"\n")
    } else {
      temp = equity(numattable, c(2,1), chips, blinds, dealer, chipstart, decision,iters)
      p1_luck[i] = temp[2]
      p2_luck[i] = temp[1]
      p1_skill[i] = temp[4]
      p2_skill[i] = temp[3]
      p1_chip[i] = chips[2]+temp[2]+temp[4]
      p2_chip[i] = chips[1]+temp[1]+temp[3]
      cat(c(p1_luck[i],p2_luck[i],p1_skill[i],p2_skill[i],(chips[2]+temp[2]+temp[4]),(chips[1]+temp[1]+temp[3])),"\n") 
    }
  }
  cat("final output", c(mean(p1_luck),mean(p2_luck),mean(p1_skill),mean(p2_skill),mean(p1_chip),mean(p2_chip)),"\n")
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

num_hand = 1000
iters = 3000
M <- 1

dec1_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
 dec1_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_one,num_hand,iters)
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

dec2_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
  dec2_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_two,num_hand,iters)
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

dec3_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
  dec3_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_three,num_hand,iters)
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

dec4_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
  dec4_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_four,num_hand,iters)
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

dec5_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
  dec5_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_five,num_hand,iters)
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

dec6_result_list = matrix(nrow=M,ncol=6)
for(i in 1:M){
  dec6_result_list[i,] = avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_six,num_hand,iters)
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