library("holdem")
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
  
  if(b4$rb[1,1] == 20 && b4$rb[2,1] == 10){
    p1_luck_equity = p1_luck_equity + pre_flop_win_prob[1]*(b4$rb[1,1]+b4$rb[2,1]) - b4$rb[1,1]
    p2_luck_equity = p2_luck_equity + pre_flop_win_prob[2]*(b4$rb[1,1]+b4$rb[2,1]) - b4$rb[2,1]
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
  }
  
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

# Calculating winning probability using Monte Carlo method
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
  for (i in 1:iters) {
    if(round == "pre_flop"){
      board_index = sample(pre_flop_left,5)
      board_info = switch2(board_index)
      boardcards = board_info$num
      boardsuits = board_info$st
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
    }
    if(round == "flop"){
      dealt_board = switch2(flop_index[5:7])
      board_index = sample(flop_left,2)
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
    }
    if(round == "turn"){
      dealt_board = switch2(turn_index[5:8])
      board_index = sample(turn_left,1)
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
    }
    if(round == "river"){
      dealt_board = switch2(river_index[5:8])
      boardcards = c(dealt_board$num)
      boardsuits = c(dealt_board$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){
        temp = temp + 1}
    }
  }
  winprob[1] = temp/iters 
  winprob[2] = 1-winprob[1]
  return(winprob)
}

# Calculating average equity
avg_equity = function(numattable, playerseats, chips, blinds, dealer, chipstart, decision, num_hand, iters){
  result_one = c()
  result_two = c()
  result_three = c()
  result_four = c()
  for(i in 1:num_hand){
    temp = equity(numattable, playerseats, chips, blinds, dealer, chipstart, decision,iters)
    print(temp)
    result_one = c(result_one, temp[1])
    result_two = c(result_two,temp[2])
    result_three = c(result_three,temp[3])
    result_four = c(result_four,temp[4])
  }
  cat("final output", c(mean(result_one),mean(result_two),mean(result_three),mean(result_four)))
}

# Example
numattable1 = 2
playerseats1 = c(2,1)
chips1 = c(1000,1000)
blinds1 = c(10,20)
dealer1 = 1
chipstart1 = 1000

decision1 = list(zelda, vera) 
decision2 = list(zelda, william)
decision3 = list(zelda,tommy)
decision4 = list(vera, vera)
iters = 100

avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,1000,iters)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,1000,iters)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,1000,iters)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,1000,iters)
