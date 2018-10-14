library("holdem")
options(digits=6)

#equity
equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1){
  
  #strflsh1
  #boardcards = c(4,5,6,8,13)
  #boardsuits = c(2,3,2,2,2)
  #player1cards = c(2,3)
  #player1suits = c(2,1)
  #player2cards = c(7,3)
  #player2suits = c(2,4)
  
  p1_luck_equity = 0
  p2_luck_equity = 0
  p1_skill_equity = 0
  p2_skill_equity = 0
  
  player1cards = pre_flop$num[1:2]
  player2cards = pre_flop$num[3:4]
  player1suits = pre_flop$st[1:2]
  player2suits = pre_flop$st[3:4]
  
  # pre-flop equity
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision3) 
  #pre_flop_win_prob = win_prob(c(),c(),player1cards,player1suits,player2cards,player2suits)
  pre_flop_win_prob = win_prob()
  
  if(b4$rb[1,1] == 20 && b4$rb[2,1] == 10){
    p1_luck_equity =+ pre_flop_win_prob[1]*(b4$rb[1,1]+b4$rb[2,1]) - b4$rb[1,1]
    p2_luck_equity =+ pre_flop_win_prob[2]*(b4$rb[1,1]+b4$rb[2,1]) - b4$rb[2,1]
    }
  else{
    p1_luck_equity =+ pre_flop_win_prob[1]*(2*blinds1[2]) - blinds1[2]
    p2_luck_equity =+ pre_flop_win_prob[2]*(2*blinds1[2]) - blinds1[2]
    p1_skill_equity =+ pre_flop_win_prob[1]*(b4$p1-2*blinds1[2]) - (b4$rb[1,1] - blinds1[2])
    p2_skill_equity =+ pre_flop_win_prob[2]*(b4$p1-2*blinds1[2]) - (b4$rb[2,1] - blinds1[2])
    }
  #pre_flop_chip = calcwin1(numattable1,playerseats1, b3, b4)
  
  # the flop equity
  if(b4$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntable1, decision1) 
  flop_win_prob = win_prob(b3$brdnum1[1:3],b3$brdsuit1[1:3],player1cards,player1suits,player2cards,player2suits)
  p1_luck_equity =+ (flop_win_prob[1]-pre_flop_win_prob[1])*b4$p1
  p2_luck_equity =+ (flop_win_prob[2]-pre_flop_win_prob[2])*b4$p1
  p1_skill_equity =+ (flop_win_prob[1])*(b5$p1-b4$p1) - b5$rb[1,2]
  p2_skill_equity =+ (flop_win_prob[2])*(b5$p1-b4$p1) - b5$rb[2,2]
  #flop_chip = calcwin1(numattable1,playerseats1, b3, b5)
  
  # the turn equity
  if(b5$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b5,3, ntable1, decision1)
  turn_win_prob = win_prob(b3$brdnum1[1:4],b3$brdsuit1[1:4],player1cards,player1suits,player2cards,player2suits)
  p1_luck_equity =+ (turn_win_prob[1]-pre_flop_win_prob[1])*b5$p1
  p2_luck_equity =+ (turn_win_prob[2]-pre_flop_win_prob[2])*b5$p1
  p1_skill_equity =+ (turn_win_prob[1])*(b6$p1-b5$p1) - b6$rb[1,3]
  p2_skill_equity =+ (turn_win_prob[2])*(b6$p1-b5$p1) - b6$rb[2,3]
  #turn_chip = calcwin1(numattable1,playerseats1, b3, b6)
  
  # the river equity
  if(b6$all1 == 2){
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break
  }
  b7 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b6,4, ntable1, decision1)
  river_win_prob = win_prob(b3$brdnum1,b3$brdsuit1,player1cards,player1suits,player2cards,player2suits)
  p1_luck_equity =+ (river_win_prob[1]-pre_flop_win_prob[1])*b6$p1
  p2_luck_equity =+ (river_win_prob[2]-pre_flop_win_prob[2])*b6$p1
  p1_skill_equity =+ (river_win_prob[1])*(b7$p1-b6$p1) - b7$rb[1,3]
  p2_skill_equity =+ (river_win_prob[2])*(b7$p1-b6$p1) - b7$rb[2,3]
  #river_chip = calcwin1(numattable1,playerseats1, b3, b7)
  
  return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
}



deal_card = function(numpl){
  ## numpl is the number of players at the table
  numcards = 2*numpl+5
  crds1 = order(runif(52))[1:numcards]
  crds2 = switch2(crds1)
  num1 = crds2$num
  suit1 = crds2$st
  brdnum1 = num1[(numcards-4):numcards]
  brdsuit1 = suit1[(numcards-4):numcards]
  plnum1 = matrix(num1[1:(2*numpl)],ncol=2)
  plsuit1 = matrix(suit1[1:(2*numpl)],ncol=2)
  ## order them
  for(i in c(1:numpl)){
    if(plnum1[i,1]<plnum1[i,2]){
      a = plnum1[i,1]
      plnum1[i,1] = plnum1[i,2]
      plnum1[i,2] = a
      a = plsuit1[i,1]
      plsuit1[i,1] = plsuit1[i,2]
      plsuit1[i,2] = a
    }
  }
  b9 = list(plnum1=plnum1, plsuit1=plsuit1,brdnum1=brdnum1, brdsuit1=brdsuit1)
  b9
}

sample_cards = function(numattable1){
  all_cards = order(runif(52))
  index = sample(52,size=numattable1*2+5,replace=FALSE)
  return(index)
}


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
  win_prob = c()
  temp = 0
  for (i in 1:iters) {
    if(round == "pre_flop"){
      board_index = sample(pre_flop_left,5)
      board_info = switch2(board_index)
      boardcards = board_info$num
      boardsuits = board_info$st
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){temp =+ 1}
    }
    
    if(round == "flop"){
      dealt_board = switch2(flop_index[5:7])
      board_index = sample(flop_left,2)
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){temp =+ 1}
    }
    
    if(round == "turn"){
      dealt_board = switch2(turn_index[5:8])
      board_index = sample(turn_left,1)
      board_info = switch2(board_index)
      boardcards = c(dealt_board$num,board_info$num)
      boardsuits = c(dealt_board$st,board_info$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){temp =+ 1}
    }
    
    if(round == "river"){
      dealt_board = switch2(river_index[5:8])
      boardcards = c(dealt_board$num)
      boardsuits = c(dealt_board$st)
      p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
      p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
      if(p1_value >= p2_value){temp =+ 1}
    }
  }
  win_prob[1] = temp/iters 
  win_prob[2] = 1-win_prob[1]
  return(win_prob)
}

#Example
#numattable1 = 2
#dealt_index = sample_cards(numattable1)
#win_prob(dealt_index,"pre_flop",3)

avg_equity = function(numattable, playerseats, chips, blinds, dealer, chipstart, decision, num_hand){
  result = matrix(nrow = num_hand, ncol = 4)
  result_one = c()
  result_two = c()
  result_three = c()
  result_four = c()
  for(i in 1:num_hand){
    temp = equity(numattable, playerseats, chips, blinds, dealer, chipstart, decision)
    print(temp)
    result_one = c(result_one, temp[1])
    result_two = c(result_two,temp[2])
    result_three = c(result_three,temp[3])
    result_four = c(result_four,temp[4])
  }
  cat("final output", c(mean(result_one),mean(result_two),mean(result_three),mean(result_four)))
}

# Example
#numattable1 = 2
#playerseats1 = c(2,1)
#chips1 = c(1000,1000)
#blinds1 = c(10,20)
#dealer1 = 1
#chipstart1 = 1000
#decision1 = list(zelda, vera) 
#decision2 = list(zelda, william)
#decision3 = list(zelda,tommy)
#decision4 = list(vera, william)

#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,20)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,50)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,100)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,200)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,300)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,1000)

#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,20)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,50)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,100)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,200)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,300)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,1000)

#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,20)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,50)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,100)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,200)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,300)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,1000)

#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,20)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,50)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,100)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,200)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,300)
#avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,1000)
