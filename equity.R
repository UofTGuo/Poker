library("holdem")
options(digits=6)

#equity
equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1){
  
  p1_luck_equity = 0
  p2_luck_equity = 0
  p1_skill_equity = 0
  p2_skill_equity = 0
  b3 = deal1(numattable1)
  player1cards = b3$plnum1[1,]
  player2cards = b3$plnum1[2,]
  player1suits = b3$plsuit1[1,]
  player2suits = b3$plsuit1[2,]
  
  # pre-flop equity
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1) 
  pre_flop_win_prob = win_prob(c(),c(),player1cards,player1suits,player2cards,player2suits)
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

#helper function - winning probability in each betting round
win_prob = function(boardcards,boardsuits,player1cards,player1suits,player2cards,player2suits){
  winning_prob = c()
  p1_value = handeval(c(boardcards,player1cards),c(boardsuits,player1suits))
  p2_value = handeval(c(boardcards,player2cards),c(boardsuits,player2suits))
  winning_prob[1] = p1_value/(p1_value+p2_value)
  winning_prob[2] = p2_value/(p1_value+p2_value)
  return(winning_prob)
}

avg_equity = function(numattable, playerseats, chips, blinds, dealer, chipstart, decision, num_hand){
  result = matrix(nrow = num_hand, ncol = 4)
  for(i in 1:num_hand){
    result[i,] = equity(numattable, playerseats, chips, blinds, dealer, chipstart, decision)
  }
  return(c(mean(result[,1]),mean(result[,2]),mean(result[,3]),mean(result[,4])))
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
decision4 = list(vera, william)

avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,20)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,50)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,100)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,200)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,300)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision1,1000)

avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,20)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,50)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,100)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,200)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,300)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision2,1000)

avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,20)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,50)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,100)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,200)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,300)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision3,1000)

avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,20)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,50)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,100)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,200)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,300)
avg_equity(numattable1,playerseats1,chips1,blinds1,dealer1,chipstart1,decision4,1000)
