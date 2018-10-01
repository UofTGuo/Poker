library("holdem")

#luck equity
luck_equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1){
  
  b3 = deal1(numattable1)
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1) 
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntable1, decision1) 
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b5,3, ntable1, decision1) 
  b7 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b6,4, ntable1, decision1) 
  
  # pre-flop equity
  
  # the flop equity
  
  # the turn equity
  
  # the river equity
  
  chips2 = calcwin1(numattable1,playerseats1, b3, b7)
  }


#skill equity
skill_equity = function(numattable1, playerseats1, chips1, blinds1, dealer1, chipstart1, decision1){
  
  b3 = deal1(numattable1)
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1) 
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntable1, decision1) 
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b5,3, ntable1, decision1) 
  b7 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b6,4, ntable1, decision1) 
  
  # pre-flop equity
  
  # the flop equity
  
  # the turn equity
  
  # the river equity
  
  chips2 = calcwin1(numattable1,playerseats1, b3, b7)

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

# Example
numattable1 = 2
playerseats1 = c(2,1)
chips1 = c(100,100)
blinds1 = c(10,20)
dealer1 = 1
ntable1 = 1
myfast1 = 2
t1 = 0.5
t2 = 1
chipstart1 = 100
decision1 = list(gravity, tommy)


