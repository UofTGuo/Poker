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
  
  #endstate
  es = 0
  
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntables1, decision1) 
  pre_flop_win_prob = win_prob(numattable1,dealt_index,"pre_flop", iters)
  
  # case of small blind directly folding
  if(b4$rb[1,1] == blinds1[2] && b4$rb[2,1] == blinds1[1]){
	  #Schoenberg's Way
	  p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
          p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])
          p1_skill_equity = p1_skill_equity + blinds1[1] - p1_luck_equity
	  p2_skill_equity = p2_skill_equity - blinds1[1] - p2_luck_equity
	  return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
		   p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity, es))
	  break
  }
  # case of small blind *not* directly folding
  else {
	  p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
	  p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])
  }

  if(b4$all1 == 2){
    #case of small blind matching blinds then folding
    if(b4$rb[1,1] > b4$rb[2,1]){
      p1_skill_equity = p1_skill_equity + 1*(b4$p1) - b4$rb[1,1] - p1_luck_equity
      p2_skill_equity = p2_skill_equity + 0*(b4$p1) - b4$rb[2,1] - p2_luck_equity
      es = 1
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
      break
    } 
    #case of small blind raising and big blind folding
    else if(b4$rb[1,1] < b4$rb[2,1]){
      p1_skill_equity = p1_skill_equity + 0*(b4$p1) - b4$rb[1,1] - p1_luck_equity
      p2_skill_equity = p2_skill_equity + 1*(b4$p1) - b4$rb[2,1] - p2_luck_equity
      es = 2
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
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
      p1_skill_equity = p1_skill_equity + (1-flop_win_prob[1])*b4$p1 + b5$rb[2,2]
      p2_skill_equity = p2_skill_equity - (1-flop_win_prob[1])*b4$p1 - b5$rb[2,2]
      es = 3
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
      break
    }
    #player 1 folding
    else if(b5$rb[1,2] < b5$rb[2,2]){
      p1_skill_equity = p1_skill_equity - (1-flop_win_prob[2])*b4$p1 - b5$rb[1,2]
      p2_skill_equity = p2_skill_equity + (1-flop_win_prob[2])*b4$p1 + b5$rb[1,2]
      es = 4
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
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
      p1_skill_equity = p1_skill_equity + (1-turn_win_prob[1])*b5$p1 + b6$rb[2,3]
      p2_skill_equity = p2_skill_equity - (1-turn_win_prob[1])*b5$p1 - b6$rb[2,3]
      es = 5
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
      break
    } 
    #player 1 folding
    else if(b6$rb[1,3] < b6$rb[2,3]){
      p1_skill_equity = p1_skill_equity - (1-turn_win_prob[2])*b5$p1 - b6$rb[1,3]
      p2_skill_equity = p2_skill_equity + (1-turn_win_prob[2])*b5$p1 + b6$rb[1,3]
      es = 6
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
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
    # player 2 folding
    if(b7$rb[1,4] > b7$rb[2,4]){
      p1_skill_equity = p1_skill_equity + (1-river_win_prob[1])*b6$p1 + b7$rb[2,4]
      p2_skill_equity = p2_skill_equity - (1-river_win_prob[1])*b6$p1 - b7$rb[2,4]
      es = 7
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
      break
    }
    # player 1 folding
    else if(b7$rb[1,4] < b7$rb[2,4]){
      p1_skill_equity = p1_skill_equity - (1-river_win_prob[2])*b6$p1 - b7$rb[1,4]
      p2_skill_equity = p2_skill_equity + (1-river_win_prob[2])*b6$p1 + b7$rb[1,4]
      es = 8
      return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
                p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity,es))
      break
    }
  }
  p1_skill_equity = p1_skill_equity + (river_win_prob[1])*(b7$p1-b6$p1) - b7$rb[1,4]
  p2_skill_equity = p2_skill_equity + (river_win_prob[2])*(b7$p1-b6$p1) - b7$rb[2,4]
  es = 9
  return(c( p1_luck_equity, p2_luck_equity, p1_skill_equity, p2_skill_equity, 
		p1_luck_equity + p1_skill_equity, p2_luck_equity + p2_skill_equity, es))
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
        } 
      else if(p1_value == p2_value){
        tie = tie + 1
      }
    }
    winprob[3] <- tie/iters
    winprob[1] <- temp/iters+winprob[3]/2
    winprob[2] <- 1-winprob[1]
    return(winprob)
    break
  }

  # exact calculation
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
        } 
      else if(p1_value == p2_value){
        tie = tie + 1
        }
    }
    winprob[3] <- tie/flop_comb
    winprob[1] <- temp/flop_comb+winprob[3]/2
    winprob[2] <- 1-winprob[1]
    return(winprob)
    break
  }

  # exact calculation
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
        } 
      else if(p1_value == p2_value){
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
  p1_luck = p2_luck = p1_skill = p2_skill = p1_chip = p2_chip = endstate = cond = numeric(num_hand)
  cond[1:(num_hand/2)] = "TRUE"
  cond[(num_hand/2 + 1):num_hand] = "FALSE"
  for(i in 1:num_hand){
    if(cond[i]){
      #cat("player seats are c(1,2) \n")
      temp = equity(numattable, c(1,2), chips, blinds, dealer, chipstart, decision,iters)
      p1_luck[i] = temp[1]
      p2_luck[i] = temp[2]
      p1_skill[i] = temp[3]
      p2_skill[i] = temp[4]
      p1_chip[i] = temp[5]
      p2_chip[i] = temp[6]
      endstate[i] = temp[7]
      cat(c(p1_luck[i],p2_luck[i],p1_skill[i],p2_skill[i],p1_chip[i],p2_chip[i],endstate[i]),"\n")
    } 
    else{
      #cat("player seats are c(2,1) \n")
      temp = equity(numattable, c(2,1), chips, blinds, dealer, chipstart, decision,iters)
      p1_luck[i] = temp[1]
      p2_luck[i] = temp[2]
      p1_skill[i] = temp[3]
      p2_skill[i] = temp[4]
      p1_chip[i] = temp[5]
      p2_chip[i] = temp[6]
      endstate[i] = temp[7]
      cat(c(p1_luck[i],p2_luck[i],p1_skill[i],p2_skill[i],p1_chip[i],p2_chip[i],endstate[i]),"\n")
    }
  }
  result_matrix <<- cbind(p1_luck, p2_luck, p1_skill, p2_skill, p1_chip, p2_chip, endstate)
  #cat("final output", c(mean(p1_luck),mean(p2_luck),mean(p1_skill),mean(p2_skill),mean(p1_chip),mean(p2_chip)),"\n")
  output = c(mean(p1_luck),mean(p2_luck),mean(p1_skill),mean(p2_skill),mean(p1_chip),mean(p2_chip))
  return(output)
}

# Example
numattable1 = 2
chips1 = c(20000,20000)
blinds1 = c(50,100)
dealer1 = 1
chipstart1 = 20000
num_hand = 3000
mc_iters = 2000

#GoodLoose
decision_a = list(good_loose_player, good_loose_player)
decision_b = list(good_loose_player, bad_loose_player)
decision_c = list(good_loose_player, good_tight_player)
decision_d = list(good_loose_player, bad_tight_player)
decision_e = list(good_loose_player, tommy)
decision_f = list(good_loose_player, zelda)
#BadLoose
decision_g = list(bad_loose_player, bad_loose_player)
decision_h = list(bad_loose_player, good_tight_player)
decision_i = list(bad_loose_player, bad_tight_player)
decision_j = list(bad_loose_player, tommy)
decision_k = list(bad_loose_player, zelda)
#GoodTight
decision_l = list(good_tight_player, good_tight_player)
decision_m = list(good_tight_player, bad_tight_player)
decision_n = list(good_tight_player, tommy)
decision_o = list(good_tight_player, zelda)
#BadTight
decision_p = list(bad_tight_player, bad_tight_player)
decision_q = list(bad_tight_player, tommy)
decision_r = list(bad_tight_player, zelda)
#Tommy
decision_s = list(tommy, tommy)
decision_t = list(tommy, zelda)
#Zelda
decision_u = list(zelda, zelda)

#Average Equity
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_a,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_b,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_c,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_d,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_e,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_f,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_g,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_h,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_i,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_j,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_k,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_l,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_m,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_n,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_o,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_p,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_q,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_r,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_s,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_t,num_hand,mc_iters)
#avg_equity(numattable1,chips1,blinds1,dealer1,chipstart1,decision_u,num_hand,mc_iters)

#write.csv(result_matrix,file=".csv")
