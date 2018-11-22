#####=====SETUP=====
##
#==Load Packages==
#library(holdem)
##

##
#==Load and Correct Data==
allhands <- read.csv("all_hands3.csv",stringsAsFactors=F)
#==optional==
#attach(allhands)
##

##
#==SUBSET FOR TESTING==
test <- allhands[1:20,]
##
#####

#####=====FUNCTIONS======
##==Split Betting Actions==
str.sep <- function(string){
	sep <- gsub("(?<=[a-z])(?=[0-9])|(?<=[0-9])(?=[a-z])"," ",string,perl=TRUE)
	spl <- strsplit(sep," ")
	return(spl)
}
#Definitions:
#cr: SB call, BB raise
#k: SB call, BB check
#r: SB raise
#f: SB fold
##

##
#==Card Converter==
card.convert <- function(cards){ #string with even number of characters; one cell of the DF
	#vectors of cards and suits as numbers and letters
	Clett <- c("T","J","Q","K","A")
	Cnumb <- c(10,11,12,13,14)
	Slett <- c("d","c","h","s")
	Snumb <- c(0,1,2,3)

	chars <- unlist(strsplit(cards,split=""))
	
	for(i in 1:length(chars)){
		if(i %% 2 == 1){
			if(chars[i] %in% Clett){
				for(j in 1:5){
					if(chars[i] %in% Clett[j]){
						chars[i] <- Cnumb[j]
					}
				}
			}
		} else {
			if(chars[i] %in% Slett){
				for(k in 1:5){
					if(chars[i] %in% Slett[k]){
						chars[i] <- Snumb[k]
					}
				}
			}
		}
	}
	chars <- as.numeric(chars)
	for(r in seq(1,length(chars),by=2)){
		chars[r] <- chars[r]-1+13*chars[r+1]
	}
	dealt_index <- chars[seq(1,length(chars),by=2)]
	#dealt_index <- switch2(dealt_index11)
	return(dealt_index)
}
##

##
#===Win Probability===
win_prob = function(numattable1=2,dealt_index, round, iters=1000){
  all_index <- sample(52)
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
##

##==Equity Calculator==
equity <- function(bet.log,dealt_index,numattable1=2, blinds1=c(50,100),iters=1000){
  ### Pre-flop equity

  p1_luck_equity = 0
  p2_luck_equity = 0
  p1_skill_equity = 0
  p2_skill_equity = 0
  
  b4 = b4.setup(bet.log,blinds1)
  pre_flop_win_prob = win_prob(numattable1,dealt_index,"pre_flop", iters)
  
  # case of small blind directly folding
  if(b4$rb[1,1] == blinds1[2] && b4$rb[2,1] == blinds1[1]){

  #New Proposed or Schoenberg's Way for luck (skill changes depending on this value)
    p1_luck_equity = p1_luck_equity + min((2*blinds1[2]*pre_flop_win_prob[1]-blinds1[2]),blinds1[1])
    p2_luck_equity = p2_luck_equity + (2*blinds1[2]*pre_flop_win_prob[2] - blinds1[2])
  #p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
  #p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])
    p1_skill_equity = p1_skill_equity + blinds1[1] - p1_luck_equity
    p2_skill_equity = p2_skill_equity - blinds1[1] - p2_luck_equity
    return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
    break

  } else {

  #New Proposed or Schoenberg's Way for luck (skill changes depending on this value)
    p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
    p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])
  #p1_luck_equity = p1_luck_equity + (2*blinds1[2]*pre_flop_win_prob[1] - blinds1[2])
  #p2_luck_equity = p2_luck_equity + max((2*blinds1[2]*pre_flop_win_prob[2]-blinds1[2]),-blinds1[1])

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

  return(c(p1_luck_equity,p2_luck_equity,p1_skill_equity,p2_skill_equity))
}
##
#####

#####=====SET UP 2=====
##==Betting Logs and Cards Dealt==
pf.log <- vector("list",nrow(test))
f.log <- vector("list",nrow(test))
t.log <- vector("list",nrow(test))
r.log <- vector("list",nrow(test))
cards <- vector("list",nrow(test))
for(i in 1:nrow(test)){
	pf.log[i] <- str.sep(test$PreFlop[i])
	f.log[i] <- str.sep(test$Flop[i])
	t.log[i] <- str.sep(test$Turn[i])
	r.log[i] <- str.sep(test$River[i])
	cards[i] <- list(card.convert(test$Cards[i]))
}
##

##==b4 (Pre-Flop) Set Up==
b4.setup <- function(vector,blinds1=c(50,100)){ #argument is list[[i]]
	b4 <- vector("list",3)
	names(b4) <- c("p1","rb","all1")
	b4$all1 <- 0
	b4$rb <- matrix(,2,4)
	b4$rb[1,1] <- blinds1[2]
	b4$rb[2,1] <- blinds1[1]
	b4$p1 <- numeric(1)
	raiser <- 2
	caller <- 1

	for(i in 1:length(vector)){
		if(vector[i] == "f"){
			b4$all1 <- 2
			#print("fold")
		} else if(vector[i] == "k"){
			b4$rb[2,1] <- b4$rb[1,1]
			#print("double check")		
		} else if(vector[i] == "cr"){
			b4$rb[2,1] <- b4$rb[1,1]
			b4$rb[1,1] <- as.numeric(vector[i+1])
			caller <- (caller && 2)+1
			#print("cr")
		} else if(vector[i] == "r"){
			b4$rb[raiser,1] <- as.numeric(vector[i+1])
			raiser <- (raiser %% 2)+1
			caller <- raiser
			#print("raise")
		} else if(vector[i] == "c"){
			b4$rb[caller,1] <- b4$rb[(caller %% 2)+1,1]
			#print("call")
		}
	}
	b4$p1 <- sum(b4$rb,na.rm=T)
	return(b4)
}
#####

#####=====All In One Function Runner=====
##
for(i in 1:nrow(test)){
	cat( equity(pf.log[[i]],cards[[i]]),"\n" )
}
##
#####

#####======Unneeded/Deprecated Functions=====
##==b3 Setup==
b3.setup <- function(numattable1,dealt_index){
	#dealt_index <- c(dealt_index,sample((1:52)[-dealt_index],2))
	player_index = dealt_index[1:(2*numattable1)]
	player_info = switch2(player_index)
	player1cards = player_info$num[1:2]
	player2cards = player_info$num[3:4]
	player1suits = player_info$st[1:2]
	player2suits = player_info$st[3:4]

	board_index = dealt_index[(2*numattable1+1):(2*numattable1+5)]
	board_index <- board_index[!is.na(board_index)]
	board_info = switch2(board_index)

	b3 = deal1(numattable1)
	b3$plnum1[1,] = player1cards
	b3$plnum1[2,] = player2cards
	b3$plsuit1[1,] = player1suits
	b3$plsuit1[2,] = player2suits
	b3$brdnum1 =  board_info$num
	b3$brdsuit1 = board_info$st

	return(b3)
}
##
#####