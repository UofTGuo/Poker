library("holdem")

#Play around the "holdem" package

#hand1
hand1 = function(numattable1, playerseats1, chips1, blinds1, dealer1,
         ntable1,myfast1,t1,t2,chipstart1,lowercut1, decision1){
  ## numattable1 = number of players at the table
  ## playerseats1 = list of indices, who's in seat 1, seat 2, etc.
  ## chips1 = list of chips left, FOR PLAYERS AT THIS TABLE ONLY!
  ## blinds = vector of small and then big blind
  ## dealer1 = seat that the dealer is in.
  ## ntable1 = how many tables remain.
  ## myfast1 = 2 if you want the tournament to run quickly, or 0 if you want to show graphics and have
  ## to click the mouse to proceed while showing each key hand.
  ## t1 = fraction of times to show double ups
  ## t2 = fraction of times to show eliminations
  ## chipstart1 = how many chips each player starts with
  ## lowercut1 = if a player has fewer chips than this, he/she name won't appear on the graphic display
  ## decision1 = the players' codes to determine their betting and folding
  
  chips2 = chips1 ## this will be the revised chips counts, at the end
  if(numattable1 < 1.5) return(chips2)

  # Usage of built-in functions bid1 and bid2:
  # bid1(numattable1, playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1)
  # bid2(numattable1, playerseats1, blinds1, dealer1, b3, b4, round1, ntable1, decision1)
  
  # numattable1 = number of players currently remaining at the given table
  # playerseats1 = list of indices, who's in seat 1, seat 2, etc
  # chips1 = list of chips left, for players at this table only
  # blinds1 = vector of(small blind amount, big blind amount)
  # dealer1 = seat that the dealer is in
  # b3 = cards the players have
  # ntable = how many tables remain in the tournament
  # decision1 = vector of the functions governing the players' betting
  
  #Return 
  # i1 = vector indicating who is still in the hand(1) or is out(0) 
  # p1 = the size of the pot
  # c1 = the number of chips everyone has left
  # rb = the betting for the whole hand
  # all1 = 0 if there is more betting in the hand, or 2 if the betting in the hand is all over
  # bl1 = the betting for the current round
  # il1 = player number indices of who bet
  # out1 = list of who is out, i.e. who folded this round or had folded previously
  
  b3 = deal1(numattable1)
  b4 = bid1(numattable1,playerseats1, chips1, blinds1, dealer1, b3, ntable1, decision1) # cat("\n...",b4$bl1,"\n....",b4$il1,"\n")
  b5 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b4,2, ntable1, decision1) 
  b6 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b5,3, ntable1, decision1) 
  b7 = bid2(numattable1,playerseats1, blinds1, dealer1, b3,b6,4, ntable1, decision1) 
  chips2 = calcwin1(numattable1,playerseats1, b3, b7)
  draw1 = 0
  u21 = runif(1)
  if(((max(chips2/(chips1+.01)) > 1.99) &&
      (u21 < t1)) || ((max(chips1/(chips2+.01)) > 99) && (u21 < t2))) {
    if(myfast1 < 1) {
      text(1,lowercut1,"click to continue",cex=.7)
      locator(1) }
    mygraphics1(numattable1,playerseats1,chips1,blinds1,dealer1,
                b3,b4,b5,b6,b7,chips2,ntable1,myfast1,chipstart1,
                name1,lowercut1)
    draw1 = 2
  }
  list(chips2=chips2,draw1=draw1)
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
lowercut1 = 10
decision1 = list(gravity, tommy)

hand1(numattable1, playerseats1, chips1, blinds1, dealer1,ntable1,myfast1,t1,t2,chipstart1,
         lowercut1, decision1)

#tourn1
tour1 = function(name1, decision1, chipstart1, bigstart1, inc1, winners1,myfast1, t1, t2,
                 graphiccutoff1, lowercut1){
  ## Note: don't have more than 10 winners! That might mess this up.
  # blinds last 10 hands, then increase by factor of inc1. (rounded)
  # If # players left is 11-20, then each hand, players are split
  # into 2 tables of equal size (or one off if # players is odd). Then 10 hands are played, # no matter what.
  # Small = 1/2 big (rounded to nearest integer).
  # If num players > 20, then each 10 hands, players are randomly split
  # into tables of size 10. Remaining players sit out 10 hands.
  # a) Start loop. Initiate blinds.
  # b) Assign seats. Set num of tables.
  # c) For each table, play a hand. Repeat 10 times.
  # d) After each hand, update nplayers1, and
  # if nplayers1 <= winners1, then store winners. See if all done too.
  # e) On key hands, do instant replay! (if all-in & call, and
  #    total pool > 8 times big.
  # f) Increase blinds and repeat b-e.
  
  ## name1 = names of players
  ## decision1 = the players' codes to determine their betting and folding
  ## chipstart = how many chips each player starts with
  ## bigstart1 = the initial big blind
  ## inc1 = how much the blinds increase every 10 hands. If inc1=2, for instance, then blinds double
  ## after every 10 hands
  ## winners1 = number of top finishers receiving payouts
  ## myfast1 = 2 if you want it to go fast. 0 = slow
  ## t1 = fraction of times to show double-ups
  ## t2 = fraction of times to show eliminations
  ## graphiccutoff1 = if a player's chip stack changes by a factor less than this amount, then the
  ## graphic display will not bother replotting his/her name
  ## lowercut1 = if a player has fewer chips than this, his/her name won't appear on the graphic display
  
  nplayers1 = length(name1)
  plot(c(0,nplayers1+1),c(lowercut1,chipstart1*nplayers1),pch=name1[1:nplayers1],
       type="n",xlab="player number",ylab="chips",log="y")
  chip1 = rep(chipstart1, nplayers1)
  text(x=c(1:nplayers1),y=chip1,cex=2,labels=name1[1:nplayers1],srt=270,col=2)
  chip7 = chip1
  big1 = round(bigstart1)
  sm1 = round(big1/2)
  blinds1 = c(sm1,big1)
  nleft1 = nplayers1
  plleft = 1:nplayers1   ## plleft will be the indices of who's left.
  places1 = rep(0,winners1)
  stp3 = 0
  while(stp3 < 1){
    if(nleft1 > 20.5){
      ntable1 = floor(nleft1/10)
      pl1 = sample(nleft1)
      tables1 = list(tbnums = rep(10,ntable1))
      for(j in c(1:ntable1)){
        tables1[[1+j]] = plleft[pl1[(1:10)+(j-1)*10]]
      }
    }
    if((nleft1 < 20.5) && (nleft1 > 10.5)){
      ntable1 = 2
      pl1 = sample(nleft1)
      thalf1 = ceiling(nleft1/2)
      bhalf1 = nleft1 - thalf1
      tables1 = list(tbnums = c(thalf1, bhalf1))
      tables1[[2]] = plleft[pl1[1:thalf1]]
      tables1[[3]] = plleft[pl1[(thalf1+1):nleft1]]
    }
    if(nleft1 < 10.5){
      ntable1 = 1
      pl1 = sample(nleft1)
      tables1 = list(tbnums = nleft1)
      tables1[[2]] = plleft[pl1]
      ## so, with 10 players or fewer, I'm re-shuffling seats every 10 hands.
    }
    cat("\n Big blind is ",blinds1[2],"\n")
    for(i in 1:ntable1){
      k=0
      for(j in 1:10){
        chip3 = chip1[tables1[[1+i]]]
        k = k+1
        if(k > tables1[[1]][i]) k = 1
        cat(j)
        x32 = hand1(tables1[[1]][i], tables1[[1+i]], chip3,
                    blinds1, 1, ntable1,myfast1,t1,t2,chipstart1,lowercut1, decision1)
        chip2 = x32$chips2
        chip1[tables1[[1+i]]] = chip2
        chipdif8 = (abs(chip1-chip7)/pmax(chip1,chip7,rep(1,nplayers1)) > graphiccutoff1)
        if(x32$draw1 > 1){
          text(x=c(1:nplayers1),y=chip1,cex=2,labels=name1[1:nplayers1],srt=270,col=2)
          chip7 = chip1
        } else if(sum(chipdif8)>.5){
          text(x=c(1:nplayers1)[chipdif8],
               y=chip7[chipdif8],cex=2,col="white",labels=name1[chipdif8],srt=270)
          text(x=c(1:nplayers1)[chipdif8],
               y=chip7[chipdif8],cex=2,col="white",labels=name1[chipdif8],srt=270)
          text(x=c(1:nplayers1)[chipdif8],
               y=chip1[chipdif8],cex=2,col=2,labels=name1[chipdif8],srt=270)
          chip7[chipdif8] = chip1[chipdif8]
        }
        ## Now remove eliminated players, even if they were blinds.
        ## This may let some people miss their big blind. Note this.
        j1 = sum(chip2 < .5) ## the number eliminated.
        if(j1 > .5){
          j2 = tables1[[i+1]][c(1:tables1[[1]][i])[chip2 < .5]] ## their indices
          j3 = j2[order(chip3[j2],decreasing=T)] ## ordered by how much they had before
          j4 = min(winners1,nleft1)
          nleft1 = nleft1 - j1
          if(nleft1 < winners1 - .5) places1[(nleft1+1):j4] = j3[1:(j4-nleft1)]
          cat("\n Eliminated: ",j2,".....",nleft1," players remaining.\n")
          tables1[[1]][i] = tables1[[1]][i] - j1
          tables1[[1+i]] = tables1[[1+i]][chip2>.5]
        }
        if(nleft1 < 1.5) break
      }
      if(nleft1 < 1.5) break
    }
    big1 = round(blinds1[2]*inc1)
    sm1 = round(big1/2)
    blinds1 = c(sm1,big1)
    plleft = c(1:nplayers1)[chip1>0.5]
    nleft1 = length(plleft)
    if(nleft1 < 1.5){
      stp3 = 2
      places1[1] = plleft
      z2 = winners1+1
      plot(c(0,nplayers1+1),c(lowercut1,chipstart1*nplayers1),
           type="n",xlab="player number",ylab="chips",log="y")
      text(1*nplayers1/z2,nplayers1*chipstart1, "1st:",col=4,cex=2)
      text(2*nplayers1/z2,nplayers1*chipstart1, "2nd:",col=4,cex=2)
      text(3*nplayers1/z2,nplayers1*chipstart1, "3rd:",col=4,cex=2)
      if(z2 > 4.5) for(z1 in c(4:winners1))
        text(z1*nplayers1/z2,nplayers1*chipstart1, paste(z1,"th:"),col=4,cex=2)
      for(z1 in c(1:winners1)) text(z1*nplayers1/(winners1+1),
                                    (nplayers1/2)*chipstart1,name1[places1[z1]],col=4,cex=2)
    }
  }
  places1
} 

# Example
name1 = c("gravity","tommy")
decision1 = list(gravity, tommy)
chipstart1 = 100
bigstart1 = 20
inc1 = 1.50
winners1 = 1
myfast1 = 2
t1 = 0.5
t2 = 1
graphiccutoff1 = 0.1
lowercut1 = 10

tourn1(name1, decision1, chipstart1, bigstart1, inc1, winners1, myfast1, t1, t2, graphiccutoff1, lowercut1)


#strflsh1
#boardcards = c(4,5,6,8,13)
#boardsuits = c(2,3,2,2,2)
boardcards = c()
boardsuits = c()
player1cards = c(2,3)
player1suits = c(2,1)
player2cards = c(7,3)
player2suits = c(2,4)
strflsh1(c(boardcards,player1cards),c(boardsuits,player1suits))
strflsh1(c(boardcards,player2cards),c(boardsuits,player2suits))

a = handeval(c(boardcards,player1cards),c(boardsuits,player1suits)) ## pl.1's value
b = handeval(c(boardcards,player2cards),c(boardsuits,player2suits)) ## pl.2's value



