#new players

## tight player:
martin = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
         roundbets, blinds1, chips1, ind1, dealer1, tablesleft, bluff = 0.1){

  
  #call if (chance of winning * (call amount + pot)) >= (chance of losing * call amount)
  #bet if 
  
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call
  if(round1 == 1){ ## pre-flop:
    
    ## only play "good" hands
    ##  (does not take in to account position)
    
    ## Case p1: AK, AA, KK: 
    ##    Call: if amount to call is >=  4*big blinds (4bb)
    ##    else raise: to 4bb if amount to call < 4bb
    ## Case p2: QQ, JJ, suited AJ, AQ, KQ, KJ, QJ: 
    ##    Call: if amount to call is >=  3bb
    ##    Raise: to 3bb if amount to call < 3bb
    ##    Fold: if amount to call is >=  4bb
    ## Case p3: AJ, AQ, KQ, KJ, QJ, 55-TT: 
    ##    Call: if amount to call is >=  2bb
    ##    Raise: to 4bb if amount to call < 2bb
    ##    Fold: if amount to call is >= than 3bb
    ## Case p4: else: 
    ##    call with bluff %; else fold

    #case p1:
    if((crds1[1,1] %in% c(13,14)) & (crds1[2,1] %in% c(13,14))){
      a1 = max(a2, 4*blinds1)
    }
    
    #case p2:
    if(((crds1[1,1] %in% c(11,12)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] == crds1[2,2]) & (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 4*blinds1){
        a1 = max(a2, 3*blinds1)
      } else {a1 = 0}
    }
    
    #case p3:
    if(((crds1[1,1] %in% c(5,10)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] != crds1[2,2]) & (crds1[1,1] != crds1[2,1]) &
        (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 3*blinds1){
        a1 = max(a2, 2*blinds1)
      } else {a1 = 0}
    }
    
    #case p4:
    else{
      if (a2 < 3*blinds1){
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }
      } else{a1 = 0}
    }
  }
    #end of pre-flop
  
  if(round1 == 2){ ## post-flop:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< pot
    ## bet: half pot
    ## fold: if amount to call > pot
    
    ## Case f2: two pair
    ## call: if amount =< 3* pot
    ## bet: pot
    ## fold: if amount to call > 3* pot
    
    ## Case f3: three of a kind
    ## call: if amount =< 5 * pot
    ## bet: pot
    ## fold: if amount to call > 5 * pot
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 17% * (pot + amount to call)
    ##  fold: else
    
    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 35% * (pot + amount to call)
    ##  fold: else
    
    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (if amount less than pot) with bluff %; else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= pot1){
        a1 <- max(a2, 1/2*pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4b:
    if (straightdraw1(c(crds1[1:2,1], board1[1:3,1])) == 4){
      if (a2 <= 0.17*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,1], board1[1:3,1])) == 4){
      if (a2 <= 0.35*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:3,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:3,1])))) |
        (full1(c(crds1[1:2,1], board1[1:3,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= pot1){
        u <- runif(1,0,1)
        if (u < bluff){
          a1 <- a2
        }
      } else{a1 <- 0}
    }
  }
  #end of post flop
  
  if(round1 == 3){ ## turn:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< pot
    ## bet: none
    ## fold: if amount to call > pot
    
    ## Case f2: two pair
    ## call: if amount =< 3* pot
    ## bet: pot
    ## fold: if amount to call > 3* pot
    
    ## Case f3: three of a kind
    ## call: if amount =< 5 * pot
    ## bet: pot
    ## fold: if amount to call > 5 * pot
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 9% * (pot + amount to call)
    ##  fold: else
    
    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 9 outs):
    ##  call: if amount =< 20% * (pot + amount to call)
    ##  fold: else
    
    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (if amount less than pot) with bluff %; else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= pot1){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4b:
    if (straightdraw1(c(crds1[1:2,1], board1[1:4,1])) == 4){
      if (a2 <= 0.09*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,1], board1[1:4,1])) == 4){
      if (a2 <= 0.20*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:4,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:4,1])))) |
        (full1(c(crds1[1:2,1], board1[1:4,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= pot1){
        u <- runif(1,0,1)
        if (u < bluff){
          a1 <- a2
        }
      } else{a1 <- 0}
    }
  }
  #end of turn
  
  if(round1 == 4){ ## river:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< half pot
    ## bet: none
    ## fold: if amount to call > pot
    
    ## Case f2: two pair
    ## call: if amount =< 2* pot
    ## bet: pot
    ## fold: if amount to call > 2* pot
    
    ## Case f3: three of a kind
    ## call: if amount =< 3 * pot
    ## bet: pot
    ## fold: if amount to call > 3 * pot
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 4 * pot
    ##  bet: pot
    ##  fold: if amount to call > 4 * pot

    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot

    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (if amount less than pot) with bluff (%) or raise 1.5x with bluff (%); else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 0.5*pot1){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 2*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 4*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:5,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:5,1])))) |
        (full1(c(crds1[1:2,1], board1[1:5,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= pot1){
        u <- runif(1,0,1)
        if (u < bluff){
          if (a2==0){a1 <- 0.5 * pot1}
          else{a1 <- a2 * 1.5}
        }
      } else{a1 <- 0}
    }
  }
  #end of river
  round(a1)
} ## end of martin

# loose player
marlon = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
                  roundbets, blinds1, chips1, ind1, dealer1, tablesleft, bluff = 0.3){
  
  
  #call if (chance of winning * (call amount + pot)) >= (chance of losing * call amount)
  #bet if 
  
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call
  if(round1 == 1){ ## pre-flop:
    
    ## only play "good" hands
    ##  (does not take in to account position)
    
    ## Case p1: AK, AA, KK: 
    ##    Call: if amount to call is >=  4*big blinds (4bb)
    ##    else raise: to 4bb if amount to call < 4bb
    ## Case p2: QQ, JJ, suited AJ, AQ, KQ, KJ, QJ: 
    ##    Call: if amount to call is >=  3bb
    ##    Raise: to 3bb if amount to call < 3bb
    ##    Fold: if amount to call is >=  5bb (with % 1-bluff)
    ## Case p3: AJ, AQ, KQ, KJ, QJ, 55-TT: 
    ##    Call: if amount to call is >=  2bb
    ##    Raise: to 4bb if amount to call < 2bb
    ##    Fold: if amount to call is >= than 5bb (with % 1-bluff)
    ## Case p4: else: 
    ##    call if amount <= pot; else call with bluff %; else fold
    
    #case p1:
    if((crds1[1,1] %in% c(13,14)) & (crds1[2,1] %in% c(13,14))){
      a1 = max(a2, 4*blinds1)
    }
    
    #case p2:
    if(((crds1[1,1] %in% c(11,12)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] == crds1[2,2]) & (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 5*blinds1){
        a1 = max(a2, 3*blinds1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    #case p3:
    if(((crds1[1,1] %in% c(5,10)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] != crds1[2,2]) & (crds1[1,1] != crds1[2,1]) &
        (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 5*blinds1){
        a1 = max(a2, 3*blinds1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    #case p3:
    else{
      if (a2 == blinds1) {a1 = a2}
      if (a2 <= 3 * blinds1){
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }
      } else{a1 = 0}
    }
    }
  
  #end of pre-flop
  
  if(round1 == 2){ ## post-flop:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< pot
    ## bet:  half pot
    ## fold: if amount to call > pot (with % 1-bluff)
    
    ## Case f2: two pair
    ## call: if amount =< 3* pot 
    ## bet: pot
    ## fold: if amount to call > 3* pot (with % 1-bluff)
    
    ## Case f3: three of a kind
    ## call: if amount =< 5 * pot 
    ## bet: pot
    ## fold: if amount to call > 5 * pot (with % 1-bluff)
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 17% * (pot + amount to call)
    ##  fold: else (with % 1-bluff)
    
    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 35% * (pot + amount to call)
    ##  fold: else (with % 1-bluff)
    
    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (if amount less than 2 * pot) with bluff %; else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= pot1){
        a1 <- max(a2, 1/2*pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f4b:
    if (straightdraw1(c(crds1[1:2,1], board1[1:3,1])) == 4){
      if (a2 <= 0.17*(pot1 + a2)){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:3,1])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,1], board1[1:3,1])) == 4){
      if (a2 <= 0.35*(pot1 + a2)){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:3,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:3,1])))) |
        (full1(c(crds1[1:2,1], board1[1:3,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= 2*pot1){
        u <- runif(1,0,1)
        if (u < bluff){
          a1 <- a2
        }
      } else{a1 <- 0}
    }
  }
  #end of post flop
  
  if(round1 == 3){ ## turn:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< pot
    ## bet: none
    ## fold: if amount to call > pot  (with % 1-bluff)
    
    ## Case f2: two pair
    ## call: if amount =< 3* pot
    ## bet: pot
    ## fold: if amount to call > 3* pot (with % 1-bluff)
    
    ## Case f3: three of a kind
    ## call: if amount =< 5 * pot
    ## bet: pot
    ## fold: if amount to call > 5 * pot (with % 1-bluff)
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 4 outs):
    ##  call: if amount =< 9% * (pot + amount to call)
    ##  fold: else (with % 1-bluff)
    
    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot
    ## draw 1 more (assume 9 outs):
    ##  call: if amount =< 20% * (pot + amount to call)
    ##  fold: else (with % 1-bluff)
    
    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (if amount less than pot) with bluff %; else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= pot1){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f4b:
    if (straightdraw1(c(crds1[1:2,1], board1[1:4,1])) == 4){
      if (a2 <= 0.09*(pot1 + a2)){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:4,1])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,1], board1[1:4,1])) == 4){
      if (a2 <= 0.20*(pot1 + a2)){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:4,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:4,1])))) |
        (full1(c(crds1[1:2,1], board1[1:4,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= pot1){
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) { #call
          if (a2 == 0) {a1 = pot1}
          else {a1 = a2}
        }
        if (u <= 0.5 * bluff){ #raise
          if (a2 == 0) {a1 = pot1}
          else {a1 <- a2 * 2}
        }
      }
      else {a1 = 0}
    }
    
  }
  #end of turn
  
  if(round1 == 4){ ## river:
    
    ## Case f1: pair (does not cair if its high pair)
    ## call: if amount =< half pot
    ## bet: none
    ## fold: if amount to call > pot (with % 1-bluff, raise 2x with % (0.5)*(1-bluff))
    
    ## Case f2: two pair
    ## call: if amount =< 2* pot
    ## bet: pot
    ## fold: if amount to call > 2* pot (with % 1-bluff, raise 2x with % (0.5)*(1-bluff))
    
    ## Case f3: three of a kind
    ## call: if amount =< 3 * pot
    ## bet: pot
    ## fold: if amount to call > 3 * pot (with % 1-bluff, raise 2x with % (0.5)*(1-bluff))
    
    ## Case f4: straight made/draw
    ## made:
    ##  call: if amount =< 4 * pot
    ##  bet: pot
    ##  fold: if amount to call > 4 * pot (with % 1-bluff, raise 2x with % (0.5)*(1-bluff))
    
    ## Case f5: flush made/draw
    ## made:
    ##  call: if amount =< 5 * pot
    ##  bet: pot
    ##  fold: if amount to call > 5 * pot (with % 1-bluff, raise 2x with % (0.5)*(1-bluff))
    
    ## Case f6: full house or 4 of a kind
    ## made:
    ##  call: always
    ##  bet: pot 
    
    ## Case f7: straightflush
    ## made:
    ##  call: always
    ##  bet: pot
    
    ## Case f8: else
    ##  call: (with % 1-bluff, raise 2x with % (0.5)*(1-bluff)); else fold
    
    # case f1:
    if (onepair1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 0.5*pot1){
        a1 <- a2
      } else {
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) {
          a1 = a2
        }
        if (u <= 0.5 * bluff){
          a1 = a2 * 2
        }else {a1 = 0}
      }
    }
    
    # case f2:
    if (twopair1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 2*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) {
          a1 = a2
        }
        if (u <= 0.5 * bluff){
          a1 = a2 * 2
        }else {a1 = 0}
      }
    }
    
    # case f3:
    if (trip1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 3*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) {
          a1 = a2
        }
        if (u <= 0.5 * bluff){
          a1 = a2 * 2
        }else {a1 = 0}
      }
    }
    
    # case f4:
    if (straight1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 4*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) {
          a1 = a2
        }
        if (u <= 0.5 * bluff){
          a1 = a2 * 2
        }else {a1 = 0}
      }
    }
    
    
    # case f5:
    if (flush1(c(crds1[1:2,1], board1[1:5,1])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else {
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) {
          a1 = a2
        }
        if (u <= 0.5 * bluff){
          a1 = a2 * 2
        }else {a1 = 0}
      }
    }
    
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:5,1])) >0) & (straight1(c(crds1[1:2,1], board1[1:5,1])))) |
        (full1(c(crds1[1:2,1], board1[1:5,1])))){
      a1 <- max(a2, pot1)
    }
    
    # case f8
    else{
      if (a2 <= pot1){
        u <- runif(1,0,1)
        if ((u < bluff) & (u > 0.5*bluff)) { #call
          if (a2 == 0) {a1 = pot1}
          else {a1 = a2}
        }
        if (u <= 0.5 * bluff){ #raise
          if (a2 == 0) {a1 = pot1}
          else {a1 <- a2 * 2}
        }
      }
      else {a1 = 0}
    }
    
  }
  #end of river
  round(a1)
} ## end of martin



## The function is currently defined as
function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
         roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call
  if(round1 == 1){ ## pre-flop:
    ## AK: Make a big raise if nobody has yet. Otherwise call.
    ## AQ: call a small raise, or make one if nobody has yet.
    ## AJ, AT, KQ, KJ, QJ: call a tiny raise.
    ## A9, KT, K9, QT, JT, T9: call a tiny raise if in late position
    ## (within 2 of the dealer).
    ## Suited A2-AJ: call a small raise.
    ## 22-99: call a small raise.
    ## TT-KK: make a huge raise. If someone's raised huge already, then go all in.
    ## AA: make a small raise. If there's been a raise already,
    ## then double how much it is to you.
    a3 = 2*blinds1+1 ## how much a tiny raise would be
    a4 = 4*blinds1+1 ## how much a small raise would be
    a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
    a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
    a7 = dealer1 - ind1
    if(a7 < -.5) a7 = a7 + numattable1 ## your position: a7 = how many hands til you're dealer
    if((crds1[1,1] == 14) && (crds1[2,1] == 13)){
      a1 = max(a2,a5)
    }
    if((crds1[1,1] == 14) && (crds1[2,1] == 12)){
      if(a2 < a4){
        a1 = a4
      } else if(a2 > a5){
        a1 = 0
      } else a1 = a2
    }
    if(((crds1[1,1] == 14) && ((crds1[2,1] < 11.5) && (crds1[2,1] > 9.5))) ||
       ((crds1[1,1] == 13) && (crds1[2,1] > 10.5)) ||
       ((crds1[1,1] == 12) && (crds1[2,1] == 11))){
      if(a2 < a3) a1 = a2
    }
    if(((crds1[1,1] == 14) && (crds1[2,1] == 9)) ||
       ((crds1[1,1] == 13) && ((crds1[2,1] == 10) || (crds1[2,1] == 9))) ||
       ((crds1[1,1] == 12) && (crds1[2,1] == 10)) ||
       ((crds1[1,1] == 11) && (crds1[2,1] == 10)) ||
       ((crds1[1,1] == 10) && (crds1[2,2] == 9))){
      if((a2 < a3) && (a7<2.5)) a1 = a2
    }
    if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 14) && (crds1[2,1] < 11.5)){
      if(a2<a4) a1 = a2
      ## Note: this trumps the previous section, since it comes later in the code.
    }
    if((crds1[1,1] == crds1[2,1])){ ## pairs:
      if(crds1[1,1] < 9.5){
        if(a2 < a4) a1 = a2
      } else if(crds1[1,1] < 13.5){
        if(a2<a5) a1 = a5 else a1 = mychips1
      } else {
        if(a2 < blinds1 + .5) a1 = a4 else a1 = min(2*a2,mychips1)
      }
    }
  }
  if(round1 == 2){ ## post-flop:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## If you have top pair or an overpair or two pairs or a set,
    ## make a big bet (call any bigger bet).
    ## Otherwise, if nobody's made even a small bet yet,
    ## then with prob. 20% make a big bluff bet.
    ## If you're the last to decide and nobody's bet yet, then increase this prob. to 50%.
    ## If you have an inside straight draw or flush draw then make
    ## a small bet (call any bigger bet).
    ## If you have a straight or better, then just call.
    ## Otherwise fold.
    a5 = min(sum(roundbets[,1]),mychips1) ## how much
    ## a big bet would be (prev round's pot size)
    a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be
    x = handeval(c(crds1[1:2,1], board1[1:3,1]),
                 c(crds1[1:2,2], board1[1:3,2])) ## what you have
    x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
    y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
    z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
    topcard1 = max(board1[1:3,1])
    a7 = runif(1) ## random number uniformly distributed between 0 and 1
    a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet
    ## The next 5 lines may seem weird, but the purpose is explained in the next comment:
    a9 = a8 - dealer1
    for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
    a10 = ind1 - dealer1
    if(a10 < .5) a10 = a10 + numattable1
    a11 = 2*(a10 == max(a9)) ## So a11 = 2 if you're last to decide; otherwise a11 = 0.
    if((x1 > 1000000) && (x < 3000000)){
      if(a2 < a6) a1 = a2
    } else if((x1 > 3000000) && (x < 6000000)){
      if(a2 < a6) a1 = a2
    } else if(x > 1000000 + 15^3*topcard1){
      a1 = max(a5,a2)
    } else if((a2 < a6) && ((a7 < .20) || ((a7 < .50) && (a11>1)))){
      a1 = a6
    }
    if((y == 4) || (z == 4)) a1 = max(a6, a2)
    if(x > 4000000) a1 = a2
  }
  if(round1 == 3){ ## after turn:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## Otherwise, if you have top pair or better, go all in.
    ## If you had top pair or overpair but now don't, then check/call a medium bet
    ## but fold to more.
    ## If you have an inside straight draw or flush draw then check/call a medium bet as well.
    ## Otherwise check/fold.
    a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
    a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
    x = handeval(c(crds1[1:2,1], board1[1:4,1]),
                 c(crds1[1:2,2], board1[1:4,2])) ## what you have
    x1 = handeval(c(board1[1:4,1]),c(board1[1:4,2])) ## what's on the board
    y = straightdraw1(c(crds1[1:2,1], board1[1:4,1]))
    z = flushdraw1(c(crds1[1:2,2], board1[1:4,2]))
    topcard1 = max(board1[1:4,1])
    oldtopcard1 = max(board1[1:3,1])
    if((x1 > 1000000) && (x < 3000000)){
      if(a2 < a6) a1 = a2
    } else if((x1 > 3000000) && (x < 6000000)){
      if(a2 < a6) a1 = a2
    } else if(x > 1000000 + 15^3*topcard1){
      a1 = mychips1
    } else if(x > 1000000 + 15^3*oldtopcard1){
      if(a2 < a5) a1 = a2
    } else if((y == 4) || (z == 4)){
      if(a2 < a5) a1 = a2
    }
  }
  if(round1 == 4){ ## after river:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## Otherwise, if you have two pairs or better, go all in.
    ## If you have one pair, then check/call a small bet.
    ## With nothing, go all-in with probability 10%; otherwise check/fold.
    a6 = .45+runif(1)/10 ## random number between .45 and .55
    a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet:
    ## around 1/2 of pot size; VARIES RANDOMLY
    x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2]))
    x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
    if((x1 > 1000000) && (x < 3000000)){
      if(a2 < a5) a1 = a2
    } else if((x1 > 3000000) && (x < 6000000)){
      if(a2 < a5) a1 = a2
    } else if(x > 2000000){
      a1 = mychips1
    } else if(x > 1000000){
      if(a2 < a5) a1 = a2
    } else if(runif(1)<.10){
      a1 = mychips1
    }
  }
  round(a1)
} ## end of zelda





