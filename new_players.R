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
    ##    call with bluff % if amount <= 3bb (always limp in at bb); else fold

    #case p1:
    if((crds1[1,1] %in% c(13,14)) & (crds1[2,1] %in% c(13,14))){
      a1 <- max(a2, 4*blinds1[2])
    }
    
    #case p2:
    if(((crds1[1,1] %in% c(11,12)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] == crds1[2,2]) & (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 4*blinds1[2]){
        a1 <- max(a2, 3*blinds1[2])
      } else {a1 = 0}
    }
    
    #case p3:
    if(((crds1[1,1] %in% c(5,10)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] != crds1[2,2]) & (crds1[1,1] != crds1[2,1]) &
        (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 3*blinds1[2]){
        a1 <- max(a2, 2*blinds1[2])
      } else {a1 = 0}
    }
    
    #case p4:
    else{
      if (a2 <= blinds1[2]){a1 <- a2} #limp in
      if (a2 <= 3*blinds1[2]){
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
    ## draw 1 more (assume 9 outs):
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
    if (flush1(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,2], board1[1:3,2])) == 4){
      if (a2 <= 0.35*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:3,1])))) |
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
    if (flush1(c(crds1[1:2,1], board1[1:4,1]),c(crds1[1:2,2], board1[1:4,2])) > 0){
      if (a2 <= 7*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    # case f5b:
    if (flushdraw1(c(crds1[1:2,2], board1[1:4,2])) == 4){
      if (a2 <= 0.20*(pot1 + a2)){
        a1 <- a2
      } else{
        a1 <- 0
      }
    }
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:4,1]),c(crds1[1:2,2], board1[1:4,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:4,1])))) |
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
    if (flush1(c(crds1[1:2,1], board1[1:5,1]),c(crds1[1:2,2], board1[1:5,2])) > 0){
      if (a2 <= 5*pot1){
        a1 <- max(a2, pot1)
      } else{
        a1 <- 0
      }
    }
    
    
    # case f6 and f7
    if (((flush1(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:5,1])))) |
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
  min(a1, mychips1)
} ## end of martin

# loose player
marlon = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
                  roundbets, blinds1, chips1, ind1, dealer1, tablesleft, bluff = 0.3){
  
  
  #call if (chance of winning * (call amount + pot)) >= (chance of losing * call amount)
  #bet if 
  
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call
  if(round1 == 1){ ## pre-flop:
    
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
    ##    call if amount <= blinds (limp in); else call if <= 3bb with bluff %; else fold
    
    #case p1:
    if((crds1[1,1] %in% c(13,14)) & (crds1[2,1] %in% c(13,14))){
      a1 = max(a2, 4*blinds1[2])
    }
    
    #case p2:
    if(((crds1[1,1] %in% c(11,12)) & (crds1[2,1] == crds1[1,1])) |
       
       ((crds1[1,1] %in% c(11,14)) & (crds1[2,1] %in% c(11,14)) & 
        (crds1[1,1] == crds1[2,2]) & (crds1[2,1] + crds1[1,1] <27))){
      if(a2 < 5*blinds1[2]){
        a1 = max(a2, 3*blinds1[2])
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
      if(a2 < 5*blinds1[2]){
        a1 = max(a2, 3*blinds1[2])
      } else {
        u <- runif(1,0,1)
        if (u < bluff){
          a1 = a2
        }else {a1 = 0}
      }
    }
    
    #case p3:
    else{
      if (a2 <= blinds1[2]) {a1 <- a2} #limp in
      if (a2 <= 3 * blinds1[2]){
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
    if (flush1(c(crds1[1:2,1], board1[1:3,1]),c(crds1[1:2,2], board1[1:3,2])) > 0){
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
    if (flushdraw1(c(crds1[1:2,2], board1[1:3,2])) == 4){
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
    if (((flush1(c(crds1[1:2,1], board1[1:3,1]),c(crds1[1:2,2], board1[1:3,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:3,1])))) |
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
    ##  call: (if amount less than pot) with 0.5* bluff %, reraise 2 times with 0.5*bluff%; else fold
    
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
    if (flush1(c(crds1[1:2,1], board1[1:4,1]),c(crds1[1:2,2], board1[1:4,2])) > 0){
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
    if (flushdraw1(c(crds1[1:2,2], board1[1:4,2])) == 4){
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
    if (((flush1(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:4,1])))) |
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
    if (flush1(c(crds1[1:2,1], board1[1:5,1]),c(crds1[1:2,2], board1[1:5,2])) > 0){
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
    if (((flush1(c(crds1[1:2,1], board1[1:5,1]),c(crds1[1:2,2], board1[1:5,2])) >0) & (straight1(c(crds1[1:2,1], board1[1:5,1])))) |
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
  min(a1, mychips1)
} ## end of marlin


### bad players ###

## bad tight player:
marty = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
                  roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
  
  
  #call if (chance of winning * (call amount + pot)) >= (chance of losing * call amount)
  #bet if 
  
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call
  if(round1 == 1){ ## pre-flop:
    
    ## only play "good" hands
    ##  (does not take in to account position)
    
    ## Case p1: AK, AA, KK: 
    ##    all in
    ## Case p2: else:
    ##    fold if amount to call > bb
    
    #case p1:
    if((crds1[1,1] %in% c(13,14)) & (crds1[2,1] %in% c(13,14))){
      a1 = mychips1
    }
    
    #case p2:
    else{
      if (a2 <= blinds1[2]){
        a1 <- a2
      }
      else{a1 <- 0}
    }
  }
  #end of pre-flop
  
  if(round1 == 2){ ## post-flop:
    
    ## Case f1: straight, flush, full house made:
    ##  all in
    
    ## Case f2: else
    ##  check or fold 
    
    # case f1
    if ((flush1(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) +
         straight1(c(crds1[1:2,1], board1[1:3,1])) +
        full1(c(crds1[1:2,1], board1[1:3,1])))>0){
      a1 <- mychips1
    }
    
    # case f2
    else{
      a1 <- 0
    }
  }
  #end of post flop
  
  if(round1 == 3){ ## turn:
    
    ## Case f1: straight, flush, full house made:
    ##  all in
    
    ## Case f2: else
    ##  check or fold 
    
    # case f1
    if ((flush1(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) +
         straight1(c(crds1[1:2,1], board1[1:4,1])) +
         full1(c(crds1[1:2,1], board1[1:4,1])))>0){
      a1 <- mychips1
    }
    
    # case f2
    else{
      a1 <- 0
    }
  }
  #end of turn
  
  if(round1 == 4){ ## river:
    
    ## Case f1: straight, flush, full house made:
    ##  all in
    
    ## Case f2: else
    ##  check or fold 
    
    # case f1
    if ((flush1(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) +
         straight1(c(crds1[1:2,1], board1[1:3,1])) +
         full1(c(crds1[1:2,1], board1[1:3,1])))>0){
      a1 <- mychips1
    }
    
    # case f2
    else{
      a1 <- 0
    }
  }
  #end of river
  min(a1, mychips1)
} ## end of marty



## bad lose player:
# loose player
marly = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1,
                  roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
  
  ## always call
  
  a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
  a2 = min(mychips1, currentbet) ## how much it costs to call

  min(a2, mychips1)
} ## end of marly


