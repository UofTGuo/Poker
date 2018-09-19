# AceInTheHole        ben andy
# bruinpokerpower     sam michael
# CallingStation        steven kevinvallee
# elephant            ross kwan
# FillLaak            myley chris
# gmoney              eric royen
# HoldThem            emilyfu scott
# ivey                shang
# jcardshark          nick joseph
# krabcake            victor jay
# missingtooth        kevinfujii emilykosko

AceInTheHole = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1, roundbets, 
    blinds1, chips1, ind1, dealer1, tablesleft){
#Pre-flop:
#if pair of less than 10, call unless someone has gone all in
#If pair of 10 or higher, raise bet by 2*blinds
#If two cards of same suit, call
#If AK, AQ, AJ, AT, raise bet by 3*blinds.

#Post-flop:
#If three of a kind, go all in
#If flush, go all in
#If straight, go all in
#If two pair, go all in with 50% probability otherwise call
#If pair higher than T, call
#If straight draw, raise bet by 2*blinds
#If flush draw, raise bet by 2*blinds

#After turn:
#same
#After river:
#same but no draw commands.
a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
a2 = min(mychips1, currentbet) ## how much it costs to call

if(round1==1) { #pre-flop
if (crds1[1, 1] == crds1[2, 1] && crds1[2, 1] < 10)
a1=currentbet
if(crds1[1,1] == crds1[2, 1] && crds1[2, 1] >= 10)
a1= currentbet + 2*blinds1
if(crds1[1, 2] == crds1[2, 2])
a1= currentbet
if(crds1[1, 1] == 14 && (crds1[2, 1]>9 && crds1[2,1] < 14))
a1=currentbet + 3*blinds1
}

if(round1 ==2) { #post-flop
x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
a3 = 2*blinds1
a4 = runif(1)
a5 = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
a6 = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))

x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
if (x>3000000) {
    a1 = mychips1
}
else if (x > 2000000 && x <3000000){
    if (a4 > .5){
    a1 = mychips1
}
    else
    a1 = a2
}
else if ((a5 = 4) || (a6 = 4))
    a1 = currentbet + a3
}

if(round1 == 3){ #after the turn
x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
a3 = 2*blinds1
a4 = runif(1)
a5 = straightdraw1(c(crds1[1:2,1], board1[1:4,1]))
a6 = flushdraw1(c(crds1[1:2,2], board1[1:4,2]))

x1 = handeval(c(board1[1:4,1]),c(board1[1:4,2])) ## what's on the board

if (x>3000000) {
    a1 = mychips1
}
else if (x > 2000000 && x <3000000){
    if (a4 > .5){
    a1 = mychips1
}
    else
    a1 = a2
}
else if ((a5 = 4) || (a6 = 4))
    a1 = currentbet + a3
}


if (round1 == 4){ #after the river
x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2])) ## what you have
x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
a3 = 2*blinds1
a4 = runif(1)
a5 = straightdraw1(c(crds1[1:2,1], board1[1:5,1]))
a6 = flushdraw1(c(crds1[1:2,2], board1[1:5,2]))

if (x>3000000) {
    a1 = mychips1
}
else if (x > 2000000 && x <3000000){
    if (a4 > .5){
    a1 = mychips1
}
    else
    a1 = a2
} #close the else-if statement

} #close the entire if statement
a1
} #close the function



bruinpokerpower = function(numattable1, crds1, board1,  round1, currentbet,  mychips1, pot1, 
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
 a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
 a2 = min(mychips1, currentbet) ## how much it costs to call    
    if(round1 == 1){ ## pre-flop:
## AK, AQ, AJ, AT, KQ: call a small raise, or make one if nobody has yet.
## Suited A5-A9: call a small raise.
## 99-TT: make a small raise.
## JJ-AA: make a big raise.
a3 = 3*blinds1+1 ## how much a small raise would be
a4 = max(7*blinds1,mychips1/3)+1 ## how much a big raise would be
a5 = dealer1 - ind1
if(a5 < -.5) a5 = a5 + numattable1 ## your position: a5 = how many hands til you're dealer
if(((crds1[1,1] == 14) && (crds1[2,1] >9.5)) || ((crds1[1,1] == 13) && (crds1[2,1] == 12))) { a1 = a3 }
if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 14) && (crds1[2,1] >4.5))
{ a1 = a3 }
if((crds1[1,1] == crds1[2,1])){ ## pairs: if((crds1[1,1] < 10.5) && (crds1[1,1] > 8.5))
{ a1 = a3 }}
if((crds1[1,1] == crds1[2,1])){ ## pairs: if((crds1[1,1] > 10.5))
{ a1 = a4 }}
}

if(round1 == 2){ ## post-flop: 
a6 = min(0.5*sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
a7 = min(.25*sum(roundbets[,1]),mychips1) ## how much a small bet would be
a8 = runif(1) ## random number uniformly distributed between 0 and 1	
a9 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
a10 = a9 - dealer1
for(i in 1:length(a10)) if(a10[i]<.5) a10[i] = a10[i] + numattable1
a11 = ind1 - dealer1
if(a11 < .5) a11 = a11 + numattable1
a12 = 2*(a11 == max(a10))
x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
## Otherwise, if nobody's made even a small bet yet, then with prob. 10% make a big bluff bet.
## If you're the last to decide and nobody's bet yet, then increase this prob. to 40%.
## If you have a pair or two pairs, then check/call up to small bet.
## If we have 3 of a kind or better, make a big bet
if((a2 < a7) && ((a8 < .20) || ((a8 < .50) && (a12>1)))){
a1 = a7}
if((x1 > 1000000) && (x < 3000000)){
 if(a2 < a7) a1 = a2
if((x1 > 3000000) && (x < 6000000)){
if(a2 < a6) a1 = a2
}
}
}


if(round1 == 3){ ## after turn: 
## If you have full house or better, go all in
## If you had top pair or overpair but now don't, then check/call a medium bet but fold to more.
## If you have an inside straight draw or flush draw then check/call a medium bet as well.
a7 = min(1/4*sum(roundbets[,1:2]),mychips1) ## small bet (1/4 of prev round's pot size)
a6 = min(.5*sum(roundbets[,1:2]),mychips1) ## medium bet (1/2 of prev round's pot size)
x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
x1 = handeval(c(board1[1:4,1]),c(board1[1:4,2])) ## what's on the board
y = straightdraw1(c(crds1[1:2,1], board1[1:4,1]))
z = flushdraw1(c(crds1[1:2,2], board1[1:4,2]))
topcard1 = max(board1[1:4,1])
oldtopcard1 = max(board1[1:3,1])
if(x > 1000000 + 15^3*oldtopcard1){
if(a2 < a6) a1 = a2} 
if((y == 4) || (z == 4)){
if(a2 < a6) a1 = a2}
if ( x > 5000000){(a1 = mychips1)}}

if(round1 == 4){ ## after river: 
a7 = .45+runif(1)/10  ## random number between .45 and .55
a6 = min(a7*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
## If you have one pair, then check/call a small bet.
## If you have 3 of a kind or better, then check or call

if(x > 1000000){
	  if(a2 < a6) a1 = a2
if (x > 3000000){
	if (a2>a6) a1 = a2
	if(a2 < a6) a1 = a2
    }
}
}
round(a1)
} ## end of bruinpokerpower





CallingStation = function(numattable1, crds1, board1,  round1, currentbet,  mychips1, pot1, 
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
    a1 = 0 ## how much I'm going to end up betting. Note that the default is zero.
    a2 = min(mychips1, currentbet) ## how much it costs to call
    
    if(round1 == 1){ ## pre-flop:
	## AK: Make a big raise if nobody has yet. Call any raise.
	## AQ, AJ: call a small raise, or make one if nobody has yet.
	## AT, KQ, KJ, QJ, KT, QT, JT: make big raise if in late position (within 3 of dealer).  Otherwise fold
	## A9, K9, K8, Q9, J9, T9, 98, 87: call a tiny raise if in late position (within 3 of the dealer).
	## Suited A2-AJ: call a small raise.
	## 22-99: call a small raise, if in late position (within 2 of dealer then go all in).
	## TT-KK: make a huge raise. If someone's raised huge already, then go all in.
	## AA: make a big raise. If there's been a raise already, then go all in.
	
	a3 = 2*blinds1+1 ## how much a tiny raise would be
	a4 = 4*blinds1+1 ## how much a small raise would be
	a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
	a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
	a7 = dealer1 - ind1
	if(a7 < -.5) a7 = a7 + numattable1 ## your position: a7 = how many hands til you're dealer
	
	if((crds1[1,1] == 14) && (crds1[2,1] == 13)){
	    a1 = max(a2,a5)
	}
    if(((crds1[1,1] == 14) && (crds1[2,1] > 10.5)) && (crds1[2,1] < 12.5)){
	    if(a2 < a4){
		a1 = a4
	    } else if(a2 > a5){
		a1 = 0
	    } else a1 = a2
	}
    	if(((crds1[1,1] == 14) && (crds1[2,1] == 10)) || 
	    ((crds1[1,1] == 13) && (crds1[2,1] == 12) && (crds1[2,1] == 11)) ||
	    ((crds1[1,1] == 12) && (crds1[2,1] == 11) && (crds1[2,1] == 10)) ||
	    ((crds1[1,1] == 11) && (crds1[2,1] == 10))){
	    if(a2 < a3) a1 = a2
	}

	if(((crds1[1,1] == 14) && (crds1[2,1] == 9))|| 
	    ((crds1[1,1] == 13) && ((crds1[2,1] == 9) || (crds1[2,1] == 8)))||
	    ((crds1[1,1] == 12) && (crds1[2,1] == 9)) ||
	    ((crds1[1,1] == 11) && (crds1[2,1] == 9)) ||
	    ((crds1[1,1] == 10) && (crds1[2,1] == 9))||
	    ((crds1[1,1] == 9) && (crds1[2,1] == 8)) ||
	    ((crds1[1,1] == 8) && (crds1[2,1] == 7))){
	    if((a2 < a3) && (a7<3.5)) a1 = max(a2,a5)
	}
		if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 14) && (crds1[2,1] < 11.5)){
	    if(a2<a4) a1 = a2
	    ## Note: this trumps the previous section, since it comes later in the code.
	}
	if((crds1[1,1] == crds1[2,1])){ ## pairs:
	    if(crds1[1,1] < 9.5){
		if(a2 < a4) a1 = a4 else if(a7<2.5) a1 = mychips1
	    } else if(crds1[1,1] > 13.5){
		if(a2<a5) a1 = a5 else a1 = mychips1
	    } else {
		if(a2 < blinds1 + .5) a1 = a4 else a1 = min(2*a2,mychips1)
	    }
	}
    }
    
	
    
    
    
    
    if(round1 == 2){ ## post-flop: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## If there's 3-of-a-kind on the board and you don't have a full house or more, bet small amount.
	## If you have top pair or an overpair or two pairs or a set, check then if there is a bet go all in. 
	## Otherwise, if nobody's made even a small bet yet, then with prob. 33% make a big bluff bet.
	## If you're the last to decide and nobody's bet yet, then increase this prob. to 67%.
	## If you have an inside straight draw or flush draw then check (call any small bet).
	## If you have an open-ended straight draw or flush draw then go all in
	## If you have a straight or better, then raise double.
	## Otherwise fold.
	
	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be	
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
	a7 = runif(1) ## random number uniformly distributed between 0 and 1
	a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
	## The next 5 lines may seem weird, but the purpose is explained in the next comment:
	a9 = a8 - dealer1
	for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
	a10 = ind1 - dealer1
	if(a10 < .5) a10 = a10 + numattable1
	a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.
	
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a5) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    if(a2 < a6) a1 = a6
	} else if(x > 1000000 + 15^3*topcard1){
	    if(a2 < 0.5) a1 = 0 
	    if(a2 > 0.5) a1 = mychips1
	} else if((a2 < a6) && ((a7 < .33) || ((a7 < .67) && (a11>1)))){
	    a1 = a6
	}
	if((y == 4) || (z == 4)) a1 = mychips1
	if(x > 4000000) a1 = 2*a2
    }
    
    if(round1 == 3){ ## after turn: 
	## If there's a pair on the board and you don't have a set, then make medium bet.
	## All in if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have top pair or better, medium bet.
	## If you had top pair or overpair but now don't, then check/call a small bet but fold to more.
	## If you have an inside straight draw or flush draw then check/call a medium bet as well.
	## Otherwise check/fold.
	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
	x1 = handeval(c(board1[1:4,1]),c(board1[1:4,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:4,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:4,2]))
	topcard1 = max(board1[1:4,1])
	oldtopcard1 = max(board1[1:3,1])
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a6) a1 = a5
	} else if((x1 > 3000000) && (x < 6000000)){
	    a1 = mychips1
	} else if(x > 1000000 + 15^3*topcard1){
	    a1 = a5
	} else if(x > 1000000 + 15^3*oldtopcard1){
	    if(a2 < a6) a1 = a2
	} else if((y == 4) || (z == 4)){
	    if(a2 < a5) a1 = a2
	}
    }
    
    if(round1 == 4){ ## after river: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Bet medium if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have two pairs or better, go all in.
	## If you have one pair, then check/call medium bet.
	## With nothing, go all-in with probability 7.5%; otherwise check/fold.
	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
	x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2]))
	x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a5) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    if(a2 < a5) a1 = a5
	} else if(x > 2000000){
	    a1 = mychips1
	} else if(x > 1000000){
	    if(a2 < mychips1) a1 = a2
	} else if(runif(1)<.075){
	    a1 = mychips1
	}
    }
    
    round(a1)
} ## end CallingStation






elephant = function(numattable1, crds1, board1,  round1, currentbet,  mychips1, pot1, 
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
    a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
    a2 = min(mychips1, currentbet) ## how much it costs to call
    
    if(round1 == 1){ ## pre-flop:
	## AK: Make a big raise if nobody has yet. Otherwise call.
	## AQ: call a small raise, or make one if nobody has yet.
	## AJ, AT, KQ, KJ, QJ: call a tiny raise.
	## A9, KT, K9, QT, JT, T9: call a tiny raise if in late position (within 2 of the dealer).
	## Suited A2-AJ: call a small raise.
	## 22-TT: call a small raise.
	## JJ-KK: make a huge raise. If someone's raised huge already, then go all in.
	## AA: make a small raise. If there's been a raise already, then double how much it is to you.
	
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
	    if(crds1[1,1] < 10.5){
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
	## If you have top pair or an overpair or two pairs or a set, make a big bet (call any bigger bet). 
	## Otherwise, if nobody's made even a small bet yet, then with prob. 05% make a big bluff bet.
	## If you're the last to decide and nobody's bet yet, then increase this prob. to 70%.
	## If you have a flush draw then make a small bet (call any bigger bet).
	## If you have a straight or better, then just call.
	## Otherwise fold.
	
	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be	
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
	a7 = runif(1) ## random number uniformly distributed between 0 and 1
	a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
	## The next 5 lines may seem weird, but the purpose is explained in the next comment:
	a9 = a8 - dealer1
	for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
	a10 = ind1 - dealer1
	if(a10 < .5) a10 = a10 + numattable1
	a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.
	
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a6) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    if(a2 < a6) a1 = a2
	} else if(x > 1000000 + 15^3*topcard1){
	    a1 = max(a5,a2)
	} else if((a2 < a6) && ((a7 < .05) || ((a7 < .70) && (a11>1)))){
	    a1 = a6
	}
	if((z == 4)) a1 = max(a6, a2)
	if(x > 4000000) a1 = a2
    }
    if(round1 == 3){ ## after turn: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have top pair or better, go all in.
	## If you had top pair or overpair but now don't, then check/call a medium bet but fold to more.
	## If you have a flush draw then check/call a medium bet as well.
	## Otherwise check/fold.
	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
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
	} else if((z == 4)){
	    if(a2 < a5) a1 = a2
	}
    }
    if(round1 == 4){ ## after river: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have two pairs or better, go all in.
	## If you have one pair, then check/call a small bet.
	## With nothing, go all-in with probability 10%; otherwise check/fold.
	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
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
} ## end of elephant




FillLaak = function(numattable1, crds1, board1,  round1, currentbet,   
mychips1, pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
a1 = 0
a2 = min(mychips1, currentbet)

if(round1 == 1){

## call small raise with 76,87,98,T9,JT suited
## AA-TT, AK, AQ: go all-in
## if less than 5 times the big blind, go all in if 1st card is 10 or higher and 2nd card is 9 or higher, or any pair
## AJ, AT, KQ, KJ, QJ, 99-22: call a small raise, or make one if no one has yet.


	a3 = 2*blinds1+1 ## how much a tiny raise would be
	a4 = 4*blinds1+1 ## how much a small raise would be
	a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
	a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
	a7 = dealer1 - ind1
	if(a7 < -.5) a7 = a7 + numattable1

	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 11) && (crds1[2,1] == 10)){
	    if(a2<a4) a1 = a2
 	}
	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 10) && (crds1[2,1] == 9)){
	    if(a2<a4) a1 = a2
 	}
	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 9) && (crds1[2,1] == 8)){
	    if(a2<a4) a1 = a2
 	}
	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 8) && (crds1[2,1] == 7)){
	    if(a2<a4) a1 = a2
 	}
	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 7) && (crds1[2,1] == 6)){
	    if(a2<a4) a1 = a2
 	}
	if((crds1[1,1] == crds1[2,1]) && (crds1[1,1] > 9.5)) a1 = mychips1
	if((crds1[1,1] == 14) && (crds1[2,1] > 11.5)) a1 = mychips1
	if((mychips1 < 5*blinds1) && (crds1[1,1] >= 10) && (crds1[2,1] >= 9)) a1 = mychips1
	if((mychips1 < 5*blinds1) && (crds1[1,1] == crds1[2,1])) a1 = mychips1
	if(((crds1[1,1] == 14) && ((crds1[2,1] < 11.5) && (crds1[2,1] > 9.5))) ||
	    ((crds1[1,1] == 13) && (crds1[2,1] > 10.5)) ||
	    ((crds1[1,1] == 12) && (crds1[2,1] == 11))){
	    if(a2 < a4){
		a1 = a4
	    } else if(a2 > a5){
		a1 = 0
	    } else a1 = a2
	}
	if((crds1[1,1] == crds1[2,1]) && (crds1[2,1] < 9.5))
	if(a2 < a4){
	a1 = a4
	    } else if(a2 > a5){
		a1 = 0
	    } else a1 = a2
	}

if(round1 == 2){
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## If you have top pair or an overpair or two pairs or a set, go all in
	## If you have second or third pair, check/call up to small bet.
	## Otherwise, if nobody's made even a small bet yet, then with prob. 10% make a big bluff bet.
	## If you're the last to decide and nobody's bet yet, then increase this prob. to 75%.
	## If you have an inside straight draw then make a small bet (call any bigger bet).
	## If you have a straight or better, then just call.
	## If you have flush draw, go all in
	## Otherwise fold.


	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
	a7 = runif(1) ## random number uniformly distributed between 0 and 1
	a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
	## The next 5 lines may seem weird, but the purpose is explained in the next comment:
	a9 = a8 - dealer1
	for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
	a10 = ind1 - dealer1
	if(a10 < .5) a10 = a10 + numattable1
	a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.


	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a6) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    if(a2 < a6) a1 = a2
	} else if(x > 1000000 + 15^3*topcard1){
	    a1 = mychips1
	} else if(x > 1000000){
	    if(a2 < a6) a1 = a2
	} else if((a2 < a6) && ((a7 < .10) || ((a7 < .75) && (a11>1)))){
	    a1 = a6
	}
	if(y == 4){
	    if(a2 < a6) a1 = a2
	}
	if(z == 4) a1 = mychips1
	if(x > 4000000) a1 = a2
}
if(round1 == 3){ ## after turn:
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have top pair or better, go all in.
	## If you had top pair or overpair but now don't, then check/call a medium bet but fold to more.
	## If you have an inside straight draw or flush draw then check/call a small bet.
	## If you have nothing, go all in with 25% chance

	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
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
	    if(a2 < a6) a1 = a2
	} else if(runif(1)<.25){
	    a1 = mychips1
	}
    }
    if(round1 == 4){ ## after river:
	## If you have two pairs or better, go all in.
	## If you have one pair, then check/call a small bet.
	## With nothing, go all-in with probability 15%; otherwise check/fold.

	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
	x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2]))
	x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
	if(x > 2000000){
	    a1 = mychips1
	} else if(x > 1000000){
	    if(a2 < a5) a1 = a2
	} else if(runif(1)<.15){
	    a1 = mychips1
	}
    }
    round(a1)
} ## End of FillLaak


gmoney = function(numattable1, crds1, board1,  round1, currentbet,
mychips1, pot1, 
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
    a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
    a2 = min(mychips1, currentbet) ## how much it costs to call
    
    if(round1 == 1){ ## pre-flop:
    ## AK: Make a big raise.
    ## AQ: call a medium raise, or make a small one if nobody has yet.
    ## AJ, AT, KQ, KJ, QJ: call a small raise, make a tiny raise.
    ## A9, KT, K9, QT, JT, T9: call a small raise.
    ## Suited A6-AJ: call a medium raise, make a small raise.
    ## 22-77: call a small raise.
    ## 88-KK: make a large raise. If someone's raised huge already, then go all in.
    ## AA: make a huge raise. If someone has raised huge already, then go all in.
    ## if 2 consecutive suited cards greater than 8, make a tiny raise, or call a small raise.

    a3 = 2*blinds1+1 ## how much a tiny raise would be
    a4 = 4*blinds1+1 ## how much a small raise would be
    a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
    a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
    a7 = dealer1 - ind1
    if(a7 < -.5) a7 = a7 + numattable1 ## your position: a7 = how many hands til you're dealer
    
    if((crds1[1,1] == 14) && (crds1[2,1] == 13)){
        a1 = a5
    }
    if((crds1[1,1] == 14) && (crds1[2,1] == 12)){
        if(a2 < a5){
        a1 = a5
        } else if(a2 > a5){
        a1 = 0
        } else a1 = a4
    }
    if(((crds1[1,1] == 14) && ((crds1[2,1] < 11.5) && (crds1[2,1] > 9.5)))
|| 
        ((crds1[1,1] == 13) && (crds1[2,1] > 10.5)) ||
        ((crds1[1,1] == 12) && (crds1[2,1] == 11))){
        if(a2 < a4) a1 = a2
        else if (a2 > a4) { a1 = 0}
        else a1 = a3
    }
    if(((crds1[1,1] == 14) && (crds1[2,1] == 9)) ||
        ((crds1[1,1] == 13) && ((crds1[2,1] == 10) || (crds1[2,1] == 9))) ||
        ((crds1[1,1] == 12) && (crds1[2,1] == 10)) ||
        ((crds1[1,1] == 11) && (crds1[2,1] == 10)) ||
        ((crds1[1,1] == 10) && (crds1[2,2] == 9))){
        if(a2 < a4) a1 = a2
    }
    if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 14) && (crds1[2,1] <
11.5) && (crds1[2,1] > 5.5)){
        if(a2<a5) a1 = a2
        else if (a2 > a5) a1 = 0
        else a2 = a4
    }
    if((crds1[1,1] == crds1[2,1])){ ## pairs:
        if(crds1[1,1] < 7.5){
        if(a2 < a4) a1 = a2
        } else if(crds1[1,1] < 13.5){
        if(a2<a5) a1 = a5 else a1 = mychips1
        } else {
        if(a2 < a6) a1 = a6 else a1 = mychips1
        }
    }
    if((crds1[1,2] > 7.5) && (crds1[1,1] - crds1[1,2] == 1) && (crds1[2,1]
== crds1[2,2]))
        { if (a2 < a4) a1 = a4
          else if (a2 > a4) a1 = 0
          else a2 = a3
        }
    }
    if(round1 == 2){ ## post-flop:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## If you have top pair or an overpair or two pairs or a set, go all in
    ## Otherwise, if nobody's made even a small bet yet, then with prob. 50% make a small bluff bet.
    ## If you're the last to decide and nobody's bet yet, then increase this prob. to 90%.
    ## If you have an inside straight draw or flush draw then make a small bet (call any bigger bet).
    ## If you have a straight or then make a small bet or call any bet.
    ## Otherwise fold.
    
    a4 = 4*blinds1+1 ## how much a small raise would be
    a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
    a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be    
    x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2],
board1[1:3,2])) ## what you have
    x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
    y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
    z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
    topcard1 = max(board1[1:3,1])
    a7 = runif(1) ## random number uniformly distributed between 0 and 1
    a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
    ## The next 5 lines may seem weird, but the purpose is explained in the next comment:
    a9 = a8 - dealer1
    for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
    a10 = ind1 - dealer1
    if(a10 < .5) a10 = a10 + numattable1
    a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.
    
    if((x1 > 1000000) && (x < 3000000)){
        if(a2 < a6) a1 = a2
    } else if((x1 > 3000000) && (x < 6000000)){
        if(a2 < a6) a1 = a2
    } else if(x > 1000000 + 15^3*topcard1){
        a1 = mychips1
    } else if((a2 < a4) && ((a7 < .50) || ((a7 < .90) && (a11>1)))){
        a1 = a4
    }
    if((y == 4) || (z == 4)) a1 = max(a6, a2)
    if(x > 4000000) {
    if(a2 <a6) { a1 = a6 }
    else { a1 = a2 }
    }
    }
    if(round1 == 3){ ## after turn:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## Otherwise, if you have top pair or better, make a large bet or call any other bet.
    ## If you had top pair or overpair but now don't, then check/call a large bet but fold to more.
    ## If you have an inside straight draw or flush draw then check/call a small bet as well.
    ## If you have an inside straight draw or flush draw and a pair, make a large bet 
    ## Otherwise check/fold.
    a7 = min(sum(roundbets[,1]),mychips1) ## large bet
    a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
    a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
    x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2],
board1[1:4,2])) ## what you have
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
        if (a2 < a7) a1 = a7
        else a1 = a2
    } else if(x > 1000000 + 15^3*oldtopcard1){
        if(a2 < a7) a1 = a2
    } else if((y == 4) || (z == 4)){
        if(a2 < a6) a1 = a2
    }
      else if((y == 4) || (z == 4) && x > 2000000){
        if(a2 < a7) a1 = a7
    }
    }
    if(round1 == 4){ ## after river:
    ## If there's a pair on the board and you don't have a set, then check/call up to small bet.
    ## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
    ## Otherwise, if you have two pairs or better, go all in.
    ## If you have one pair, then check/call a medium.
    ## With nothing, go all-in with probability 2%; otherwise check/fold.
    ## If you are dealer and nobody has made a small bet, go all in
    a6 = .45+runif(1)/10  ## random number between .45 and .55
    a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
    a7 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
    x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2],
board1[1:5,2]))
    x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
    if((x1 > 1000000) && (x < 3000000)){
        if(a2 < a5) a1 = a2
    } else if((x1 > 3000000) && (x < 6000000)){
        if(a2 < a5) a1 = a2
    } else if(x > 2000000){
        a1 = mychips1
    } else if(x > 1000000){
        if(a2 < a7) a1 = a2
    } else if(runif(1)<.02){
        a1 = mychips1
    }
    if (ind1 == dealer1 && a2 < a5){
        a1 = mychips1
     }
    }
    round(a1)
} ## end of gmoney



HoldThem = function(numattable1, crds1, board1,  round1, currentbet,  mychips1, pot1, 
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
    a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
    a2 = min(mychips1, currentbet) ## how much it costs to call
    
    if(round1 == 1){ ## pre-flop:
	## AK: Make a big raise, and call up to a big raise.
	## AQ: call a small raise, or make one if nobody has yet.
	## AJ, KQ, KJ, JT: call a small raise, or make one if nobody has yet.
	## AT, KT, QT, QJ: call a tiny raise.
	## Suited A2-AK: call a tiny raise, or make a small raise if nobody has yet.
	## 22-99: call a big raise.
	## TT-KK: make a small raise. If someone's raised big already, then go all in.
	## AA: make a small raise. If there's been a raise already, then go all in.
	
	a3 = 2*blinds1+1 ## how much a tiny raise would be
	a4 = 4*blinds1+1 ## how much a small raise would be
	a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
	a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
	a7 = dealer1 - ind1
	if(a7 < -.5) a7 = a7 + numattable1 ## your position: a7 = how many hands til you're dealer

	if((crds1[1,1] == 14) && (crds1[2,1] == 13)){
	    if(a2 < a5){
		a1 = a5
	    } else if(a2 > a5){
		a1 = 0
	  	}
	if((crds1[1,1] == 14) && (crds1[2,1] == 12)){
	    if(a2 < a4){
		a1 = a4
	    } else if(a2 > a5){
		a1 = 0
	    } else a1 = a2
	}
	if(((crds1[1,1] == 14) && ((crds1[2,1] < 11.5) && (crds1[2,1] > 10.5))) || 
	    ((crds1[1,1] == 13) && (crds1[2,1] > 10.5)) ||
	    ((crds1[1,1] == 10) && (crds1[2,1] == 11))){
	    if(a2 < a4) a1 = a4
	} else if(a2 > a4){
		a1 = 0
	    } else a1 = a4
	}
    
	if(((crds1[1,1] == 14) && (crds1[2,1] == 10)) || 
	    ((crds1[1,1] == 13) && (crds1[2,1] == 10)) ||
	    ((crds1[1,1] == 12) && (crds1[2,1] == 10)) ||
	    ((crds1[1,1] == 12) && (crds1[2,1] == 11))){
	    if(a2 < a3) a1 = a2
	}
	if((crds1[1,2] == crds1[2,2]) && (crds1[1,1] == 14)){
	    if(a2 < a4) a1 = a2
	    else if(a2 > a4){
		a1 = 0
	    } else a1 = a4	
	}
	
    
	    ## Note: this trumps the previous section, since it comes later in the code.
    
	if((crds1[1,1] == crds1[2,1])){ ## pairs:
	    if(crds1[1,1] < 9.5){
		if(a2 < a5) a1 = a2
	    } else if(crds1[1,1] < 13.5){
		if(a2<a4) a1 = a4 else a1 = mychips1
	    } else {
		if(a2 < blinds1 + .5) a1 = a4 else a1 = mychips1
	    }
	}
    }
    
    
if(round1 == 2){ ## post-flop: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## If you have top pair or an overpair or two pairs or a set, make a big bet (call any bigger bet). 
	## Otherwise, if nobody's made even a small bet yet, then with prob. 20% make a big bluff bet.
	## If you're the last to decide and nobody's bet yet, then increase this prob. to 50%.
	## If you have an inside straight draw or flush draw then make a small bet (call any bigger bet).
	## If you have a straight or better, then just call.
	## Otherwise fold.
	
	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be	
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
	a7 = runif(1) ## random number uniformly distributed between 0 and 1
	a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you
	## The next 5 lines may seem weird, but the purpose is explained in the next comment:
	a9 = a8 - dealer1
	for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
	a10 = ind1 - dealer1
	if(a10 < .5) a10 = a10 + numattable1
	a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.
	
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
	## If you had top pair or overpair but now don't, then check/call a medium bet but fold to more.
	## If you have an inside straight draw or flush draw then check/call a medium bet as well.
	## Otherwise check/fold.
	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
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
	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
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
} ## end of HoldThem



ivey = function(numattable1, crds1, board1,  round1, currentbet, mychips1, 
    pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
    ## if pair of at least 8 or suited connectors, then all in  
    ## if nobody's bet (or raised the blinds) yet, then bet (or raise the blinds) the minimum amount possible.
    a1 = 0
    bigb = dealer1 + 3
    if(bigb > numattable1) bigb = bigb - numattable1 + 1
    
if(round1 == 1){
        if((roundbets[ind1,1] < blinds1 - .5) && (currentbet < blinds1+.5)){
            a1 = min(2*blinds1, mychips1)
        } else if ((ind1 == bigb) && (currentbet < blinds1 - .5)) a1 = min(blinds1, mychips1)
    }
    if((round1 > 
1.5) && (currentbet < .5)) a1 = min(blinds1, mychips1)
    if((crds1[1,1] == crds1[2,1]) && (crds1[2,1] > 7.5)|| ((crds1[1,1]-crds1[2,1]==1) && (crds1[1,2] == crds1[2,2]))) a1 = mychips1
    a1
} ## end of ivey


jcardshark = function(numattable1, crds1, board1,  round1, currentbet, mychips1, 
    pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
	## any pocket pair
	## AK, AQ, AJ, A10
	## KQ, KJ, QJ
	## suited connectors above eight with 75%
a1=0
x=runif(1)
if((crds1[1,1]>13.5) && (crds1[2,1] > 9.5)) a1=mychips1
if((crds1[1,1]>12.5) && (crds1[2,1] > 10.5)) a1=mychips1
if( ( (crds1[1,1]-crds1[2,1])==1) && (crds1[1,2]==crds1[2,2]) && (crds1[1,1]>7.5) && (x>=0.75) ) a1=mychips1
a1
} #end of jcardshark: nick & joseph


krabcake = function(numattable1, crds1, board1, round1, currentbet, mychips1, pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
## if no one has bet yet, then bet minimum amount possible
## pre-flop:
## all in on TT-AA
## big raise on any combination of face cards
## call on 77-99, with 65% probability
## call on A9, A8, A7 suited with 60% probability
## call on AT, KT, QT, JT, with 55% probability
## call on T9, 98, 87, suited with 50% probability
a1 = 0
a2 = min(mychips1, currentbet) ## amt to call
bigb = dealer1 + 2
    if(bigb > numattable1) bigb = bigb - numattable1
    if(round1 == 1){
	if((roundbets[ind1,1] < blinds1 - .5) && (currentbet < blinds1+.5)){
	    a1 = min(2*blinds1, mychips1)
	} else if ((ind1 == bigb) && (currentbet < blinds1 - .5)) a1 = min(blinds1, mychips1)
    }
    if((round1 > 1.5) && (currentbet < .5)) a1 = min(blinds1, mychips1)
if(round1 == 1){ ##pre-flop
a3 = 2*blinds1+1 ## how much a tiny raise would be
a4 = 4*blinds1+1 ## how much a small raise would be
a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
if((crds1[1,1] == crds1[2,1]) && (crds1[2,1] > 9.5)) a1 = mychips1      
if((crds1[1,1] > 10.5) && (crds1[2,1] > 10.5)) a1 = a5
    a5 = runif(1) ## random number uniformly distributed between 0 and 1
    if((crds1[1,1] == crds1[2,1]) && (crds1[2,1] > 6.5) && (crds1[2,1] < 9.5)){ ##77-99
	if((a5 < .65)) a1 = a2
} 
    a6 = runif(1)
    if(((crds1[1,1] == 10) || (crds1[2,1] == 10)) && ((crds1[1,1] > 10.5) || (crds1[2,1] > 10.5))){ ##AT, KT, QT, JT
if((a6 < .55)) a1 = a2
}
    a7 = runif(1)
    if((crds1[1,2] == crds1[2,2]) && (((crds1[1,1] == 14) || (crds1[2,1] == 14)) && (((crds1[1,1] > 6.5) && (crds1[1,1] < 9.5)) || ((crds1[2,1] > 6.5) && (crds1[2,1] < 9.5))))){ ##A9, A8, A7 suited
	if((a7 < .60)) a1 = a2
	}
    a8 = runif(1)
    if((crds1[1,2] == crds1[2,2]) && (((crds1[1,1] == 10) && (crds1[2,1] == 9)) || ((crds1[1,1] == 9) && (crds1[2,1] == 10)) || ((crds1[1,1] == 9) && (crds1[2,1] == 8)) || ((crds1[1,1] == 8) && (crds1[2,1] == 9)) || ((crds1[1,1] == 8) && (crds1[2,1] == 7)) || ((crds1[1,1] == 7) && (crds1[2,1] == 8)))){ ##T9, 98, 87 suited
	if((a8 < .50)) a1 = a2
	}      
}
  if(round1 == 2){ ## post-flop: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## If you have top pair or an overpair or two pairs or a set, make a big bet (call any bigger bet). 
	## If you have an inside straight draw or flush draw then make a small bet (call any bigger bet).
	## If you have a straight or better, then just call.
	## Otherwise fold.
	
	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be	
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
		
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a6) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    if(a2 < a6) a1 = a2
	} else if(x > 1000000 + 15^3*topcard1){
	    a1 = max(a5,a2)
	}
	if((y == 4) || (z == 4)) a1 = max(a6, a2)
	if(x > 4000000) a1 = a2
    }


if(round1 == 3){ ## after turn: 
	## If there's a pair on the board and you don't have a set, then check/call up to small bet.
	## Same thing if there's 3-of-a-kind on the board and you don't have a full house or more.
	## Otherwise, if you have top pair or better, go all in.
	## If you had top pair or overpair but now don't, then check/call a medium bet but fold to more.
	## If you have an inside straight draw or flush draw then check/call a medium bet as well.
	## Otherwise check/fold.
	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
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
	## With nothing, just fold.
	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
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
	}
    }
    round(a1) 
} ## end of krabcake





missingtooth = function(numattable1, crds1, board1,  round1, currentbet,  mychips1, pot1,
    roundbets, blinds1, chips1, ind1, dealer1, tablesleft){

    a1 = 0 ## how much I'm gonna end up betting. Note that the default is zero.
    a2 = min(mychips1, currentbet) ## how much it costs to call

    if(round1 == 1){ ## pre-flop:


	## JJ-AA: huge raise
	## 88-TT: medium raise
	## 22-77: call a small raise
	## AT-AK: medium raise, call anything but a huge raise
	## A7-A9: call up to a medium raise
	## A6-A2: call up to a small raise
	## KQ, KJ, KT, K9: call up to a small raise
	## QJ, QT: call a small raise with probability 50%, or else call a tiny raise
	## JT, T9, 98: call a small raise with probability 30%, or else call a tiny raise

	a3 = 2*blinds1+1 ## how much a tiny raise would be
	a4 = 4*blinds1+1 ## how much a small raise would be
	a5 = max(8*blinds1,mychips1/4)+1 ## how much a big raise would be
	a6 = max(12*blinds1,mychips1/2)+1 ## how much a huge raise would be
	a7 = 6*blinds1+1 ## how much a medium raise would be
	random1 = runif(1)

	if((crds1[1,1] == crds1[2,1]) && (crds1[1,1] > 10.5)){ ## JJ-AA
		a1 = max(a2, a5)
	}

	if((crds1[1,1] == crds1[2,1]) && (crds1[1,1] > 7.5) && (crds1[1,1] < 10.5)){ ## 88-TT
		if(a2 < a7){
		a1 = a7
	    } else if(a2 > a5){
		a1 = 0
	    } else a1 = a2
	}

	if((crds1[1,1] == crds1[2,1]) && (crds1[1,1] < 7.5)){ ## 22-77
		if(a2 < a4) a1 = a2
	}

	if((crds1[1,1] == 14) && (crds1[2,1] > 9.5) && (crds1[2,1] < 13.5)){ ## AT-AK
		if(a2 < a7){
			a1 = a7
		} else if(a2 > a6){
			a1 = 0
		} else a1 = a2
	}

	if((crds1[1,1] == 14) && (crds1[2,1] < 9.5) && (crds1[2,1] > 6.5)){ ## A9-A7
		if(a2 < a7){
			a1 = a2
		} else a1 = 0
	}

	if((crds1[1,1] == 14) && (crds1[2,1] < 6.5)){ ## A2-A6
		if(a2 < a4){
			a1 = a2
		} else a1 = 0
	}

	if((crds1[1,1] == 13) && (crds1[2,1] < 12.5) && (crds1[2,1] > 8.5)){ ## KQ-K9
		if(a2 < a4){
			a1 = a2
		} else a1 = 0
	}

	if((crds1[1,1] == 12) && (crds1[2,1] < 11.5) && (crds1[2,1] > 9.5)){ ## QT,QJ
		if((a2 < a4) && (random1 < .5)){
			a1 = a2
		} else if (a2 < a3){
			a1 = a2
		    } else a1 = 0
		}
	if((crds1[1,1] < 11.5) && (crds1[1,1] > 8.5) && (crds1[2,1] == crds1[1,1]) && (crds1[1,2] == crds1[2,2])){ 
	## JT, T9, 98 suited
		if((a2 < a4) && (random1 < .3)){
			a1 = a2
		} else if (a2 < a3){
			a1 = a2
		} else a1 = 0
	}
    }
    

if(round1 == 2){ ## post-flop:
	## If thereâs a pair on the board and you donât have a set, then check/call up to a small bet.
	## If thereâs 3 of a kind on the board and you have high cards or a full house, make a small bet + 1.
	## If you have top pair or an over pair, make a medium bet.
	## If you have two pairs or a set, make a medium bet.
	## If you have an inside straight draw or flush draw, call a small bet.
	## If you have a straight or better, then call.
	## If youâre the last to decide and nobodyâs bet, make a small bet + 1.
	## Otherwise fold.

	a5 = min(sum(roundbets[,1]),mychips1) ## how much a big bet would be (prev round's pot size)
	a6 = min(.5*sum(roundbets[,1]),mychips1) ## how much a small bet would be
	a7 = min(.75*sum(roundbets[,1]),mychips1) ## how much a medium bet would be
	x = handeval(c(crds1[1:2,1], board1[1:3,1]), c(crds1[1:2,2], board1[1:3,2])) ## what you have
	x1 = handeval(c(board1[1:3,1]),c(board1[1:3,2])) ## what's on the board
	y = straightdraw1(c(crds1[1:2,1], board1[1:3,1]))
	z = flushdraw1(c(crds1[1:2,2], board1[1:3,2]))
	topcard1 = max(board1[1:3,1])
	random2 = runif(1) ## random number on [0,1]
	a8 = (1:numattable1)[roundbets[,1] == roundbets[ind1,1]] ## others who can still bet with you

	## The next 5 lines may seem weird, but the purpose is explained in the next comment:
	a9 = a8 - dealer1
	for(i in 1:length(a9)) if(a9[i]<.5) a9[i] = a9[i] + numattable1
	a10 = ind1 - dealer1
	if(a10 < .5) a10 = a10 + numattable1
	a11 = 2*(a10 == max(a9))   ## So a11 = 2 if you're last to decide; otherwise a11 = 0.

	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a6) a1 = a2
	} else if((x1 > 3000000) && ((x > 6000000) || ((crds1[1,1] > 10.5) && (crds1[2,1] > 10.5)))){
	    if(a2 < a6) a1 = a2 + 1
	} else if(((crds1[1,1] == crds1[2,1]) && (crds1[1,1] > topcard1)) || ((crds1[1,1] == topcard1) || (crds1[2,1] == topcard1)) ){
	    if(a2 < a7) a1 = a2
	} else if((x1 < 3000000) && (x < 4000000) && (x > 2000000) ){
	    if(a2 < a7) a1 = a7 + 1
	} else if((a11 > 1) && (a2 < a6) ){
	    a1 = a6 + 1
	}
	if (((y == 4) || (z == 4)) && (a2 < a6)) a1 = a2
	if (x > 4000000) a1 = a2

}


    if(round1 == 3){ ## after turn:

	## If thereâs a pair on the board and you donât have a set, then check/fold.
	## If thereâs a 3 of a kind of the board and you donât have a full house, then check/fold.
	## If you have a pair, then call a small raise.
	## If you have top pair or better, do a medium raise unless thereâs a flush or straight draw on the board.
	## If you have an inside straight draw or flush draw then check/call a small bet.
	a6 = min(1/3*sum(roundbets[,1:2]),mychips1) ## small bet (1/3 of prev round's pot size)
	a5 = min(.75*sum(roundbets[,1:2]),mychips1) ## medium bet (3/4 of prev round's pot size)
	x = handeval(c(crds1[1:2,1], board1[1:4,1]), c(crds1[1:2,2], board1[1:4,2])) ## what you have
	x1 = handeval(c(board1[1:4,1]),c(board1[1:4,2])) ## what's on the board
	y = straightdraw1(board1[1:4,1])
	z = flushdraw1(board1[1:4,2])

	e = straightdraw1(c(crds1[1:2,1], board1[1:4,1]))
	f = flushdraw1(c(crds1[1:2,2], board1[1:4,2]))

	topcard1 = max(board1[1:4,1])
	oldtopcard1 = max(board1[1:3,1])

	if((x1 > 1000000) && (x < 3000000)){
	    a1 = 0
	} else if((x1 > 3000000) && (x < 6000000)){
	    a1 = 0
	} else if((x > 1000000 + 15^3*topcard1) && (y < 2) && (z < 2)){
	    a1 = a5
	} else if(x > 1000000){
	    if(a2 < a6) a1 = a2
	} else if((e == 4) || (f == 4)){
	    if(a2 < a6) a1 = a2

	}
    }



    if(round1 == 4){ ## after river:
	## If thereâs a pair on the board and you donât have a set, then check/call up to a small bet.
	## If thereâs 3 of a kind on the board and you donât have a full house, then check/fold.
	## If straight or better, go all-in
	## If straight or flush draw on the board, then check/fold
	## If top pair, call a small bet
	## If 3 of a kind with pocket pair, medium raise
	## If 2 pair, call medium bet

	a6 = .45+runif(1)/10  ## random number between .45 and .55
	a5 = min(a6*sum(roundbets[,1:3]),mychips1) ## small bet: around 1/2 of pot size; VARIES RANDOMLY
	a7 = 1.5*a5
	topcard1 = max(board1[1:5,1])
	x = handeval(c(crds1[1:2,1], board1[1:5,1]), c(crds1[1:2,2], board1[1:5,2]))
	x1 = handeval(c(board1[1:5,1]),c(board1[1:5,2])) ## what's on the board
	if((x1 > 1000000) && (x < 3000000)){
	    if(a2 < a5) a1 = a2
	} else if((x1 > 3000000) && (x < 6000000)){
	    a1 = 0
	} else if(x > 4000000){
	    a1 = mychips1
	} else if((crds1[1,1] == crds1[2,1]) && (x < 4000000) && (x > 3000000)){
	    if(a2 < a7) a1 = a7
	} else if((x > 2000000) && (x < 3000000)){
	    if(a2 < a7) a1 = a2
	} else if((topcard1 == crds1[1,1]) || (topcard1 == crds1[2,1])){
		if (a2 < a5) a1 = a2
	}
    }
    round(a1)
} ## end of missingtooth


