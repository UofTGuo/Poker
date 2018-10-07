#convert function: "cards" argument should have 8 characters, "board" argument can have 6,8,10
card.convert <- function(hand, board="NULL") {
	#vectors of cards and suits as numbers and letters
	Clett <- c("T","J","Q","K","A")
	Cnumb <- c(10,11,12,13,14)
	Slett <- c("d","c","h","s")
	Snumb <- c(1,2,3,4)
	list <- strsplit(hand,split="") #split cards into individual elements
	plnum1 <- matrix(0,2,2)  #initialize matrices
	plsuit1 <- matrix(0,2,2) #
	
	#convert cards (in positions 1,3,5,7) to numbers
	for (o in 1:(length(list[[1]])/2)) {
		if (list[[1]][2*o-1] %in% Clett) {
			for (i in 1:5) {
				if (list[[1]][2*o-1] %in% Clett[i]) {
					list[[1]][2*o-1] <- as.numeric(Cnumb[i])
				}
			}	
		}
		plnum1[o] <- list[[1]][2*o-1]
	}	
	#convert suits (in positions 2,4,6,8) to numbers
	for (e in 1:(length(list[[1]])/2)) {
		if (list[[1]][2*e] %in% Slett) {
			for (i in 1:4) {
				if (list[[1]][2*e] %in% Slett[i]) {
					list[[1]][2*e] <- as.numeric(Snumb[i])
				}
			}
		}
		plsuit1[e] <- list[[1]][2*e]
	}		
	plnum1 <- t(plnum1)
	plsuit1 <- t(plsuit1)

	brdnum1 <- c()
	brdsuit1 <- c()
	if (board!="NULL"){
		blist <- strsplit(board,split="") #split cards into individual elements
		#convert cards (in positions 1,3,5,7,9) to numbers
		for (o in 1:(length(blist[[1]])/2)) {
			if (blist[[1]][2*o-1] %in% Clett) {
				for (i in 1:5) {
					if (blist[[1]][2*o-1] %in% Clett[i]) {
						blist[[1]][2*o-1] <- as.numeric(Cnumb[i])
					}
				}	
			}
			brdnum1[o] <- blist[[1]][2*o-1]
		}	
		#convert suits (in positions 2,4,6,8,10) to numbers
		for (e in 1:(length(blist[[1]])/2)) {
			if (blist[[1]][2*e] %in% Slett) {
				for (i in 1:4) {
					if (blist[[1]][2*e] %in% Slett[i]) {
						blist[[1]][2*e] <- as.numeric(Snumb[i])
					}
				}
			}
			brdsuit1[e] <- blist[[1]][2*e]
		}
	}	
	return(list(plnum1=plnum1, plsuit1=plsuit1, brdnum1=brdnum1, brdsuit1=brdsuit1))
}

#Examples
#card.convert("AcJs")
#card.convert("AcJs3d2d")
#card.convert("AcJs3d2d","Kc5sQc")
#card.convert("AcJs3d2d","Kc5sQc5d6d")