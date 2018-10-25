#0 indicates last move (c or f) was small blind
#1 indicates last move was big blind

allhands <- read.csv("all_hands3.csv")
test <- allhands

vec <- vector("list",nrow(test))
for(i in 1:nrow(test)) {
	vec[i] <- strsplit(as.character(test$PreFlop[i]),"r")
	if(vec[[i]][1]=="c"){
		test$PF.Player[i] <- (length(vec[[i]])) %% 2
	} else {
		test$PF.Player[i] <- (length(vec[[i]])+1) %% 2
	}
}
test$PreFlop <- sapply(vec,tail,1)
#test$PreFlop <- as.numeric(substring(test$PreFlop,1,nchar(test$PreFlop)-1))

vec <- vector("list",nrow(test))
for(i in 1:nrow(test)) {
	vec[i] <- strsplit(as.character(test$Flop[i]),"r")
	if(vec[[i]][1]=="c" | is.na(vec[[i]][1])) {
		test$F.Player[i] <- (length(vec[[i]])+1) %% 2
	} else {
		test$F.Player[i] <- (length(vec[[i]])) %% 2
	}
}
test$Flop <- sapply(vec,tail,1)

vec <- vector("list",nrow(test))
for(i in 1:nrow(test)) {
	vec[i] <- strsplit(as.character(test$Turn[i]),"r")
	if(vec[[i]][1]=="c" | is.na(vec[[i]][1])) {
		test$T.Player[i] <- (length(vec[[i]])+1) %% 2
	} else {
		test$T.Player[i] <- (length(vec[[i]])) %% 2
	}
}
test$Turn <- sapply(vec,tail,1)

vec <- vector("list",nrow(test))
for(i in 1:nrow(test)) {
	vec[i] <- strsplit(as.character(test$River[i]),"r")
	if(vec[[i]][1]=="c" | is.na(vec[[i]][1])) {
		test$R.Player[i] <- (length(vec[[i]])+1) %% 2
	} else {
		test$R.Player[i] <- (length(vec[[i]])) %% 2
	}
}
test$River <- sapply(vec,tail,1)

test$PreFlop

test