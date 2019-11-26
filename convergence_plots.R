#Take data from https://github.com/UofTGuo/Poker/tree/master/simulation_result/3K_hands
data <- read.csv("https://raw.githubusercontent.com/UofTGuo/Poker/master/simulation_result/3K_hands/BadLoose_vs_GoodTight.csv")
#data[1:1500,] corresponds to the hands where the first player is big blind; in this example it is BadLoose
#data[1501:3000,] corresponds to the hands where the second player is big blind; in this example it is GoodTight
p1p2 <- data[1:1500,]

p1_avg_luck <- p2_avg_luck <- p1_avg_skill <- p2_avg_skill <- numeric(nrow(p1p2))
p2_se_luck <- p1_se_luck <- p2_se_skill <- p1_se_skill <- rep(0,nrow(p1p2))
for(i in 1:nrow(p1p2)){
	p1_avg_luck[i] <- mean(p1p2[1:i,2])
	p2_avg_luck[i] <- mean(p1p2[1:i,3])
	p1_avg_skill[i] <- mean(p1p2[1:i,4])
	p2_avg_skill[i] <- mean(p1p2[1:i,5])
	if(i >= 2){
		p1_se_luck[i] <- sd(p1p2[1:i,2])/sqrt(i)
		p2_se_luck[i] <- sd(p1p2[1:i,3])/sqrt(i)
		p1_se_skill[i] <- sd(p1p2[1:i,4])/sqrt(i)
		p2_se_skill[i] <- sd(p1p2[1:i,5])/sqrt(i)
	}
}

# Second graph for BadLoose commented out. They're mirrored, so one is sufficient.
#par(mfrow=c(1,2))
#Change main="" to show who big blind is
plot(1:nrow(p1p2), p1_avg_skill, type="l", lwd=2, ylim=c(-3000,3000), lty=1,
	xlab="Hand Number", ylab="Average Profit", main="Iterated Average of Profit for BadLoose(BB) vs. GoodTight")
points(1:nrow(p1p2), p1_avg_skill - 1.96*p1_se_skill, type="l", lty=1, col="grey")
points(1:nrow(p1p2), p1_avg_skill + 1.96*p1_se_skill, type="l", lty=1, col="grey")
points(1:nrow(p1p2), p1_avg_luck, type="l", lwd=2, lty=2)
points(1:nrow(p1p2), p1_avg_luck - 1.96*p1_se_luck, type="l", lty=2, col="grey")
points(1:nrow(p1p2), p1_avg_luck + 1.96*p1_se_luck, type="l", lty=2, col="grey")
legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("black","grey","black","grey"), lty=c(1,1,2,2))

#Think about how to describe titles?
#plot(1:nrow(p1p2), p2_avg_skill, type="l", lwd=2, ylim=c(-3000,3000), lty=1,
#	xlab="Hand Number", ylab="Average Profit", main="Iterated Average of GoodTight(SB) vs. BadLoose")
#points(1:nrow(p1p2), p2_avg_skill - 1.96*p2_se_skill, type="l", lty=1, col="grey")
#points(1:nrow(p1p2), p2_avg_skill + 1.96*p2_se_skill, type="l", lty=1, col="grey")
#points(1:nrow(p1p2), p2_avg_luck, type="l", lwd=2, lty=2)
#points(1:nrow(p1p2), p2_avg_luck - 1.96*p2_se_luck, type="l", lty=2, col="grey")
#points(1:nrow(p1p2), p2_avg_luck + 1.96*p2_se_luck, type="l", lty=2, col="grey")
#legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("black","grey","black","grey"), lty=c(1,1,2,2))

### Latter 1500 hands
#Use p2 instead of p1 to focus on the same player as in the first 1500 hands

p1p2 <- data[1501:3000,]
p1_avg_luck <- p2_avg_luck <- p1_avg_skill <- p2_avg_skill <- numeric(nrow(p1p2))
p2_se_luck <- p1_se_luck <- p2_se_skill <- p1_se_skill <- rep(0,nrow(p1p2))
for(i in 1:nrow(p1p2)){
	p1_avg_luck[i] <- mean(p1p2[1:i,2])
	p2_avg_luck[i] <- mean(p1p2[1:i,3])
	p1_avg_skill[i] <- mean(p1p2[1:i,4])
	p2_avg_skill[i] <- mean(p1p2[1:i,5])
	if(i >= 2){
		p1_se_luck[i] <- sd(p1p2[1:i,2])/sqrt(i)
		p2_se_luck[i] <- sd(p1p2[1:i,3])/sqrt(i)
		p1_se_skill[i] <- sd(p1p2[1:i,4])/sqrt(i)
		p2_se_skill[i] <- sd(p1p2[1:i,5])/sqrt(i)
	}
}
plot(1:nrow(p1p2), p2_avg_skill, type="l", lwd=2, ylim=c(-3000,3000), lty=1,
	xlab="Hand Number", ylab="Average Profit", main="Iterated Average of BadLoose(SB) vs. GoodTight")
points(1:nrow(p1p2), p2_avg_skill - 1.96*p2_se_skill, type="l", lty=1, col="grey")
points(1:nrow(p1p2), p2_avg_skill + 1.96*p2_se_skill, type="l", lty=1, col="grey")
points(1:nrow(p1p2), p2_avg_luck, type="l", lwd=2, lty=2)
points(1:nrow(p1p2), p2_avg_luck - 1.96*p2_se_luck, type="l", lty=2, col="grey")
points(1:nrow(p1p2), p2_avg_luck + 1.96*p2_se_luck, type="l", lty=2, col="grey")
legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("black","grey","black","grey"), lty=c(1,1,2,2))
