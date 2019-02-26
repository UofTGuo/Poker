#Take data from https://github.com/UofTGuo/Poker/tree/master/simulation_result/3K_hands
data <- read.csv("https://raw.githubusercontent.com/UofTGuo/Poker/master/simulation_result/3K_hands/BadLoose_vs_GoodTight.csv")
gtbl <- data[1501:3000,]

gt_avg_luck <- bl_avg_luck <- gt_avg_skill <- bl_avg_skill <- numeric(nrow(gtbl))
bl_se_luck <- gt_se_luck <- bl_se_skill <- gt_se_skill <- rep(0,nrow(gtbl))
for(i in 1:nrow(gtbl)){
	bl_avg_luck[i] <- mean(gtbl[1:i,2])
	gt_avg_luck[i] <- mean(gtbl[1:i,3])
	bl_avg_skill[i] <- mean(gtbl[1:i,4])
	gt_avg_skill[i] <- mean(gtbl[1:i,5])
	if(i >= 2){
		bl_se_luck[i] <- sd(gtbl[1:i,2])/sqrt(i)
		gt_se_luck[i] <- sd(gtbl[1:i,3])/sqrt(i)
		bl_se_skill[i] <- sd(gtbl[1:i,4])/sqrt(i)
		gt_se_skill[i] <- sd(gtbl[1:i,5])/sqrt(i)
	}
}

# Second graph for BadLoose commented out. They're mirrored, so one is sufficient.
#par(mfrow=c(1,2))
#Change main="" to show who big blind is
plot(1:nrow(gtbl), gt_avg_skill, type="l", col="green", lwd=2,
	xlab="Hand Number", ylab="Average", main="Iterated Average of GoodTight(BB) vs. BadLoose")
points(1:nrow(gtbl), gt_avg_skill - 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_skill + 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_luck, type="l", col="red", lwd=2)
points(1:nrow(gtbl), gt_avg_luck - 1.96*gt_se_luck, type="l", lty=2, col="black")
points(1:nrow(gtbl), gt_avg_luck + 1.96*gt_se_luck, type="l", lty=2, col="black")
legend("bottomright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("green","darkgrey","red","black"), lty=c(1,2,1,2))

#Think about how to describe titles?
#plot(1:nrow(gtbl), bl_avg_skill, type="l", col="green", lwd=2,
#	xlab="Hand Number", ylab="Average", main="Iterated Average of BadLoose(SB) vs. GoodTight")
#points(1:nrow(gtbl), bl_avg_skill - 1.96*bl_se_skill, type="l", lty=2, col="darkgrey")
#points(1:nrow(gtbl), bl_avg_skill + 1.96*bl_se_skill, type="l", lty=2, col="darkgrey")
#points(1:nrow(gtbl), bl_avg_luck, type="l", col="red", lwd=2)
#points(1:nrow(gtbl), bl_avg_luck - 1.96*bl_se_luck, type="l", lty=2, col="black")
#points(1:nrow(gtbl), bl_avg_luck + 1.96*bl_se_luck, type="l", lty=2, col="black")
#legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("green","darkgrey","red","black"), lty=c(1,2,1,2))