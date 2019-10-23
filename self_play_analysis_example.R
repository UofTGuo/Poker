### 3K simulation ###
#obtain dataset
GoodLoose_vs_GoodLoose <- read.csv("https://raw.githubusercontent.com/UofTGuo/Poker/master/simulation_result/3K_hands/GoodLoose_vs_GoodLoose.csv")

#format data as player perspective over 3000 hands
p1_luck <- c(GoodLoose_vs_GoodLoose[-1][1:1500,1],GoodLoose_vs_GoodLoose[-1][1501:3000,2])
p2_luck <- c(GoodLoose_vs_GoodLoose[-1][1:1500,2],GoodLoose_vs_GoodLoose[-1][1501:3000,1])
p1_skill <- c(GoodLoose_vs_GoodLoose[-1][1:1500,3],GoodLoose_vs_GoodLoose[-1][1501:3000,4])
p2_skill <- c(GoodLoose_vs_GoodLoose[-1][1:1500,4],GoodLoose_vs_GoodLoose[-1][1501:3000,3])
p1_chips <- c(GoodLoose_vs_GoodLoose[-1][1:1500,5],GoodLoose_vs_GoodLoose[-1][1501:3000,6])
p2_chips <- c(GoodLoose_vs_GoodLoose[-1][1:1500,6],GoodLoose_vs_GoodLoose[-1][1501:3000,5])
datta <- data.frame(p1_luck,p2_luck,p1_skill,p2_skill,p1_chips,p2_chips)
colMeans(datta)
apply(datta,2,sd)/sqrt(3000)
cor(p1_luck,p1_skill)
cor(p2_luck,p2_skill)

#iterated average plot - from iterated_average_h.R
gtbl <- datta

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

plot(1:nrow(gtbl), gt_avg_skill, type="l", col="green", lwd=2,
	xlab="Hand Number", ylab="Average Equity", main="Iterated Average of Equity for GoodLoose Self Play")
points(1:nrow(gtbl), gt_avg_skill - 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_skill + 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_luck, type="l", col="red", lwd=2)
points(1:nrow(gtbl), gt_avg_luck - 1.96*gt_se_luck, type="l", lty=2, col="black")
points(1:nrow(gtbl), gt_avg_luck + 1.96*gt_se_luck, type="l", lty=2, col="black")
legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("green","darkgrey","red","black"), lty=c(1,2,1,2))

### 10K simulation ###
#obtain data
GoodLoose_vs_GoodLoose10k <- read.csv("https://raw.githubusercontent.com/UofTGuo/Poker/master/simulation_result/10K_hands/GoodLoose_vs_GoodLoose.csv")

#reformat
p1_luck <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,1],GoodLoose_vs_GoodLoose10k[-1][5001:10000,2])
p2_luck <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,2],GoodLoose_vs_GoodLoose10k[-1][5001:10000,1])
p1_skill <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,3],GoodLoose_vs_GoodLoose10k[-1][5001:10000,4])
p2_skill <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,4],GoodLoose_vs_GoodLoose10k[-1][5001:10000,3])
p1_chips <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,5],GoodLoose_vs_GoodLoose10k[-1][5001:10000,6])
p2_chips <- c(GoodLoose_vs_GoodLoose10k[-1][1:5000,6],GoodLoose_vs_GoodLoose10k[-1][5001:10000,5])
datta10k <- data.frame(p1_luck,p2_luck,p1_skill,p2_skill,p1_chips,p2_chips)
colMeans(datta10k)
apply(datta10k,2,sd)/sqrt(10000)
cor(p1_luck,p1_skill)
cor(p2_luck,p2_skill)

#iterated average plot - from iterated_average_h.R
gtbl <- datta10k

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

plot(1:nrow(gtbl), gt_avg_skill, type="l", col="green", lwd=2,
	xlab="Hand Number", ylab="Average Equity", main="Iterated Average of Equity for GoodLoose Self Play")
points(1:nrow(gtbl), gt_avg_skill - 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_skill + 1.96*gt_se_skill, type="l", lty=2, col="darkgrey")
points(1:nrow(gtbl), gt_avg_luck, type="l", col="red", lwd=2)
points(1:nrow(gtbl), gt_avg_luck - 1.96*gt_se_luck, type="l", lty=2, col="black")
points(1:nrow(gtbl), gt_avg_luck + 1.96*gt_se_luck, type="l", lty=2, col="black")
legend("topright", legend=c("Skill","SE(Skill)","Luck","SE(Luck)"), col=c("green","darkgrey","red","black"), lty=c(1,2,1,2))

