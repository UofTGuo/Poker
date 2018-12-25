options(scipen=999)
###################
#Tommy vs Tommy
dat <- read.table("decision_s_data.txt")
dat1 <- dat[1:1500,]
dat2 <- dat[1501:3000,]

mean1 <- colMeans(dat1)
se1 <- sqrt(diag(var(dat1)))/sqrt(nrow(dat1))
corr1 <- c(cor(dat1)[1,3],cor(dat1)[2,4])
slr1 <- c(mean1[3]/mean1[1],mean1[4]/mean1[2])

round(mean1,3); round(se1,2)
round(c(corr1,slr1))
table(dat1[,7])

mean2 <- colMeans(dat2)
se2 <- sqrt(diag(var(dat2)))/sqrt(nrow(dat2))
corr1 <- c(cor(dat2)[1,3],cor(dat2)[2,4])
slr1 <- c(mean2[3]/mean2[1],mean2[4]/mean2[2])

round(mean2,3); round(se2,2)
round(c(corr2,slr2))
table(dat2[,7])