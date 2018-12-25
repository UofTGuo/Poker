options(scipen=999)
##########
test <- read.table("sch_d1_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)

##########
test <- read.table("sch_d2_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)

##########
test <- read.table("sch_d3_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)

##########
test <- read.table("sch_d4_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)

##########
test <- read.table("sch_d5_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)

##########
test <- read.table("sch_d6_data.txt")
means0 <-colMeans(test)
se0 <- c(sqrt(diag(var(test)))/sqrt(nrow(test1)) )
corr0 <- c(cor(test)[1,3],cor(test)[2,4])
slr0 <- c(means0[3]/means0[1],means0[4]/means0[2])
pff0 <- length(test$V5[test$V5==50])/nrow(test)

round(means0,3); round(se0,2)
round(c(corr0,slr0,pff0),3)

test1 <- test[seq(2,nrow(test),by=2),]
means1 <-colMeans(test1)
se1 <- c(sqrt(diag(var(test1)))/sqrt(nrow(test1)) )
corr1 <- c(cor(test1)[1,3],cor(test1)[2,4])
slr1 <- c(means1[3]/means1[1],means1[4]/means1[2])
pff1 <- length(test1$V5[test1$V5==50])/nrow(test1)

round(means1,3); round(se1,2)
round(c(corr1,slr1,pff1),3)

test2 <- test[seq(1,nrow(test),by=2),c(2,1,4,3,6,5)]
means2 <-colMeans(test2)
se2 <- c(sqrt(diag(var(test2)))/sqrt(nrow(test2)) )
corr2 <- c(cor(test2)[1,3],cor(test2)[2,4])
slr2 <- c(means2[3]/means2[1],means2[4]/means2[2])
pff2 <- length(test2$V5[test2$V5==50])/nrow(test2)

round(means2,3); round(se2,2)
round(c(corr2,slr2,pff2),3)
