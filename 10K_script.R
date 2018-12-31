#Script for 10K hands

#Example of New Players Table(GoodLoose vs. BadLoose)
# (GoodLoose|BadLoose) GoodLoose is big blind, BadLoose is small blind
mean(GoodLoose_vs_BadLoose$p1_luck[1:5000])
mean(GoodLoose_vs_BadLoose$p2_luck[1:5000])
mean(GoodLoose_vs_BadLoose$p1_skill[1:5000])
mean(GoodLoose_vs_BadLoose$p2_skill[1:5000])
mean(GoodLoose_vs_BadLoose$p1_chip[1:5000])
mean(GoodLoose_vs_BadLoose$p2_chip[1:5000])
sd(GoodLoose_vs_BadLoose$p1_luck[1:5000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p2_luck[1:5000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p1_skill[1:5000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p2_skill[1:5000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p1_chip[1:5000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p2_chip[1:5000])/sqrt(5000)
#Correlations
cor(cbind(GoodLoose_vs_BadLoose$p1_skill[1:5000],GoodLoose_vs_BadLoose$p1_luck[1:5000]))
cor(cbind(GoodLoose_vs_BadLoose$p2_skill[1:5000],GoodLoose_vs_BadLoose$p2_luck[1:5000]))

# (BadLoose|GoodLoose) BadLoose is big blind, GoodLoose is small blind
mean(GoodLoose_vs_BadLoose$p2_luck[5001:10000])
mean(GoodLoose_vs_BadLoose$p1_luck[5001:10000])
mean(GoodLoose_vs_BadLoose$p2_skill[5001:10000])
mean(GoodLoose_vs_BadLoose$p1_skill[5001:10000])
mean(GoodLoose_vs_BadLoose$p2_chip[5001:10000])
mean(GoodLoose_vs_BadLoose$p1_chip[5001:10000])
sd(GoodLoose_vs_BadLoose$p2_luck[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p1_luck[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p2_skill[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p1_skill[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p2_chip[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_BadLoose$p1_chip[5001:10000])/sqrt(5000)
#Correlations
cor(cbind(GoodLoose_vs_BadLoose$p2_skill[5001:10000],GoodLoose_vs_BadLoose$p2_luck[5001:10000]))
cor(cbind(GoodLoose_vs_BadLoose$p1_skill[5001:10000],GoodLoose_vs_BadLoose$p1_luck[5001:10000]))


#Example of Tommy vs. New players Table(Tommy vs. GoodLoose)
# (Tommy|GoodLoose) Tommy is big blind, GoodLoose is small blind
mean(BadTight_vs_Tommy$p2_luck[5001:10000])
mean(BadTight_vs_Tommy$p1_luck[5001:10000])
mean(BadTight_vs_Tommy$p2_skill[5001:10000])
mean(BadTight_vs_Tommy$p1_skill[5001:10000])
mean(BadTight_vs_Tommy$p2_chip[5001:10000])
mean(BadTight_vs_Tommy$p1_chip[5001:10000])
sd(BadTight_vs_Tommy$p2_luck[5001:10000])/sqrt(5000)
sd(BadTight_vs_Tommy$p1_luck[5001:10000])/sqrt(5000)
sd(BadTight_vs_Tommy$p2_skill[5001:10000])/sqrt(5000)
sd(BadTight_vs_Tommy$p1_skill[5001:10000])/sqrt(5000)
sd(BadTight_vs_Tommy$p2_chip[5001:10000])/sqrt(5000)
sd(BadTight_vs_Tommy$p1_chip[5001:10000])/sqrt(5000)
#Correlations
cor(cbind(BadTight_vs_Tommy$p2_skill[5001:10000],BadTight_vs_Tommy$p2_luck[5001:10000]))
cor(cbind(BadTight_vs_Tommy$p1_skill[5001:10000],BadTight_vs_Tommy$p1_luck[5001:10000]))

# (GoodLoose|Tommy) GoodLoose is big blind, Tommy is small blind
mean(BadTight_vs_Tommy$p1_luck[1:5000])
mean(BadTight_vs_Tommy$p2_luck[1:5000])
mean(BadTight_vs_Tommy$p1_skill[1:5000])
mean(BadTight_vs_Tommy$p2_skill[1:5000])
mean(BadTight_vs_Tommy$p1_chip[1:5000])
mean(BadTight_vs_Tommy$p2_chip[1:5000])
sd(BadTight_vs_Tommy$p1_luck[1:5000])/sqrt(5000)
sd(BadTight_vs_Tommy$p2_luck[1:5000])/sqrt(5000)
sd(BadTight_vs_Tommy$p1_skill[1:5000])/sqrt(5000)
sd(BadTight_vs_Tommy$p2_skill[1:5000])/sqrt(5000)
sd(BadTight_vs_Tommy$p1_chip[1:5000])/sqrt(5000)
sd(BadTight_vs_Tommy$p2_chip[1:5000])/sqrt(5000)
#Correlations
cor(cbind(BadTight_vs_Tommy$p1_skill[1:5000],BadTight_vs_Tommy$p1_luck[1:5000]))
cor(cbind(BadTight_vs_Tommy$p2_skill[1:5000],BadTight_vs_Tommy$p2_luck[1:5000]))

#Example of Zelda vs. New players Table(Zelda vs. GoodLoose)
# (Zelda|GoodLoose) Zelda is big blind, GoodLoose is small blind
mean(GoodLoose_vs_Zelda$p2_luck[5001:10000])
mean(GoodLoose_vs_Zelda$p1_luck[5001:10000])
mean(GoodLoose_vs_Zelda$p2_skill[5001:10000])
mean(GoodLoose_vs_Zelda$p1_skill[5001:10000])
mean(GoodLoose_vs_Zelda$p2_chip[5001:10000])
mean(GoodLoose_vs_Zelda$p1_chip[5001:10000])
sd(GoodLoose_vs_Zelda$p2_luck[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p1_luck[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p2_skill[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p1_skill[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p2_chip[5001:10000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p1_chip[5001:10000])/sqrt(5000)
#Correlations
cor(cbind(GoodLoose_vs_Zelda$p2_skill[5001:10000],GoodLoose_vs_Zelda$p2_luck[5001:10000]))
cor(cbind(GoodLoose_vs_Zelda$p1_skill[5001:10000],GoodLoose_vs_Zelda$p1_luck[5001:10000]))

# (GoodLoose|Zelda) GoodLoose is big blind, Zelda is small blind
mean(GoodLoose_vs_Zelda$p1_luck[1:5000])
mean(GoodLoose_vs_Zelda$p2_luck[1:5000])
mean(GoodLoose_vs_Zelda$p1_skill[1:5000])
mean(GoodLoose_vs_Zelda$p2_skill[1:5000])
mean(GoodLoose_vs_Zelda$p1_chip[1:5000])
mean(GoodLoose_vs_Zelda$p2_chip[1:5000])
sd(GoodLoose_vs_Zelda$p1_luck[1:5000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p2_luck[1:5000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p1_skill[1:5000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p2_skill[1:5000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p1_chip[1:5000])/sqrt(5000)
sd(GoodLoose_vs_Zelda$p2_chip[1:5000])/sqrt(5000)
#Correlations
cor(cbind(GoodLoose_vs_Zelda$p1_skill[1:5000],GoodLoose_vs_Zelda$p1_luck[1:5000]))
cor(cbind(GoodLoose_vs_Zelda$p2_skill[1:5000],GoodLoose_vs_Zelda$p2_luck[1:5000]))

#Histogram
#hist(GoodLoose_vs_BadLoose$p2_chip[5001:10000])
#hist(GoodLoose_vs_BadLoose$p1_chip[1:5000])
