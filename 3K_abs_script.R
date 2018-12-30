#Script for abs luck/skill equity with 3K hands

#Example of New Players Table(GoodLoose vs. BadLoose)
# (GoodLoose|BadLoose) GoodLoose is big blind, BadLoose is small blind
mean(abs(GoodLoose_vs_BadLoose$p1_luck[1:1500]))
mean(abs(GoodLoose_vs_BadLoose$p2_luck[1:1500]))
mean(abs(GoodLoose_vs_BadLoose$p1_skill[1:1500]))
mean(abs(GoodLoose_vs_BadLoose$p2_skill[1:1500]))
mean(abs(GoodLoose_vs_BadLoose$p1_chip[1:1500]))
mean(abs(GoodLoose_vs_BadLoose$p2_chip[1:1500]))
sd(abs(GoodLoose_vs_BadLoose$p1_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p2_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p1_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p2_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p1_chip[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p2_chip[1:1500]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_BadLoose$p2_skill[1:1500]),abs(GoodLoose_vs_BadLoose$p2_luck[1:1500])))

# (BadLoose|GoodLoose) BadLoose is big blind, GoodLoose is small blind
mean(abs(GoodLoose_vs_BadLoose$p2_luck[1501:3000]))
mean(abs(GoodLoose_vs_BadLoose$p1_luck[1501:3000]))
mean(abs(GoodLoose_vs_BadLoose$p2_skill[1501:3000]))
mean(abs(GoodLoose_vs_BadLoose$p1_skill[1501:3000]))
mean(abs(GoodLoose_vs_BadLoose$p2_chip[1501:3000]))
mean(abs(GoodLoose_vs_BadLoose$p1_chip[1501:3000]))
sd(abs(GoodLoose_vs_BadLoose$p2_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p1_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p2_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p1_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p2_chip[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_BadLoose$p1_chip[1501:3000]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_BadLoose$p1_skill[1501:3000]),abs(GoodLoose_vs_BadLoose$p1_luck[1501:3000])))


#Example of Tommy vs. New players Table(Tommy vs. GoodLoose)
# (Tommy|GoodLoose) Tommy is big blind, GoodLoose is small blind
mean(abs(GoodLoose_vs_Tommy$p2_luck[1501:3000]))
mean(abs(GoodLoose_vs_Tommy$p1_luck[1501:3000]))
mean(abs(GoodLoose_vs_Tommy$p2_skill[1501:3000]))
mean(abs(GoodLoose_vs_Tommy$p1_skill[1501:3000]))
mean(abs(GoodLoose_vs_Tommy$p2_chip[1501:3000]))
mean(abs(GoodLoose_vs_Tommy$p1_chip[1501:3000]))
sd(abs(GoodLoose_vs_Tommy$p2_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p1_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p2_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p1_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p2_chip[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p1_chip[1501:3000]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_Tommy$p1_skill[1501:3000]),abs(GoodLoose_vs_Tommy$p1_luck[1501:3000])))

# (GoodLoose|Tommy) GoodLoose is big blind, Tommy is small blind
mean(abs(GoodLoose_vs_Tommy$p1_luck[1:1500]))
mean(abs(GoodLoose_vs_Tommy$p2_luck[1:1500]))
mean(abs(GoodLoose_vs_Tommy$p1_skill[1:1500]))
mean(abs(GoodLoose_vs_Tommy$p2_skill[1:1500]))
mean(abs(GoodLoose_vs_Tommy$p1_chip[1:1500]))
mean(abs(GoodLoose_vs_Tommy$p2_chip[1:1500]))
sd(abs(GoodLoose_vs_Tommy$p1_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p2_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p1_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p2_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p1_chip[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Tommy$p2_chip[1:1500]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_Tommy$p2_skill[1:1500]),abs(GoodLoose_vs_Tommy$p2_luck[1:1500])))


#Example of Zelda vs. New players Table(Zelda vs. GoodLoose)
# (Zelda|GoodLoose) Zelda is big blind, GoodLoose is small blind
mean(abs(GoodLoose_vs_Zelda$p2_luck[1501:3000]))
mean(abs(GoodLoose_vs_Zelda$p1_luck[1501:3000]))
mean(abs(GoodLoose_vs_Zelda$p2_skill[1501:3000]))
mean(abs(GoodLoose_vs_Zelda$p1_skill[1501:3000]))
mean(abs(GoodLoose_vs_Zelda$p2_chip[1501:3000]))
mean(abs(GoodLoose_vs_Zelda$p1_chip[1501:3000]))
sd(abs(GoodLoose_vs_Zelda$p2_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p1_luck[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p2_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p1_skill[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p2_chip[1501:3000]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p1_chip[1501:3000]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_Zelda$p1_skill[1501:3000]),abs(GoodLoose_vs_Zelda$p1_luck[1501:3000])))

# (GoodLoose|Zelda) GoodLoose is big blind, Zelda is small blind
mean(abs(GoodLoose_vs_Zelda$p1_luck[1:1500]))
mean(abs(GoodLoose_vs_Zelda$p2_luck[1:1500]))
mean(abs(GoodLoose_vs_Zelda$p1_skill[1:1500]))
mean(abs(GoodLoose_vs_Zelda$p2_skill[1:1500]))
mean(abs(GoodLoose_vs_Zelda$p1_chip[1:1500]))
mean(abs(GoodLoose_vs_Zelda$p2_chip[1:1500]))
sd(abs(GoodLoose_vs_Zelda$p1_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p2_luck[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p1_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p2_skill[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p1_chip[1:1500]))/sqrt(1500)
sd(abs(GoodLoose_vs_Zelda$p2_chip[1:1500]))/sqrt(1500)
#Correlations
cor(cbind(abs(GoodLoose_vs_Zelda$p2_skill[1:1500]),abs(GoodLoose_vs_Zelda$p2_luck[1:1500])))
