#GoodLoose is small blind, BadLoose is big blind
mean(GoodLoose_vs_BadLoose$p1_luck[1:1500])
mean(GoodLoose_vs_BadLoose$p2_luck[1:1500])
mean(GoodLoose_vs_BadLoose$p1_skill[1:1500])
mean(GoodLoose_vs_BadLoose$p2_skill[1:1500])
mean(GoodLoose_vs_BadLoose$p1_chip[1:1500])
mean(GoodLoose_vs_BadLoose$p2_chip[1:1500])

sd(GoodLoose_vs_BadLoose$p1_luck[1:1500])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_luck[1:1500])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p1_skill[1:1500])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_skill[1:1500])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p1_chip[1:1500])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_chip[1:1500])/sqrt(1500)

#BadLoose is small blind, GoodLoose is big blind
mean(GoodLoose_vs_BadLoose$p1_luck[1501:3000])
mean(GoodLoose_vs_BadLoose$p2_luck[1501:3000])
mean(GoodLoose_vs_BadLoose$p1_skill[1501:3000])
mean(GoodLoose_vs_BadLoose$p2_skill[1501:3000])
mean(GoodLoose_vs_BadLoose$p1_chip[1501:3000])
mean(GoodLoose_vs_BadLoose$p2_chip[1501:3000])

sd(GoodLoose_vs_BadLoose$p1_luck[1501:3000])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_luck[1501:3000])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p1_skill[1501:3000])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_skill[1501:3000])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p1_chip[1501:3000])/sqrt(1500)
sd(GoodLoose_vs_BadLoose$p2_chip[1501:3000])/sqrt(1500)

#Combined
mean(GoodLoose_vs_BadLoose$p1_luck)
mean(GoodLoose_vs_BadLoose$p2_luck)
mean(GoodLoose_vs_BadLoose$p1_skill)
mean(GoodLoose_vs_BadLoose$p2_skill)
mean(GoodLoose_vs_BadLoose$p1_chip)
mean(GoodLoose_vs_BadLoose$p2_chip)

sd(GoodLoose_vs_BadLoose$p1_luck)/sqrt(3000)
sd(GoodLoose_vs_BadLoose$p2_luck)/sqrt(3000)
sd(GoodLoose_vs_BadLoose$p1_skill)/sqrt(3000)
sd(GoodLoose_vs_BadLoose$p2_skill)/sqrt(3000)
sd(GoodLoose_vs_BadLoose$p1_chip)/sqrt(3000)
sd(GoodLoose_vs_BadLoose$p2_chip)/sqrt(3000)