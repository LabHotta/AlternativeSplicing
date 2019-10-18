library(dplyr)
library(tidyr)
library(broom)

LHY1_timecourses_FSC2 <- read.csv("LHY1_splicing_FSC2_all.txt", sep="\t")
LHY1_timecourses_FSC2b<- LHY1_timecourses_FSC2[,c(1,3,5)]
LHY1_timecourses_FSC2b$ZT[LHY1_timecourses_FSC2b$ZT == -1.5] <- 22.5
LHY1_timecourses_FSC2b$ZT[LHY1_timecourses_FSC2b$ZT == 24.5] <- 0.5
LHY1_timecourses_FSC2b$ZT <- paste("ZT", LHY1_timecourses_FSC2b$ZT, sep="")
LHY1_timecourses_FSC2b$ZT<-factor(LHY1_timecourses_FSC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY1_timecourses_FSC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {LHY1_timecourses_FSC2b[grep(x,LHY1_timecourses_FSC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(LHY.1 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
     })

LHY1_timecourses_ASC2 <- read.csv("LHY1_splicing_ASC2_all.txt", sep="\t")
LHY1_timecourses_ASC2b<- LHY1_timecourses_ASC2[,c(1,3,5)]
LHY1_timecourses_ASC2b$ZT[LHY1_timecourses_ASC2b$ZT == -1.5] <- 22.5
LHY1_timecourses_ASC2b$ZT[LHY1_timecourses_ASC2b$ZT == 24.5] <- 0.5
LHY1_timecourses_ASC2b$ZT <- paste("ZT", LHY1_timecourses_ASC2b$ZT, sep="")
LHY1_timecourses_ASC2b$ZT<-factor(LHY1_timecourses_ASC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY1_timecourses_ASC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {LHY1_timecourses_ASC2b[grep(x,LHY1_timecourses_ASC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(LHY.1 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


LHY4_timecourses_FSC2 <- read.csv("LHY4_splicing_FSC2_all.txt", sep="\t")
LHY4_timecourses_FSC2b<- LHY4_timecourses_FSC2[,c(1,3,5)]
LHY4_timecourses_FSC2b$ZT[LHY4_timecourses_FSC2b$ZT == -1.5] <- 22.5
LHY4_timecourses_FSC2b$ZT[LHY4_timecourses_FSC2b$ZT == 24.5] <- 0.5
LHY4_timecourses_FSC2b$ZT <- paste("ZT", LHY4_timecourses_FSC2b$ZT, sep="")
LHY4_timecourses_FSC2b$ZT<-factor(LHY4_timecourses_FSC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY4_timecourses_FSC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {LHY4_timecourses_FSC2b[grep(x,LHY4_timecourses_FSC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(LHY.1 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


LHY4_timecourses_ASC2 <- read.csv("LHY4_splicing_ASC2_all.txt", sep="\t")
LHY4_timecourses_ASC2b<- LHY4_timecourses_ASC2[,c(1,3,5)]
LHY4_timecourses_ASC2b$ZT[LHY4_timecourses_ASC2b$ZT == -1.5] <- 22.5
LHY4_timecourses_ASC2b$ZT[LHY4_timecourses_ASC2b$ZT == 24.5] <- 0.5
LHY4_timecourses_ASC2b$ZT <- paste("ZT", LHY4_timecourses_ASC2b$ZT, sep="")
LHY4_timecourses_ASC2b$ZT<-factor(LHY4_timecourses_ASC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY4_timecourses_ASC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {LHY4_timecourses_ASC2b[grep(x,LHY4_timecourses_ASC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(LHY.4 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3731_timecourses_FSC2 <- read.csv("PRR3731_splicing_FSC2_all.txt", sep="\t")
PRR3731_timecourses_FSC2b<- PRR3731_timecourses_FSC2[,c(1,3,5)]
PRR3731_timecourses_FSC2b$ZT[PRR3731_timecourses_FSC2b$ZT == -1.5] <- 22.5
PRR3731_timecourses_FSC2b$ZT[PRR3731_timecourses_FSC2b$ZT == 24.5] <- 0.5
PRR3731_timecourses_FSC2b$ZT <- paste("ZT", PRR3731_timecourses_FSC2b$ZT, sep="")
PRR3731_timecourses_FSC2b$ZT<-factor(PRR3731_timecourses_FSC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3731_timecourses_FSC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR3731_timecourses_FSC2b[grep(x,PRR3731_timecourses_FSC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.31 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})

PRR3731_timecourses_ASC2 <- read.csv("PRR3731_splicing_ASC2_all.txt", sep="\t")
PRR3731_timecourses_ASC2b<- PRR3731_timecourses_ASC2[,c(1,3,5)]
PRR3731_timecourses_ASC2b$ZT[PRR3731_timecourses_ASC2b$ZT == -1.5] <- 22.5
PRR3731_timecourses_ASC2b$ZT[PRR3731_timecourses_ASC2b$ZT == 24.5] <- 0.5
PRR3731_timecourses_ASC2b$ZT <- paste("ZT", PRR3731_timecourses_ASC2b$ZT, sep="")
PRR3731_timecourses_ASC2b$ZT<-factor(PRR3731_timecourses_ASC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3731_timecourses_ASC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR3731_timecourses_ASC2b[grep(x,PRR3731_timecourses_ASC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.31 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})

PRR3732_timecourses_FSC2 <- read.csv("PRR3732_splicing_FSC2_all.txt", sep="\t")
PRR3732_timecourses_FSC2b<- PRR3732_timecourses_FSC2[,c(1,3,5)]
PRR3732_timecourses_FSC2b$ZT[PRR3732_timecourses_FSC2b$ZT == -1.5] <- 22.5
PRR3732_timecourses_FSC2b$ZT[PRR3732_timecourses_FSC2b$ZT == 24.5] <- 0.5
PRR3732_timecourses_FSC2b$ZT <- paste("ZT", PRR3732_timecourses_FSC2b$ZT, sep="")
PRR3732_timecourses_FSC2b$ZT<-factor(PRR3732_timecourses_FSC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3732_timecourses_FSC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR3732_timecourses_FSC2b[grep(x,PRR3732_timecourses_FSC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.32 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3732_timecourses_ASC2 <- read.csv("PRR3732_splicing_ASC2_all.txt", sep="\t")
PRR3732_timecourses_ASC2b<- PRR3732_timecourses_ASC2[,c(1,3,5)]
PRR3732_timecourses_ASC2b$ZT[PRR3732_timecourses_ASC2b$ZT == -1.5] <- 22.5
PRR3732_timecourses_ASC2b$ZT[PRR3732_timecourses_ASC2b$ZT == 24.5] <- 0.5
PRR3732_timecourses_ASC2b$ZT <- paste("ZT", PRR3732_timecourses_ASC2b$ZT, sep="")
PRR3732_timecourses_ASC2b$ZT<-factor(PRR3732_timecourses_ASC2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3732_timecourses_ASC2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR3732_timecourses_ASC2b[grep(x,PRR3732_timecourses_ASC2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.32 ~ Organ, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})