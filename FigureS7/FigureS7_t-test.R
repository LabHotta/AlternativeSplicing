library(dplyr)
library(tidyr)
library(broom)

PRR371_timecourses_F1C1 <- read.csv("PRR371_splicing_F1C1_all.txt", sep="\t")
PRR371_timecourses_F1C1b<- PRR371_timecourses_F1C1[,c(1,3,6)]
PRR371_timecourses_F1C1b$ZT[PRR371_timecourses_F1C1b$ZT == -1.5] <- 22.5
PRR371_timecourses_F1C1b$ZT[PRR371_timecourses_F1C1b$ZT == 24.5] <- 0.5
PRR371_timecourses_F1C1b$ZT <- paste("ZT", PRR371_timecourses_F1C1b$ZT, sep="")
PRR371_timecourses_F1C1b$ZT<-factor(PRR371_timecourses_F1C1b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR371_timecourses_F1C1b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR371_timecourses_F1C1b[grep(x,PRR371_timecourses_F1C1b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.1 ~ Splice, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
     })


PRR371_timecourses_F1C2 <- read.csv("PRR371_splicing_F1C2_all.txt", sep="\t")
PRR371_timecourses_F1C2b<- PRR371_timecourses_F1C2[,c(1,3,6)]
PRR371_timecourses_F1C2b$ZT[PRR371_timecourses_F1C2b$ZT == -1.5] <- 22.5
PRR371_timecourses_F1C2b$ZT[PRR371_timecourses_F1C2b$ZT == 24.5] <- 0.5
PRR371_timecourses_F1C2b$ZT <- paste("ZT", PRR371_timecourses_F1C2b$ZT, sep="")
PRR371_timecourses_F1C2b$ZT<-factor(PRR371_timecourses_F1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR371_timecourses_F1C2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR371_timecourses_F1C2b[grep(x,PRR371_timecourses_F1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR37.1 ~ Splice, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})

PRR731_timecourses_F1C1 <- read.csv("PRR731_splicing_F1C1_all.txt", sep="\t")
PRR731_timecourses_F1C1b<- PRR731_timecourses_F1C1[,c(1,3,6)]
PRR731_timecourses_F1C1b$ZT[PRR731_timecourses_F1C1b$ZT == -1.5] <- 22.5
PRR731_timecourses_F1C1b$ZT[PRR731_timecourses_F1C1b$ZT == 24.5] <- 0.5
PRR731_timecourses_F1C1b$ZT <- paste("ZT", PRR731_timecourses_F1C1b$ZT, sep="")
PRR731_timecourses_F1C1b$ZT<-factor(PRR731_timecourses_F1C1b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR731_timecourses_F1C1b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR731_timecourses_F1C1b[grep(x,PRR731_timecourses_F1C1b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR73.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})

PRR731_timecourses_F1C2 <- read.csv("PRR731_splicing_F1C2_all.txt", sep="\t")
PRR731_timecourses_F1C2b<- PRR731_timecourses_F1C2[,c(1,3,6)]
PRR731_timecourses_F1C2b$ZT[PRR731_timecourses_F1C2b$ZT == -1.5] <- 22.5
PRR731_timecourses_F1C2b$ZT[PRR731_timecourses_F1C2b$ZT == 24.5] <- 0.5
PRR731_timecourses_F1C2b$ZT <- paste("ZT", PRR731_timecourses_F1C2b$ZT, sep="")
PRR731_timecourses_F1C2b$ZT<-factor(PRR731_timecourses_F1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR731_timecourses_F1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR731_timecourses_F1C2b[grep(x,PRR731_timecourses_F1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR73.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})



PRR952_timecourses_F1C1 <- read.csv("PRR952_splicing_F1C1_all.txt", sep="\t")
PRR952_timecourses_F1C1b<- PRR952_timecourses_F1C1[,c(1,3,6)]
PRR952_timecourses_F1C1b$ZT[PRR952_timecourses_F1C1b$ZT == -1.5] <- 22.5
PRR952_timecourses_F1C1b$ZT[PRR952_timecourses_F1C1b$ZT == 24.5] <- 0.5
PRR952_timecourses_F1C1b$ZT <- paste("ZT", PRR952_timecourses_F1C1b$ZT, sep="")
PRR952_timecourses_F1C1b$ZT<-factor(PRR952_timecourses_F1C1b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR952_timecourses_F1C1b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR952_timecourses_F1C1b[grep(x,PRR952_timecourses_F1C1b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR95.2 ~ Splice, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR952_timecourses_F1C2 <- read.csv("PRR952_splicing_F1C2_all.txt", sep="\t")
PRR952_timecourses_F1C2b<- PRR952_timecourses_F1C2[,c(1,3,6)]
PRR952_timecourses_F1C2b$ZT[PRR952_timecourses_F1C2b$ZT == -1.5] <- 22.5
PRR952_timecourses_F1C2b$ZT[PRR952_timecourses_F1C2b$ZT == 24.5] <- 0.5
PRR952_timecourses_F1C2b$ZT <- paste("ZT", PRR952_timecourses_F1C2b$ZT, sep="")
PRR952_timecourses_F1C2b$ZT<-factor(PRR952_timecourses_F1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR952_timecourses_F1C2b$ZT)
Times <-as.data.frame(Times)

separado <- apply(Times,1,function(x) {PRR952_timecourses_F1C2b[grep(x,PRR952_timecourses_F1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
apply(Times,1,function(x) {
  anova<-separado[[paste(x)]]
  anova_results<-aov(PRR95.2 ~ Splice, data = anova)
  summary(anova_results)
  #anova_results <- tidy(anova_results)
  #write.table(anova_results, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  tukey_results<-TukeyHSD(anova_results)
  TK<-(tukey_results)
  TK_data<-as.data.frame(TK[1:1])
  #tukey_results <- tidy(tukey_results)
  write.table(TK_data, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR954_timecourses_F1C1 <- read.csv("PRR954_splicing_F1C1_all.txt", sep="\t")
PRR954_timecourses_F1C1b<- PRR954_timecourses_F1C1[,c(1,3,6)]
PRR954_timecourses_F1C1b$ZT[PRR954_timecourses_F1C1b$ZT == -1.5] <- 22.5
PRR954_timecourses_F1C1b$ZT[PRR954_timecourses_F1C1b$ZT == 24.5] <- 0.5
PRR954_timecourses_F1C1b$ZT <- paste("ZT", PRR954_timecourses_F1C1b$ZT, sep="")
PRR954_timecourses_F1C1b$ZT<-factor(PRR954_timecourses_F1C1b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR954_timecourses_F1C1b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR954_timecourses_F1C1b[grep(x,PRR954_timecourses_F1C1b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR95.4  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR954_timecourses_F1C2 <- read.csv("PRR954_splicing_F1C2_all.txt", sep="\t")
PRR954_timecourses_F1C2b<- PRR954_timecourses_F1C2[,c(1,3,6)]
PRR954_timecourses_F1C2b$ZT[PRR954_timecourses_F1C2b$ZT == -1.5] <- 22.5
PRR954_timecourses_F1C2b$ZT[PRR954_timecourses_F1C2b$ZT == 24.5] <- 0.5
PRR954_timecourses_F1C2b$ZT <- paste("ZT", PRR954_timecourses_F1C2b$ZT, sep="")
PRR954_timecourses_F1C2b$ZT<-factor(PRR954_timecourses_F1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR954_timecourses_F1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR954_timecourses_F1C2b[grep(x,PRR954_timecourses_F1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR95.4  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


TOC11_timecourses_F1C1 <- read.csv("TOC11_splicing_F1C1_all.txt", sep="\t")
TOC11_timecourses_F1C1b<- TOC11_timecourses_F1C1[,c(1,3,6)]
TOC11_timecourses_F1C1b$ZT[TOC11_timecourses_F1C1b$ZT == -1.5] <- 22.5
TOC11_timecourses_F1C1b$ZT[TOC11_timecourses_F1C1b$ZT == 24.5] <- 0.5
TOC11_timecourses_F1C1b$ZT <- paste("ZT", TOC11_timecourses_F1C1b$ZT, sep="")
TOC11_timecourses_F1C1b$ZT<-factor(TOC11_timecourses_F1C1b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(TOC11_timecourses_F1C1b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {TOC11_timecourses_F1C1b[grep(x,TOC11_timecourses_F1C1b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(TOC1.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


TOC11_timecourses_F1C2 <- read.csv("TOC11_splicing_F1C2_all.txt", sep="\t")
TOC11_timecourses_F1C2b<- TOC11_timecourses_F1C2[,c(1,3,6)]
TOC11_timecourses_F1C2b$ZT[TOC11_timecourses_F1C2b$ZT == -1.5] <- 22.5
TOC11_timecourses_F1C2b$ZT[TOC11_timecourses_F1C2b$ZT == 24.5] <- 0.5
TOC11_timecourses_F1C2b$ZT <- paste("ZT", TOC11_timecourses_F1C2b$ZT, sep="")
TOC11_timecourses_F1C2b$ZT<-factor(TOC11_timecourses_F1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(TOC11_timecourses_F1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {TOC11_timecourses_F1C2b[grep(x,TOC11_timecourses_F1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(TOC1.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})