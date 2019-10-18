LHY1_timecourses_I1C2 <- read.csv("LHY1_splicing_I1C2_all.txt", sep="\t")
LHY1_timecourses_I1C2b<- LHY1_timecourses_I1C2[,c(1,3,6)]
LHY1_timecourses_I1C2b$ZT[LHY1_timecourses_I1C2b$ZT == -1.5] <- 22.5
LHY1_timecourses_I1C2b$ZT[LHY1_timecourses_I1C2b$ZT == 24.5] <- 0.5
LHY1_timecourses_I1C2b$ZT <- paste("ZT", LHY1_timecourses_I1C2b$ZT, sep="")
LHY1_timecourses_I1C2b$ZT<-factor(LHY1_timecourses_I1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY1_timecourses_I1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {LHY1_timecourses_I1C2b[grep(x,LHY1_timecourses_I1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(LHY.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


LHY1_timecourses_I5C2 <- read.csv("LHY1_splicing_I5C2_all.txt", sep="\t")
LHY1_timecourses_I5C2b<- LHY1_timecourses_I5C2[,c(1,3,6)]
LHY1_timecourses_I5C2b$ZT[LHY1_timecourses_I5C2b$ZT == -1.5] <- 22.5
LHY1_timecourses_I5C2b$ZT[LHY1_timecourses_I5C2b$ZT == 24.5] <- 0.5
LHY1_timecourses_I5C2b$ZT <- paste("ZT", LHY1_timecourses_I5C2b$ZT, sep="")
LHY1_timecourses_I5C2b$ZT<-factor(LHY1_timecourses_I5C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY1_timecourses_I5C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {LHY1_timecourses_I5C2b[grep(x,LHY1_timecourses_I5C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(LHY.1  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


LHY4_timecourses_I1C2 <- read.csv("LHY4_splicing_I1C2_all.txt", sep="\t")
LHY4_timecourses_I1C2b<- LHY4_timecourses_I1C2[,c(1,3,6)]
LHY4_timecourses_I1C2b$ZT[LHY4_timecourses_I1C2b$ZT == -1.5] <- 22.5
LHY4_timecourses_I1C2b$ZT[LHY4_timecourses_I1C2b$ZT == 24.5] <- 0.5
LHY4_timecourses_I1C2b$ZT <- paste("ZT", LHY4_timecourses_I1C2b$ZT, sep="")
LHY4_timecourses_I1C2b$ZT<-factor(LHY4_timecourses_I1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY4_timecourses_I1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {LHY4_timecourses_I1C2b[grep(x,LHY4_timecourses_I1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(LHY.4  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


LHY4_timecourses_I5C2 <- read.csv("LHY4_splicing_I5C2_all.txt", sep="\t")
LHY4_timecourses_I5C2b<- LHY4_timecourses_I5C2[,c(1,3,6)]
LHY4_timecourses_I5C2b$ZT[LHY4_timecourses_I5C2b$ZT == -1.5] <- 22.5
LHY4_timecourses_I5C2b$ZT[LHY4_timecourses_I5C2b$ZT == 24.5] <- 0.5
LHY4_timecourses_I5C2b$ZT <- paste("ZT", LHY4_timecourses_I5C2b$ZT, sep="")
LHY4_timecourses_I5C2b$ZT<-factor(LHY4_timecourses_I5C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(LHY4_timecourses_I5C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {LHY4_timecourses_I5C2b[grep(x,LHY4_timecourses_I5C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(LHY.4  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3731_timecourses_I1C2 <- read.csv("PRR3731_splicing_I1C2_all.txt", sep="\t")
PRR3731_timecourses_I1C2b<- PRR3731_timecourses_I1C2[,c(1,3,6)]
PRR3731_timecourses_I1C2b$ZT[PRR3731_timecourses_I1C2b$ZT == -1.5] <- 22.5
PRR3731_timecourses_I1C2b$ZT[PRR3731_timecourses_I1C2b$ZT == 24.5] <- 0.5
PRR3731_timecourses_I1C2b$ZT <- paste("ZT", PRR3731_timecourses_I1C2b$ZT, sep="")
PRR3731_timecourses_I1C2b$ZT<-factor(PRR3731_timecourses_I1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3731_timecourses_I1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR3731_timecourses_I1C2b[grep(x,PRR3731_timecourses_I1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR37.31  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3731_timecourses_I5C2 <- read.csv("PRR3731_splicing_I5C2_all.txt", sep="\t")
PRR3731_timecourses_I5C2b<- PRR3731_timecourses_I5C2[,c(1,3,6)]
PRR3731_timecourses_I5C2b$ZT[PRR3731_timecourses_I5C2b$ZT == -1.5] <- 22.5
PRR3731_timecourses_I5C2b$ZT[PRR3731_timecourses_I5C2b$ZT == 24.5] <- 0.5
PRR3731_timecourses_I5C2b$ZT <- paste("ZT", PRR3731_timecourses_I5C2b$ZT, sep="")
PRR3731_timecourses_I5C2b$ZT<-factor(PRR3731_timecourses_I5C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3731_timecourses_I5C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR3731_timecourses_I5C2b[grep(x,PRR3731_timecourses_I5C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR37.31  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3732_timecourses_I1C2 <- read.csv("PRR3732_splicing_I1C2_all.txt", sep="\t")
PRR3732_timecourses_I1C2b<- PRR3732_timecourses_I1C2[,c(1,3,6)]
PRR3732_timecourses_I1C2b$ZT[PRR3732_timecourses_I1C2b$ZT == -1.5] <- 22.5
PRR3732_timecourses_I1C2b$ZT[PRR3732_timecourses_I1C2b$ZT == 24.5] <- 0.5
PRR3732_timecourses_I1C2b$ZT <- paste("ZT", PRR3732_timecourses_I1C2b$ZT, sep="")
PRR3732_timecourses_I1C2b$ZT<-factor(PRR3732_timecourses_I1C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3732_timecourses_I1C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR3732_timecourses_I1C2b[grep(x,PRR3732_timecourses_I1C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR37.32  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})


PRR3732_timecourses_I5C2 <- read.csv("PRR3732_splicing_I5C2_all.txt", sep="\t")
PRR3732_timecourses_I5C2b<- PRR3732_timecourses_I5C2[,c(1,3,6)]
PRR3732_timecourses_I5C2b$ZT[PRR3732_timecourses_I5C2b$ZT == -1.5] <- 22.5
PRR3732_timecourses_I5C2b$ZT[PRR3732_timecourses_I5C2b$ZT == 24.5] <- 0.5
PRR3732_timecourses_I5C2b$ZT <- paste("ZT", PRR3732_timecourses_I5C2b$ZT, sep="")
PRR3732_timecourses_I5C2b$ZT<-factor(PRR3732_timecourses_I5C2b$ZT, levels=c("ZT0.5", "ZT2.5", "ZT4.5", "ZT6.5",  "ZT8.5", "ZT10.5", "ZT12.5", "ZT14.5", "ZT16.5", "ZT18.5", "ZT20.5", "ZT22.5"))
Times <- levels(PRR3732_timecourses_I5C2b$ZT)
Times <-as.data.frame(Times)
separado <- apply(Times,1,function(x) {PRR3732_timecourses_I5C2b[grep(x,PRR3732_timecourses_I5C2b$ZT,fixed = TRUE),]})
Times2 <- t(Times)
Times2 <- as.vector(Times2)
names(separado) <- Times2
write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
apply(Times,1,function(x) {
  testet<-separado[[paste(x)]]
  testet_results<-t.test(PRR37.32  ~ Splice, paired = TRUE, data = testet)
  testet_results2 <- tidy(testet_results)
  write.table(testet_results2, "resultados.txt", sep = "\t", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
})
