library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(ggpmisc)

theme_set(theme_cowplot())

AS1 <- "#F7882F"
AS2 <- "#4484CE"

LHY_ratio <- read.csv("LHY_ratio_time2.txt", sep="\t")
LHY_ratio$AS = factor(LHY_ratio$AS, levels=c("I1R", "I5R"))

LHY_ratio_time <- ggplot(data=LHY_ratio, aes(x=ZT, y=LHY_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS1, AS2))+
  scale_fill_manual(values=c(AS1, AS2))+
  #scale_shape_manual(values=c(15,16,17,18))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.7,0.9,0.9), name="log(AS/FS)", limits=c(-2.7,0.9))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
LHY_ratio_time
#amplitude 1.88, peak at ZT22-23, through at ZT07

#method to identify the peak and though of the smoothed fit
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.55/80)-2
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.55/80)-2
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.55/80)-2
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.55/80)-2
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


AS4 <- "#FECE00"
AS3 <- "#8FC33A"
AS5 <- "#105989"
#AS5 <- "#FC4A1A"

PRR37_ratio <- read.csv("PRR37_ratio_time.txt", sep="\t")
#PRR37_ratio$AS = factor(LHY_ratio$AS, levels=c("E3S","I3R","I6R","I7R"))
PRR37_ratio$AS = factor(LHY_ratio$AS, levels=c("I6R","I7R","I3R"))

PRR37_ratio_time <- ggplot(data=PRR37_ratio, aes(x=ZT, y=PRR37_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS3, AS4, AS5))+
  scale_fill_manual(values=c(AS3, AS4, AS5))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  #geom_smooth(colour = "black")+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+
  #geom_smooth(aes(col = AS))+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.7,0.9,0.9), name="log(AS/FS)", limits=c(-2.7,0.9))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR37_ratio_time
#amplitude 0.39, peak at ZT07, through at ZT17-18

#method to identify the peak and though of the smoothed fit
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:160, ]
fit4b <- fit2[161:240, ]
fit5 <- cbind(fit3, fit4, fit4b)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.55/80)-2
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.55/80)-2
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.55/80)-2
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.55/80)-2
maxAS10 <- data.frame(which(fit5 == max(fit5$fit4b), arr.ind=TRUE))
maxAS1 <- ((maxAS10$row-1)*26.55/80)-2
minAS10 <- data.frame(which(fit5 == min(fit5$fit4b), arr.ind=TRUE))
minAS1 <- ((minAS10$row-1)*26.55/80)-2
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS, maxAS1, minAS1)
round(FinalMaxMin)

#AS4 <- "#FECE00"
#AS3 <- "#8FC33A"
#AS5 <- "#105989"
AS6 <- "#FC4A1A"

PRR37_ratio_S <- read.csv("PRR37_ratio_time_S.txt", sep="\t")
PRR37_ratio_S$AS = factor(LHY_ratio_S$AS, levels=c("E3S"))

PRR37_ratio_time_S <- ggplot(data=PRR37_ratio_S, aes(x=ZT, y=PRR37_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS6))+
  scale_fill_manual(values=c(AS6))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+
  #geom_smooth(colour="black")+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.7,0.9,0.9), name="log(AS/FS)", limits=c(-2.7,0.9))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR37_ratio_time_S
#amplitude 0.94, peak at ZT0, through at ZT11

fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.55/80)-2
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.55/80)-2
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.55/80)-2
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.55/80)-2
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


AS8 <- "#94618E"
AS7 <- "#BE9E0E"

PRR73_ratio <- read.csv("PRR73_ratio_time.txt", sep="\t")
PRR73_ratio$AS = factor(PRR73_ratio$AS, levels=c("I2R","I6R"))

PRR73_ratio_time <- ggplot(data=PRR73_ratio, aes(x=ZT, y=PRR73_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.7, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS7,AS8))+
  scale_fill_manual(values=c(AS7,AS8))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  #geom_smooth(colour="black")+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+  
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.7,0.9,0.9), name="log(AS/FS)", limits=c(-2.7,0.9))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR73_ratio_time
#amplitude 0.86, peak at ZT06, through at ZT17

fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.55/80)-2
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.55/80)-2
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.55/80)-2
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.55/80)-2
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

AS9 <- "#4ABDAC"
AS10 <- "#EB6E80"
AS11 <- "#1B7B34"

PRR95_ratio <- read.csv("PRR95_ratio_time.txt", sep="\t")
PRR95_ratio$AS = factor(PRR95_ratio$AS, levels=c("Altss", "I3R",   "I7R"))

PRR95_ratio_time <- ggplot(data=PRR95_ratio, aes(x=ZT, y=PRR95_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.4, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.4, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS9,AS10,AS11))+
  scale_fill_manual(values=c(AS9,AS10,AS11))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+  
  #geom_smooth(colour="black")+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.4,0.6,0.6), name="log(AS/FS)", limits=c(-2.4,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR95_ratio_time


AS12 <- "#c41b12"

TOC1_ratio <- read.csv("TOC1_ratio_time.txt", sep="\t")
#TOC1_ratio$AS = factor(TOC1_ratio$AS, levels=c("I2R","I6R"))

TOC1_ratio_time <- ggplot(data=TOC1_ratio, aes(x=ZT, y=TOC1_ratio)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.4, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.4, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  scale_colour_manual(values=c(AS12))+
  scale_fill_manual(values=c(AS12))+
  geom_jitter(aes(col = AS), position=position_jitter(0.2), size = 2) +
  #stat_summary(fun.y=mean, geom="line", size = 1.0)+
  geom_smooth(aes(group = AS, colour = AS, fill=AS, outfit=fit2<<-..y..))+  
  #geom_smooth(colour="black")+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.4,0.6,0.6), name="log(AS/FS)", limits=c(-2.4,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
TOC1_ratio_time

FS_line = "solid"
AS_line = "dashed"
AS2_line = "dotdash"

FS <- "#4d4d4d"
AS1 <- "#105989"
AS2 <- "#FC4A1A"

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
f1_line = "solid"
e1_line = "longdash"
e5_line = "dotdash"

Leaf_splicing_data <- read.csv("leaf_timecourses.txt", sep="\t")
Leaf_splicing <- ggplot(data=Leaf_splicing_data, aes(x=Time, y=Expression)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.4, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.4, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(group=Gene), colour="#969696", size = 0.5) +
  #stat_summary(aes(col=Organ), fun.y=mean, geom="line", size = 1.5)+
  geom_smooth(aes(col=Organ, fill=Organ, outfit=fit2<<-..y..))+
  scale_colour_manual(values=c(f1))+
  scale_fill_manual(values=c(f1))+
  scale_linetype_manual(values=c(f1_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.4,2.4,1.2), name="Expression value", limits=c(-2.4, 2.4))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")
Leaf_splicing 



Internodes_splicing_data <- read.csv("internodes_timecourses.txt", sep="\t")
Internodes_splicing <- ggplot(data=Internodes_splicing_data, aes(x=Time, y=Expression)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -2.4, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -2.4, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(group=Gene, linetype=Organ), colour="#969696", size = 0.5) +
  #stat_summary(aes(col=Organ, linetype=Organ), fun.y=mean, geom="line", size = 1.5)+
  geom_smooth(aes(col=Organ, fill=Organ, outfit=fit2<<-..y..))+
  scale_colour_manual(values=c(e1,e5))+
  scale_fill_manual(values=c(e1,e5))+
  scale_linetype_manual(values=c(e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(-2.4,2.4,1.2), name="Expression value", limits=c(-2.4, 2.4))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")
Internodes_splicing

plot_grid(LHY_ratio_time, PRR37_ratio_time, PRR73_ratio_time, PRR37_ratio_time_S,Leaf_splicing, Internodes_splicing,  labels = c("A", "B", "C", "D", "E", "F"), ncol = 2, align = "none", rel_widths = c(1.15, 1),rel_heights = c(1,1,1.2),label_size = 20)
