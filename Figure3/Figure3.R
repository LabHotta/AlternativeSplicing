library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)

theme_set(theme_cowplot())

#1100 x 1100

#FS <- "#874D96"
#AS <- "#3A934C"
FS_line = "solid"
AS_line = "dashed"

FS <- "#4d4d4d"
AS1 <- "#F7882F"
AS2 <- "#4484CE"

LHY1_timecourses_I1C2 <- read.csv("LHY1_splicing_I1C2_all.txt", sep="\t")
LHY1_timecourses_I1C2$Splice = factor(LHY1_timecourses_I1C2$Splice, levels=c("FS", "AS"))
LHY.1_I1C2 <- ggplot(data=LHY1_timecourses_I1C2, aes(x=CT, y=LHY.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.075, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.075, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 3.29, y = 0.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.95, y = 0.5, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 5.66, y = -0.075, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 15.35, y = -0.075, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 17.58, y = -0.075, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 19.81, y = -0.075, label = "\u002a", size = 8, colour=AS1)+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  #stat_summary(aes(group=Splice, col=Splice, linetype=Splice), fun.y=mean, geom="line", size = 1.0)+
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_colour_manual(values=c(FS, AS1))+
  scale_fill_manual(values=c(FS, AS1))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.6,0.2), name="Normalized Expression", limits=c(-0.075,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
LHY.1_I1C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

LHY1_timecourses_I5C2 <- read.csv("LHY1_splicing_I5C2_all.txt", sep="\t")
LHY1_timecourses_I5C2$Splice = factor(LHY1_timecourses_I5C2$Splice, levels=c("FS", "AS"))
LHY.1_I5C2 <- ggplot(data=LHY1_timecourses_I5C2, aes(x=CT, y=LHY.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.075, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.075, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 3.94, y = 0.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.62, y = 0.5, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 7.47, y = -0.075, label = "\u002a", size = 8, colour=AS1)+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS1))+
  scale_colour_manual(values=c(FS, AS1))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.6,0.2), name="Normalized Expression", limits=c(-0.075,0.6))+
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
LHY.1_I5C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


LHY4_timecourses_I1C2 <- read.csv("LHY4_splicing_I1C2_all.txt", sep="\t")
LHY4_timecourses_I1C2$Splice = factor(LHY4_timecourses_I1C2$Splice, levels=c("FS", "AS"))
LHY.4_I1C2 <- ggplot(data=LHY4_timecourses_I1C2, aes(x=CT, y=LHY.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.1, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.1, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 3.94, y = 0.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.95, y = 0.75, label = "\u25bc", size = 6, colour=AS2)+
  #geom_errorbar(aes(ymin=LHY.4-LHY.4_SD, ymax=LHY.4+LHY.4_SD, colour = Splice), width=.5,  size = 1.0, position=position_dodge(.3))+ 
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS2))+
  scale_colour_manual(values=c(FS, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.1,0.9))+
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
LHY.4_I1C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


LHY4_timecourses_I5C2 <- read.csv("LHY4_splicing_I5C2_all.txt", sep="\t")
LHY4_timecourses_I5C2$Splice = factor(LHY4_timecourses_I5C2$Splice, levels=c("FS", "AS"))
LHY.4_I5C2 <- ggplot(data=LHY4_timecourses_I5C2, aes(x=CT, y=LHY.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.1, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.1, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 2.63, y = 0.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.95, y = 0.75, label = "\u25bc", size = 6, colour=AS2)+
  #geom_errorbar(aes(ymin=LHY.4-LHY.4_SD, ymax=LHY.4+LHY.4_SD, colour = Splice), width=.5,  size = 1.0, position=position_dodge(.3))+ 
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS2))+
  #annotate("text", x = 12, y = 1.2, label = "LHY4_C1F1P1", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(FS, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.1,0.9))+
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
LHY.4_I5C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

AS4 <- "#FECE00"
AS3 <- "#8FC33A"

PRR3731_timecourses_I1C2 <- read.csv("PRR3731_splicing_I1C2_all.txt", sep="\t")
PRR3731_timecourses_I1C2$Splice = factor(PRR3731_timecourses_I1C2$Splice, levels=c("FS", "AS"))
PRR37.31_I1C2 <- ggplot(data=PRR3731_timecourses_I1C2, aes(x=CT, y=PRR37.31, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.03, ymax = 0.3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.03, ymax = 0.3, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 2.63, y = 0.25, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 6.23, y = 0.25, label = "\u25bc", size = 6, colour=AS3)+
  annotate("text", x = -1.95, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 2.04, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 3.85, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 22.05, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  scale_fill_manual(values=c(FS, AS3))+
  scale_colour_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.3,0.1), name="Normalized Expression", limits=c(-0.03,0.3))+
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
PRR37.31_I1C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR3731_timecourses_I5C2 <- read.csv("PRR3731_splicing_I5C2_all.txt", sep="\t")
PRR3731_timecourses_I5C2$Splice = factor(PRR3731_timecourses_I5C2$Splice, levels=c("FS", "AS"))
PRR37.31_I5C2 <- ggplot(data=PRR3731_timecourses_I5C2, aes(x=CT, y=PRR37.31, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.03, ymax = 0.3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.03, ymax = 0.3, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 8.52, y = 0.25, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 8.19, y = 0.25, label = "\u25bc", size = 6, colour=AS3)+
  annotate("text", x = 2.04, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 3.85, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 5.66, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 9.29, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 11.09, y = -0.03, label = "\u002a", size = 8, colour=AS3)+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS3))+
  scale_colour_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.3,0.1), name="Normalized Expression", limits=c(-0.03,0.3))+
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
PRR37.31_I5C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR3732_timecourses_I1C2 <- read.csv("PRR3732_splicing_I1C2_all.txt", sep="\t")
PRR3732_timecourses_I1C2$Splice = factor(PRR3732_timecourses_I1C2$Splice, levels=c("FS", "AS"))
PRR37.32_I1C2 <- ggplot(data=PRR3732_timecourses_I1C2, aes(x=CT, y=PRR37.32, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.1, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.1, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS4))+
  scale_colour_manual(values=c(FS, AS4))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  annotate("text", x = 1.32, y = 0.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 5.58, y = 0.5, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = 0.23, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 13.12, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 24.23, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.6,0.2), name="Normalized Expression", limits=c(-0.1,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
PRR37.32_I1C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR3732_timecourses_I5C2 <- read.csv("PRR3732_splicing_I5C2_all.txt", sep="\t")
PRR3732_timecourses_I5C2$Splice = factor(PRR3732_timecourses_I5C2$Splice, levels=c("FS", "AS"))
PRR37.32_I5C2 <- ggplot(data=PRR3732_timecourses_I5C2, aes(x=CT, y=PRR37.32, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.1, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.1, ymax = 0.6, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 11.14, y = 0.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 8.52, y = 0.5, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = -1.95, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 0.23, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 2.04, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 7.47, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 22.05, y = -0.1, label = "\u002a", size = 8, colour=AS4)+
    geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_fill_manual(values=c(FS, AS4))+
  scale_colour_manual(values=c(FS, AS4))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.6,0.2), name="Normalized Expression", limits=c(-0.1,0.6))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR37.32_I5C2

#method to identify the peak and though of the smoothed fit in C2
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:140, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*26.18/80)-1.95
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*26.18/80)-1.95
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*26.18/80)-1.95
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*26.18/80)-1.95
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


plot_grid(LHY.1_I1C2, LHY.1_I5C2, LHY.4_I1C2, LHY.4_I5C2, PRR37.31_I1C2, PRR37.31_I5C2, PRR37.32_I1C2, PRR37.32_I5C2, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 2, align = "none", rel_widths = c(1.16, 1),rel_heights = c(1,1,1,1.2),label_size = 20)
