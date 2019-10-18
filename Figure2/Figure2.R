library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)

theme_set(theme_cowplot())

#export 1100 x 1350

#FS <- "#874D96"
#AS <- "#3A934C"
FS_line = "solid"
AS_line = "dashed"

FS <- "#4d4d4d"
AS1 <- "#F7882F"
AS2 <- "#4484CE"

LHY1_timecourses_F1C1 <- read.csv("LHY1_splicing_F1C1_all.txt", sep="\t")
LHY1_timecourses_F1C1$Splice = factor(LHY1_timecourses_F1C1$Splice, levels=c("FS", "AS"))
LHY.1_F1C1 <- ggplot(data=LHY1_timecourses_F1C1, aes(x=CT, y=LHY.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.21, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.21, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_colour_manual(values=c(FS, AS1))+
  scale_fill_manual(values=c(FS, AS1))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  annotate("text", x = 1.15, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.44, y = 1.75, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 2.04, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 4.7, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 8.87, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 14.88, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 20.6, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
    #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
    scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.61,0.7), name="Normalized Expression", limits=c(-0.21,2.1))+
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
LHY.1_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

LHY1_timecourses_F1C2 <- read.csv("LHY1_splicing_F1C2_all.txt", sep="\t")
LHY1_timecourses_F1C2$Splice = factor(LHY1_timecourses_F1C2$Splice, levels=c("FS", "AS"))
LHY.1_F1C2 <- ggplot(data=LHY1_timecourses_F1C2, aes(x=CT, y=LHY.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.21, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.21, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("text", x = 3.94, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.95, y = 1.75, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 2.04, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 13.12, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 15.35, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 17.58, y = -0.21, label = "\u002a", size = 8, colour=AS1)+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  #stat_summary(aes(group=Splice, col=Splice, linetype=Splice), fun.y=mean, geom="line", size = 1.0)+
  geom_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  scale_colour_manual(values=c(FS, AS1))+
  scale_fill_manual(values=c(FS, AS1))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.1,0.7), name="Normalized Expression", limits=c(-0.21,2.1))+
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
LHY.1_F1C2

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

LHY4_timecourses_F1C1 <- read.csv("LHY4_splicing_F1C1_all.txt", sep="\t")
LHY4_timecourses_F1C1$Splice = factor(LHY4_timecourses_F1C1$Splice, levels=c("FS", "AS"))
LHY.4_F1C1 <- ggplot(data=LHY4_timecourses_F1C1, aes(x=CT, y=LHY.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  #stat_summary(aes(group=Splice, col=Splice, linetype=Splice), fun.y=mean, geom="line", size = 1.0)+
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 4.7, y = 1, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 0.2, y = 1, label = "\u25bc", size = 6, colour=AS2)+
  scale_colour_manual(values=c(FS, AS2))+
  scale_fill_manual(values=c(FS, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.25,1.2))+
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
LHY.4_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)


LHY4_timecourses_F1C2 <- read.csv("LHY4_splicing_F1C2_all.txt", sep="\t")
LHY4_timecourses_F1C2$Splice = factor(LHY4_timecourses_F1C2$Splice, levels=c("FS", "AS"))
LHY.4_F1C2 <- ggplot(data=LHY4_timecourses_F1C2, aes(x=CT, y=LHY.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  #geom_errorbar(aes(ymin=LHY.4-LHY.4_SD, ymax=LHY.4+LHY.4_SD, colour = Splice), width=.5,  size = 1.0, position=position_dodge(.3))+ 
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 5.25, y = 1, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -0.31, y = 1, label = "\u25bc", size = 6, colour=AS2)+
  scale_colour_manual(values=c(FS, AS2))+
  scale_fill_manual(values=c(FS, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.25,1.2))+
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
LHY.4_F1C2

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

PRR3731_timecourses_F1C1 <- read.csv("PRR3731_splicing_F1C1_all.txt", sep="\t")
PRR3731_timecourses_F1C1$Splice = factor(PRR3731_timecourses_F1C1$Splice, levels=c("FS", "AS"))
PRR37.31_F1C1 <- ggplot(data=PRR3731_timecourses_F1C1, aes(x=CT, y=PRR37.31, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.24, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.24, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.97, y = 1.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 5.37, y = 1.5, label = "\u25bc", size = 6, colour=AS3)+
  annotate("text", x = 0.52, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 10.96, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 12.96, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 24.52, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
    scale_colour_manual(values=c(FS, AS3))+
  scale_fill_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.8,0.6), name="Normalized Expression", limits=c(-0.24,1.8))+
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
PRR37.31_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR3731_timecourses_F1C2 <- read.csv("PRR3731_splicing_F1C2_all.txt", sep="\t")
PRR3731_timecourses_F1C2$Splice = factor(PRR3731_timecourses_F1C2$Splice, levels=c("FS", "AS"))
PRR37.31_F1C2 <- ggplot(data=PRR3731_timecourses_F1C2, aes(x=CT, y=PRR37.31, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.24, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.24, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.54, y = 1.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.21, y = 1.5, label = "\u25bc", size = 6, colour=AS3)+
  annotate("text", x = 0.23, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 5.66, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 7.47, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 11.09, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 22.05, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = -1.95, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  annotate("text", x = 24.23, y = -0.24, label = "\u002a", size = 8, colour=AS3)+
  scale_colour_manual(values=c(FS, AS3))+
  scale_fill_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.8,0.6), name="Normalized Expression", limits=c(-0.24,1.8))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_text(size=18),
        legend.position = "none"
  )
PRR37.31_F1C2

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



PRR3732_timecourses_F1C1 <- read.csv("PRR3732_splicing_F1C1_all.txt", sep="\t")
PRR3732_timecourses_F1C1$Splice = factor(PRR3732_timecourses_F1C1$Splice, levels=c("FS", "AS"))
PRR37.32_F1C1 <- ggplot(data=PRR3732_timecourses_F1C1, aes(x=CT, y=PRR37.32, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.235, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.235, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 10.57, y = 0.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7, y = 0.75, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = 4.7, y = -0.235, label = "\u002a", size = 8, colour=AS4)+
  scale_colour_manual(values=c(FS, AS4))+
  scale_fill_manual(values=c(FS, AS4))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.235,0.9))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
PRR37.32_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR3732_timecourses_F1C2 <- read.csv("PRR3732_splicing_F1C2_all.txt", sep="\t")
PRR3732_timecourses_F1C2$Splice = factor(PRR3732_timecourses_F1C2$Splice, levels=c("FS", "AS"))
PRR37.32_F1C2 <- ggplot(data=PRR3732_timecourses_F1C2, aes(x=CT, y=PRR37.32, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.235, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.235, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 9.18, y = 0.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 6.89, y = 0.75, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = 0.23, y = -0.235, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 3.85, y = -0.235, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 19.81, y = -0.235, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 22.05, y = -0.235, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = -1.95, y = -0.24, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 24.23, y = -0.24, label = "\u002a", size = 8, colour=AS4)+
  scale_colour_manual(values=c(FS, AS4))+
  scale_fill_manual(values=c(FS, AS4))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.235,0.9))+
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
PRR37.32_F1C2

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

AS5 <- "#94618E"

PRR733_timecourses_F1C1 <- read.csv("PRR733_splicing_F1C1_all.txt", sep="\t")
PRR733_timecourses_F1C1$Splice = factor(PRR733_timecourses_F1C1$Splice, levels=c("FS", "AS"))
PRR73.3_F1C1 <- ggplot(data=PRR733_timecourses_F1C1, aes(x=CT, y=PRR73.3, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.36, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.36, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 9.59, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.32, y = 1.75, label = "\u25bc", size = 6, colour=AS5)+
  annotate("text", x = 0.52, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 12.96, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 24.52, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  scale_colour_manual(values=c(FS, AS5))+
  scale_fill_manual(values=c(FS, AS5))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.1,0.7), name="Normalized Expression", limits=c(-0.36,2.1))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
PRR73.3_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:80, ]
fit4 <- fit2[81:160, ]
fit5 <- cbind(fit3, fit4)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS)
round(FinalMaxMin)

PRR733_timecourses_F1C2 <- read.csv("PRR733_splicing_F1C2_all.txt", sep="\t")
PRR733_timecourses_F1C2$Splice = factor(PRR733_timecourses_F1C2$Splice, levels=c("FS", "AS"))
PRR73.3_F1C2 <- ggplot(data=PRR733_timecourses_F1C2, aes(x=CT, y=PRR73.3, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.36, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.36, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 12.45, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 9.83, y = 1.75, label = "\u25bc", size = 6, colour=AS5)+
  annotate("text", x = 0.23, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 5.66, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 15.35, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 17.58, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 22.05, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = -1.95, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 24.23, y = -0.36, label = "\u002a", size = 8, colour=AS5)+
  scale_colour_manual(values=c(FS, AS5))+
  scale_fill_manual(values=c(FS, AS5))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.1,0.7), name="Normalized Expression", limits=c(-0.36,2.1))+
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
PRR73.3_F1C2

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

plot_grid(LHY.1_F1C1, LHY.1_F1C2, LHY.4_F1C1, LHY.4_F1C2, PRR37.31_F1C1, PRR37.31_F1C2, PRR37.32_F1C1, PRR37.32_F1C2, PRR73.3_F1C1, PRR73.3_F1C2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), ncol = 2, align = "none", rel_widths = c(1.17, 1),rel_heights = c(1,1,1,1,1.2),label_size = 20)
