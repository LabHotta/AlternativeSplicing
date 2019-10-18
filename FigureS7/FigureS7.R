library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)

theme_set(theme_cowplot())

#1100x1350

#FS <- "#874D96"
#AS <- "#3A934C"
FS_line = "solid"
AS_line = "dashed"
AS2_line = "dotdash"

FS <- "#4d4d4d"
AS1 <- "#105989"
AS2 <- "#FC4A1A"


PRR371_timecourses_F1C1 <- read.csv("PRR371_splicing_F1C1_all.txt", sep="\t")
PRR371_timecourses_F1C1$Splice = factor(PRR371_timecourses_F1C1$Splice, levels=c("FS", "AS1", "AS2"))
PRR37.1_F1C1 <- ggplot(data=PRR371_timecourses_F1C1, aes(x=CT, y=PRR37.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.45, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.45, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 8.295, y = 2, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.3215, y = 2, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 5.699, y = 2, label = "\u25bc", size = 6, colour=AS2)+
  annotate("text", x = 0.52, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 0.52, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  annotate("text", x = 24.52, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 24.52, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  annotate("text", x = 4.7, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 4.7, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  scale_fill_manual(values=c(FS, AS1, AS2))+
  scale_colour_manual(values=c(FS, AS1, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line, AS2_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.4,0.8), name="Normalized Expression", limits=c(-0.45,2.4))+
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
PRR37.1_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:160, ]
fit4b <- fit2[161:240, ]
fit5 <- cbind(fit3, fit4, fit4b)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
maxAS10 <- data.frame(which(fit5 == max(fit5$fit4b), arr.ind=TRUE))
maxAS1 <- ((maxAS10$row-1)*25.96/80)-1.44
minAS10 <- data.frame(which(fit5 == min(fit5$fit4b), arr.ind=TRUE))
minAS1 <- ((minAS10$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS, maxAS1, minAS1)
round(FinalMaxMin)

PRR371_timecourses_F1C2 <- read.csv("PRR371_splicing_F1C2_all.txt", sep="\t")
PRR371_timecourses_F1C2$Splice = factor(PRR371_timecourses_F1C2$Splice, levels=c("FS", "AS1", "AS2"))

PRR37.1_F1C2 <- ggplot(data=PRR371_timecourses_F1C2, aes(x=CT, y=PRR37.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.45, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.45, ymax = 2.4, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.1425, y = 2, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.4425, y = 2, label = "\u25bc", size = 6, colour=AS1)+
  annotate("text", x = 6.62875, y = 2, label = "\u25bc", size = 6, colour=AS2)+
  annotate("text", x = 11.09, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 11.09, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  annotate("text", x = 13.12, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 13.12, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  annotate("text", x = 15.35, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 15.35, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  annotate("text", x = 19.81, y = -0.35, label = "\u002a", size = 8, colour=AS1)+
  annotate("text", x = 19.81, y = -0.45, label = "\u002a", size = 8, colour=AS2)+
  scale_fill_manual(values=c(FS, AS1, AS2))+
  scale_colour_manual(values=c(FS, AS1, AS2))+
  scale_linetype_manual(values=c(FS_line, AS_line, AS2_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.4,0.8), name="Normalized Expression", limits=c(-0.45,2.4))+
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
PRR37.1_F1C2

#method to identify the peak and though of the smoothed fit in C2
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


AS3 <- "#BE9E0E"

PRR731_timecourses_F1C1 <- read.csv("PRR731_splicing_F1C1_all.txt", sep="\t")
PRR731_timecourses_F1C1$Splice = factor(PRR731_timecourses_F1C1$Splice, levels=c("FS", "AS"))
PRR73.1_F1C1 <- ggplot(data=PRR731_timecourses_F1C1, aes(x=CT, y=PRR73.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.75, ymax = 2.7, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.75, ymax = 2.7, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 12.189, y = 2.25, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 9.593, y = 2.25, label = "\u25bc", size = 6, colour=AS3)+
  annotate("text", x = 22.56, y = -0.75, label = "\u002a", size = 8, colour=AS3)+
  scale_fill_manual(values=c(FS, AS3))+
  scale_colour_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.7,0.9), name="Normalized Expression", limits=c(-0.75,2.7))+
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
PRR73.1_F1C1

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


PRR731_timecourses_F1C2 <- read.csv("PRR731_splicing_F1C2_all.txt", sep="\t")
PRR731_timecourses_F1C2$Splice = factor(PRR731_timecourses_F1C2$Splice, levels=c("FS", "AS"))
PRR73.1_F1C2 <- ggplot(data=PRR731_timecourses_F1C2, aes(x=CT, y=PRR73.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.75, ymax = 2.7, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.75, ymax = 2.7, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 10.4855, y = 2.25, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 8.522, y = 2.25, label = "\u25bc", size = 6, colour=AS3)+
  scale_fill_manual(values=c(FS, AS3))+
  scale_colour_manual(values=c(FS, AS3))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.7,0.9), name="Normalized Expression", limits=c(-0.75,2.7))+
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
PRR73.1_F1C2

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

AS4 <- "#4ABDAC"
AS5 <- "#EB6E80"
AS6 <- "#1B7B34"

PRR952_timecourses_F1C1 <- read.csv("PRR952_splicing_F1C1_all.txt", sep="\t")
PRR952_timecourses_F1C1$Splice = factor(PRR952_timecourses_F1C1$Splice, levels=c("FS", "AS1", "AS2"))
PRR95.2_F1C1 <- ggplot(data=PRR952_timecourses_F1C1, aes(x=CT, y=PRR95.2, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.6, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.6, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.9705, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 5.05, y = 1.75, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = 5.3745, y = 1.75, label = "\u25bc", size = 6, colour=AS5)+
  annotate("text", x = 0.52, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 0.52, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 24.52, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 24.52, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 6.78, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 6.78, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 8.87, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 8.87, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 12.96, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 16.8, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 16.8, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  scale_fill_manual(values=c(FS, AS4, AS5))+
  scale_colour_manual(values=c(FS, AS4, AS5))+
  scale_linetype_manual(values=c(FS_line, AS_line, AS2_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.1,0.7), name="Normalized Expression", limits=c(-0.6, 2.1))+
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
PRR95.2_F1C1

#method to identify the peak and though of the smoothed fit in C1
fit2 <- data.frame(fit2)
fit3 <- fit2[1:60, ]
fit4 <- fit2[81:160, ]
fit4b <- fit2[161:240, ]
fit5 <- cbind(fit3, fit4, fit4b)
fit5 <- data.frame(fit5)
maxFS0 <- data.frame(which(fit5 == max(fit5$fit3), arr.ind=TRUE))
maxFS <- ((maxFS0$row-1)*25.96/80)-1.44
maxAS0 <- data.frame(which(fit5 == max(fit5$fit4), arr.ind=TRUE))
maxAS <- ((maxAS0$row-1)*25.96/80)-1.44
minFS0 <- data.frame(which(fit5 == min(fit5$fit3), arr.ind=TRUE))
minFS <- ((minFS0$row-1)*25.96/80)-1.44
minAS0 <- data.frame(which(fit5 == min(fit5$fit4), arr.ind=TRUE))
minAS <- ((minAS0$row-1)*25.96/80)-1.44
maxAS10 <- data.frame(which(fit5 == max(fit5$fit4b), arr.ind=TRUE))
maxAS1 <- ((maxAS10$row-1)*25.96/80)-1.44
minAS10 <- data.frame(which(fit5 == min(fit5$fit4b), arr.ind=TRUE))
minAS1 <- ((minAS10$row-1)*25.96/80)-1.44
FinalMaxMin <- cbind(maxFS, minFS, maxAS, minAS, maxAS1, minAS1)
round(FinalMaxMin)


PRR952_timecourses_F1C2 <- read.csv("PRR952_splicing_F1C2_all.txt", sep="\t")
PRR952_timecourses_F1C2$Splice = factor(PRR952_timecourses_F1C2$Splice, levels=c("FS", "AS1", "AS2", "AS3"))
PRR95.2_F1C2 <- ggplot(data=PRR952_timecourses_F1C2, aes(x=CT, y=PRR95.2, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.6, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin =  -0.6, ymax = 2.1, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.524375, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.724375, y = 1.75, label = "\u25bc", size = 6, colour=AS4)+
  annotate("text", x = 14.92563, y = 1.75, label = "\u25bc", size = 6, colour=AS5)+
  annotate("text", x = 0.23, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 0.23, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 3.85, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 3.85, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 5.66, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 5.66, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 7.47, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 7.47, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 9.28, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 9.28, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 11.09, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 11.09, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
  annotate("text", x = 24.23, y = -0.5, label = "\u002a", size = 8, colour=AS4)+
  annotate("text", x = 24.23, y = -0.6, label = "\u002a", size = 8, colour=AS5)+
    scale_fill_manual(values=c(FS, AS4, AS5))+
  scale_colour_manual(values=c(FS, AS4, AS5))+
  scale_linetype_manual(values=c(FS_line, AS_line, AS2_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,2.1,0.7), name="Normalized Expression", limits=c(-0.6,2.1))+
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
PRR95.2_F1C2

#method to identify the peak and though of the smoothed fit in C2
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


PRR954_timecourses_F1C1 <- read.csv("PRR954_splicing_F1C1_all.txt", sep="\t")
PRR954_timecourses_F1C1$Splice = factor(PRR954_timecourses_F1C1$Splice, levels=c("FS", "AS"))
PRR95.4_F1C1 <- ggplot(data=PRR954_timecourses_F1C1, aes(x=CT, y=PRR95.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.3, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.3, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 6.997, y = 1.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 5.05, y = 1.5, label = "\u25bc", size = 6, colour=AS6)+
  annotate("text", x = 20.64, y = -0.3, label = "\u002a", size = 8, colour=AS6)+
    scale_fill_manual(values=c(FS, AS6))+
  scale_colour_manual(values=c(FS, AS6))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.8,0.6), name="Normalized Expression", limits=c(-0.3,1.8))+
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
PRR95.4_F1C1

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

PRR954_timecourses_F1C2 <- read.csv("PRR954_splicing_F1C2_all.txt", sep="\t")
PRR954_timecourses_F1C2$Splice = factor(PRR954_timecourses_F1C2$Splice, levels=c("FS", "AS"))
PRR95.4_F1C2 <- ggplot(data=PRR954_timecourses_F1C2, aes(x=CT, y=PRR95.4, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.3, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.3, ymax = 1.8, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 7.113, y = 1.5, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 7.313, y = 1.5, label = "\u25bc", size = 6, colour=AS6)+
    scale_fill_manual(values=c(FS, AS6))+
  scale_colour_manual(values=c(FS, AS6))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.8,0.6), name="Normalized Expression", limits=c(-0.3,1.8))+
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
PRR95.4_F1C2

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



AS7 <- "#70D6FF"

TOC11_timecourses_F1C1 <- read.csv("TOC11_splicing_F1C1_all.txt", sep="\t")
TOC11_timecourses_F1C1$Splice = factor(TOC11_timecourses_F1C1$Splice, levels=c("FS", "AS"))
TOC1.1_F1C1 <- ggplot(data=TOC11_timecourses_F1C1, aes(x=CT, y=TOC1.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.15, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.15, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 10.5665, y = 1, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 10.891, y = 1, label = "\u25bc", size = 6, colour=AS7)+
  annotate("text", x = 0.52, y = -0.1, label = "\u002a", size = 8, colour=AS7)+
  annotate("text", x = 4.7, y = -0.15, label = "\u002a", size = 8, colour=AS7)+
  annotate("text", x = 12.96, y = -0.15, label = "\u002a", size = 8, colour=AS7)+
  annotate("text", x = 24.52, y = -0.15, label = "\u002a", size = 8, colour=AS7)+
    scale_fill_manual(values=c(FS, AS7))+
  scale_colour_manual(values=c(FS, AS7))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.15,1.2))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 1), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position = "none"
  )
TOC1.1_F1C1

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

TOC11_timecourses_F1C2 <- read.csv("TOC11_splicing_F1C2_all.txt", sep="\t")
TOC11_timecourses_F1C2$Splice = factor(TOC11_timecourses_F1C2$Splice, levels=c("FS", "AS"))
TOC1.1_F1C2 <- ggplot(data=TOC11_timecourses_F1C2, aes(x=CT, y=TOC1.1, group=Splice)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.1, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.1, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Splice, shape = Splice),position=position_jitter(0.1), size = 2) +
  stat_smooth(aes(group = Splice, colour = Splice, fill=Splice, outfit=fit2<<-..y..), span = 0.8)+
  annotate("text", x = 13.98525, y = 1, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = 14.18525, y = 1, label = "\u25bc", size = 6, colour=AS7)+
  annotate("text", x = -1.95, y = -0.15, label = "\u002a", size = 8, colour=AS7)+
  annotate("text", x = 22.05, y = -0.15, label = "\u002a", size = 8, colour=AS7)+
    scale_fill_manual(values=c(FS, AS7))+
  scale_colour_manual(values=c(FS, AS7))+
  scale_linetype_manual(values=c(FS_line, AS_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.15,1.2))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 1), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_text(size=18),
        legend.position = "none"
  )
TOC1.1_F1C2

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

plot_grid(PRR37.1_F1C1, PRR37.1_F1C2, PRR73.1_F1C1, PRR73.1_F1C2, PRR95.2_F1C1, PRR95.2_F1C2, PRR95.4_F1C1, PRR95.4_F1C2, TOC1.1_F1C1, TOC1.1_F1C2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), ncol = 2, align = "none", rel_widths = c(1.15, 1),rel_heights = c(1,1,1,1,1.2),label_size = 20)
