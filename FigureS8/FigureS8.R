
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)

theme_set(theme_cowplot())

#exportar 1100 x 1350

#FS <- "#874D96"
#AS <- "#3A934C"
FS_line = "solid"
AS_line = "dashed"

FS <- "#4d4d4d"
AS1 <- "#F7882F"
AS2 <- "#4484CE"

L1 = "#00441b"
I1 = "#a20021"
I5 = "#fabc3c"
L1_line = "solid"
I1_line = "longdash"
I5_line = "dotdash"


LHY1_timecourses_FSC2 <- read.csv("LHY1_splicing_FSC2_all.txt", sep="\t")
LHY1_timecourses_FSC2$Organ = factor(LHY1_timecourses_FSC2$Organ, levels=c("L1", "I1", "I5"))
LHY.1_FSC2 <- ggplot(data=LHY1_timecourses_FSC2, aes(x=CT, y=LHY.1, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = 0.22, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 11.09, y = -0.20, label = "\u2020", size = 5, colour=FS)+
  annotate("text", x = 15.35, y = -0.20, label = "\u2020", size = 5, colour=FS)+
  annotate("text", x = 15.35, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 24.22, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 17.58, y = -0.20, label = "\u2020", size = 5, colour=FS)+
  annotate("text", x = 17.58, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 17.58, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.25,0.9))+
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
LHY.1_FSC2


LHY1_timecourses_ASC2 <- read.csv("LHY1_splicing_ASC2_all.txt", sep="\t")
LHY1_timecourses_ASC2$Organ = factor(LHY1_timecourses_ASC2$Organ, levels=c("L1", "I1", "I5"))
LHY.1_ASC2 <- ggplot(data=LHY1_timecourses_ASC2, aes(x=CT, y=LHY.1, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = -1.95, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = -1.95, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 0.23, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 0.23, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 19.81, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 19.81, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 22.05, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 22.05, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 24.23, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 24.23, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.25,0.9))+
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
LHY.1_ASC2


LHY4_timecourses_FSC2 <- read.csv("LHY4_splicing_FSC2_all.txt", sep="\t")
LHY4_timecourses_FSC2$Organ = factor(LHY4_timecourses_FSC2$Organ, levels=c("L1", "I1", "I5"))
LHY.4_FSC2 <- ggplot(data=LHY4_timecourses_FSC2, aes(x=CT, y=LHY.4, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.12, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.12, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = 1.15, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.44, y = 1.75, label = "\u25bc", size = 6, colour=AS1)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.15,1.2))+
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
LHY.4_FSC2


LHY4_timecourses_ASC2 <- read.csv("LHY4_splicing_ASC2_all.txt", sep="\t")
LHY4_timecourses_ASC2$Organ = factor(LHY4_timecourses_ASC2$Organ, levels=c("L1", "I1", "I5"))
LHY.4_ASC2 <- ggplot(data=LHY4_timecourses_ASC2, aes(x=CT, y=LHY.4, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.15, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.15, ymax = 1.2, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = 1.15, y = 1.75, label = "\u25bc", size = 6, colour=FS)+
  annotate("text", x = -1.44, y = 1.75, label = "\u25bc", size = 6, colour=AS1)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.2,0.4), name="Normalized Expression", limits=c(-0.15,1.2))+
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
LHY.4_ASC2


PRR3731_timecourses_FSC2 <- read.csv("PRR3731_splicing_FSC2_all.txt", sep="\t")
PRR3731_timecourses_FSC2$Organ = factor(PRR3731_timecourses_FSC2$Organ, levels=c("L1", "I1", "I5"))
PRR37.31_FSC2 <- ggplot(data=PRR3731_timecourses_FSC2, aes(x=CT, y=PRR37.31, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = -1.95, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = -1.95, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 3.85, y = -0.20, label = "\u2020", size = 5, colour=FS)+
  annotate("text", x = 3.85, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 5.66, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 5.66, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 7.47, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 7.47, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 9.28, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 9.28, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 11.09, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 11.09, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.5,0.5), name="Normalized Expression", limits=c(-0.25,1.5))+
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
PRR37.31_FSC2


PRR3731_timecourses_ASC2 <- read.csv("PRR3731_splicing_ASC2_all.txt", sep="\t")
PRR3731_timecourses_ASC2$Organ = factor(PRR3731_timecourses_ASC2$Organ, levels=c("L1", "I1", "I5"))
PRR3731_ASC2 <- ggplot(data=PRR3731_timecourses_ASC2, aes(x=CT, y=PRR37.31, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 1.5, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = 3.85, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 3.85, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 5.66, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 5.66, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 7.47, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 9.28, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  annotate("text", x = 11.09, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 11.09, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,1.5,0.5), name="Normalized Expression", limits=c(-0.25,1.5))+
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
PRR3731_ASC2


PRR3732_timecourses_FSC2 <- read.csv("PRR3732_splicing_FSC2_all.txt", sep="\t")
PRR3732_timecourses_FSC2$Organ = factor(PRR3732_timecourses_FSC2$Organ, levels=c("L1", "I1", "I5"))
PRR3732_FSC2 <- ggplot(data=PRR3732_timecourses_FSC2, aes(x=CT, y=PRR37.32, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.25,0.91))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = "none"
  )
PRR3732_FSC2


PRR3732_timecourses_ASC2 <- read.csv("PRR3732_splicing_ASC2_all.txt", sep="\t")
PRR3732_timecourses_ASC2$Organ = factor(PRR3732_timecourses_ASC2$Organ, levels=c("L1", "I1", "I5"))
PRR3732_ASC2 <- ggplot(data=PRR3732_timecourses_ASC2, aes(x=CT, y=PRR37.32, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 12, xmax = 24, ymin = -0.25, ymax = 0.9, alpha = .5, fill = "#e3e3e3")+
  geom_jitter(aes(col=Organ, shape = Organ),position=position_jitter(0.2), size = 2) +
  stat_smooth(aes(group = Organ, colour = Organ, fill=Organ, outfit=fit2<<-..y..),method="loess",  span = 0.8)+
  scale_fill_manual(values=c(L1,I1,I5))+
  scale_colour_manual(values=c(L1,I1,I5))+
  scale_linetype_manual(values=c(L1_line, I1_line, I5_line)) +
  annotate("text", x = 3.85, y = -0.15, label = "\u0023", size = 5, colour=FS)+
  annotate("text", x = 3.85, y = -0.12, label = "\u002a", size = 8, colour=FS)+
  #annotate("segment", x = 1, xend = 1, y = 1.75, yend = 1.25, colour = FS, size=1.2, arrow=arrow())+
  #annotate("segment", x = -1, xend = -1, y = 1.75, yend = 1.25, colour = AS1, size=1.2, arrow=arrow())+
  scale_x_continuous(breaks=seq(0,24,6), name="ZT (h)", limits=c(-2, 24.55))+
  scale_y_continuous(breaks=seq(0,0.9,0.3), name="Normalized Expression", limits=c(-0.25,0.9))+
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
        legend.position = "none"
  )
PRR3732_ASC2


plot_grid(LHY.1_FSC2, LHY.1_ASC2, LHY.4_FSC2, LHY.4_ASC2, PRR37.31_FSC2, PRR3731_ASC2, PRR3732_FSC2,PRR3732_ASC2, labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 2, align = "none", rel_widths = c(1.16, 1),rel_heights = c(1,1,1,1.2),label_size = 20)
