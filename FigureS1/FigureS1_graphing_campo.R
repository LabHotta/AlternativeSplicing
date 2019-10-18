library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)

summer <- "#d73027"
winter <- "#4575b4"
summer_line <- "solid"
winter_line <- "longdash"


diaria = read.csv("campo_temperatura_diaria.txt", sep="\t")

temperature_daily_field <- ggplot(data=diaria, aes(x=ZT, y=Temperature, group=Sampling)) +
                        annotate("rect", xmin = -2, xmax = 0, ymin = 8, ymax = 32, alpha = .5, fill = "#e3e3e3")+
                        annotate("rect", xmin = 12, xmax = 24, ymin = 8, ymax = 32, alpha = .5, fill = "#e3e3e3")+
                        geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
                        scale_colour_manual(values=c(summer, winter))+
                        scale_linetype_manual(values=c(summer_line, winter_line)) +
                        scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24.55))+
                        scale_y_continuous(name="Temperature ("~degree~"C)", breaks=seq(8,32,4), limits=c(8, 32))+
                        theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
                              text = element_text(size=18), 
                              axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size=18),
                              axis.text.y = element_text(size=18),
                              legend.position="none")
temperature_daily_field


light_daily_field <- ggplot(data=diaria, aes(x=ZT, y=Light, group=Sampling)) +
                  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 3, alpha = .5, fill = "#e3e3e3")+
                  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 3, alpha = .5, fill = "#e3e3e3")+
                  geom_line(aes(colour=Sampling, linetype=Sampling), size = 1.2) +
                  scale_colour_manual(values=c(summer, winter))+
                  scale_linetype_manual(values=c(summer_line, winter_line)) +
                  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24.55))+
                  scale_y_continuous(name="Light Intensity (MJ/" ~ m^{2}~")", breaks=seq(0,3,0.6), limits=c(0, 3))+
                  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
                  text = element_text(size=18), 
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_text(size=18),
                  axis.text.y = element_text(size=18),
                  legend.position="none")
light_daily_field

plot_grid(temperature_daily_field, light_daily_field, labels = c("B", "C"), ncol= 2, align = "v",label_size = 20)
