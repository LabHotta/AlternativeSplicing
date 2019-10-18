library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(ggpmisc)
library(broom)

theme_set(theme_cowplot())

AS1 <- "#F7882F"
AS2 <- "#4484CE"

AS1_line = "dashed"
AS2_line = "dotdash"

#regressão LHY_L1C1 X Primer
LHY_L1C1 <- read.csv("LHY_temperature_L1C1_primers.txt", sep="\t")
#LHY_L1C1$Temperature <- as.numeric(LHY_L1C1$Temperature)
#LHY_L1C1$LHY_ratio <- as.numeric(LHY_L1C1$LHY_ratio)
my.formula <- y ~ x
LHY_L1C1_Primer <- ggplot(data=LHY_L1C1, aes(x=Temperature, y=LHY_ratio, group=Primer, colour=Primer)) +
  scale_colour_manual(values=c(AS1, AS2))+
  scale_linetype_manual(values=c(AS1_line, AS2_line)) +
  scale_x_continuous(breaks=seq(10,30,5), limits=c(10, 30))+
  scale_y_continuous(breaks=seq(-2.4,1.2,1.2), limits=c(-2.452,1.2))+
  geom_smooth(aes(group=Primer, col=Primer, linetype=Primer), method=lm, se = FALSE)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, label.x.npc = "right", digits = 3) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = my.formula),
                  aes(label = paste("p-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x.npc = 'right', label.y.npc = 0.75)+
  geom_jitter(aes(col=Primer), position=position_jitter(0.2), size = 2) +
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
  ) +
  labs(y = "log(AS/FS)", x = expression(paste('Temperature (',degree,'C)',sep='')))
LHY_L1C1_Primer


#regressão LHY_L1C2 X Primer
LHY_L1C2 <- read.csv("LHY_temperature_L1C2_primers.txt", sep="\t")
my.formula <- y ~ x
LHY_L1C2_Primer <- ggplot(data=LHY_L1C2, aes(x=Temperature, y=LHY_ratio, group=Primer, colour=Primer)) +
  scale_colour_manual(values=c(AS1, AS2))+
  scale_linetype_manual(values=c(AS1_line, AS2_line)) +
  scale_x_continuous(breaks=seq(10,30,5), limits=c(10, 30))+
  scale_y_continuous(breaks=seq(-2.4,1.2,1.2), limits=c(-2.452,1.2))+
  geom_smooth(aes(group=Primer, col=Primer, linetype=Primer), method=lm, se = FALSE)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, label.x.npc = "right", digits = 3) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = my.formula),
                  aes(label = paste("p-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x.npc = 'right', label.y.npc = 0.75)+
  geom_jitter(aes(col=Primer), position=position_jitter(0.2), size = 2) +
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
  ) +
  labs(y = "log(AS/FS)", x = expression(paste('Temperature (',degree,'C)',sep='')))
LHY_L1C2_Primer

LHY_I1C2 <- read.csv("LHY_temperature_I1C2_primers.txt", sep="\t")
my.formula <- y ~ x
LHY_I1C2_Primer <- ggplot(data=LHY_I1C2, aes(x=Temperature, y=LHY_ratio, group=Primer, colour=Primer)) +
  scale_colour_manual(values=c(AS1, AS2))+
  scale_linetype_manual(values=c(AS1_line, AS2_line)) +
  scale_x_continuous(breaks=seq(10,30,5), limits=c(10, 30))+
  scale_y_continuous(breaks=seq(-2.4,1.2,1.2), limits=c(-2.452,1.2))+
  geom_smooth(aes(group=Primer, col=Primer, linetype=Primer), method=lm, se = FALSE)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, label.x.npc = "right", digits = 3) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = my.formula),
                  aes(label = paste("p-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x.npc = 'right', label.y.npc = 0.75)+
  geom_jitter(aes(col=Primer), position=position_jitter(0.2), size = 2) +
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
  ) +
  labs(y = "log(AS/FS)", x = expression(paste('Temperature (',degree,'C)',sep='')))
LHY_I1C2_Primer

#regressão LHY_I5C2 X Primer
LHY_I5C2 <- read.csv("LHY_temperature_I5C2_primers.txt", sep="\t")
my.formula <- y ~ x
LHY_I5C2_Primer <- ggplot(data=LHY_I5C2, aes(x=Temperature, y=LHY_ratio, group=Primer, colour=Primer)) +
  scale_colour_manual(values=c(AS1, AS2))+
  scale_linetype_manual(values=c(AS1_line, AS2_line)) +
  scale_x_continuous(breaks=seq(10,30,5), limits=c(10, 30))+
  scale_y_continuous(breaks=seq(-2.4,1.2,1.2), limits=c(-2.452,1.2))+
  geom_smooth(aes(group=Primer, col=Primer, linetype=Primer), method=lm, se = FALSE)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, label.x.npc = "right", digits = 3) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = my.formula),
                  aes(label = paste("p-value = ", signif(..p.value.., digits = 2), sep = "")),
                  label.x.npc =  0.6, label.y.npc = 0.75)+
  geom_jitter(aes(col=Primer), position=position_jitter(0.2), size = 2) +
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
  ) +
  labs(y = "log(AS/FS)", x = expression(paste('Temperature (',degree,'C)',sep='')))
LHY_I5C2_Primer


plot_grid(LHY_L1C1_Primer, LHY_L1C2_Primer, LHY_I1C2_Primer, LHY_I5C2_Primer, labels = c("A", "B", "C", "D"), ncol = 2, align = "none", rel_widths = c(1.15, 1),rel_heights = c(1,1,1),label_size = 20)

