setwd("/Users/mkumari/Downloads")
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
library(zoom)
library(cowplot)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

# Sales Percentage

#Cars BAU
df <- read.csv("MK_FromMarshall - SalesPercentageCarsBAU.csv", header=TRUE)
df_1Melted <- melt(df, id.var = "Year")
colourCount = length(unique(df_1Melted$variable))
p1 <- ggplot(df_1Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Cars BAU") + 
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.x=element_blank(),axis.title.y = element_blank(),
    axis.text.y = element_text(size=13),
    axis.text.x = element_text(size=13),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank(),
    legend.text = element_text(size=13))

#LDBAU
df2 <- read.csv("MK_FromMarshall - SalesPercentageLDBAU.csv", header=TRUE)
df_2Melted <- melt(df2, id.var = "Year")
colourCount = length(unique(df_2Melted$variable))
p2 <- ggplot(df_2Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), position = "right") +
  labs(title = "LD BAU") + 
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.y=element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size=15),
    axis.title.x=element_blank(),axis.text.x = element_text(size=13),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank())

#Cars ZEV
df3 <- read.csv("MK_FromMarshall - SalesPercentageCarsZEV.csv", header=TRUE)
df_3Melted <- melt(df3, id.var = "Year")
colourCount = length(unique(df_3Melted$variable))
p3 <- ggplot(df_3Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Cars ZEV") + 
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.x=element_blank(),axis.title.y = element_blank(),
    axis.text.y = element_text(size=13),axis.text.x = element_text(size=13),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank())


#LD ZEV
df4 <- read.csv("MK_FromMarshall - SalesPercentageLDZEV.csv", header=TRUE)
df_4Melted <- melt(df4, id.var = "Year")
colourCount = length(unique(df_4Melted$variable))
p4 <- ggplot(df_4Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), position = "right") +
  labs(title = "LD ZEV") + 
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.y=element_blank(), axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=15),
    axis.title.x=element_blank(),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank())


#Cars ZEV+B
df5 <- read.csv("MK_FromMarshall - SalesPercentageCarsZEV+B.csv", header=TRUE)
df_5Melted <- melt(df5, id.var = "Year")
colourCount = length(unique(df_5Melted$variable))
p5 <- ggplot(df_5Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  labs(title = "Cars ZEV+B") + 
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.x=element_blank(),axis.title.y = element_blank(),
    axis.text.y = element_text(size=13),axis.text.x = element_text(size=13),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank())


#LD ZEV

df6 <- read.csv("MK_FromMarshall - SalesPercentageLDZEV+B.csv", header=TRUE)
df_6Melted <- melt(df6, id.var = "Year")
colourCount = length(unique(df_6Melted$variable))
p6 <- ggplot(df_6Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
  geom_area()+
  theme_bw()+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"), position = "right") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  labs(title = "LD ZEV") + 
  #geom_text(aes(label=variable),position = position_stack(vjust = 0.5))+
  guides(colour = guide_legend(nrow = 1))+
  theme( 
    axis.title.y=element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size=15),
   axis.text.x = element_text(size=13),
    axis.title.x=element_blank(),
    plot.title = element_text(hjust = 0.5, size=15), legend.position = "none", legend.title = element_blank())

p_final <- ggarrange(p1, p2, p3, p4,p5,p6, ncol=2, nrow=3, common.legend = TRUE, legend="right",
                     heights = c(1,1,1), widths = c(1.1,1))
annotate_figure(p_final,top = text_grob("Sales Percentage", hjust = 1, size = 20), 
                left=text_grob("Percentage Sales Share (%)", size=15,rot = 90))
