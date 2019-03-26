# Sales Percentage
# 6 figures of density area charts with a common legend and common y-axis
library(ggplot2)
library(RColorBrewer)

#set working directory
setwd("/Users/mkumari/Downloads")

#function to stacked bar charts.
stacked_bar <- function(file, left){
  #left=TRUE means y-axis shows up on left side. left=FALSE means y-axis shows up on right side
  df_Melted <- melt(file, id.var = "Year")
  colourCount = length(unique(df_Melted$variable))
  p <- ggplot(df_Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
    geom_area()+
    theme_bw()+ 
    scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
    guides(colour = guide_legend(nrow = 1))+
    {if(left==TRUE)scale_y_continuous(labels = function(x) paste0(x, "%"))}+
    {if(left==FALSE)scale_y_continuous(labels = function(x) paste0(x, "%"), position="right")}+
    theme( 
      plot.title = element_text(hjust = 0.5, size=15), 
      axis.title.x=element_blank(),
      axis.text.x = element_text(size=13),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=13),
      legend.title = element_blank(),
      legend.text = element_text(size=15),
      legend.position = "none")
  return(p)
}


#Cars BAU
df <- read.csv("MK_FromMarshall - SalesPercentageCarsBAU.csv", header=TRUE)
p1 <- stacked_bar(df, TRUE) +   labs(title = "Cars BAU")

#LDBAU
df2 <- read.csv("MK_FromMarshall - SalesPercentageLDBAU.csv", header=TRUE)
p2 <- stacked_bar(df2, FALSE) + labs(title = "LD BAU")

#Cars ZEV
df3 <- read.csv("MK_FromMarshall - SalesPercentageCarsZEV.csv", header=TRUE)
p3 <- stacked_bar(df3, TRUE) +   labs(title = "Cars ZEV")

#LD ZEV
df4 <- read.csv("MK_FromMarshall - SalesPercentageLDZEV.csv", header=TRUE)
p4 <- stacked_bar(df4, FALSE) + labs(title = "LD ZEV")

#Cars ZEV+B
df5 <- read.csv("MK_FromMarshall - SalesPercentageCarsZEV+B.csv", header=TRUE)
p5 <- stacked_bar(df5, TRUE)  + labs(title = "Cars ZEV+B")

#LD ZEV+B
df6 <- read.csv("MK_FromMarshall - SalesPercentageLDZEV+B.csv", header=TRUE)
p6 <- stacked_bar(df6, FALSE) + labs(title = "LD ZEV+B")

#plot final
p_final <- ggarrange(p1, p2, p3, p4,p5,p6, ncol=2, nrow=3, common.legend = TRUE, legend="right",
                     heights = c(1,1,1), widths = c(1.1,1))
annotate_figure(p_final,top = text_grob("Sales Percentage", size = 20), 
                left=text_grob("Percentage Sales Share (%)", size=15,rot = 90))
