# R script for Rheology data, 29\&7 July
# S.Li Claire
# Select the Session -> Set working directory -> To source  file location.
#Ctrl + Enter               Run current line or selection
#Ctrl + Shift + Enter       Run all lines
#Ctrl + Alt + B             Run from beginning to current line
#Ctrl + Alt + E             Run from current line to end
# Assignment operator:      Alt + -
install.packages('tidyverse')
install.packages("ggplot2")
install.packages("openxlsx")
update.packages(checkBuilt = TRUE)

Yes
library("tidyverse")
library(magrittr)
library(ggplot2)
library(readxl)
library(readr)
library(openxlsx)
setwd("/Users/apple/Documents/R workshop 2017/project July_GrowthCurve")
### Use Openxlsx to set sample data frames ###
# Set PigInMuc Reference 1 flow curve dataframe #
###check class to be numeric, Change column names to be CONSISTENT!###
df <- openxlsx::read.xlsx("data/GrowthCurve.xlsx"
                              , 6
                              , startRow = 1
                              , cols = c(8,9,10))
##change data frame values for column: condition
df$condition[df$condition == "Control"] <- "Culture media"
df$condition[df$condition == "Methylcellulose"] <- "Methylcellulose media"
df$condition[df$condition == "Xanthan Gum"] <- "Xanthan Gum media"
colnames(df) <- c("doublingtime" , "mediacondition","se")
sapply(df, class)
summary(df)
head(df)
#plot bar graph
g <- ggplot(df, aes(x = mediacondition, y = doublingtime, 
                                  fill = mediacondition)) +
  geom_col(width = 0.8, position = position_dodge(0.05)) +
  geom_errorbar(aes(ymin = doublingtime - se,
                    ymax = doublingtime + se, width = 0.1))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, 
                       labels = c("Culture Media"
                                  , "Methylcellulose\n+Culture Media"
                                  , "Xanthan Gum\n+Culture Media"))+
  ylim (c(0,20)) +
  xlab(expression("Growth Conditions")) +
  ylab(expression("Doubling Time [hours]")) +
  guides(fill = guide_legend(reverse = FALSE, title = "Culture Condition", byrow = TRUE))

#Add annotations
library(grid)
g_doubletime <- g +
  annotate("segment", x = 1.0, xend = 2, y = 14, yend = 14) +
  annotate("segment", x = 1.0, xend = 1.0, y = 13.6, yend = 14) +
  annotate("segment", x = 2, xend = 2, y = 13.6, yend = 14) +
  
  annotate("segment", x = 2, xend = 3, y = 15, yend = 15) +
  annotate("segment", x = 2, xend = 2, y = 14.6, yend = 15) +
  annotate("segment", x = 3, xend = 3, y = 14.6, yend = 15) +
  
  annotate("segment", x = 1, xend = 3, y = 16.5, yend = 16.5) +
  annotate("segment", x = 1, xend = 1, y = 16.1, yend = 16.5) +
  annotate("segment", x = 3, xend = 3, y = 16.1, yend = 16.5) +
  
  annotate("text", x = 1.5, y = 14.5, label = "ns.") +
  annotate("text", x = 2.5, y = 15.5, label = "ns.") +
  annotate("text" ,x = 2, y = 17, label = "ns." ) +
  #annotate("text", x = 2, y = 17, label = "ns. P=0.276") +
  #theme(legend.position = c(0.15,0.9)) +
  theme(legend.spacing.y = unit(0.3, 'cm'),
        legend.title.align = 0)
  ## important additional element
  ##guides(fill = guide_legend(byrow = TRUE))
g_doubletime

