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
df_ratio <- openxlsx::read.xlsx("data/GrowthCurve.xlsx"
                              , 10
                              , startRow = 1
                              , cols = c(1,2,3,4))

colnames(df_ratio) <- c("hour" ,"ratio","grwcondition","se")
sapply(df_ratio, class)
summary(df_ratio)
df_ratio
#
g <- ggplot(filter (df_ratio, hour %in% c(12,24,36,48,60)), 
            aes(x = hour, y = ratio,
                    colour = grwcondition)) +
  geom_line() + 
  geom_point(size = 2.7, aes(shape = grwcondition)) +
  geom_errorbar(aes(ymin = ratio - se,
                    ymax = ratio + se, width = 0.25)) +
  #ylim(c(0,1.5))+
  scale_y_continuous(breaks = c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.6)) +
  xlab(expression("Time [hours]")) +
  ylab("Attachment Ratio\n(on ceiling / on floor)") +
  #geom_hline(yintercept = c(300,3000), colour = '#CCCCCC')+
  theme_classic()+
  theme(legend.spacing.y = unit(0.4, 'cm'))+
  guides(color=guide_legend(byrow = TRUE),
         shape=guide_legend(byrow = TRUE)) +
  scale_color_viridis_d(direction = -1,
                        labels = c("Culture Media",
                                   "Methylcellulose\n+Culture Media",
                                   "Xanthan Gum\n+Culture Media"))+
  scale_shape_discrete(labels = c("Culture Media",
                                  "Methylcellulose\n+Culture Media",
                                  "Xanthan Gum\n+Culture Media")) +
  #annotation_logticks(sides = 'l')+
  #theme(legend.position = c(0.8,0.2))+
  labs(colour = "Growth Condition", shape = "Growth Condition")
g
