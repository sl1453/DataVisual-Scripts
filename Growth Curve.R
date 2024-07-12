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
df_con <- openxlsx::read.xlsx("data/GrowthCurve.xlsx"
                                   , 7
                                   , startRow = 1
                                   , cols = c(1,5,6))

colnames(df_con) <- c("hour" , "cellcount","se")
sapply(df_con, class)
summary(df_con)
head(df_con)
#
df_XanGum <- openxlsx::read.xlsx("data/GrowthCurve.xlsx"
                                   , 7
                                   , startRow = 1
                                   , cols = c(1,11,12))

colnames(df_XanGum) <- c("hour" , "cellcount","se")
sapply(df_XanGum, class)
summary(df_XanGum)
head(df_XanGum)
#
df_MCell <- openxlsx::read.xlsx("data/GrowthCurve.xlsx"
                                 , 7
                                 , startRow = 1
                                 , cols = c(1,18,19))

colnames(df_MCell) <- c("hour" , "cellcount","se")
sapply(df_MCell, class)
summary(df_MCell)
head(df_MCell)
#List&combine into large dataframe
library(purrr)
list_df<- list(df_con, df_XanGum, df_MCell)
names(list_df) <- c("control", "xanthan gum","methylcellulose")
print(head(list_df))
df<- bind_rows(list_df, .id = "media")
df

#Create base graph
df$hour <- factor(df$hour)
df$media <- factor(df$media)
sapply(df,class)
g <- ggplot(df, aes(x = hour, y = cellcount,
                    fill = media, shape = media)) +
  geom_line(aes(color = media, group = media)) + 
  geom_point(size = 2.7) +
  geom_errorbar(aes(ymin = cellcount - se,
                    ymax = cellcount + se, width = 0.05)) +
  scale_y_log10(breaks = c(0,10,100,1000), limits = c(50,3000)) +
  xlab(expression("Time " * ' [hours]')) +
  ylab(expression("Cell Count")) +
  #geom_hline(yintercept = c(300,3000), colour = '#CCCCCC')+
  theme_classic()+
  scale_fill_viridis_d(direction = -1,
                        labels = c("Culture Media",
                                   "Methylcellulose\n+Culture Media",
                                   "Xanthan Gum\n+Culture Media"))+
  scale_shape_manual(values = c(21,24,22), 
                     labels = c("Culture Media",
                                  "Methylcellulose\n+Culture Media",
                                  "Xanthan Gum\n+Culture Media"))+
  scale_color_viridis_d(direction = -1,
                        labels = c("Culture Media",
                                   "Methylcellulose\n+Culture Media",
                                   "Xanthan Gum\n+Culture Media"))+
  annotation_logticks(sides = 'l')+
  theme(legend.position = c(0.8,0.3),
        legend.spacing.y = unit(0.4, 'cm'))+
  guides(color=guide_legend(byrow = TRUE),
         shape=guide_legend(byrow = TRUE),
         fill= guide_legend(byrow = TRUE)) +
  labs(colour = "Media Condition", shape = "Media Condition", fill="Media Condition") 
g
#Add annotations
library(grid)
g_growcurve <- g +
  annotate("segment", x = 2, xend = 4, y = 1700, yend = 1700,
           arrow = arrow(ends = "both", angle = 90,
                         length = unit(.2,"cm"))) +
  annotate("text", x = 3, y = 2200, label = "log growth phase\n12h-36h")
g_growcurve

