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
install.packages("viridisLite")
install.packages("openxlsx")
install.packages("viridis")
update.packages(checkBuilt = TRUE)
Yes

library(openxlsx)
library("tidyverse")
library(magrittr)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
setwd("/Users/apple/Documents/R workshop 2017/project July_rheology")
# if read the whole excel file#
#FlowCurve_XanGum_MCell_Control <- read_excel("data/FlowCurve_XanGum_MCell_Control.xlsx")
# Create control shear rate #
con_shear_rate <- read_excel("data/FlowCurve_XanGum_MCell_Control.xlsx", 1
                     , range = "G1:G25"
                     , col_types = "numeric"
                     )
#Get a summary of data#
con_shear_rate
#see all rows#
print(n=25, con_shear_rate)
#if read the whole sheet#
#df_con <- read_excel("/Users/apple/Documents/R workshop 2017/project July_rheology/data/FlowCurve_XanGum_MCell_Control.xlsx", 1
#                     , col_types = c (col_types = "guess", "guess", "guess", "guess", "guess",
#                                      "numeric", "numeric", "numeric", "numeric", "numeric",
#                                      "guess", "numeric", "numeric", "numeric", 
#                                      "guess", "guess","guess"))
# Drop row 1 & 2 #
con_shear_rate <- con_shear_rate[-c (1, 2) , ]
# Use openxlsx to set control flow curve dataframe #
df_con <- openxlsx::read.xlsx("data/FlowCurve_XanGum_MCell_Control.xlsx", 1
                    , startRow = 2
                    , cols = c(7,9))

df_con

sapply(df_con, class)
summary(df_con)
colnames(df_con) <- c("Shear.Rate" , "Viscosity") 
# Set XanGum flow curve dataframe #
df_XanGum <- read.xlsx("data/flowCurve_reps.xlsx", 7
                       , startRow = 3
                       , cols = c(16,17,18))
colnames(df_XanGum) <- c("Shear.Rate", "Viscosity","se")
sapply(df_XanGum, class)
df_XanGum
# Set MCell flow curve dataframe#
df_MCell <- read.xlsx("data/flowCurve_reps.xlsx", 2
                      , startRow = 3
                      , cols = c(16,17,18))
colnames(df_MCell) <- c("Shear.Rate", "Viscosity","se")
sapply(df_MCell, class)
df_MCell

#plot#
#Check if all data frames have same column names!#
plot <- ggplot(NULL, aes(x = Shear.Rate, y = Viscosity)) + 
  geom_point(data = df_con) +
  geom_point(data = df_XanGum) +
  geom_point(data = df_MCell) +
  scale_x_log10(breaks = c(1, 10, 100)) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000),
                minor = c(1:10,
                          seq(10, 100, by = 10),
                          seq(100, 1000, by = 100),
                          seq(1000, 10000, by = 1000),
                          seq(10000, 100000, by = 10000))) +
  xlab(expression("Shear Rate " * gamma * ' [s'^-{} * "]")) +
  ylab(expression("Viscosity " * eta * " [mPas]" )) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1)
plot
# Add logarithmic tick marks #
plot + annotation_logticks()
# Add title # 
plot + annotate("text", x = 10, y = Inf,
                label = "Mdia Flow Curves", vjust = 1.5, size = 3)
# Change themes, Choices:https://ggplot2.tidyverse.org/reference/ggtheme.html #
plot + theme_classic() + annotation_logticks() + scale_fill_viridis_c()







#########START FROM HERE after setting dfs#########
#########START FROM HERE after setting dfs#########
#create a list of dataframes#
list_df_flowcurve <- list(df_con, df_XanGum, df_MCell)
names(list_df_flowcurve) <- c("Culture Media", "Xanthan Gum+Culture Media",
                              "Methylcellulose+Culture Media")
print(list_df_flowcurve)

#Combine lists into one large data frame, Add id Column #
library(purrr)
library(dplyr)
df_flowcurve <- bind_rows(list_df_flowcurve, .id = "id")
df_flowcurve
#Another way using base R#
#df_flowcurve <- do.call(rbind, list_df_flowcurve)
#df_flowcurve$id <- factor(rep(1:length(list_df_flowcurve), 
                              #each = sapply(list_df_flowcurve, nrow)))
#Add colour bind palete & themes & tick marks, specify shapes#
library(viridisLite)
#Plot with ggplot#
g <- ggplot(df_flowcurve, aes(x = Shear.Rate, y = Viscosity, 
                              #colour = id,
                              shape = id,
                              size = id,
                              fill = id))+
  geom_point() +
  scale_color_manual(values = c("#440154","#440154","#440154"),
                     labels = c("Culture Media"
                                , "Methylcellulose\n+Culture Media"
                                , "Xanthan Gum\n+Culture Media")) +
  scale_size_manual(values = c(("Control" = 3.7),
                               2.7, 2.7),
                    labels = c("Culture Media"
                               , "Methylcellulose\n+Culture Media"
                               , "Xanthan Gum\n+Culture Media")) +
  scale_shape_manual(values = c(21, 24, 22),
                     labels = c("Culture Media"
                                , "Methylcellulose\n+Culture Media"
                                , "Xanthan Gum\n+Culture Media")) +
  scale_fill_viridis_d(direction = -1,
                       labels = c("Culture Media"
                                  , "Methylcellulose\n+Culture Media"
                                  , "Xanthan Gum\n+Culture Media")) +
  
  scale_x_log10(breaks = c(1, 10, 100)) + 
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000),
                minor = c(1:10,
                          seq(10, 100, by = 10),
                          seq(100, 1000, by = 100),
                          seq(1000, 10000, by = 1000),
                          seq(10000, 100000, by = 10000))) +
  geom_errorbar(aes(ymin = Viscosity - se,
  ymax = Viscosity + se), width = 0.05, size = 0.5) +
  
  xlab(expression("Shear Rate " * gamma * ' [s'^-{} * "]")) +
  ylab(expression("Viscosity " * eta * " [mPas]" )) +
  geom_hline(yintercept = 1, colour = '#CCCCCC') +
  geom_vline(xintercept = 1, colour = '#CCCCCC') +
  theme_classic() +
  annotation_logticks() +
  labs(shape = "Media", #colour = "Media", 
       size = "Media", fill = "Media") +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 11),
        legend.title.align = 0,
        legend.spacing.y = unit(0.3, 'cm')) 

byrow <- guide_legend(byrow = TRUE)
g <- g + guides(shape = byrow, size = byrow, fill = byrow)
g
#specify shape with + scale_shape_manual(values= c(15, 16, 17))





#export pdf files#
pdf("outputs/flowCurve_Average_7.pdf", width = 6.36, height = 3.91)
print(g)
dev.off()

