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
setwd("/Users/apple/Documents/R workshop 2017/project July_rheology")
### Use Openxlsx to set sample data frames ###
# Set PigInMuc Reference 1 flow curve dataframe #
###check class to be numeric, Change column names to be CONSISTENT!###
df_MCell_stor <- openxlsx::read.xlsx("data/Gmodulus_reps.xlsx"
                                     , 5
                                   , startRow = 3
                                   , cols = c(15,16,17,20))

colnames(df_MCell_stor) <- c("Frequency" , "value","se","topBottom")
sapply(df_MCell_stor, class)
summary(df_MCell_stor)
df_MCell_stor
#
df_MCell_loss <- openxlsx::read.xlsx("data/Gmodulus_reps.xlsx"
                                     , 5
                                   , startRow = 3
                                   , cols = c(15,18,19,20))
colnames(df_MCell_loss) <- c("Frequency" , "value","se","topBottom")
sapply(df_MCell_loss, class)
summary(df_MCell_loss)
df_MCell_loss
####combine 2 dataframes####
library(purrr)
list_df_MCell<- list(df_MCell_stor, df_MCell_loss)
names(list_df_MCell) <- c("storage modulus", "loss modulus")
print(head(list_df_MCell))
df_MCell <- bind_rows(list_df_MCell, .id = "modulus")
head(df_MCell)

############# Plot All in One Graph ###################
g <- ggplot(df_MCell, aes(x = Frequency, y = value, 
                        shape = modulus,
                        fill = topBottom)) +
  geom_point(size = 2.7, color = "#440154") +
  scale_shape_manual(values = c(21, 24)) +
  
  scale_fill_manual(values = c("#fde725","#440154"),
                    labels = c("Top","bottom"))+
  scale_x_log10() + 
  scale_y_log10(labels=scales::comma_format(),
    limits= c(1, 100))+
                #minor = c(1:10,
                          #seq(0.000001, 0.00001, by = 0.00001),
                          #seq(0.00001, 0.0001, by = 0.0001),
                          #seq(0.0001, 0.001, by = 0.001),
                          #seq(0.001, 0.01, by = 0.01),
                          #seq(0.01, 0.1, by = 0.01),
                          #seq(0.1, 1, by = 0.1),
                          #seq(1, 10, by = 1),
                          #seq(10, 100, by = 10))) +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')+
  
  geom_errorbar(aes(ymin = value - se,
                    ymax = value + se), width = 0.05, linewidth = 0.5)+
  theme_classic() + 
  annotation_logticks() +
  labs(shape = "modulus", fill = "Methylcellulose\n+Culture Media")+
  theme(strip.background = element_blank(),
        legend.title.align = 0,
        legend.spacing.y = unit(0.3, 'cm'))

byrow <- guide_legend(byrow = TRUE, 
                      override.aes=list(shape=21))
g <- g + guides(fill = byrow)
g
############ Plot All in One Graph End Here #################
############ Plot facet In Same Scale #######################
g <- ggplot(df_all, aes(x = Frequency, y = value, 
                        shape = modulus,
                        fill = sample)) +
  geom_point(size = 2.7, color = "#440154") +
  scale_shape_manual(values = c(21, 24)) +
  
  scale_fill_manual(values = c("#fde725","#addc30","#440154","#21918c"),
                    labels = c("Culture Media",
                               "Mucus"
                               , "Xanthan Gum\n+Culture Media"
                               , "Methylcellulose\n+Culture Media"))+
  scale_x_log10() + 
  scale_y_log10(labels=scales::label_comma(accuracy = 0.0000001),
                limit = c(0.0000001, 50)) +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')+
  
  geom_errorbar(aes(ymin = value - se,
                    ymax = value + se), width = 0.05, linewidth = 0.5)
#Faceted on sample
g <- g+ facet_wrap(.~ sample, scales = "free") +
  #scale_color_viridis_d() +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank(),
        legend.title.align = 0,
        legend.spacing.y = unit(0.3, 'cm'))

byrow <- guide_legend(byrow = TRUE, 
                      override.aes=list(shape=21))
g <- g + guides(fill = byrow)
g
############ Plot facet In Same Scale End Here ##############
#plot control vs MCell vs XanGum
g1 <- ggplot(filter(df_all, sample %in% c('Control','Methylcellulose',
                    'Xanthan Gum')), aes(x = Frequency, y = value, 
                                        shape = modulus,
                                        color = sample)) +
  geom_point(size = 2.7) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g1+ facet_wrap( . ~ sample, scales = "free") +
  scale_color_viridis_d() +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank())
#plot control vs Pig Mucus
g_cp <- ggplot(filter(df_all, sample %in% c('Control','Pig Mucus')), 
               aes(x = Frequency, y = value, 
                   shape = modulus, color = sample)) +
  geom_point(size = 2.7) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g_cp+ facet_wrap( . ~ sample, scales = "free") +
  scale_color_viridis_d() +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank())
#########INDIVIDUAL PLOTA#######
#plot control
g_con <- ggplot(filter(df_all, sample %in% c('Control')), 
                aes(x = Frequency, y = value, 
                    shape = modulus)) +
  geom_point(size = 2.7, color = viridis::viridis(1,begin =1, end =1)) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g_con <- g_con + #facet_wrap( . ~ sample, scales = "free") +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank()) +
  labs(shape = "Control Media")

#plot xanGum
g_xanGum <- ggplot(filter(df_all, sample %in% c('Xanthan Gum')), 
                aes(x = Frequency, y = value, 
                    shape = modulus)) +
  geom_point(size = 2.7, color = viridis::viridis(1,begin =0.8,
                                                  end =0.8)) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g_XanGum <- g_xanGum + #facet_wrap( . ~ sample, scales = "free") +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank()) +
  labs(shape = "Xanthan Gum Media")

#plot MCell
g_MCell <- ggplot(filter(df_all, sample %in% c('Methylcellulose')), 
                aes(x = Frequency, y = value, 
                    shape = modulus)) +
  geom_point(size = 2.7, color = viridis::viridis(1, begin = 0,
                                                  end = 0)) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g_MCell <- g_MCell + #facet_wrap( . ~ sample, scales = "free") +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank()) +
  labs(shape = "Methylcellulose Media")

#plot pig Mucus
g_Pigmuc <- ggplot(filter(df_all, sample %in% c('Pig Mucus')), 
                  aes(x = Frequency, y = value, 
                      shape = modulus)) +
  geom_point(size = 2.7, color = viridis::viridis(1, begin = 0.5,
                                                  end = 0.5)) +
  scale_x_log10() + 
  scale_y_log10() +
  xlab(expression("Frequency " * ' [Hz]')) +
  ylab(expression("(G\')/(G\") [Pa]")) +
  geom_vline(xintercept = 10, colour = '#CCCCCC')
#Faceted on sample
g_Pigmuc <- g_Pigmuc + #facet_wrap( . ~ sample, scales = "free") +
  theme_classic() + 
  annotation_logticks() +
  theme(strip.background = element_blank()) +
  labs(shape = "Mucus Reference\npig small intestine")

#GROUP into ONE GRAPH
install.packages('patchwork')
library(patchwork)
(g_con)/(g_Pigmuc)
(g_con | g_Pigmuc)/
(g_xanGum | g_MCell)























































