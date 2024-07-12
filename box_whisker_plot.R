library("readxl")
library("ggplot2")
setwd("~/Documents/R workshop 2017/project pattern/analysis/data 7")
data <- read_excel("Chi_Square_48h&60h_df_7.xlsx", sheet = "Sheet1")


data$Media <- as.factor(data$Media)


# Boxplot with ggplot2
ggplot(data, aes(x = Media, y = ChiSquare)) + 
  geom_boxplot() +
  xlab("Category") +
  ylab("Value")
dev.off()
data
library(readxl)
library(ggplot2)

# Read data
data <- read_excel("Chi_Square_48h&60h_df_7.xlsx", sheet = "Sheet1")
data$Media <- as.factor(data$Media)
# Open a PDF device to save your plot
pdf("Boxplot_ChiSquareAll.pdf", width = 8, height = 6)

# Create boxplot with ggplot2
ggplot(data, aes(x = Media, y = ChiSquare)) + 
  geom_boxplot() +
  xlab("Category") +
  ylab("Value")

# Close the PDF device
dev.off()
