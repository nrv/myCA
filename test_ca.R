# See https://www.displayr.com/how-correspondence-analysis-works/ for data examples

library(ggplot2)
library(ggrepel)
source("myCA.R")

# Read original data used to compute CA
mydata <- read.csv(file="data/test_ca.data.csv", check.names=FALSE, head=TRUE, quote = "\"", sep=",", dec=".")
Y = data.matrix(mydata[,c("Big", "Athletic", "Friendly", "Trainable", "Resourceful", "Animal", "Lucky")])
rownames(Y) = mydata[,c("Name")]

# Read supplementary rows
otherdata <- read.csv(file="data/test_ca.suppl.csv", check.names=FALSE, head=TRUE, quote = "\"", sep=",", dec=".")
Y2 = data.matrix(otherdata[,c("Big", "Athletic", "Friendly", "Trainable", "Resourceful", "Animal", "Lucky")])
rownames(Y2) = otherdata[,c("Name")]

# Compute CA
myCA = myCA(Y)

# Project supplementary rows
other = myCA_project(myCA, Y2)

# Concatenate data before ploting
Pdf = as.data.frame(myCA$colProj)
rownames(Pdf) = colnames(Y)
Pdf$type = 'characteristic'

Tdf = as.data.frame(myCA$rowProj)
rownames(Tdf) = rownames(Y)
Tdf$type = 'animal'

Hdf = as.data.frame(other$rowProj)
rownames(Hdf) = rownames(Y2)
Hdf$type = 'animal'

plotData = rbind(Pdf, Tdf, Hdf)

# Plot
mytheme <- theme_bw(14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = rel(1)),
    axis.title = element_text(size = rel(1)),
    strip.text = element_text(size = rel(1)),
    strip.background = element_rect(fill = "grey90"),
    legend.position = "none",
    panel.border = element_rect()
  )

plotColors = c('red', 'blue')
names(plotColors) = c('animal', 'characteristic')

ggplot(data = plotData, aes(x = V1, y = V2, color = type)) +
  geom_point() + coord_fixed() + mytheme +
  geom_hline(yintercept = 0, linetype="dashed") +  geom_vline(xintercept = 0, linetype="dashed") +
  geom_label_repel(data=plotData, aes(fill = type, label=rownames(plotData)), color = 'white') + 
  scale_color_manual(breaks = names(plotColors), values = plotColors) +
  scale_fill_manual(breaks = names(plotColors), values = plotColors)
