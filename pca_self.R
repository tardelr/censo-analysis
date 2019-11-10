library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(factoextra)

datasets.path <- '~/Desktop/analyses/pca_self/'

rotina <- read.csv( file.path( datasets.path, 'rotina.csv' ), colClasses = c( Dia='character' ) )

agg.rotina <- aggregate(rotina[,seq(6)], by=list(dia=rotina$Dia), FUN=mean)
values.only <- agg.rotina[,-1]

res.pca <- prcomp(~ ., data = values.only, scale = TRUE, na.action = na.omit )
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
