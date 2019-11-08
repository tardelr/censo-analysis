library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(factoextra)

datasets.path <- '~/Desktop/datasets/censo_assets'

assets2000 <- read.csv( file.path( datasets.path, 'sp00.csv' ), colClasses = c( areap='character' ) )

assets2000 <- assets2000 %>% 
  subset( v0103 == 3550308 ) %>% 
  select( "areap", "v0214", "v0215", "v0216", "v0217", "v0218", "v0219", "v0220", "v0221", "v0222", "v0223" ) %>%
  na.omit()

# assets2000[is.na(assets2000)] <- -1

agg.assets <- aggregate(assets2000[,-1], by=list(areap=assets2000$areap), FUN=mean)
values.only <- agg.assets[,-1]

res.pca <- prcomp(~ ., data = values.only, scale = TRUE, na.action = na.omit )
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

factors2000 <- res.pca$rotation[,1]
ses2000 <- res.pca$x[,1]
hist(ses2000)
ses.area.2000 <-  cbind(agg.assets, ses2000)

write.csv(ses.area.2000, '~/Desktop/datasets/ses_score/ses2000.csv')

# Repeating analysis to 2010

assets2010 <- read.csv( file.path( datasets.path, 'sp10.csv'), colClasses = c( v0011 ='character' ))

assets2010 <- assets2010 %>%
  subset( v0002 == 50308) %>%
  select("v0011", "v0214", "v0215", "v0216", "v0217", "v0218", "v0219", "v0220", "v0221", "v0222") %>%
  na.omit()

# assets2010[is.na(assets2010)] <- -1

agg.assets.2010 <- aggregate(assets2010[,-1], by=list(areap=assets2010$v0011), FUN=mean)
values.only.2010 <- agg.assets.2010[,-1]

res.pca <- prcomp(~ ., data = values.only.2010, scale = T )
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

factors2010 <- res.pca$rotation[,1]
ses2010 <- res.pca$x[,1]
hist(ses2010)
ses.area.2010 <-  cbind(agg.assets.2010, ses2010)

write.csv(ses.area.2010, '~/Desktop/datasets/ses_score/ses2010.csv')

# 
