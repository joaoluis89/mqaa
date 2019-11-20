library(readxl)
library(psycho)
library(ggplot2)
library(cluster)
library(tidyverse)
library(fpc)
library(cluster)
library(HSAUR)
library(ggplot2)
library(ggmap)
library(rnaturalearth)

DataSet <- read_excel("Documents/MQAA/DataSet Utilizado/DataSetSemReferencias.xlsx")
sapply(DataSet, function(x) sum (is.na(x)))
summary(DataSet)
DataSet$`HDI rank` <- NULL 
Countries <- DataSet$Country
DataSet$Country <- NULL
DataSet$Region <- NULL
DataSet$Subregion <- NULL
DataSet$GINI <- NULL
DataSet$HI <- NULL
DataSet$DI <- type.convert(DataSet$DI)


DataSet = log1p(DataSet)
DataSet = standardize(DataSet)
DataSet$Country = Countries
DataSet = DataSet[complete.cases(DataSet), ]
Countries <- DataSet$Country
DataSet$Country <- NULL

summary(DataSet)
View(DataSet)

fit <- kmeans(x=DataSet, centers = 4)
DataSet$Centroid= as.factor(fit$cluster)
DataSet$Country = Countries
#fit <- Xmeans(data=DataSet, kmax = 4)
plotcluster(DataSet, fit$cluster)
clusplot(DataSet, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
plotcluster(DataSet, fit$cluster)

#####

world <- ne_countries(scale = "medium", returnclass = "sf")

world$name
ggplot(data = world) +
  geom_sf()

######

world<-map_data("world")

######

library(maps)
world<-map_data("world")

set.seed(123)
w2<-data.frame(world,data=sample(10,length(unique(world$region)),T)[world$region])
w2
ggplot(w2,aes(long,lat,group=group,fill=data))+
  geom_polygon(color="white")+
  scale_fill_gradient(low="lightgreen",high="darkgreen")+
  theme(panel.background = element_rect(fill = "lightsteelblue2"))
