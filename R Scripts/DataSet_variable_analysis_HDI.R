library(readxl)
library(ggplot2)
library(plyr)

DataSet <- read_excel("Documents/MQAA/DataSet Utilizado/DataSetSemReferencias.xlsx")
#DataSet$`HDI rank` <- NULL 
#DataSet$Country <- NULL
View(DataSet)


totRegion <- count(DataSet$Region)

totRegion
bp <- ggplot(totRegion, aes(x=x, y=freq, fill=x)) + geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y")

pie
#Descrição
summary(DataSet$EYS)
sd(DataSet$EYS)

  #Histograma e Densidade
p <- ggplot(DataSet, aes(x=EYS)) + geom_density()
p + labs(title="EYS Density Curve")

p <- ggplot(DataSet, aes(x=EYS, color=Region)) + geom_density()
p + labs(title="EYS Density Curve")

#Distribution function without North America
p <- ggplot(subset(DataSet, Region!="N. America"), aes(x=EYS, color=Region)) + geom_density()
p + labs(title="EYS Density Curve")
#p + geom_vline(aes(xintercept=mean(EYS)), color="blue", linetype="dashed", size=1)

#Boxplots

p <- ggplot(DataSet, aes(y=EYS)) + geom_boxplot()
p

p <- ggplot(DataSet, aes(x=Region, y=EYS, color=Region)) + geom_boxplot()
p

q <- ggplot(DataSet, aes(x=Subregion, y=EYS, color=Subregion)) + geom_boxplot()
q


