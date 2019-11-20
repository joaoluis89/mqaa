library(readxl)
library(psycho)
library(ggplot2)
library(cluster)
library(tidyverse)
library(fpc)
library(cluster)
library(HSAUR)
library(ggmap)
library(rnaturalearth)
library(corrplot)
library(GGally)
library(mctest)
library(rje)

###### Tratativa dos dados

DataSet <- read_excel("Documents/MQAA/DataSet Utilizado/DataSetSemReferencias.xlsx")
sapply(DataSet, function(x) sum (is.na(x)))
summary(DataSet)

DataSet$`HDI rank` <- NULL 
Countries <- DataSet$Country
DataSet$Country <- NULL
Regions <- DataSet$Region
DataSet$Region <- NULL
DataSet$Subregion <- NULL
# DataSet$GINI <- NULL
# DataSet$HI <- NULL
DataSet$DI <- type.convert(DataSet$DI)
# DataSet$Country = Countries
# DataSet$Region = Regions
DataSet = DataSet[complete.cases(DataSet), ]
# Countries <- DataSet$Country
# DataSet$Country <- NULL

summary(DataSet)
View(DataSet)

##### Padrocinacao

# DataSet = log1p(DataSet)
# DataSet = standardize(DataSet)



###### Correlacao
ggpairs(DataSet)
DataSet$PD <- log10(DataSet$PD)
DataSet$GNI <- log10(DataSet$GNI)

ggpairs(DataSet)

correlationMatrix <- cor(DataSet, method = "pearson", use = "complete.obs")
corrplot(correlationMatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


###### Regracao Linear Multipla - todas as variáveis

model <- lm(DI ~ HDI + LEB + EYS + MYS + GNI + PD + IH + CPI + GINI + HI, data = DataSet)

summary(model)


##### Detecção de multicolinearidade

dataSetWithoutDI <- DataSet[, 1:10]
omcdiag(dataSetWithoutDI,DataSet$DI)
imcdiag(dataSetWithoutDI,DataSet$DI)

###### Removendo valiaveis colineares
# hdi <- DataSet$HDI

dataSetWithoutColinearity <- DataSet[,c(1,6:10)]
# dataSetWithoutColinearity$HDI <- hdi

View(dataSetWithoutColinearity)

#####nova deteccao de colinearidade
dataSetWithoutDI <- DataSet[, 0:9]
omcdiag(dataSetWithoutColinearity,DataSet$DI)
imcdiag(dataSetWithoutColinearity,DataSet$DI)


###### Regracao Linear Multipla - todas as variáveis

powerSet(1:10)

model <- lm(DI ~ HDI + PD + CPI + GINI, data = DataSet)
model <- lm(DI ~ HDI, data = DataSet)
resid(model)
plot(model)

###### Sccatter plot


pairs(DataSet)

df.reg.subset = df.reg[, c("LEAB", "EYOS", "MYOS", "GNIPC", "ROH")]

# Imprimindo a matriz de dispersao.
pairs(df.reg.subset, cex = 1.5)

model <- lm(DI ~ HDI + LEB + EYS + MYS + GNI + PD + IH + CPI, data = DataSet)

summary(model)



# q <- ggplot(DataSet, aes(x=HDI, color=Region)) + geom_density()
# q
