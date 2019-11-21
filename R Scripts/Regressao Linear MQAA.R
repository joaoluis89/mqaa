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
library(olsrr)

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



###### Regracao Linear Multipla - todas as variáveis

model <- lm(DI ~ HDI + LEB + EYS + MYS + GNI + PD + IH + CPI + GINI + HI, data = DataSet)
summary(model)

###### Correlacao
ggpairs(DataSet)
DataSet$PD <- log10(DataSet$PD)
DataSet$GNI <- log10(DataSet$GNI)

ggpairs(DataSet)

correlationMatrix <- cor(DataSet, method = "pearson", use = "complete.obs")
corrplot(correlationMatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


##### Detecção de multicolinearidade

dataSetWithoutDI <- DataSet[, 1:10]
omcdiag(dataSetWithoutDI,DataSet$DI)
imcdiag(dataSetWithoutDI,DataSet$DI)

##### Verificando a variável colinear de maior Rˆ2

bestRSquared <- 0
bestCollumnsIndexes
for (j in 1:5) {
  bestModel <-  lm(DI ~., data = DataSet[,c(11,j)])
  print(summary(bestModel))
  if((summary(bestModel)$adj.r.squared) > bestRSquared) {
    bestRSquared <- summary(bestModel)$adj.r.squared
    bestCollumnsIndexes <- j
  }
}

###### Removendo valiaveis colineares
# hdi <- DataSet$HDI

dataSetWithoutColinearity <- DataSet[,c(1,6:10)]
# dataSetWithoutColinearity$HDI <- hdi

View(dataSetWithoutColinearity)

#####nova deteccao de colinearidade
omcdiag(dataSetWithoutColinearity,DataSet$DI)
imcdiag(dataSetWithoutColinearity,DataSet$DI)

###### regressao sem variaveis colineares

model <- lm(DI ~ HDI + CPI + GINI, data = DataSet)
summary(model)
coef(model)
#### Y = -1.01 + 3.77 HDI + 0.06 CPI + 0.03 GINI

###### Regracao Linear Multipla - força bruta para identificar a melhor regressão

combination <- powerSet(1:10)
bestRSquared <- 0
bestCollumnsIndexes
for (j in 2:length(combination)) {
  columSet <- combination[[j]]
  bestModel <-  lm(DI ~., data = DataSet[,c(11,columSet)])
  if((summary(bestModel)$adj.r.squared) > bestRSquared) {
    bestRSquared <- summary(bestModel)$adj.r.squared
    bestCollumnsIndexes <- columSet
  }
}


bestRSquared
bestCollumnsIndexes
model <- lm(DI ~., data = DataSet[,c(11,bestCollumnsIndexes)])
summary(model)
coef(model)


#####nova deteccao de colinearidade
dataSetWithoutDI <- DataSet[, bestCollumnsIndexes]
omcdiag(dataSetWithoutDI,DataSet$DI)
imcdiag(dataSetWithoutDI,DataSet$DI)


#######removendo colinearidade
noColinearitydataSet <- DataSet[,c(11,bestCollumnsIndexes)]
noColinearitydataSet <- noColinearitydataSet[,-2]
omcdiag(noColinearitydataSet[,-1],DataSet$DI)
imcdiag(dataSetWithoutDI[,-1],DataSet$DI)


#### Executando o modelo
model <- lm(DI ~., data = noColinearitydataSet)
summary(model)
coef(model)

##### Analisando os resíduos
ols_plot_resid_qq(model)
