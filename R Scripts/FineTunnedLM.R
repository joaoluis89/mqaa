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


model <- lm(DI ~ HDI + CPI + GINI, data = DataSet)
shapiro.test(model$residuals)
summary(model)
coef(model)
plot(model)
predict(model, list(HDI = 0.825, CPI = 40, GINI = 42.7), interval = "conf")


