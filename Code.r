#Load Libraries 

library(readxl)
library(AER)
library(tseries)
library(dynlm)
library(strucchange)
library(stats)
library(zoo)
library(ggplot2)
library(dplyr)

#Load The Data

Data <- read.csv('C:/Users/AG/Desktop/sudan/Central Bank of Sudan Balance Sheet.csv')

Data <- subset(Data, select = c(-1))

Data <- Data[,1:12]

View(Data)

#Basic Statistics 

summary(Data)

var(Data)

#Data That I Will Work On It

Use <- data.frame(rowSums(Data))

use <- ts(Use, start = 2012, frequency = 1)

View(use)

#Plot The Data

plot(use)

#Test The Data

acf(diff(diff(use)), plot = FALSE)

pacf(diff(use), plot = FALSE)

use_par <- expand.grid(ar = 0:2, diff = 1, ma = 0:2, sar = 0:1, sdiff = 1, sma = 0:1)
use_aic <- rep(0, nrow(use_par))
for (i in seq(along = use_aic))
  use_aic[i] <- AIC(arima(use, unlist(use_par[i, 1:3]), unlist(use_par[i, 4:6])), k = log(length(use)))
use_par[which.min(use_aic),]

#ARIMA Model

use_arima <- arima(use, order = c(1, 1, 0), seasonal = c(0, 1, 0))

use_arima

tsdiag(use_arima)

#Prediction

use_pre <- predict(use_arima, n.ahead = 18*4)

#Unit Root Test

adf.test(use)

adf.test(log(use))

adf.test(diff(log(use)), k=0)

pp.test(diff(log(use)))

#Stationary Test

kpss.test(diff(log(use)))

#Cointegration Test

po.test(log(use))

#The Model

Model <- dynlm(use ~ L(use) + L(use, 4), data = use)

summary(Model)

#Heteroscedasticity Test

bptest(Model)

#Serialcorrelation

bgtest(Model)

dwtest(Model)

