#Business Project    ||    Lorenzo Magni matr. 816114

#-----------------------------Data Summary--------------------------------------

#carico librerie
library(tseries)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(forecast)


#Carico dati stocks
getSymbols("AAPL",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))

getSymbols("BABA",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))

getSymbols("AMZN",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))

getSymbols("FCA.MI",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))

getSymbols("VOW.DE",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))

getSymbols("MSFT",src = "yahoo",
           from = as.Date("2017-09-29"), to = as.Date("2018-10-31"))


colnames(FCA.MI)[6] <- "PriceFCA.MI"
colnames(VOW.DE)[6] <- "PriceVOW.DE"
colnames(AAPL)[6] <- "PriceAAPL"
colnames(BABA)[6] <- "PriceBABA"
colnames(AMZN)[6] <- "PriceAMZN"
colnames(MSFT)[6] <- "PriceMSFT"

aapl.xts = (AAPL$PriceAAPL)
vow.de.xts = (VOW.DE$PriceVOW.DE)
msft.xts = (MSFT$PriceMSFT)
amzn.xts = na.approx(AMZN$PriceAMZN)
baba.xts = (BABA$PriceBABA)
fca.xts = (FCA.MI$PriceFCA.MI)

allPrice.xts = na.omit(cbind(aapl.xts, vow.de.xts, fca.xts, msft.xts, baba.xts, amzn.xts))
ChartSimple = ts(allPrice.xts)
plot(ChartSimple)
plot(allPrice.xts, col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"))
legend(x="topleft", legend=c("AAPL","VOW.DE","FCA.MI","MSFT","BABA","AMZN"),
       col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"), lty= 1:1 )

#---------------------------Descriptive_Analytics-------------------------------
#Conversione in mensile
aaplMonthly.xts = to.monthly(aapl.xts)
vow.deMonthly.xts = to.monthly(vow.de.xts)
fcaMonthly.xts = to.monthly(fca.xts)
msftMonthly.xts = to.monthly(msft.xts)
amznMonthly.xts = to.monthly(amzn.xts)
babaMonthly.xts = to.monthly(baba.xts)

colnames(aaplMonthly.xts)[4] <- "PriceAAPL"
aaplMonthly.xts = aaplMonthly.xts$PriceAAPL

colnames(vow.deMonthly.xts)[4] <- "PriceVOW.DE"
vow.deMonthly.xts = vow.deMonthly.xts$PriceVOW.DE

colnames(fcaMonthly.xts)[4] <- "PriceFCA.MI"
fcaMonthly.xts = fcaMonthly.xts$PriceFCA.MI

colnames(msftMonthly.xts)[4] <- "PriceMSFT"
msftMonthly.xts = msftMonthly.xts$PriceMSFT

colnames(babaMonthly.xts)[4] <- "PriceBABA"
babaMonthly.xts = babaMonthly.xts$PriceBABA

colnames(amznMonthly.xts)[4] <- "PriceAMZN"
amznMonthly.xts = amznMonthly.xts$PriceAMZN


#compute simple monthly returns
aaplSimpleMonthlyReturns.xts =
  CalculateReturns(aaplMonthly.xts , method="simple")
vow.deSimpleMonthlyReturns.xts =
  CalculateReturns(vow.deMonthly.xts , method="simple")
fcaSimpleMonthlyReturns.xts =
  CalculateReturns(fcaMonthly.xts , method="simple")
msftSimpleMonthlyReturns.xts =
  CalculateReturns(msftMonthly.xts , method="simple")
babaSimpleMonthlyReturns.xts =
  CalculateReturns(babaMonthly.xts , method="simple")
amznSimpleMonthlyReturns.xts =
  CalculateReturns(amznMonthly.xts , method="simple")

SMmerge = na.omit(cbind(aaplSimpleMonthlyReturns.xts,vow.deSimpleMonthlyReturns.xts,
                fcaSimpleMonthlyReturns.xts,msftSimpleMonthlyReturns.xts,
                babaSimpleMonthlyReturns.xts, amznSimpleMonthlyReturns.xts))

plot(SMmerge, col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"))
legend(x="topright", legend=c("AAPL","VOW.DE","FCA.MI","MSFT","BABA","AMZN"),
       col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"), lty=1:1)

#compute compounded monthly returns
aaplCompundedMonthlyReturns.xts =
  CalculateReturns(aaplMonthly.xts , method="compound")
vow.deCompundedMonthlyReturns.xts =
  CalculateReturns(vow.deMonthly.xts , method="compound")
fcaCompundedMonthlyReturns.xts =
  CalculateReturns(fcaMonthly.xts , method="compound")
msftCompundedMonthlyReturns.xts =
  CalculateReturns(msftMonthly.xts , method="compound")
babaCompundedMonthlyReturns.xts =
  CalculateReturns(babaMonthly.xts , method="compound")
amznCompundedMonthlyReturns.xts =
  CalculateReturns(amznMonthly.xts , method="compound")

CCMmerge = na.omit(cbind(aaplCompundedMonthlyReturns.xts,
                 vow.deCompundedMonthlyReturns.xts,
                 fcaCompundedMonthlyReturns.xts,
                 msftCompundedMonthlyReturns.xts,
                 babaCompundedMonthlyReturns.xts,
                 amznCompundedMonthlyReturns.xts))

plot(CCMmerge, col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"))
legend(x="bottomleft", legend=c("AAPL","VOW.DE","FCA.MI","MSFT","BABA","AMZN"),
       col=c("orange","cornflowerblue","chartreuse3","grey","red","violet"), lty=1:1)

#Diagnostic plots

#compunded_monthly
par(mfrow=c(2,3))

aapl.mat = coredata(aaplCompundedMonthlyReturns.xts)
hist(aapl.mat,main="Histogram of AAPL monthly returns",
     probability=TRUE, col="orange", breaks = 20)

vow.de.mat = coredata(vow.deCompundedMonthlyReturns.xts)
hist(vow.de.mat,main="Histogram of VOW.DE monthly returns",
     probability=TRUE, col="cornflowerblue", breaks = 20)

msft.mat = coredata(msftCompundedMonthlyReturns.xts)
hist(msft.mat,main="Histogram of MSFT monthly returns",
     probability=TRUE, col="grey", breaks = 20)

fca.mat = coredata(fcaCompundedMonthlyReturns.xts)
hist(fca.mat,main="Histogram of FCA.MI monthly returns",
     probability=TRUE, col="chartreuse3", breaks = 20)

baba.mat = coredata(babaCompundedMonthlyReturns.xts)
hist(baba.mat,main="Histogram of BABA monthly returns",
     probability=TRUE, col="red", breaks = 20)

amzn.mat = coredata(amznCompundedMonthlyReturns.xts)
hist(amzn.mat,main="Histogram of AMZN monthly returns",
     probability=TRUE, col="violet", breaks = 20)



#Smoothed density plots

#compunded_monthly
par(mfrow=c(2,3))

aapl.density = density(aapl.mat, na.rm=TRUE)
plot(aapl.density,type="l",xlab="returns", col="orange", lwd=2,
     ylab="density estimate",main="Smoothed AAPL histogram")

vow.de.density = density(vow.de.mat, na.rm=TRUE)
plot(vow.de.density,type="l",xlab="returns", col="cornflowerblue", lwd=2,
     ylab="density estimate",main="Smoothed VOW.DE histogram")

msft.density = density(msft.mat, na.rm=TRUE)
plot(msft.density,type="l",xlab="returns", col="grey", lwd=2,
     ylab="density estimate",main="Smoothed MSFT histogram")

fca.density = density(fca.mat, na.rm=TRUE)
plot(fca.density,type="l",xlab="returns", col="chartreuse3", lwd=2,
     ylab="density estimate",main="Smoothed FCA.MI histogram")

baba.density = density(baba.mat, na.rm=TRUE)
plot(baba.density,type="l",xlab="returns", col="red", lwd=2,
     ylab="density estimate",main="Smoothed BABA histogram")

amzn.density = density(amzn.mat, na.rm=TRUE)
plot(amzn.density,type="l",xlab="returns", col="violet", lwd=2,
     ylab="density estimate",main="Smoothed AMZN histogram")

#Boxplot plots

#compunded_monthly
par(mfrow=c(2,3))

boxplot(aapl.mat, main="AAPL boxplot",
        ylab="monthly AAPL return", col="orange")


boxplot(vow.de.mat, main="VOW.DE boxplot",
        ylab="monthly VOW.DE return", col="cornflowerblue")


boxplot(msft.mat, main="MSFT boxplot",
        ylab="monthly msft return", col="grey")


boxplot(fca.mat, main="FCA.MI boxplot",
        ylab="monthly FCA.MI return", col="chartreuse3")


boxplot(baba.mat, main="BABA boxplot",
        ylab="monthly BABA return", col="red")


boxplot(amzn.mat, main="AMZN boxplot",
        ylab="monthly AMZN return", col="violet")


#QQ Plots

#compunded_monthly
par(mfrow=c(2,3))

qqnorm(aapl.mat, main="AAPL", col="orange")

qqnorm(vow.de.mat, main="VOW.DE", col="cornflowerblue")

qqnorm(msft.mat, main="MSFT", col="grey")

qqnorm(fca.mat, main="FCA.MI.MI", col="chartreuse3")

qqnorm(baba.mat, main="BABA", col="red")

qqnorm(amzn.mat, main="AMZN", col="violet")


#boxplot over data merged
par(mfrow=c(1,1))
boxplot(cbind(aapl.mat,vow.de.mat,msft.mat,fca.mat,baba.mat,amzn.mat),col = blues9 )

#calcolo mean, varianza, sd, etc...
#mean
meanMerge = cbind(mean(aapl.mat, na.rm = TRUE),
                  mean(vow.de.mat, na.rm = TRUE),
                  mean(msft.mat, na.rm = TRUE),
                  mean(fca.mat, na.rm = TRUE),
                  mean(baba.mat, na.rm = TRUE),
                  mean(amzn.mat, na.rm = TRUE))

#rinominare prima riga e poi colonne, e non avviare le seguenti righe di codice insieme 
rownames(meanMerge)[1] <- "values"
colnames(meanMerge)[1] <- "meanAAPL"
colnames(meanMerge)[2] <- "meanVOW.DE"
colnames(meanMerge)[3] <- "meanMSFT"
colnames(meanMerge)[4] <- "meanFCA.MI"
colnames(meanMerge)[5] <- "meanBABA"
colnames(meanMerge)[6] <- "meanAMZN"


#variance
varMerge = cbind(var(aapl.mat, na.rm = TRUE),
                 var(vow.de.mat, na.rm = TRUE),
                 var(msft.mat, na.rm = TRUE),
                 var(fca.mat, na.rm = TRUE),
                 var(baba.mat, na.rm = TRUE),
                 var(amzn.mat, na.rm = TRUE))

rownames(varMerge)[1] <- "values"
colnames(varMerge)[1] <- "varAAPL"
colnames(varMerge)[2] <- "varVOW.DE"
colnames(varMerge)[3] <- "varMSFT"
colnames(varMerge)[4] <- "varFCA.MI"
colnames(varMerge)[5] <- "varBABA"
colnames(varMerge)[6] <- "varAMZN"

#standard deviation
sdMerge = cbind(sd(aapl.mat, na.rm = TRUE),
                sd(vow.de.mat, na.rm = TRUE),
                sd(msft.mat, na.rm = TRUE),
                sd(fca.mat, na.rm = TRUE),
                sd(baba.mat, na.rm = TRUE),
                sd(amzn.mat, na.rm = TRUE))

rownames(sdMerge)[1] <- "values"
colnames(sdMerge)[1] <- "sdAAPL"
colnames(sdMerge)[2] <- "sdVOW.DE"
colnames(sdMerge)[3] <- "sdMSFT"
colnames(sdMerge)[4] <- "sdFCA.MI"
colnames(sdMerge)[5] <- "sdBABA"
colnames(sdMerge)[6] <- "sdAMZN"

#skewness
skewnessMerge = cbind(skewness(aapl.mat, na.rm = TRUE),
                      skewness(vow.de.mat, na.rm = TRUE),
                      skewness(msft.mat, na.rm = TRUE),
                      skewness(fca.mat, na.rm = TRUE),
                      skewness(baba.mat, na.rm = TRUE),
                      skewness(amzn.mat, na.rm = TRUE))

rownames(skewnessMerge)[1] <- "values"
colnames(skewnessMerge)[1] <- "skewnessAAPL"
colnames(skewnessMerge)[2] <- "skewnessVOW.DE"
colnames(skewnessMerge)[3] <- "skewnessMSFT"
colnames(skewnessMerge)[4] <- "skewnessFCA.MI"
colnames(skewnessMerge)[5] <- "skewnessBABA"
colnames(skewnessMerge)[6] <- "skewnessAMZN"


#kurtosis
kurtosisMerge = cbind(kurtosis(aapl.mat, na.rm = TRUE),
                      kurtosis(vow.de.mat, na.rm = TRUE),
                      kurtosis(msft.mat, na.rm = TRUE),
                      kurtosis(fca.mat, na.rm = TRUE),
                      kurtosis(baba.mat, na.rm = TRUE),
                      kurtosis(amzn.mat, na.rm = TRUE))

rownames(kurtosisMerge)[1] <- "values"
colnames(kurtosisMerge)[1] <- "kurtosisAAPL"
colnames(kurtosisMerge)[2] <- "kurtosisVOW.DE"
colnames(kurtosisMerge)[3] <- "kurtosisMSFT"
colnames(kurtosisMerge)[4] <- "kurtosisFCA.MI"
colnames(kurtosisMerge)[5] <- "kurtosisBABA"
colnames(kurtosisMerge)[6] <- "kurtosisAMZN"


#quantile
quantileMerge = cbind(quantile(aapl.mat, na.rm = TRUE),
                      quantile(vow.de.mat, na.rm = TRUE),
                      quantile(msft.mat, na.rm = TRUE),
                      quantile(fca.mat, na.rm = TRUE),
                      quantile(baba.mat, na.rm = TRUE),
                      quantile(amzn.mat, na.rm = TRUE))

colnames(quantileMerge)[1] <- "quantileAAPL"
colnames(quantileMerge)[2] <- "quantileVOW.DE"
colnames(quantileMerge)[3] <- "quantileMSFT"
colnames(quantileMerge)[4] <- "quantileFCA.MI"
colnames(quantileMerge)[5] <- "quantileBABA"
colnames(quantileMerge)[6] <- "quantileAMZN"


#Sample covariance matrix
SCovMatr = var(cbind(aapl.mat, vow.de.mat, msft.mat, fca.mat, baba.mat, amzn.mat),
               na.rm = TRUE)
#Sample correlation matrix
SCorMatr = cor(cbind(aapl.mat, vow.de.mat, msft.mat, fca.mat, baba.mat, amzn.mat),
               use = "pairwise.complete.obs")



#plot pairwise scatterplot
pairs(cbind(aapl.mat, vow.de.mat, msft.mat, fca.mat, baba.mat, amzn.mat),
      col="blue", pch=18, cex=1.5, cex.axis=1.5, main = "Scatterplot Matrix")

#scatterplot of most correlated istruments
plot(fca.mat,vow.de.mat,
     main="Monthly cc returns on Fiat and Volkswagen",  pch=16, cex = 3,  col=c("orange", "cornflowerblue"))

plot(baba.mat,amzn.mat,
     main="Monthly cc returns on Alibaba and Amazon",  pch=16, cex = 3,  col=c("red", "chartreuse3"))

plot(aapl.mat,msft.mat,
     main="Monthly cc returns on Apple and Microsoft",  pch=16, cex = 3,  col=c("orange", "grey"))

#-------------------------Predictive analytics----------------------------------

#Carico dati stocks, questa volta dal 2007
getSymbols("AAPL",src = "yahoo",
           from = as.Date("2007-10-01"), to = as.Date("2019-01-31"))

getSymbols("BABA",src = "yahoo",
           from = as.Date("2014-10-01"), to = as.Date("2019-01-31"))

getSymbols("AMZN",src = "yahoo",
           from = as.Date("2007-10-01"), to = as.Date("2019-01-31"))

getSymbols("FCA.MI",src = "yahoo",
           from = as.Date("2007-10-01"), to = as.Date("2019-01-31"))

getSymbols("VOW.DE",src = "yahoo",
           from = as.Date("2007-10-01"), to = as.Date("2019-01-31"))

getSymbols("MSFT",src = "yahoo",
           from = as.Date("2007-10-01"), to = as.Date("2019-01-31"))


colnames(FCA.MI)[6] <- "PriceFCA.MI2007"
colnames(VOW.DE)[6] <- "PriceVOW.DE2007"
colnames(AAPL)[6] <- "PriceAAPL2007"
colnames(BABA)[6] <- "PriceBABA2007"
colnames(AMZN)[6] <- "PriceAMZN2007"
colnames(MSFT)[6] <- "PriceMSFT2007"

aapl2007.xts = AAPL$PriceAAPL2007
vow.de2007.xts = VOW.DE$PriceVOW.DE2007
msft2007.xts = MSFT$PriceMSFT2007
amzn2007.xts = AMZN$PriceAMZN2007
baba2007.xts = BABA$PriceBABA2007
fca2007.xts = FCA.MI$PriceFCA.MI2007

#Conversione in mensile
aaplMonthly2007.xts = to.monthly(aapl2007.xts)
vow.deMonthly2007.xts = to.monthly(vow.de2007.xts)
fcaMonthly2007.xts = to.monthly(fca2007.xts)
msftMonthly2007.xts = to.monthly(msft2007.xts)
amznMonthly2007.xts = to.monthly(amzn2007.xts)
babaMonthly2007.xts = to.monthly(baba2007.xts)

aapl2007.z= as.zoo(aaplMonthly2007.xts)
vow.de2007.z= as.zoo(vow.deMonthly2007.xts)
fca2007.z= as.zoo(fcaMonthly2007.xts)
msft2007.z= as.zoo(msftMonthly2007.xts)
baba2007.z= as.zoo(babaMonthly2007.xts)
amzn2007.z= as.zoo(amznMonthly2007.xts)

#APPL forecasting
fit <- stl( aapl2007.z[,1], s.window="period" )
returns <- diff( log(aapl2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for AAPL returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus )


#VOW.DE forecasting
t <- stl( vow.de2007.z[,1], s.window="period" )
returns <- diff( log(vow.de2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for VOW.DE returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus )


#FCA.MI forecasting
t <- stl( fca2007.z[,1], s.window="period")
returns <- diff( log(fca2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order= c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for FCA.MI returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus)


#msft forecasting
t <- stl( msft2007.z[,1], s.window="period")
returns <- diff( log(msft2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for MSFT returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus )



#BABA forecasting
#Alibaba è quotato da "poco" tempo, di conseguenza il forecasting non funziona alla
#perfezione. Penso sia dovuto al fatto che non ha una grande quantità di dati da analizzare.

t<- stl( baba2007.z[,1], s.window="period" )
returns <- diff( log(baba2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for BABA returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus )

#AMZN forecasting
t <- stl( amzn2007.z[,1], s.window="period" )
returns <- diff( log(amzn2007.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for AMZN returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest, col=redfocus )

#-----------------------------Calcolo della Beta----------------------------
#AAPL BETA
start_date <- "2007-10-01"
aapl <- getSymbols("AAPL", from = start_date, auto.assign = F)
sp500 <- getSymbols("^GSPC", from = start_date, auto.assign = F)

r<-function(x) {m<-to.monthly(x[,6])[,4];diff(m)/lag(m)}

coef(lm(r(aapl)[2:37]~r(sp500)[2:37]))

#BABA BETA
start_date <- "2007-10-01"
baba <- getSymbols("BABA", from = start_date, auto.assign = F)
sp500 <- getSymbols("^GSPC", from = start_date, auto.assign = F)

coef(lm(r(baba)[2:37]~r(sp500)[2:37]))

#AMZN BETA
start_date <- "2007-10-01"
amzn <- getSymbols("AMZN", from = start_date, auto.assign = F)
sp500 <- getSymbols("^GSPC", from = start_date, auto.assign = F)

coef(lm(r(amzn)[2:37]~r(sp500)[2:37]))

#FCA.MI BETA
start_date <- "2007-10-01"
fca.mi <- getSymbols("FCA.MI", from = start_date, auto.assign = F)
ftsemib.mi <- getSymbols("FTSEMIB.MI", from = start_date, auto.assign = F)
#Utilizzo na.omit() per aggirare il problema dei valori mancanti, segnalato in console.
ftsemib.mifill <- na.omit(ftsemib.mi)
coef(lm(r(fca.mi)[2:37]~r(ftsemib.mifill)[2:37]))

#VOW.DE BETA
start_date <- "2007-10-01"
vow.de <- getSymbols("VOW.DE", from = start_date, auto.assign = F)
gdaxi <- getSymbols("^GDAXI", from = start_date, auto.assign = F)
#Utilizzo na.omit() per aggirare il problema dei valori mancanti, segnalato in console.
gdaxi_fill <- na.omit(gdaxi)
coef(lm(r(vow.de)[2:37]~r(gdaxi_fill)[2:37]))

#MSFT BETA
start_date <- "2007-10-01"
msft <- getSymbols("MSFT", from = start_date, auto.assign = F)
sp500 <- getSymbols("^GSPC", from = start_date, auto.assign = F)

coef(lm(r(msft)[2:37]~r(sp500)[2:37]))

#--------------------------Portfolio Management-----------------------------

# Create an optimized portfolio of returns
opt <- portfolio.optim((SMmerge))
head(opt)

# Create pf_weights
pf_weights <- opt$pw

#Create dataframe to print the names
df <- data.frame(matrix(ncol = 6, nrow = 1))
x <- c("Apple", "Volkswagen", "Fiat",
       "Microsoft", "Alibaba", "Amazon")
colnames(df) <- x

# Assign asset names
names(pf_weights) <- colnames(df)
head(pf_weights)

# Select optimum weights opt_weights
opt_weights <- pf_weights[pf_weights >= 0.01] #How is 0.01 determined?

#Barplot of opt_weights
barplot((opt_weights), main= "Weights of my portfolio",xlab="Stocks",
        ylab="Weight", col = blues9 )

# Creo il mio portafoglio virtuale in base all'analisi degli ultimi l mesi.
# Mi propongo di avere un budget di 5000 euro.
# Investo in base ai pesi calcolati con il modello mean-variance di Markovitz.
# Riporto per comodità i pesi in percentuale:
# Apple: 43,2% | Volkswagen: 49,3% | Fiat: 1,4% | Microsoft: 1% | Alibaba: 5% | Amazon: 0%
# Di conseguenza, tradotto in denaro:
# Apple: €2160 | Volkswagen: €2465 | Fiat: €70 | Microsoft: €50 | Alibaba: €250 | Amazon: €0

budget<- 5000
price_buy<- allPrice.xts["2018-01-02", drop = FALSE, which.i=FALSE]
price_sell<- allPrice.xts["2018-10-30", drop = FALSE, which.i=FALSE]
print(price_buy)
print(price_sell)

price_buy.matrix = data.matrix(as.data.frame(price_buy))
price_sell.matrix = data.matrix(as.data.frame(price_sell))
p <- function() {budget * pf_weights}
q <- function() {p() / price_buy.matrix}
floor(q())
z <- floor(q())
price_now <- function() {price_sell.matrix * z}
price_now_v <- price_now()
price_init <- function() {price_buy.matrix * z}
price_init_v <- price_init()

#ciclo per calcolare il totale iniziale
sum_init = 0
for (i in 1:6) {
  if (price_now_v[1,i] > 0){
    sum_init = sum_init + price_init_v[1,i]
    next
      sum_init = sum_init
  }
}
print(sum_init)

#ciclo per calcolare il totale finale
sum_final = 0
for (i in 1:6) {
  if (price_now_v[1,i] > 0){
    sum_final= sum_final + price_now_v[1,i]
    next
    sum_final = sum_final
  }
}
print(sum_final)

#calcolo il ritorno effettivo lordo
final_return <- (sum_final - sum_init)

#calcolo costi transazione acquisto
sum_trans_init = 0
for (i in 1:6) {
  if (price_init_v[1,i] > 0){
    sum_trans_init= sum_trans_init + (price_init_v[1,i] * 0.005)
    next
    sum_trans_init = sum_trans_init
  }
}
print(sum_trans_init)

#calcolo costi transazione vendita
sum_trans_final = 0
for (i in 1:6) {
  if (price_now_v[1,i] > 0){
    sum_trans_final= sum_trans_final + (price_now_v[1,i] * 0.005)
    next
    sum_trans_final = sum_trans_final
  }
}
print(sum_trans_final)

tot_trans <- sum_trans_init + sum_trans_final
print(tot_trans)

#calcolo il guadagno netto totale
net_return <- (final_return - tot_trans)
print (net_return)

 #---------------------------------END-------------------------------------------