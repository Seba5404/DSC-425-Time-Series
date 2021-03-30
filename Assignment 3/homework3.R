getwd()
library(magrittr)
library(forecast)
library(fpp2)
library(ggplot2)
library(astsa)

library(lmtest)

x = INDPRO
x$date
#set up date in the dataframe did this within R import function code was converting my dates to 2019 - 2020...


y = ts(x$rate, start=c(1998,1),frequency = 12)
y
y %>% autoplot()
plot(y,xlab ='Year',ylab = 'Rate',main = 'Industrial Production Index Rate over Year')

hist(y)
qqnorm(y) #qq plot 
qqline(y) ##QQ plot line
acf2(y)
acf2(diff(y))

plot(diff(y))

m = auto.arima(y,ic='bic',seasonal = F)
m

m$residuals

plot(m$residuals,type='l')

hist(m$residuals) #histogram 
qqnorm(m$residuals) #qq plot 
qqline(m$residuals) ##QQ plot line


Box.test(m$residuals, lag = 5,type = 'Ljung-Box')
summary(m$residuals)




#Write m2 model now 
m2 = sarima(y,1,0,3)
m2 = Arima(y,order =c(1,0,3))

m
coeftest(m2)
m2 = Arima(y,order = c(1,0,3), fixed = c(NA,NA,0,NA,NA)) #use this model based on coeffeicant tests that are significant 
#showed 3 lags for MA model go with this 
# only 3 varibles showed significant values 
#ACF plot slowly decays, meaning this is an MA model 

coeftest(m2)
hist(m2$residuals)
qqplot(m2$residuals)

plot(m2$residuals)

hist(m2$residuals) #histogram 
qqnorm(m2$residuals) #qq plot 
qqline(m2$residuals) ##QQ plot line


#### Forecasting for M1 and M2 models 

z = forecast(m2, h = 5, level = c(0.95, 0.95)) %>% plot()
lines(xtest)
z

getwd()

source("backtest_v2.R") #this will allow you to use the source function 
backtest(m,y,194,1)

backtest(m2,y,194,1)
