####Code for Problem 2 

getwd()
library(magrittr)
library(forecast)
library(fpp2)
library(ggplot2)
library(astsa)
# Read in the crude dataset
#oilData <- read.csv(file="crudeoil_w0416.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
#oilData$date = as.Date(oilData$date,format="%d-%B-%y")


df = NAPM
df %>% head()

df
str(df)

x = ts(df$index, start=c(1980,1),frequency = 12)
str(x)
frequency(x)

autoplot(x)
diff(x) %>% autoplot()
x %>% acf()
x %>% pacf()
# PLOT is showing an AR model 
# After 3 lags 
x %>% acf2()
x %>% diff() %>% acf2()
#               p, d, q
m = Arima(x,order =c(3,0,0))

sarima(x,3,0,0)

library(lmtest)

coeftest(m)


plot(m$resid, type='l') #residual plot
acf(m$resid) #correlogram of residuals
plot(m$residuals,type='l')

hist(m$residuals) #histogram 
qqnorm(m$residuals) #qq plot 
qqline(m$residuals) ##QQ plot line


Box.test(m$residuals, lag = 10,type = 'Ljung-Box')
summary(m$residuals)




m2 = Arima(x,order = c(3,0,0), fixed = c(NA,0,NA,NA))
m2
coeftest(m2)




m2$x
m2 %>% forecast(h=5) %>% autoplot() #How to run forecast within time series 
?forecast

xtr= window(x,start=1980, end=c(2014,12)) #set up to end onf Dec 2014 
xtest = window(x, start=2015) #start at 2015 Jan 
xtest


coeftest(m2)
y = forecast(m2,h=5) %>% plot()
lines(xtest)

y # print out results 

#now forecast 10 months
y = forecast(m,h=10) %>% plot()
lines(xtest)
y 
y = forecast(m, h = 12, level = c(0.95, 0.95)) %>% plot()
lines(xtest)

y

#xt = -1.05xt-1 + .04xt-2 - 0.19xt-3

plot(m)

51.43*(1-1.07-.16)



###### CODE for problem 3 

tpaste <- read.csv(file="groceries.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
tpaste = groceries
tpaste$Date = as.Date(tpaste$Date,format="%d-%B-%y")

#plot the time series data 
tpasteTS <- ts(tpaste$ToothPaste, start=c(2008,1), frequency=(52))
plot(tpasteTS,xlab ='Year',ylab = 'Price',main = 'ToothPaste Price over Year')

tpasteTS

autoplot(tpasteTS)
tpasteTS %>% diff() %>% diff() %>% acf2()
z = diff(tpasteTS,lag = 52)
z
z %>% autoplot()
diff(tpasteTS) %>% acf2()
plot(diff(diff(tpasteTS)))
tpasteTS %>% diff() %>% diff()%>%acf2()
?diff()
tpasteTS %>% acf2()
#Seasonality seems to be a factor within this model, going to take the difference to remove the seasonility 

tpasteTS %>% diff(lag=52) %>% acf(lag.max = 52)
tpasteTS %>% diff() %>% pacf(lag.max = 52)
?acf

z = diff(tpasteTS) #go with model difference of 1 


#               p, d, q
m = Arima(z,order =c(1,0,0))
m = Arima(tpasteTS,order =c(2,0,0)) #### Final Model 

coeftest(m)
plot(m$residuals, type='l') #residual plot
m = sarima(tpasteTS,2,1,0) #use for charts 
m = sarima(tpasteTS,2,0,0) #use for charts 
m
coeftest(m)
m$residuals
acf2(m$residuals)
m$residuals %>% autoplot()
m = sarima(z,3,0,0) #use for charts 
hist(m$residuals) #histogram 
qqnorm(m$residuals) #qq plot 
qqline(m$residuals) ##QQ plot line

library(tseries)

jarque.bera.test(m$residuals)
Box.test(m$residuals, lag = 5,type = 'Ljung-Box')


coeftest(m)
summary(m)

m = Arima(tpasteTS,order =c(2,0,0)) #### Final Model 
summary(m)
219.40*(1-.63-.26) 

##Forecast 

x =tpasteTS
m$x
x
m %>% forecast(h=5) %>% autoplot() #How to run forecast within time series 
?forecast

xtr= window(x,start=2008,1, end=c(2008,47)) #set up to end on week 47 so we predict next 4 weeks 
xtest = window(x, start=2008,48) #start at 2015 Jan 
xtest


coeftest(m2)
y = forecast(m,h=5) %>% plot()
lines(xtest)

y # print out results 

#now forecast 10 months
y = forecast(m,h=10) %>% plot()
lines(xtest)
y 
y = forecast(m, h = 5, level = c(0.95, 0.95)) %>% plot()
lines(xtest)
y

