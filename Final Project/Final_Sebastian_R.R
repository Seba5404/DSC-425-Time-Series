getwd()
library(magrittr)
library(forecast)
library(fpp2)
library(ggplot2)
library(astsa)
library(tseries)
library(lmtest)


df = data


df$dt = as.Date(df$Date,format="%Y-%M")
#Create addtional grouping column for box plots in excel 


x = ts(df$AverageTemperature, start=c(1900,1),frequency = 12)
x
x %>% diff() %>% diff() %>% autoplot()
plot(x,xlab ='Year',ylab = 'Avg Temp',main = 'US Avg Temp') #data doesnt seem to be stationary, values all over the time 
plot(diff(x,12),xlab ='Year',ylab = 'Avg Temp',main = 'US Avg Temp') #take the difference of data to remove seasonilty at 12 
# Now we see that the data is falling close to the mean, indicated its a stationary model now 

min(x)
max(x)
x %>% autoplot()
boxplot(df~AverageTemerature~date)

#box plot to view decades mean temp 
boxplot(AverageTemperature~Decade,
        data=df,
        main="Different boxplots for each decade",
        xlab="Decade",
        ylab="Degree Celsius",
        col="blue",
        border="black"
)





hist(x) #data is not normually distributed 

hist(diff(x,12)) ### THis is fine to use With lag 12, we are able to get a somewhat normally distribution within the model
###hist(diff(diff(x)))
qqnorm(diff(x,12)) #qq plot  this is fine to use 
qqline(diff(x,12)) ##QQ plot line this is fine to use 


acf2(diff(x,1))
acf2(diff(x,12))


acf2(x) #not getting much info from the acf plots regarding the regular data 
acf2(diff(x,12)) 
#from this chart, possible 2 MA terms 
# from this chart possible 2 AR terms  

######Split the data into training and testing for model building 
xtrain= window(x,start=c(1900,1), end=c(1980,12), frequency = 12) #training set 
xtest= window(x,start=c(1981,1), end=c(2013,9), frequency = 12) #training set 

hist(diff(diff(xtrain,12))) ### THis is fine to use 
# We still have normal distribution 

qqnorm(diff(diff(xtrain,12))) #qq plot  this is fine to use 
qqline(diff(diff(xtrain,12))) ##QQ plot line this is fine to use 
#somewhat straigt line so this is good to proceed 

acf2(xtrain)
acf2(diff(xtrain,12))
acf2(diff(diff(xtrain,12)))
acf2(diff(diff(xtrain,12)))
autoplot((diff(diff(xtrain,12))))

#From ACF plot, seems we have lag 2 lags to consider
#From PACF plot, seems we have 3 to consider 


# use Ljung Box Test to hypothesis that toothpase weekly sales have significant serial correaltion 
Box.test(xtrain, lag=12, type= 'Ljung-Box')
#Fail to reject, since this is a seasonal model and each point depends on each other


#Uuse Auto Arima, to see if we can build a solid model 
#use BIC crietera 
m = auto.arima(xtrain,ic='aic',seasonal = T)
m

m3 
m = auto.arima(xtrain,ic='aic',seasonal = T)

xtrain %>% autoplot()

m

# lets create a Sarima model with lowest AIC 
m2 = sarima(x, p=2, d=0, q=2, P=2, D=1, Q=0, S = 12) # aic= 3278.75 #not a good model, p values all below .05 

z = sarima(xtrain, p=2, d=0, q=2, P=2, D=1, Q=0, S = 12) # aic= 3177.43
z = sarima(xtrain, p=2, d=0, q=2, P=1, D=1, Q=0, S = 12) # aic= 2957.81
z = sarima(xtrain, p=2, d=0, q=1, P=1, D=1, Q=0, S = 12) # aic= 2956.85
z = sarima(xtrain, p=1, d=0, q=1, P=1, D=1, Q=0, S = 12) # aic= 2954.53
z = sarima(xtrain, p=1, d=0, q=0, P=2, D=1, Q=0, S = 12) # aic= 2822.26 Pretty good, but p values are still below .05 
z = sarima(xtrain, p=1, d=1, q=1, P=1, D=1, Q=1, S = 12) # AIC = 2819.9  
z = sarima(xtrain, p=2, d=1, q=1, P=1, D=1, Q=1, S = 12) # AIC = 2605.5 
z = sarima(xtrain, p=2, d=1, q=1, P=1, D=1, Q=1, S = 12) # AIC = 2605.5 
###Final SARMIMA MODEL will be 

m1= Arima(xtrain, order = c(2,1,1), seasonal = list(order = c(1,1,1), period =12))

coeftest(m1)
#P term is not useful get rid of it 

z = sarima(xtrain, p=2, d=1, q=1, P=0, D=1, Q=1, S = 12) # AIC = 2603.5 Best model to be bult using the training test 
z


m1= Arima(xtrain, order = c(2,1,1), seasonal = list(order = c(0,1,1), period =12))

m1

#Residual checks 
m1$residuals %>% autoplot()
#Good Sign, looks like white noise, showing a zero mean 
m1$residuals %>% acf2()
hist(m1$residuals)
#Showing signs of a normal distrubtion
qqnorm(m1$residuals) #qq plot  this is fine to use 
qqline(m1$residuals) ##QQ plot line this is fine to use 
# most points are falling onto the line good sign 

sqrt(mean(m1$residuals^2))


#####Now that the model is built time to do forecasting on SARIMA model built 

y = forecast(m1,h=393)
plot(forecast(m1, h=400), include = 0) #plot a better view of time periods we are forecasting for 
lines(xtest)

plot(forecast(m1,h =393, newdata=as.data.frame(xtest)),include = 0)
lines(xtest)
y
y = forecast(m1,h =393, newdata=as.data.frame(xtest))

y$residuals

#RMSE
sqrt(mean(y$residuals^2))


library(Metrics)

rmse(xtest,y)

#####LTSM Model for US temp over the years 

lm_trend_lin = tslm(x~trend)
summary(lm_trend_lin)
autoplot(x) + autolayer(lm_trend_lin$fitted.values)

x %>% autoplot()
lm_trend_lin = tslm(x~trend)
summary(lm_trend_lin)

autoplot(x) + 
  autolayer(lm_trend_lin$fitted.values)

fcast = forecast(lm_trend_lin, h=12)

autoplot(x) + 
  autolayer(lm_trend_lin$fitted.values) +
  autolayer(fcast)


# Seasonality
lm_season <- tslm(x~season)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(x) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)


# Seasonality + Trend
lm_season <- tslm(x~trend+season)

summary(lm_season)

autoplot(x) + autolayer(lm_trend_lin$fitted.values)


fcast = forecast(lm_season, h=12)

autoplot(x) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

summary(lm_season)



######Split the data into training and testing for model building 
xtrain= window(x,start=c(1900,1), end=c(1980,12), frequency = 12) #training set 
xtest= window(x,start=c(1981,1), end=c(2013,9), frequency = 12) #training set 

xtrain= window(x,start=c(1900,1), end=c(1980,12), frequency = 12) #training set 
xtest= window(x,start=c(1981,1), end=c(2013,9), frequency = 12) #training set 

xtrain %>% head()

lm_trend_lin = tslm(xtrain~trend)
summary(lm_trend_lin)
autoplot(xtrain) + autolayer(lm_trend_lin$fitted.values)

xtrain %>% autoplot()
lm_trend_lin = tslm(x~trend)
summary(lm_trend_lin)



lm_season <- tslm(xtrain~trend+season)
summary(lm_season)

autoplot(xtrain) + autolayer(lm_season$fitted.values)


#fcast = forecast(lm_season, newdata=as.data.frame(xtest), h=12)
fcast = forecast(lm_season, newdata=as.data.frame(xtest))
#Run plot for fcast 
plot(forecast(lm_season, newdata=as.data.frame(xtest)), include = 0 )

#RMSE
sqrt(mean(lm_season$residuals^2))
mean(abs(lm_season$residuals))


y = forecast(m1,h=393)
plot(forecast(m1, h=393), include = 0) #plot a better view of time periods we are forecasting for 
lines(xtest)


autoplot(fcast)

summary(lm_season)
fcast = forecast(lm_season, newdata=xtest)
autoplot(fcast) +autolayer(xtest)


sqrt(mean(lm_season$residuals^2))
mean(abs(lm_season$residuals))
plot(x)
lines(lm_season$fitted.values,col = b)
?lines()



