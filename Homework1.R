# Read in the crude dataset
oilData <- read.csv(file="crudeoil_w0416.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
oilData$date = as.Date(oilData$date,format="%d-%B-%y")

#oilTimeSeries <- ts(oilData, frequency=365.25/7, start=c(2004,1))
#plot(oilTimeSeries)


#plot the time series data 
newTS <- ts(oilData$price, start=c(2004,1), frequency=(365.25/7))
plot(newTS,xlab ='Year',ylab = 'Price',main = 'Spot Prices Over Years')


#create an empty column for rate 
oilData$rate<-NA

#Calculate the rate of change per week 
N = 627
total = 0
for (i in 1:1){
  oilData$rate = 0
} 

for (i in 2:N){
  oilData$rate[i] = ((oilData$price[i] - oilData$price[i-1])/oilData$price[i-1])
} 

#Question 3 Analzye the distribution rate of using a histogram and normal qunatile plot  
hist(oilData$rate)
qqnorm(oilData$rate)
qqline(oilData$rate)


#Question 4/5 test null hypothesis of perfect symenty for distribution of rate at 5% signifiance level 
?library(moments)
?moments
skewness(oilData$rate)
kurtosis(oilData$rate)
summary(oilData$rate)

#Skewness Test 
skew_test = skewness(oilData$rate)/sqrt(6/length(oilData$rate))
skew_test

#p=value
2* (1-pnorm(abs(skew_test)))

#kurtosis_test
kurtosis_test = (kurtosis(oilData$rate)-3)/sqrt(24/length(oilData$rate))
2* (1-pnorm(abs(kurtosis_test)))

#Question 6

library(tseries)
jarque.bera.test(oilData$rate)


#question 7 plot time series of rate 

#plot the time series rate data 
rateTS <- ts(oilData$rate, start=c(2004,1), frequency=(365.25/7))
plot(rateTS,xlab ='Year',ylab = 'Rate',main = 'Rate Prices Over Years')


#8 compute and plot the first 15 lags of the autocorrelation function and discuss if the sereis shows evidence of serial correction
?acf
(acf(oilData$rate,lag.max = 14, type="correlation"))





###Part 2 
#Question 3 plot time series of tooth paste sales 
tpaste <- read.csv(file="groceries.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
tpaste$Date = as.Date(tpaste$Date,format="%d-%B-%y")

#plot the time series data 
tpasteTS <- ts(tpaste$ToothPaste, start=c(2008,1), frequency=(52))
plot(tpasteTS,xlab ='Year',ylab = 'Price',main = 'ToothPaste Price over Year')

?ts

#Question 1 Analzye the distribution rate of using a histogram and normal qunatile plot  
hist(tpaste$ToothPaste)
qqnorm(tpaste$ToothPaste)
qqline(tpaste$ToothPaste)

#Question 2 test for normliatiy for the distribution of Toothpase using jarque Bera Test at 5% significance 

jarque.bera.test(tpaste$ToothPaste)

#8 compute and plot the first 15 lags of the autocorrelation function and discuss if the sereis shows evidence of serial correction
?acf
(acf(tpaste$ToothPaste,lag.max = 14, type="correlation"))

# use Ljung Box Test to hypothesis that toothpase weekly sales have significant serial correaltion 
Box.test(tpaste$ToothPaste, lag=10, type= 'Ljung-Box')


