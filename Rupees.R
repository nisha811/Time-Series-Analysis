############################################ARIMA MODELLING######################################################
library(xts)
library(tseries)
library(astsa)
library(fpp2)
library(forecast)

df<-read.csv('C:/Users/nisha/Desktop/USD INR Historical Data.csv', sep='\t')
df

is.ts(df)

#keeping only the first two columns
rupees <- df[2]

#inverting the series upside down
rupees <- rupees[nrow(rupees):1,]

#convert to TS
rupees<- ts(rupees,frequency=12, start=c(2008,03), end=c(2018,03))

#split to test and train
tr_rupees <- window(rupees, start=c(2008,08), end=c(2017,09))
te_rupees <- window(rupees, start=c(2017,10), end=c(2018,03))

#1. Plot the graph
plot.ts(tr_rupees)
plot.ts(log(tr_rupees))
plot.ts(diff(log(tr_rupees))) #since it is seasonal
plot.ts(diff(diff(log(tr_rupees),12)))

#2.check for stationarity

adf.test(diff(diff(log(tr_rupees),12)))

auto.arima(log(tr_rupees)) #suggests that it is 0,1,0

#3.Check for acf and pacf

acf2(diff(diff(log(tr_rupees),12)))

#seasonal part
 #acf is tailing off
 #pacf is cutting off at 2

#non seasonal part
 #acf is tailing - AR1
 #pacf is absent

#4.Fit models

auto.arima(log(tr_rupees))

fit_1<-sarima(log(tr_rupees), p=0, d=1, q=0, P=0, D=1, Q=1, S=12)

fit_5 <-sarima(log(tr_rupees), p=1,d=1,q=1,P=1,D=1,Q=2,S=12)
fit_6 <-sarima(log(tr_rupees), p=0,d=1,q=0,P=2,D=1,Q=1,S=12)#lowest


fit_1$ttable
fit_2$ttable
fit_3$ttable
fit_4$ttable
fit_5$ttable
fit_6$ttable
fit_7$ttable

fit_1$AIC
fit_6$AIC #low
fit_5$AIC

fit_5$BIC
fit_6$BIC #low


#5.Forecast

prediction <- sarima.for(log(tr_rupees), n.ahead=6, p=0,d=1,q=0,P=2,D=1,Q=1,S=12)
lines(log(te_rupees), col="blue")

#predict
next_val <- prediction$pred[1:6]
next_val
exp(next_val)

accuracy(exp(next_val),te_rupees) 

####################################EXPONENTIAL SMOOTHING##################################################################
#split to test and train
tr_rupees <- window(rupees, start=c(2012,01), end=c(2017,09))
te_rupees <- window(rupees, start=c(2017,10), end=c(2018,03))


#1. Plot the data

plot.ts(tr_rupees)

#Identify the pattern

  #multiplicative seasonality and linear trend in the graphs 

fit <- hw(tr_rupees, seasonal = "multiplicative")

summary(fit)

checkresiduals(fit)

#Step 4
#forecast
forecast_fit<-forecast(fit,h=6)

#autoplot(forecast_fit)
plot(forecast_fit)
lines(te_rupees,col="red")
accuracy(forecast_fit,te_rupees) 


######################################PROPHET###########################################################################

library(prophet)
library(dplyr)
library(lubridate)

df1<-read.csv('C:/Users/nisha/Desktop/USD INR Historical Data.csv', sep='\t')

rupees <- df1[1:2]

#inverting the series upside down
rupees <- rupees[nrow(rupees):1,]

rupees$Month <- sapply(rupees$Date, function(Month)paste("01-",Month,sep=""))

rupees$Date <- format(strptime(rupees$Month, format = "%d-%y-%b"), "%Y-%m-%d")

head(rupees)

rupees <- rupees[1:2]

colnames(rupees) <- c("ds", "y")

#test and train

df_train <- rupees[51:115, ]
df_test <- rupees[116:121, ]

m <- prophet(df_train)

future <- make_future_dataframe(m, periods = 6, freq = "month")
forecast <- predict(m, future)
plot(m, forecast, col='red')

prophet_plot_components(m, forecast)
tail(forecast)
df_test
