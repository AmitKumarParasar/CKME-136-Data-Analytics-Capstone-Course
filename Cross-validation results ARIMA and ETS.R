library('ggplot2')
library('forecast')
library('tseries')
library('magrittr')
library('dplyr')
library("reshape") # for cast and melt
library("data.table")
library("lubridate")

#S1D1 data was extracted from larger WalMart dataset
#Provided in R workspace S1D1 ARIMA and ETS.Rdata

sales_ts = ts(s1d1[, c('Weekly_Sales')])
s1d1$clean_sales = tsclean(sales_ts)

#Summary of Raw and Clean data
summary(s1d1$Weekly_Sales)
summary(s1d1$clean_sales)

#Auto ARIMA Diagnostics on Raw Data
faarima <- function(x, h){forecast(auto.arima(x), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), faarima, h=1)
sum(is.na(e)) #NA count
mean(abs(e),na.rm = TRUE) #MAE
mean(e^2, na.rm = TRUE) #MSE
sqrt(mean(e^2, na.rm=TRUE)) #RMSE
sqrt(mean(residuals(faarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE)) #RMSE Residuals
var(as.numeric(e), na.rm = TRUE) #Error variance

#Auto ARIMA Diagnostics on Cleaned Data
faarima <- function(x, h){forecast(auto.arima(tsclean(ts(x))), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), faarima, h=1)
sum(is.na(e)) #NA count
mean(abs(e),na.rm = TRUE) #MAE
mean(e^2, na.rm = TRUE) #MSE
sqrt(mean(e^2, na.rm=TRUE)) #RMSE
sqrt(mean(residuals(faarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE)) #RMSE Residuals
var(as.numeric(e), na.rm = TRUE) #Error variance

#Auto ARIMA Diagnostics on Raw Data searching more models
auto.arima(s1d1$Weekly_Sales, approximation = FALSE, stepwise = FALSE)

#Auto ARIMA Diagnostics on Cleaned Data
auto.arima(s1d1$clean_sales, approximation = FALSE, stepwise = FALSE)

#Arguments to expand model search still returned the same results as auto-arima() default.


#MANUAL ARIMA
#RAW DATA
#Manual ARIMA Diagnostics on Raw Data 1,1,1
Arima(s1d1$Weekly_Sales, order = c(1,1,1))
fmarima <- function(x, h){forecast(arima(x, order = c(1,1,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 3,0,1
Arima(s1d1$Weekly_Sales, order = c(3,0,1))
fmarima <- function(x, h){forecast(arima(x, order = c(3,0,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 2,0,1
Arima(s1d1$Weekly_Sales, order = c(2,0,1))
fmarima <- function(x, h){forecast(arima(x, order = c(2,0,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 2,1,1
Arima(s1d1$Weekly_Sales, order = c(2,1,1))
fmarima <- function(x, h){forecast(arima(x, order = c(2,1,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 3,0,2
Arima(s1d1$Weekly_Sales, order = c(3,0,2))
fmarima <- function(x, h){forecast(arima(x, order = c(3,0,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 2,0,2
Arima(s1d1$Weekly_Sales, order = c(2,0,2))
fmarima <- function(x, h){forecast(arima(x, order = c(2,0,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 1,0,2
Arima(s1d1$Weekly_Sales, order = c(1,0,2))
fmarima <- function(x, h){forecast(arima(x, order = c(1,0,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 3,1,2
Arima(s1d1$Weekly_Sales, order = c(3,1,2))
fmarima <- function(x, h){forecast(arima(x, order = c(3,1,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 2,1,2
Arima(s1d1$Weekly_Sales, order = c(2,1,2))
fmarima <- function(x, h){forecast(arima(x, order = c(2,1,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 1,1,2
Arima(s1d1$Weekly_Sales, order = c(1,1,2))
fmarima <- function(x, h){forecast(arima(x, order = c(1,1,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 3,2,2
Arima(s1d1$Weekly_Sales, order = c(3,2,2))
fmarima <- function(x, h){forecast(arima(x, order = c(3,2,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 2,2,2
Arima(s1d1$Weekly_Sales, order = c(2,2,2))
fmarima <- function(x, h){forecast(arima(x, order = c(2,2,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Raw Data 1,2,2
Arima(s1d1$Weekly_Sales, order = c(1,2,2))
fmarima <- function(x, h){forecast(arima(x, order = c(1,2,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#CLEANED DATA
#Manual ARIMA Diagnostics on Cleaned Data 2,1,2
Arima(s1d1$clean_sales, order = c(2,1,2))
fmarima <- function(x, h){forecast(arima(tsclean(ts(x)), order = c(2,1,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))
var(as.numeric(e), na.rm = TRUE)

#Manual ARIMA Diagnostics on Cleaned Data 2,1,1
Arima(s1d1$clean_sales, order = c(2,1,1))
fmarima <- function(x, h){forecast(arima(tsclean(ts(x)), order = c(2,1,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))
var(as.numeric(e), na.rm = TRUE)

#Manual ARIMA Diagnostics on Cleaned Data 2,0,1
Arima(s1d1$clean_sales, order = c(2,0,1))
fmarima <- function(x, h){forecast(arima(tsclean(ts(x)), order = c(2,1,1)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))
var(as.numeric(e), na.rm = TRUE)

#ETS Diagnostics on Raw Data
ets(s1d1$Weekly_Sales)
fets <- function(x, h) {forecast(ets(x), h = h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fets, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))
var(as.numeric(e), na.rm = TRUE)