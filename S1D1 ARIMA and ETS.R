library('ggplot2')
library('forecast')
library('tseries')
library('magrittr')
library('dplyr')
library("reshape")
library("data.table")
library("lubridate")

#S1D1 data was extracted from larger WalMart dataset
#Provided in R workspace S1D1 ARIMA and ETS.Rdata

#Plot data S1D1
s1d1plot<-ggplot(s1d1, aes(x = Date, y = Weekly_Sales)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  labs(title="Weekly Sales Store 1 Dept 1", y="Weekly Sales", x="Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(s1d1plot)

#Visualizing cleaned time series data versus actual S1D1
sales_ts = ts(s1d1[, c('Weekly_Sales')])
s1d1$clean_sales = tsclean(sales_ts)

ggplot() +
  geom_line(data = s1d1, aes(x = Date, y = Weekly_Sales, colour = "Weekly Sales"), size=1) +
  geom_line(data = s1d1, aes(x = Date, y = clean_sales,   colour = "Clean Sales"), size=1.5)  +
  ylab('') +
  labs(title="S1D1 Cleaned Versus Raw Weekly Sales")

#Decomposition not recognizing periodicity
#With periodic setting
stl(s1d1$Weekly_Sales, s.window = "periodic")
stl(s1d1$clean_sales, s.window = "periodic")
#With week approimxation
stl(s1d1$Weekly_Sales, s.window = 365.25/7)
stl(s1d1$clean_sales, s.window = 365.25/7)

#Check for stationarity
adf.test(s1d1$Weekly_Sales, alternative = "stationary") # p-value = 0.03122
#A small p-value (typically ??? 0.05) indicates strong evidence against the null hypothesis,
#so you reject the null hypothesis.
#Data IS stationary

adf.test(s1d1$clean_sales, alternative = "stationary") # p-value = 0.4131
#A large p-value (> 0.05) indicates weak evidence against the null hypothesis,
#so you fail to reject the null hypothesis.
#Data IS NOT stationary

adf.test(diff(s1d1$clean_sales, differences = 1), alternative = "stationary")
#Differencing once improves p-value to 0.01
#This suggests I in ARIMA should be set to 1
#on cleaned data in order to compensate for
#it's non-stationarity.

#Raw data is more stationary than Clean data with p-value = 0.03122 versus p-value = 0.4131

#Auto-correlations
#Raw data
par(mfrow=c(2,1))
Acf(s1d1$Weekly_Sales, main='Acf of Raw Data')
Pacf(s1d1$Weekly_Sales, main='Pacf of Raw Data')

#Cleaned data
par(mfrow=c(2,1))
Acf(s1d1$clean_sales, main='Acf of Cleaned Data')
Pacf(s1d1$clean_sales, main='Pacf of Cleaned Data')

#Cleaned data difference order 1
par(mfrow=c(2,1))
Acf(diff(s1d1$clean_sales, differences = 1), main='Acf of 1st Order Differenced Cleaned Data')
Pacf(diff(s1d1$clean_sales, differences = 1), main='Pacf of 1st Order Differenced Cleaned Data')


#Auto ARIMA Model Fit on Raw Data
#Results of Model
fit<-auto.arima(s1d1$Weekly_Sales, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='Raw Data Model Residuals Auto ARIMA 1,0,1')
print(fit)
#Forecast 13 Weeks
fcast <- forecast(fit, h=13)
plot(fcast)

#Auto ARIMA Model Fit on Cleaned Data
#Results of Model
fit_clean<-auto.arima(s1d1$clean_sales, seasonal=FALSE)
tsdisplay(residuals(fit_clean), lag.max=45, main='Cleaned Data Model Residuals Auto ARIMA 2,0,0')
print(fit_clean)
#Forecast 13 Weeks
fcast_clean <- forecast(fit_clean, h=13)
plot(fcast_clean)

#Auto ARIMA Hold Out Method on Raw Data
hold <- window(ts(s1d1$Weekly_Sales), start=130)
fit_no_holdout = auto.arima(ts(s1d1$Weekly_Sales[-c(130:143)]), seasonal = FALSE)
fcast_no_holdout <- forecast(fit_no_holdout,h=13)
plot(fcast_no_holdout, main="13 Week Hold Out for Raw Data Auto ARIMA 1,0,1",
     sub="")
lines(ts(s1d1$Weekly_Sales), col="red")

#Auto ARIMA Hold Out Method on Cleaned Data
hold_clean <- window(ts(s1d1$clean_sales), start=130)
fit_no_holdout_clean = auto.arima(ts(s1d1$clean_sales[-c(130:143)]), seasonal = FALSE)
fcast_no_holdout_clean <- forecast(fit_no_holdout_clean,h=13)
plot(fcast_no_holdout_clean, main="13 Week Hold Out for Clean Data Auto ARIMA 2,0,0",
     sub="", ylim=c(0, 60000))
lines(ts(s1d1$Weekly_Sales), col="red")

#Raw Data appears to flatline faster but captures the uptick in sales early

#Manual ARIMA Model Fit on Raw Data
##NOTE: Only best performing ARIMA models based on lowest error variance are included here
##For all ARIMA models tested, see Cross-validation results 100% script
#Results of Model
fit<-Arima(s1d1$Weekly_Sales, order = c(2,1,2))
tsdisplay(residuals(fit), lag.max=45, main='Raw Data Model Residuals ARIMA 2,1,2')
print(fit)
#Forecast 13 Weeks
fcast <- forecast(fit, h=13)
plot(fcast)

#Manual ARIMA Model Fit on Cleaned Data
#Results of Model
fit_clean<-Arima(s1d1$clean_sales, order = c(2,1,2))
tsdisplay(residuals(fit_clean), lag.max=45, main='Cleaned Data Model Residuals ARIMA 2,1,2')
print(fit_clean)
#Forecast 13 Weeks
fcast_clean <- forecast(fit_clean, h=13)
plot(fcast_clean)

#Manual ARIMA Hold Out Method on Raw Data
hold <- window(ts(s1d1$Weekly_Sales), start=130)
fit_no_holdout = Arima(ts(s1d1$Weekly_Sales[-c(130:143)]), order = c(2,1,2))
fcast_no_holdout <- forecast(fit_no_holdout,h=13)
plot(fcast_no_holdout, main="13 Week Hold Out for Raw Data ARIMA 2,1,2",
     sub="")
lines(ts(s1d1$Weekly_Sales), col="red")

#Manual ARIMA Hold Out Method on Cleaned Data
hold_clean <- window(ts(s1d1$clean_sales), start=130)
fit_no_holdout_clean = Arima(ts(s1d1$clean_sales[-c(130:143)]), order = c(2,1,2))
fcast_no_holdout_clean <- forecast(fit_no_holdout_clean,h=13)
plot(fcast_no_holdout_clean, main="13 Week Hold Out for Clean Data ARIMA 2,1,2",
     sub="", ylim=c(0, 60000))
lines(ts(s1d1$Weekly_Sales), col="red")

#ETS Model Fit on Raw Data
#Results of Model
fit<-ets(s1d1$Weekly_Sales)
tsdisplay(residuals(fit), lag.max=45, main='ETS Model Residuals')
print(fit)
#Forecast 13 Weeks
fcast <- forecast(fit, h=13)
plot(fcast)

#ETS Hold Out Method on Raw Data
hold <- window(ts(s1d1$Weekly_Sales), start=130)
fit_no_holdout = ets(ts(s1d1$Weekly_Sales[-c(130:143)]))
fcast_no_holdout <- forecast(fit_no_holdout,h=13)
plot(fcast_no_holdout, main="13 Week Hold Out for ETS Model",
     sub="")
lines(ts(s1d1$Weekly_Sales), col="red")


#Cross-validation
#forecast evaluation with a rolling origin, natural analogue to
#leave-one-out cross-validation for cross-sectional data

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

#Manual ARIMA Diagnostics on Raw Data
fmarima <- function(x, h){forecast(arima(x, order = c(2,1,2)), h=h)}
e <- tsCV(ts(s1d1$Weekly_Sales), fmarima, h=1)
sum(is.na(e))
mean(abs(e),na.rm = TRUE)
mean(e^2, na.rm = TRUE)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(fmarima(ts(s1d1$Weekly_Sales), h=1))^2, na.rm=TRUE))

#Manual ARIMA Diagnostics on Cleaned Data
fmarima <- function(x, h){forecast(arima(tsclean(ts(x)), order = c(2,1,2)), h=h)}
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