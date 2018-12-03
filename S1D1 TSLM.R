library('ggplot2')
library('forecast')
library('tseries')
library('magrittr')
library('dplyr')
library("reshape")
library("data.table")
library("lubridate")
library("AICcmodavg")

#S1D1 data was extracted from larger WalMart dataset
#Provided in R workspace S1D1 TSLM.Rdata

#Ensures s1d1 data is formatted correctly for remainder of script
s1d1$Date<-as.Date(s1d1$Date, format = "%Y-%m-%d")
s1d1$Weekly_Sales<-ts(s1d1$Weekly_Sales)
s1d1$IsHoliday<-ts(s1d1$IsHoliday)

mymodel <- tslm(s1d1$Weekly_Sales~s1d1$IsHoliday, data = s1d1)
fcast<-forecast(mymodel, h = 143)
plot(fcast)

#Results of Model
tsdisplay(residuals(mymodel), lag.max=45, main='TSLM Model Residuals')
print(mymodel)

fc <- function(y, h, xreg)
{
  if(NROW(xreg) < length(y) + h)
    stop("Not enough xreg data for forecasting")
  X <- head(xreg, length(y))
  fit <- tslm(y ~ X)
  X <- subset(xreg, start=length(y)+1, end=length(y)+h)
  forecast(fit, newdata=X)
}

# Predictors of the same length as the data
# and with the same time series characteristics.    
pred <- ts(s1d1$IsHoliday)

e<-tsCV(ts(s1d1$Weekly_Sales), fc, xreg=pred)
AIC(mymodel)
AICc(mymodel)
BIC(mymodel)
sum(is.na(e)) #NA count
mean(abs(e),na.rm = TRUE) #MAE
mean(e^2, na.rm = TRUE) #MSE
sqrt(mean(e^2, na.rm=TRUE)) #RMSE
sqrt(mean(residuals(mymodel)^2, na.rm=TRUE)) #RMSE Residuals
var(as.numeric(e), na.rm = TRUE) #Error variance