#################################################################################
##############        Forecasting:  Dynamic Regression   ARIMA   ################
#################################################################################

# Load libraries
library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)
library(tseries)
library(TSA)
library(Hmisc) 

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("UnemploymentSpain.dat",header = TRUE, sep = "")

# A new variable COVID is introduced in order to take into account the pandemic period
fdata$COVID = 0
covid_start = which(fdata$DATE=="01/03/2020")
covid_end = nrow(fdata)
fdata$COVID[covid_start:covid_end] = fdata$TOTAL[covid_start:covid_end]

# Convert to time series object
fdata_ts <- ts(fdata, start = 2001, frequency = 12) # 12 months period
#for monthly data
autoplot(fdata_ts, facets = TRUE)

# We cut the time series in order to avoid the peak due to the crisis from 2008 to 2013,
# this decision should improve the forecast
y1 <- window(fdata_ts, start=2013)
autoplot(y1, facets = TRUE)

# Selection of the explicative variable COVID
y <- y1[,2]
x <- y1[,3]/mean(y1[,3])

# x variable plot
autoplot(x)


## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model
TF.fit <- arima(y,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=12),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)


#### Fit second FT model with a regular diferentiation
TF.fit <- arima(y,
                order=c(1,1,0),
                seasonal = list(order=c(1,0,0),period=12),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)


#### Fit third FT model with a regular and seasonal diferentiation
TF.fit <- arima(y,
                order=c(1,1,0),
                seasonal = list(order=c(1,1,0),period=12),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 200)

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit)


#### Fit final model (arima noise with selected)
xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(2,1,1),
                   seasonal = list(order=c(1,1,0),period=12),
                   xtransf = xlag,
                   transfer = list(c(0,3)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
# RMSE = 24062.69
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit)
# Residuals are white noise
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)

########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")



x_fut<-x[length(x)]
# Perform future forecast
val.forecast_h3 <- TF.forecast(y.old = as.matrix(y), #past values of the series
                               x.old = as.matrix(xlag), #Past values of the explanatory variables
                               x.new = as.matrix(x_fut), #New values of the explanatory variables
                               model = arima.fit, #fitted transfer function model
                               h=1) #Forecast horizon
val.forecast_h3
# 3255765
