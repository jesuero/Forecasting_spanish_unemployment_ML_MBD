#################################################################################
##############        Forecasting:     ARIMA         ############################
#################################################################################

# Load libraries
library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)
library(tseries)

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("UnemploymentSpain.dat",header = TRUE, sep = "")

# Convert the data to time series object
y <- ts(fdata$TOTAL, start = 2001, frequency = 12) # 12 months period

# plot the time series
autoplot(y)

-----------------------------------------------------------------------------------------------------------------------
# We cut the time series in order to avoid the peak due to the crisis from 2008 to 2013,
# this decision should improve the forecast
y1 <- window(y, start=2013) # cutting the time series so it starts from 2013
autoplot(y1) # the behaviour of the time series is similar every year from 2013


## Classical additive decomposition to visualize the time series in another way
fdata_ts_dec_add <- decompose(y1,type="additive")
autoplot(fdata_ts_dec_add) + xlab("Year") +
  ggtitle("Classical additive decomposition")


## Identification and fitting frocess ----------------------------------------------------------------------------------

# Box-Cox transformation to variance stabilization 
Lambda <- BoxCox.lambda.plot(y1,12)
z <- BoxCox(y1,Lambda)
autoplot(z)

# Differentiation: after display on next line, if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# It decreases slowly so Differentiation is needed
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz,lag.max = 100)
# We see slowly decay within a 12 month period on the previous plot, so seasonal diferentiation is also needed 

# Seasonal Differentiation
B12Bz <- diff(Bz, lag = 12, differences = 1)

# Now we plot ACF and PACF of the time series -> identify significant lags and order and select p,d,q for the model
ggtsdisplay(B12Bz,lag.max = 100)

y1<-y1/1000000 # scale the time series

----------------------------------------------
# Regular part:
#   p: 0 no significant lag in PACF
#   d: 1 because differentiation is needed
#   q: 2 lags observed in ACF
# Seasonal part:
#   p: no stationary lags in PACF 
#   d: 1 because differentiation is needed
#   q: 1 lag observed in stationary ACF
----------------------------------------------
(is better to start testing with low p,d,q values because higher means more model complexity)

# Fit seasonal model with estimated order
arima.fit <- Arima(y1,
                   order=c(0,1,2),
                   seasonal = list(order=c(0,1,1), period=12), # period of the time series is 12
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
# RMSE = 49861.91
coeftest(arima.fit) # statistical significance of estimated coefficients
# relevance of coeficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
ggtsdisplay(residuals(arima.fit),lag.max = 100)
# white noise observed so the model is fitted correctly

# Check fitted forecast
autoplot(y1, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")
# The forecast is adjusted to the real values
# See a little inaccuracy when adjusting covid period

# Perform future forecast for the next month (h=1)
y_est <- forecast(arima.fit, h=1)
autoplot(y_est)
y_est #forecast November 2021 = 3235592



