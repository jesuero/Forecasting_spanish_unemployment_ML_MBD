# Forecasting with ARIMA and Dynamic Regression
In this assignment of the Machine Learning Course from the Big Data Master we work in groups to try to make a prediction for November unemployment in Spain.

The dataset contains the unemployment number and date in Spain from 01/01/2001 to 01/10/2021.

Tasks:

a) Identify the best Seasonal ARIMA model to forecast the total unemployment rate.

b) Identify the best Dynamic Regression model to forecast the total unemployment rate accounting for the COVID-19 period using an intervention variable.

c) Compare both models.

d) Obtain the best forecast for the unemployment rate of November 2021.


Files:

- UnemploymentSpain.dat --> unemployment dataset
- Assignment2_ARIMA.R --> forecasting using Seasonal ARIMA model
- DynamicRegression_COVID.R --> forecasting using Dynamic Regression model with an intervention variable for COVID (value: unemployment value for that month scaled by the mean and 0 if not COVID month) 
- Assignment2_DynamicRegression_COVID_1 --> forecasting using Dynamic Regression model with an intervention variable for COVID (value: 1 for COVID months and 0 otherwise) 
