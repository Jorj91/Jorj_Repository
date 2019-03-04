

###WEEKLY FORECASTS


dec_weekly <- decompose(energy_by_week_forecast_from2007_ts[,9])
dec_weekly
autoplot(dec_weekly)
autoplot(dec_daily)
autoplot(dec_monthly)
#ADJUSTMENT FOR SEASONALITY

weekly_seasonally_adjusted <- energy_by_week_forecast_from2007_ts[,9] - dec_weekly$seasonal
plot(weekly_seasonally_adjusted)





####SIMPLE EXPONENTIAL SMOOTHING
fcHW_weekly <- HoltWinters(weekly_seasonally_adjusted, beta=FALSE, gamma=FALSE)
fcHW_weekly
#N.B. alpha: 0.1720466
#N.B.Values of alpha that are close to 0 mean that little weight is placed on the most recent observations
#when making forecasts of future values.

fcHW_weekly$fitted
plot(fcHW_weekly)

#As a measure of the accuracy of the forecasts, we can calculate the sum of squared errors for the in-sample forecast
#errors, that is, the forecast errors for the time period covered by our original time series.
fcHW_weekly$SSE
#SSE = 155269548265


#prediction for the next 20 weeks 
fcHW_weekly_future <- forecast(fcHW_weekly, h=20)
fcHW_weekly_future
#The forecast function gives you the forecast for a week, a 80% prediction interval for the forecast, and
#a 95% prediction interval for the forecast.

#plotting the predictions 
plot(fcHW_weekly_future)

checkresiduals(fcHW_weekly_future)


# we can obtain a correlogram of the in-sample forecast errors for lags 1-20. We
#can calculate a correlogram of the forecast errors using the “Acf()” function in R.
Acf(fcHW_weekly_future$residuals, lag.max = 20)
#To test whether there is significant evidence for non-zero correlations at lags 1-20, 
#we can carry out a Ljung-Box test.
Box.test(fcHW_weekly_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.06714 -> above 0.05 -> no significant autocorrelations (there is no evidence of non-zero autocorrelations
#in the in-sample forecast errors at lags 1-20.)


qqnorm(fcHW_weekly_future$residuals)
qqline(fcHW_weekly_future$residuals)
#The values are normal as they rest on a line and aren’t all over the place.


#To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors
#are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant
#variance, we can make a time plot of the in-sample forecast errors:
plot.ts(fcHW_weekly_future$residuals)
hist(fcHW_weekly_future$residuals)

accuracy(fcHW_weekly_future)
#                   ME     RMSE      MAE      MPE     MAPE      MASE      ACF1
#Training set -1738.558 27454.25 17456.67 4.580695 18.20166 0.6428946 0.1479864




####HOLT'S EXPONENTIAL SMOOTHING
fcHW_weekly_1 <- HoltWinters(weekly_seasonally_adjusted, gamma=FALSE)
fcHW_weekly_1
#alpha: 0.304704
#beta : 0.01837634
fcHW_weekly_1$SSE
#SSE = 159940933571
plot(fcHW_weekly_1)

#prediction for the next 20 weeks
fcHW_weekly_1_future <- forecast(fcHW_weekly_1, h=20)
fcHW_weekly_1_future
plot(fcHW_weekly_1_future)

checkresiduals(fcHW_weekly_1_future)

Acf(fcHW_weekly_1_future$residuals, lag.max = 20)
Box.test(fcHW_weekly_1_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.08992 -> above 0.05 -> no significant autocorrelations

plot(fcHW_weekly_1_future$residuals)
hist(fcHW_weekly_1_future$residuals)


accuracy(fcHW_weekly_1_future)

#                     ME     RMSE      MAE      MPE     MAPE     MASE       ACF1
#Training set -3013.177 27932.05 18061.44 3.579003 18.37959 0.665167 0.08535086





####HOLT - WINTERS EXPONENTIAL'S SMOOTHING

#If you have a time series that can be described using an additive model with increasing or decreasing 
#trend and seasonality, you can use Holt-Winters exponential smoothing to make short-term forecasts.


weekly_seasonal <- energy_by_week_forecast_from2007_ts[,9]
plot(weekly_seasonal)

fcHW_weekly_2 <- HoltWinters(weekly_seasonal)
fcHW_weekly_2

#Smoothing parameters:
#alpha: 0.2504719
#beta : 0
#gamma: 0.6282323

fcHW_weekly_2$SSE
#SSE = 551450534
plot(fcHW_weekly_2)

#making forecasts for future times (next 20 weeks) not included in the original time series
fcHW_weekly_2_future <- forecast(fcHW_weekly_2, h=20)
fcHW_weekly_2_future
plot(fcHW_weekly_2_future)

checkresiduals(fcHW_weekly_2_future)

#We can investigate whether the predictive model can be improved upon by checking whether the in-sample forecast
#errors show non-zero autocorrelations at lags 1-20, by making a correlogram and carrying out the Ljung-Box test
Acf(fcHW_weekly_2_future$residuals, lag.max = 20)
Box.test(fcHW_weekly_2_future$residuals, lag=20, type="Ljung-Box")
#The correlogram shows that the autocorrelations for the in-sample forecast errors exceed the significance bounds
#for lags 1-20.
#Furthermore, the p-value for Ljung-Box test is < 2.2e-16, indicating that there is evidence of non-zero
#autocorrelations at lags 1-20.

plot(fcHW_weekly_2_future$residuals)
#the forecast errors don't have constant variance over time...
hist(fcHW_weekly_2_future$residuals)
#... but they are normally distributed around zero


accuracy(fcHW_weekly_2_future)
#                   ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
#Training set -1.874235 721.2741 503.1423 -3.867015 20.52005 0.654441 0.2178835





#ARIMA MODELS

Acf(weekly_seasonal)
Pacf(weekly_seasonal)

KPSS_urca_weekly <- ur.kpss(weekly_seasonal)
summary(KPSS_urca_weekly)
autoplot(weekly_seasonal)


#Test is of type: mu with 7 lags. 

#Value of test-statistic is: 1.4137 

#Critical value for a significance level of: 
#  10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

#We cannot accept the null hypothesis of stationarity as the value of the test statistic is higher than the 
#10%, 5% and 1% critical values (1.4137).


#SELECT THE BEST ARIMA MODEL
weekly_seasonal_arima <- auto.arima(weekly_seasonal)
#Series: weekly_seasonal 
#ARIMA(2,1,3) 

#Coefficients:
 # ar1      ar2     ma1      ma2      ma3
#-0.4893  -0.3034  -0.022  -0.2841  -0.3766
#s.e.   0.1015   0.1948   0.101   0.1992   0.1118

#sigma^2 estimated as 329742:  log likelihood=-11065.33
#AIC=22142.66   AICc=22142.72   BIC=22174.23


#FORECASTING WITH ARIMA MODEL
#predicting the next 20 weeks (specifying the confidence level for prediction intervals to be 95%)
weekly_seasonal_arima_future <- forecast(weekly_seasonal_arima, h=20, level = 95)
weekly_seasonal_arima_future
plot(weekly_seasonal_arima_future)


checkresiduals(weekly_seasonal_arima_future)

#acf and box test
Acf(weekly_seasonal_arima_future$residuals, lag.max = 20,main='ACF Residual')
Pacf(weekly_seasonal_arima_future$residuals, lag.max = 20,main='PACF Residual')
#there are spikes outside the insignificant zone for both ACF and PACF plots we can conclude that residuals are not random .
Box.test(weekly_seasonal_arima_future$residuals, lag=20, type="Ljung-Box")
#p-value < 2.2e-16 -> residuals have autocorrelation


plot(weekly_seasonal_arima_future$residuals) #time plot of forecast errors
plotForecastErrors(weekly_seasonal_arima_future$residuals) #histogram of forecast errors
mean(weekly_seasonal_arima_future$residuals) #mean is negative!

accuracy(weekly_seasonal_arima_future)

                    #ME     RMSE      MAE       MPE     MAPE     MASE       ACF1
#Training set -2.547286 573.0216 423.4349 -4.657802 17.69031 0.550765 0.01441145






###LINEAR MODEL to WEEKLY time series including trend and seasonality components
plot(weekly_seasonal)
lm_weekly <- tslm(weekly_seasonal~ trend + season)
summary(lm_weekly)
lm_weekly_future <- forecast(lm_weekly, h = 20)
summary(lm_weekly_future)
plot(lm_weekly_future)


#nicer plot of forecast
autoplot(lm_weekly_future)+
  ggtitle("Forecast of Energy consumption in the next 20 weeks (Linear Regression)") +
  xlab("Time") +
  ylab("Log(GAE) - WattHour")

#RESIDUALS
Acf(lm_weekly_future$residuals, lag.max = 20,main='ACF Residual')
#very strange shape
Pacf(lm_weekly_future$residuals, lag.max = 20,main='PACF Residual')
#there are spikes outside the insignificant zone for both ACF and PACF plots we can conclude that residuals are not random .
Box.test(lm_weekly_future$residuals, lag=20, type="Ljung-Box")

checkresiduals(lm_weekly_future)
#IMPORTANT: ACCURACY
accuracy(lm_weekly_future)

#                       ME     RMSE   MAE       MPE     MAPE      MASE      ACF1
#Training set 9.561296e-14 609.4268 478.7 -5.966806 20.72195 0.6226488 0.5341651




#STL with multiple seasonal periods
energy_by_week_forecast_from2007_ts[,9] %>% mstl() %>%
  autoplot() + xlab("Week")



forecast::findfrequency(weekly_seasonal)
Acf(weekly_seasonal)

