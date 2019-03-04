###---------------------------------------------------------------------
####WORKING ON MONTHLY FORECASTS: predict the next 12 months (TOTAL GAP)


#install.packages("xts")
#install.packages("Metrics")
#install.packages("opera")
#install.packages("nnfor")


library("lubridate")
library("dplyr")
library("tidyr")
library("tidyverse")
library("cellranger")
library("scales")
library("shiny")
library("varhandle")
library("fracdiff")
library("uroot")
library("imputeTS")
library("hms")
library("mice")
library("Hmisc")
library("rio")
library("VIM")
library("mi")
library("RcppArmadillo")
library("forecast")
library("stats")
library("shinydashboard")
library("shiny")
library("timeDate")
library("timeSeries")
library("fUnitRoots")
library("urca")
library("ggfortify")
library("changepoint")
library("strucchange")
library("ggpmisc")
library("magrittr")
library("xts")
library("Metrics")
library("opera")
library("nnfor")


summary(energy_by_month_forecast_from2007_ts)
plot.ts(energy_by_month_forecast_from2007_ts[,9])

ggAcf(energy_by_month_forecast_from2007_ts[,9], lag=48)
 
#SEASON PLOT ON THE FORECAST DATASET
ggseasonplot(energy_by_month_forecast_from2007_ts[,9], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("WattHour") +
  ggtitle("Total Energy per month (2007-2010)")



log_monthly_tot_gap <- log(energy_by_month_forecast_from2007_ts[,9])
plot(as.xts(log_monthly_tot_gap), major.format = "%Y-%m")
plot(as.xts(energy_by_month_forecast_from2007_ts[,9]), major.format = "%Y-%m")



#decompose the Tot GAP in monthly TS
dec_monthly <- decompose(log_monthly_tot_gap)
dec_monthly
plot(dec_monthly)
plot(dec_monthly$x) #observed
#nicer plot for decomposition
autoplot(dec_monthly)


plot(as.xts(dec_monthly$seasonal), major.format = "%Y-%m")
plot(as.xts(dec_monthly$trend), major.format = "%Y-%m")
plot(as.xts(dec_monthly$random), major.format = "%Y-%m")



#Detect peaks and valleys:
ggplot(dec_monthly$x, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "darkgreen") +
  stat_peaks(geom = "text", colour = "darkgreen", 
             vjust = -0.5 ) +
  stat_valleys(colour = "purple") +
  stat_valleys(geom = "text", colour = "purple",
               vjust = -0.5, hjust = 1)


#ADJUSTMENT FOR SEASONALITY

monthly_seasonally_adjusted <- log_monthly_tot_gap - dec_monthly$seasonal
plot(monthly_seasonally_adjusted)


####SIMPLE EXPONENTIAL SMOOTHING
fcHW_monthly <- HoltWinters(monthly_seasonally_adjusted, beta=FALSE, gamma=FALSE)
fcHW_monthly
#alpha: 0.06994231


fcHW_monthly$fitted
plot(fcHW_monthly)

fcHW_monthly$SSE
#1.073036


#prediction for the next 13 months (willing to complete year 2010 and until december 2011)
fcHW_monthly_future <- forecast(fcHW_monthly, h=13)
fcHW_monthly_future
plot(fcHW_monthly_future)

checkresiduals(fcHW_monthly_future)

Acf(fcHW_monthly_future$residuals, lag.max = 20)
Box.test(fcHW_monthly_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.2033
qqnorm(fcHW_monthly_future$residuals)
qqline(fcHW_monthly_future$residuals)
plot.ts(fcHW_monthly_future$residuals)
hist(fcHW_monthly_future$residuals)
accuracy(fcHW_monthly_future)
#                       ME      RMSE        MAE        MPE      MAPE      MASE        ACF1
#Training set -0.02704632 0.1527314 0.08908533 -0.2120516 0.6639956 0.6416359 -0.09993245





####HOLT'S EXPONENTIAL SMOOTHING
fcHW_monthly_1 <- HoltWinters(monthly_seasonally_adjusted, gamma=FALSE)
fcHW_monthly_1
#alpha: 0.1263313
#beta : 0.09823141
fcHW_monthly_1$SSE
#SSE = 1.174122
plot(fcHW_monthly_1)

#prediction for the next 13 months 
fcHW_monthly_1_future <- forecast(fcHW_monthly_1, h=13)
fcHW_monthly_1_future
plot(fcHW_monthly_1_future)

checkresiduals(fcHW_monthly_1_future)

Acf(fcHW_monthly_1_future$residuals, lag.max = 20)
Box.test(fcHW_monthly_1_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.3699

plot(fcHW_monthly_1_future$residuals)
hist(fcHW_monthly_1_future$residuals)

accuracy(fcHW_monthly_1_future)

# ME      RMSE       MAE        MPE    MAPE      MASE        ACF1
#Training set -0.01753661 0.1615289 0.1005527 -0.1421665 0.74875 0.7242292 -0.05766016





####HOLT - WINTERS EXPONENTIAL'S SMOOTHING


fcHW_monthly_2 <- HoltWinters(log_monthly_tot_gap)
fcHW_monthly_2


fcHW_monthly_2$SSE
#SSE = 1.494667

plot(fcHW_monthly_2)

#making forecasts for future times (next 13 months) not included in the original time series
fcHW_monthly_2_future <- forecast(fcHW_monthly_2, h=13)
fcHW_monthly_2_future
plot(fcHW_monthly_2_future)

checkresiduals(fcHW_monthly_2_future)

Acf(fcHW_monthly_2_future$residuals, lag.max = 20)
Box.test(fcHW_monthly_2_future$residuals, lag=20, type="Ljung-Box")


plot(fcHW_monthly_2_future$residuals)
hist(fcHW_monthly_2_future$residuals)


accuracy(fcHW_monthly_2_future)

#                     ME      RMSE       MAE      MPE      MAPE      MASE       ACF1
#Training set 0.03245683 0.2066513 0.1187195 0.215032 0.9027445 0.8550752 -0.2088448






#ARIMA MODELS

Acf(log_monthly_tot_gap)
Pacf(log_monthly_tot_gap)

KPSS_urca_monthly <- ur.kpss(log_monthly_tot_gap)
summary(KPSS_urca_monthly)
#Test is of type: mu with 3 lags. 

#Value of test-statistic is: 0.0458 

#Critical value for a significance level of: 
#  10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

#We can accept the null hypothesis of stationarity as the value of the test statistic is less than the 
#10%, 5% and 1% critical values (0.0458).


#SELECT THE BEST ARIMA MODEL
monthly_seasonal_arima <- auto.arima(log_monthly_tot_gap)
#Series: log_monthly_tot_gap 
#ARIMA(0,0,0)(1,1,0)[12] 

#Coefficients:
#  sar1
#-0.6787
#s.e.   0.1168

#sigma^2 estimated as 0.03735:  log likelihood=4.7
#AIC=-5.4   AICc=-5.03   BIC=-2.29


#FORECASTING WITH ARIMA MODEL
#predicting the next 13 months (specifying the confidence level for prediction intervals to be 95%)
monthly_seasonal_arima_future <- forecast(monthly_seasonal_arima, h=13, level = 95)
monthly_seasonal_arima_future
plot(monthly_seasonal_arima_future)

#example from tutorial: https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/ 

checkresiduals(monthly_seasonal_arima_future)

#acf and box test
Acf(monthly_seasonal_arima_future$residuals, lag.max = 20,main='ACF Residual')
Pacf(monthly_seasonal_arima_future$residuals, lag.max = 20,main='PACF Residual')
#Since there are no spikes outside the insignificant zone for both ACF and PACF plots we can conclude that residuals are random with no information 
#or juice in them. Hence our ARIMA model is working fine.
Box.test(monthly_seasonal_arima_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.9149 >0.05 -> residuals have no autocorrelation


plot(monthly_seasonal_arima_future$residuals) #time plot of forecast errors
plotForecastErrors(monthly_seasonal_arima_future$residuals) #histogram of forecast errors
mean(monthly_seasonal_arima_future$residuals) #mean is negative!
accuracy(monthly_seasonal_arima_future)
 #                     ME      RMSE        MAE         MPE      MAPE      MASE        ACF1
#Training set -0.005116643 0.1643677 0.08580564 -0.04694733 0.6526293 0.6180139 -0.09091381





###LINEAR MODEL to monthly time series including trend and seasonality components
plot(log_monthly_tot_gap)
lm_monthly <- tslm(log_monthly_tot_gap~ trend + season)
summary(lm_monthly)
#There is an average downward trend of -0.0003036 WH per month
#On average, the second month has consumption of 0.21 WH lower than the first quarter, the third month has consumption of 0.17 megalitres lower than the first quarter etc.

lm_monthly_future <- forecast(lm_monthly, h = 13)
summary(lm_monthly_future)
plot(lm_monthly_future)

exp(lm_monthly_future)

class(lm_monthly_future)

exp(lm_monthly_future$mean)


#nicer plot of forecast !
autoplot(lm_monthly_future)+
  ggtitle("Forecast of Energy consumption in the next 13 months (Linear Regression)") +
  xlab("Time") +
  ylab("Log(GAE) - WattHour")

#computing and plotting the EXPONENTIAL
lm_monthly_future$Exp.Forecast <- exp(lm_monthly_future$mean)
lm_monthly_future$Exp.Forecast


autoplot(lm_monthly_future$Exp.Forecast)+
  ggtitle("Forecast of Energy consumption in the next 13 months (Linear Regression)") +
  xlab("Time") +
  ylab("GAE - WattHour")


comb <- ts.union(monthly_seasonal, lm_monthly_future$Exp.Forecast)
comb <- pmin(comb[,1], comb[,2], na.rm = TRUE)
comb

autoplot(comb)+
  ggtitle("Forecast of Energy consumption in the next 13 months (Linear Regression)") +
  xlab("Time") +
  ylab("GAE - WattHour")



#RESIDUALS
checkresiduals(lm_monthly_future, test = "BG")
#N.B. In this case (since it's a Linear Regression), Breusch-Godfrey test is more appropriate
#p-value = 0.3691

forecast::accuracy(lm_monthly_future)

                       #ME      RMSE        MAE         MPE     MAPE      MASE       ACF1
#Training set -3.77931e-17 0.1400348 0.08082095 -0.01194462 0.613819 0.5821117 -0.1217716



#RESIDUALS
Acf(lm_monthly_future$residuals, lag.max = 20,main='ACF Residual')
#just one spike outside the bounds

Box.test(lm_monthly_future$residuals, lag=20, type="Ljung-Box")
#p-value = 0.1198 -> no evidence of strong autocorrelations in the residuals

#The Linear model does well in capturing all the dynamics in the data as the residuals seem to be white noise.




autoplot(energy_by_month_forecast_from2007_ts[,9])
autoplot(log_monthly_tot_gap)


#MOVING AVERAGE SMOOTHING
#the average eliminates some of the randomness in the data, leaving a smooth trend-cycle component.
ma_monthly_5 <- ma(log_monthly_tot_gap, 5)
autoplot(ma_monthly_5)
#Each value in the 5-MA is the average of the observations in the five months window 
#centred on the corresponding month. 
#There are no values for either the first two months or the last two months, 
#because we do not have two observations on either side.
#Later we will use more sophisticated methods of trend-cycle estimation which do allow estimates near the endpoints.
#the trend-cycle  is smoother than the original data and captures the main movement of the time series without all of the minor fluctuations. The order of the moving average determines the smoothness of the trend-cycle estimate. 
#In general, a larger order means a smoother curve. 
#if the seasonal period is even and of order m, we use a  2×m-MA to estimate the trend-cycle. If the seasonal period is odd and of order  m, we use a  m-MA to estimate the trend-cycle. For example, a  
#2×12-MA can be used to estimate the trend-cycle of monthly data and a 7-MA can be used to estimate the trend-cycle of daily data with a weekly seasonality.

ma_monthly_24 <- ma(log_monthly_tot_gap, 24)
autoplot(ma_monthly_24)




#Forecasting with decomposition
fit <- stl(monthly_seasonal, t.window=13, s.window="periodic",
           robust=TRUE)

fit
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")
#Forecasts of the electrical equipment orders data based on a naïve forecast of the seasonally adjusted data 
#and a seasonal naïve forecast of the seasonal component, after an STL decomposition of the data.



#STL with multiple seasonal periods
monthly_seasonal %>% mstl() %>%
  autoplot() + xlab("Month")


forecast::findfrequency(monthly_seasonal)
forecast::Acf(monthly_seasonal, lag.max = 48)




####-------------------------
#Forecasting on TRAINING and TEST (LINEAR MODEL)


monthly_seasonal <- energy_by_month_forecast_from2007_ts[,9]
dec_monthly_seasonal <- decompose(monthly_seasonal)
autoplot(dec_monthly_seasonal)
#monthly_seasonal contains the outlier


#detecting OUTLIERS

monthly_seasonal_noout <- tsoutliers(monthly_seasonal) #corresponds to august 2008 -> 185297 (trough)
#it suggests to replace it with 527893.9


#replacing the outlier with the suggested value
monthly_seasonal_noout <- tsclean(monthly_seasonal, replace.missing = TRUE, lambda = NULL)
#monthly_seasonal_noout does not contain outliers. Use this for forecasting!

#creating TRAINING and TEST set:

training_monthly <- subset(monthly_seasonal_noout, end=length(monthly_seasonal_noout)-12) #training from Jan 07 to Nov 09
training_monthly
test_monthly <- subset(monthly_seasonal_noout, start=length(monthly_seasonal_noout)-11) #test from Dec 09 to Nov 10
test_monthly


autoplot(monthly_seasonal_noout)
autoplot(training_monthly)
autoplot(test_monthly)

#training the model tslm
lm_monthly_train <- tslm(training_monthly ~ trend + season)
training_monthly #real values of the training
lm_monthly_train$fitted.values #fitted values on the training


#RESIDUALS
forecast::accuracy(lm_monthly_train) #error metrics on the training set

#                         ME        RMSE         MAE          MPE        MAPE         MASE
#Training set  0 50066.30747 38934.61521 -0.430608768 5.217740221 0.2550875654

#running the model to the test
lm_monthly_test <- forecast(lm_monthly_train, h = 12, level=95)
test_monthly #real values of the test
lm_monthly_test$mean #FITTED VALUES on the test

#FORECAST ERRORS
round(rmse(test_monthly, lm_monthly_test$mean))
round(mae(test_monthly, lm_monthly_test$mean))
mape(test_monthly, lm_monthly_test$mean)
#RMSE ON THE TEST: 75552
#MAE ON THE TEST: 61800
#MAPE ON THE TEST: 0.085

#TO DO: APPLY THE LINEAR MODEL for doing FUTURE FORECASTS
#TO DO: model also SM1, SM2, SM3 and Other is the residual


#N.B. Residuals are calculated on the training set, while forecast errors are calculated on the test set

#A forecast method that minimises the MAE will lead to forecasts of the median, 
#while minimising the RMSE will lead to forecasts of the mean. 
#Percentage errors have the advantage of being unit-free, and so are frequently used to compare forecast performances between data sets. 
#The most commonly used measure is Mean absolute percentage error: MAPE

#ACF plot of residuals
#With time series data, it is highly likely that the value of a variable observed in the current time period will be similar to its value in the previous period, 
#or even the period before that, and so on. Therefore when fitting a regression model to time series data, it is common to find autocorrelation in the residuals. 
#In this case, the estimated model violates the assumption of no autocorrelation in the errors, and our forecasts may be inefficient — there is some information left over which should be accounted for in the model in order to obtain better forecasts. The forecasts from a model with autocorrelated errors are still unbiased, and so are not “wrong”, but they will usually have larger prediction intervals than they need to. Therefore we should always look at an ACF plot of the residuals.
#Another useful test of autocorrelation in the residuals designed to take account for the regression model is the Breusch-Godfrey test, also referred to as the LM (Lagrange Multiplier) test for serial correlation. It is used to test the joint hypothesis that there is no autocorrelation in the residuals up to a certain specified order. A small p-value indicates there is significant autocorrelation remaining in the residuals.

#The Breusch-Godfrey test is similar to the Ljung-Box test, but it is specifically designed for use with regression models.
#It is always a good idea to check whether the residuals are normally distributed. As we explained earlier, this is not essential for forecasting, 
#but it does make the calculation of prediction intervals much easier.
#(The checkresiduals() function will use the Breusch-Godfrey test for regression models, but the Ljung-Box test otherwise.)



#######--------------
#####Forecasting on TRAINING and TEST (ETS MODEL)

#training the model ets
ets_monthly_train <- ets(training_monthly)

#RESIDUALS
training_monthly #real values of the training
ets_monthly_train$fitted #fitted values on the training
checkresiduals(ets_monthly_train)
#the residuals are white noise
forecast::accuracy(ets_monthly_train)

#                     ME       RMSE         MAE          MPE        MAPE         MASE         ACF1
#Training set -7306.253681 54112.7369 40996.52501 -1.318213506 5.464901368 0.6416242398 0.1640798349


#testing the model
ets_monthly_test <- ets_monthly_train %>% forecast(h = 12, level = 95) 
test_monthly #real values of the test
ets_monthly_test$mean #FITTED VALUES on the test

#FORECAST ERRORS
round(rmse(test_monthly, ets_monthly_test$mean)) #80574
round(mae(test_monthly, ets_monthly_test$mean)) #64718
mape(test_monthly, ets_monthly_test$mean) #0.089



####-------
#######Forecasting on TRAINING and TEST (NEURAL NETWORK nnetar)

#Artificial neural networks are forecasting methods that are based on simple mathematical models of the brain. 
#They allow complex nonlinear relationships between the response variable and its predictors.
#A neural network can be thought of as a network of “neurons” which are organised in layers. The predictors (or inputs) form the bottom layer, and the forecasts (or outputs) form the top layer. 
#There may also be intermediate layers containing “hidden neurons”.
#Once we add an intermediate layer with hidden neurons, the neural network becomes non-linear.
#This is known as a multilayer feed-forward network, where each layer of nodes receives inputs from the previous layers. 
#The outputs of the nodes in one layer are inputs to the next layer. 
#The inputs to each node are combined using a weighted linear combination. 
#The result is then modified by a nonlinear function before being output. 

#The weights take random values to begin with, and these are then updated using the observed data. 
#Consequently, there is an element of randomness in the predictions produced by a neural network. 
#Therefore, the network is usually trained several times using different random starting points, and the results are averaged.

#The number of hidden layers, and the number of nodes in each hidden layer, must be specified in advance.

#When it comes to forecasting, the network is applied iteratively. For forecasting one step ahead, we simply use the available historical inputs. 
#For forecasting two steps ahead, we use the one-step forecast as an input, along with the historical data. 
#This process proceeds until we have computed all the required forecasts.


#training the model neural network
nn_monthly_train <- nnetar(training_monthly)

autoplot(nn_monthly_train)


#RESIDUALS
training_monthly #real values of the training
nn_monthly_train$fitted #fitted values on the training
checkresiduals(nn_monthly_train)
#the residuals are white noise
forecast::accuracy(nn_monthly_train)
#N.B. the results of the error metrics will change due to the randomness of starting weight of each repetition

#last update: 05/01/19 h 11:36
#                 ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
#Training set 14.04775 36934.62 30789.41 -0.3955744 4.264815 0.4818758 -0.1079925


#testing the model (N.B. in case of NN to predict the confidence interval you have to specify PI = T in the forecast function)
nn_monthly_test <- nn_monthly_train %>% forecast(PI = T, level = 95, h = 12) 
test_monthly #real values of the test
nn_monthly_test$mean #FITTED VALUES on the test

#FORECAST ERRORS
round(rmse(test_monthly, nn_monthly_test$mean)) #76877
round(mae(test_monthly, nn_monthly_test$mean)) #63446
mape(test_monthly, nn_monthly_test$mean) #0.085

plot(nn_monthly_test)



#####-----
#NEURAL NETWORK ELM on training and test set

#training the model neural network elm
elm_monthly_train <- elm(training_monthly)

#RESIDUALS
training_monthly #real values of the training
elm_monthly_train$fitted #fitted values on the training

elm_monthly_train$MSE
#MSE
#2260698015

#RMSE
rmse_elm_train <- round(sqrt(elm_monthly_train$MSE))
#47547

#mape(actual, predicted)
mape_elm_train <- mape(training_monthly, elm_monthly_train$fitted)
#0.054

#testing the model
elm_monthly_test <- elm_monthly_train %>% forecast(h = 12, level = 95) 
test_monthly #real values of the test
elm_monthly_test$mean #FITTED VALUES on the test

#FORECAST ERRORS
round(rmse(test_monthly, elm_monthly_test$mean)) #93099
round(mae(test_monthly, elm_monthly_test$mean)) #80125
mape(test_monthly, elm_monthly_test$mean) #0.10


plot(elm_monthly_test)



####------

#NEURAL NETWORK MLP on training and test set

#training the model neural network mlp
mlp_monthly_train <- mlp(training_monthly)


#RESIDUALS
training_monthly #real values of the training
mlp_monthly_train$fitted #fitted values on the training

rmse_mlp_train <- round(sqrt(mlp_monthly_train$MSE))
#RMSE
#7793

#mape(actual, predicted)
mape_mlp_train <- mape(training_monthly, mlp_monthly_train$fitted)
#0.007

#testing the model
mlp_monthly_test <- mlp_monthly_train %>% forecast(h = 12, level = 95) 
test_monthly #real values of the test
mlp_monthly_test$mean #FITTED VALUES on the test

#FORECAST ERRORS
round(rmse(test_monthly, mlp_monthly_test$mean)) #132843
round(mae(test_monthly, mlp_monthly_test$mean)) #108545
mape(test_monthly, mlp_monthly_test$mean) #0.141

plot(mlp_monthly_test)


#####------
#FORECASTING ON TRAINING AND TEST (ARIMA)


monthly_seasonal

Acf(training_monthly)
KPSS_urca_monthly <- ur.kpss(training_monthly)
summary(KPSS_urca_monthly)
#We can accept the null hypothesis of stationarity as the value of the test statistic is less than the 
#10%, 5% and 1% critical values (0.07).

#training the model ARIMA
#SELECT THE BEST ARIMA MODEL on the training
arima_monthly_train <- auto.arima(training_monthly)
#Series: training_monthly 
#ARIMA(0,0,0)(0,1,0)[12] 

training_monthly #real values of the training
arima_monthly_train$fitted #fitted values on the training


#RESIDUALS
forecast::accuracy(arima_monthly_train) #error metrics on the training set
#                       ME        RMSE         MAE          MPE        MAPE        MASE         ACF1
#Training set -11411.91539 71413.58004 42260.31318 -1.659668449 5.871411412 0.661403407 0.1878692721

#running the model to the test
arima_monthly_test <- forecast(arima_monthly_train, h=12, level = 95)
test_monthly #real values of the test
arima_monthly_test$mean #FITTED VALUES on the test


#FORECAST ERRORS
round(rmse(test_monthly, arima_monthly_test$mean)) #75758
round(mae(test_monthly, arima_monthly_test$mean)) #61775
mape(test_monthly, arima_monthly_test$mean) #0.085


checkresiduals(arima_monthly_train)
checkresiduals(arima_monthly_test)



#####------

#FORECASTING ON TRAINING AND TEST (HOLT - WINTERS EXPONENTIAL'S SMOOTHING)

HW_monthly_train <- HoltWinters(training_monthly)
HW_monthly_train
#Smoothing parameters:
#alpha: 0.1655525999
#beta : 0
#gamma: 0.3174869752

HW_monthly_train$fitted[,1] #fitted values on the training

round(rmse(training_monthly, HW_monthly_train$fitted[,1])) #71687
round(mae(training_monthly, HW_monthly_train$fitted[,1])) #41997
mape(training_monthly, HW_monthly_train$fitted[,1]) #0.059


#running the model to the test
HW_monthly_test <- forecast(HW_monthly_train, h=12, level = 95)
test_monthly #real values of the test
HW_monthly_test$mean #FITTED VALUES on the test


#FORECAST ERRORS
round(rmse(test_monthly, HW_monthly_test$mean)) #89485
round(mae(test_monthly, HW_monthly_test$mean)) #66703
mape(test_monthly, HW_monthly_test$mean) #0.092

checkresiduals(HW_monthly_train)
checkresiduals(HW_monthly_test)





#PLOTTING ALL MODELS
par(mfrow=c(2,4))
plot(lm_monthly_test)
plot(ets_monthly_test)
plot(nn_monthly_test)
plot(elm_monthly_test)
plot(mlp_monthly_test) 
plot(arima_monthly_test)
plot(HW_monthly_test)


par(mfrow=c(1,1))

####------
#another way for creating training and test with window
training_monthly
beer2 <- window(monthly_seasonal,start=c(2007,1),end=c(2009,11))
beer3 <- window(monthly_seasonal, start=c(2009, 12))
beer3


beerfit1 <- tslm(beer2 ~ trend + season)

beertest1 <- forecast(beer2, h = 12, model = beerfit1)

#stl decomposition
plot(stl(monthly_seasonal_noout, s.window = "periodic"))
plot(stl(monthly_seasonal, s.window = "periodic"))
plot(decompose(monthly_seasonal))
plot(decompose(monthly_seasonal_noout))







####------- 
#FORECASTING THE NEXT 13 MONTHS WITH THE BEST 3 MODELS (LM, NN, ARIMA)

#(LINEAR MODEL)

#FORECASTING THE NEXT 13 MONTHS WITH LINEAR MODEL TRAINED BEFORE

lm_monthly_future <- tslm(monthly_seasonal_noout ~ trend + season)
lm_monthly_future_13 <- forecast(lm_monthly_future__13ds, h = 13, level = 95)
lm_monthly_future_13 #forecasts with IC
lm_monthly_future_13$mean #forecasts (dec 10 - dec 11)

lm_monthly_future_13$RangeIC <- lm_monthly_future_13$upper - lm_monthly_future_13$lower
lm_monthly_future_13$RangeIC


lm_monthly_future #coefficients of the lm run on all data
lm_monthly_train #coefficients of the lm run on the training set


autoplot(lm_monthly_future_13)+
  ggtitle("Forecast of Energy consumption in the next 13 months (Linear Regression)") +
  xlab("Time") +
  ylab("GAE - WattHour")



#MEMO
#performance on the TRAINING set
checkresiduals(lm_monthly_train)
round(forecast::accuracy(lm_monthly_train),3)
#             ME     RMSE      MAE    MPE  MAPE  MASE  ACF1
#Training set  0 50066.31 38934.61 -0.431 5.218 0.609 0.144
Acf(lm_monthly_train$residuals, lag.max = 20,main='ACF Residual')
#just one spike outside the bounds
Box.test(lm_monthly_train$residuals, lag=20, type="Ljung-Box")
##The Linear model does well in capturing all the dynamics in the data as the residuals seem to be white noise.


#performance on the TEST set
round(rmse(test_monthly, lm_monthly_test$mean))  #75552
round(mae(test_monthly, lm_monthly_test$mean))   #61800
round(mape(test_monthly, lm_monthly_test$mean),3)#0.086



#NOW
round(forecast::accuracy(lm_monthly_future_13),3)
#             ME     RMSE      MAE   MPE MAPE  MASE  ACF1
#Training set  0 54358.92 42805.16 -0.51 5.83 0.678 0.049



##FORECASTING THE NEXT 13 MONTHS WITH NEURAL NETWORK NNAR(1,1,2)[12] TRAINED BEFORE

nn_monthly_future_13 <- forecast(monthly_seasonal_noout, PI = T, h=13, level = 95, model = nn_monthly_train )

plot(nn_monthly_future_13)
plot(lm_monthly_future_13)

nn_monthly_future_13
nn_monthly_future_13$mean #forecasts

#SIMULATION OF PREDICTION INTERVALS - REVIEW
#sim <- ts(matrix(0, nrow=30L, ncol=9L),
  #        start=end(monthly_seasonal_noout)[1L]+1L)
#for(i in seq(9))
 # sim[,i] <- simulate(nn_monthly_train, nsim=30L)

#sim

#autoplot(sim)


#comparing IC ON THE TEST SET from nn and lm
#nn_monthly_test$rangeICtest <- nn_monthly_test$upper - nn_monthly_test$lower
#nn_monthly_test$rangeICtest
#lm_monthly_test$rangeICtest <- lm_monthly_test$upper - lm_monthly_test$lower
#lm_monthly_test$rangeICtest


#MEMO
#performance on the TRAINING set
checkresiduals(nn_monthly_train)
round(forecast::accuracy(nn_monthly_train),3)
#                 ME     RMSE      MAE    MPE  MAPE  MASE   ACF1
#Training set 14.048 36934.62 30789.41 -0.396 4.265 0.482 -0.108
Acf(nn_monthly_train$residuals, lag.max = 20,main='ACF Residual')
#OK
Box.test(nn_monthly_train$residuals, lag=20, type="Ljung-Box")
#OK

#performance on the TEST set
round(rmse(test_monthly, nn_monthly_test$mean))  #76877
round(mae(test_monthly, nn_monthly_test$mean))   #63446
round(mape(test_monthly, nn_monthly_test$mean),3)#0.086

#NOW
#                   ME     RMSE      MAE   MPE  MAPE  MASE   ACF1
#Training set -5811.761 63234.59 45835.96 -1.43 6.302 0.726 -0.172






##FORECASTING THE NEXT 13 MONTHS WITH ARIMA (0,0,0)(0,1,0)[12]  TRAINED BEFORE

arima_monthly_future_13 <- forecast(monthly_seasonal_noout, h=13,level = 95, model = arima_monthly_train)


autoplot(arima_monthly_future_13)+
  ggtitle("Forecast of Energy consumption in the next 13 months (ARIMA)") +
  xlab("Time") +
  ylab("GAE - WattHour")


arima_monthly_future_13$mean #forecasts
arima_monthly_future_13 #forecasts with IC
arima_monthly_future_13$RangeIC <- arima_monthly_future_13$upper - arima_monthly_future_13$lower


arima_monthly_future_13$RangeIC
#lm_monthly_future_13$RangeIC


#MEMO
#performance on the TRAINING set
checkresiduals(arima_monthly_train)
round(forecast::accuracy(arima_monthly_train),3)
#                    ME     RMSE      MAE   MPE  MAPE  MASE  ACF1
#Training set -11411.92 71413.58 42260.31 -1.66 5.871 0.661 0.188
Acf(arima_monthly_train$residuals, lag.max = 20,main='ACF Residual')
#OK
Box.test(arima_monthly_train$residuals, lag=20, type="Ljung-Box")
#OK

#performance on the TEST set
round(rmse(test_monthly, arima_monthly_test$mean))  #75758
round(mae(test_monthly, arima_monthly_test$mean))   #61775
round(mape(test_monthly, arima_monthly_test$mean),3)#0.086

#NOW
#                 ME     RMSE      MAE   MPE  MAPE  MASE  ACF1
#Training set -7634.66 72547.56 47242.79 -1.18 6.562 0.748 0.125




#####----------

###MIXTURE OF THE FORECASTS FROM THE 3 DIFFERENT MODELS (TRYING TO FORECAST 2011)

#nnetar
nn_monthly_future_13$mean
#linear
lm_monthly_future_13$mean
#ARIMA
arima_monthly_future_13$mean
#original
monthly_seasonal_noout

#X is composed of all the forecasts for the next 13 months from the 3 models NN, LM, ARIMA
X_13 <- cbind(NN=nn_monthly_future_13$mean, LM=lm_monthly_future_13$mean, ARIMA=arima_monthly_future_13$mean)
X_13
#df is composed of the original data (from jan 2007 to Nov 2010) and X
df_13 <- cbind(monthly_seasonal_noout, X_13)
df_13

colnames(df_13) <- c("Data","NN","LM","ARIMA")

#plotting the data and the predictions from the 3 models
autoplot(df_13) +
  xlab("Year") + ylab(expression("Global Active Energy [WH]"))

#The function mixture builds an aggregation rule chosen by the user.
#At each time instance t=1,2,…,T, the mixture forms a prediction of Y[t,] by assigning a weight to each expert 
#and by combining the expert advice.
MLpol013 <- mixture(model = "MLpol", loss.type = "square")
MLpol013

#weights <- predict(MLpol013, X_13, test_monthly, type='weights')
#Each row contains the convex combination to form the predictions
#weights
#head(weights)
#tail(weights)

#z are the predictions made on the test set from the mixture model

z <- ts(predict(MLpol013, X_13, monthly_seasonal_noout, type='response'), start=c(2009,12), freq=12)
z
z_13 <- ts(predict(MLpol013, X_13, type='response'), start=c(2009,12), freq=12)

#df is is composed of the original data (from jan 2007 to Nov 2010) and the predictions z from the mixture
df <- cbind(monthly_seasonal_noout, z)
df

colnames(df) <- c("Data","Mixture")
##plotting the data and the predictions from the mixture model
autoplot(df) +
  xlab("Year") + ylab(expression("Global Active Energy [WH]"))

#FORECAST ERRORS on the test
#MSE and RMSE Opera
mse_opera <- c(Opera=mean((test_monthly-z)^2))
rmse_opera <- round(sqrt(mse_opera),2)
rmse_opera #72691.61
#MAPE Opera
mape_opera <- mape(test_monthly, z)
round(mape_opera, 5)#0.079

#MSE and RMSE Individual
mse_individual <- c(NN=mean((test_monthly-nn_monthly_test$mean)^2),
                    LM=mean((test_monthly-lm_monthly_test$mean)^2),
                    ARIMA=mean((test_monthly-arima_monthly_test$mean)^2))
rmse_individual <- round(sqrt(mse_individual),2)
rmse_individual 
#      NN       LM    ARIMA 
#76876.59 75552.26 75758.12 

#MAPE Individual
mape_individual_nn <- mape(test_monthly, nn_monthly_test$mean)
round(mape_individual_nn, 3) #0.086

mape_individual_lm <- mape(test_monthly, lm_monthly_test$mean)
round(mape_individual_lm, 3) #0.086

mape_individual_arima <- mape(test_monthly, arima_monthly_test$mean)
round(mape_individual_arima, 3) #0.086


MLpol <- MLpol0
for (i in 1:length(test_monthly)) {
  MLpol <- predict(MLpol, newexperts = X[i, ], newY = test_monthly[i])
}

summary(MLpol)
print(MLpol)

MLpol$coefficients
#predictions from the Mixture:
MLpol$prediction

plot(MLpol)


##--------
#PLOT WITH IC 

require(dygraphs)

hw <- HoltWinters(monthly_seasonal_noout)
predicted <- predict(hw, n.ahead = 13, prediction.interval = TRUE)
class(predicted)

dygraph(predicted, main = "Forecast of Energy Consumption from Dec 2010 to Dec 2011") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Global Active Energy (WH)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


#nice plot  of past and future


comb_totgap_07_11_nn <- ts.union(monthly_seasonal, nn_monthly_future_13$mean)
comb_totgap_07_11_nn <- pmin(comb_totgap_07_11_nn[,1], comb_totgap_07_11_nn[,2], na.rm = TRUE)
comb_totgap_07_11_nn



lm_monthly_future_13$mean
comb_totgap_07_11
lm_monthly_future_13_RangeIC
lm_monthly_future_13$RangeIC

lm_monthly_future_13_with_RangeIC <-  ts.union(lm_monthly_future_13, lm_monthly_future_13$RangeIC)
lm_monthly_future_13_with_RangeIC
?presAnnotation

require(dygraphs)

#past and future (WH)

presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = TRUE, width = 60)
}

#LM
wid <- dygraph(comb_totgap_07_11, main = "Historical Data and Forecast for the next 13 months - LM") %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )%>%
  dyShading(from = "2010-12-01", to = "2011-12-01")%>%
  presAnnotation("2009-05-01", text = "Historical Data") %>%
  presAnnotation("2011-06-01", text = "Forecast")
wid


#NN

wid_nn <- dygraph(comb_totgap_07_11_nn, main = "Historical Data and Forecast for the next 13 months - NN") %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )%>%
  dyShading(from = "2010-12-01", to = "2011-12-01")%>%
  presAnnotation("2009-05-01", text = "Historical Data") %>%
  presAnnotation("2011-06-01", text = "Forecast")
wid_nn


lm_monthly_future_13_with_RangeIC

lm_monthly_future_13_with_RangeIC_df <- data.frame(lm_monthly_future_13_with_RangeIC)

lm_monthly_train
lm_monthly_test
lm_monthly_future

df_ic <- data.frame(x =c("Dec 10", "Jan 11", "Feb 11", "Mar 11", "Apr 11", "May 11", "Jun 11", "Jul 11", "Aug 11", "Sep 11", "Oct 11", "Nov 11", "Dec 11"),
                 F =lm_monthly_future_13_with_RangeIC_df$lm_monthly_future_13.Point.Forecast,
                 L =lm_monthly_future_13_with_RangeIC_df$lm_monthly_future_13.Lo.95,
                 U =lm_monthly_future_13_with_RangeIC_df$lm_monthly_future_13.Hi.95)

require(ggplot2)
ggplot(df_ic, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))

require(htmlwidgets)
saveWidget(wid)
?saveWidget
saveWidget(wid, file="test.html")
