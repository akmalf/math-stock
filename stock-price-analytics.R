###########################################################################
#                                                                         
# STOCK PRICE ANALYTICS AND FORECASTING                      
# oleh Akmal Fadhlurrahman                                                
#                                                                         
# Dibuat sebagai alat peraga untuk presentasi                             
# `How Mathematics “Rules” Stock Market Trading`                          
#
# This work is licensed under a Creative Commons 
# Attribution-ShareAlike 4.0 International License.
# 
# See disclaimer: github
###########################################################################

# Load libraries

# Run if needed
# Install the following packages
# install.packages(c(
#   'fpp2',
#   'quantmod',
#   'scales',
#   'tidyverse'
# ))

# Load libraries
library(fpp2)
library(quantmod)
library(scales)
library(ggplot2)

# Get stock data
# Today we will get data from Bank Central Asia ("TLKM.JK" in Yahoo Finance) 
# Let's get data from the past 2 years

START_DATE <- "2015-01-01"
END_DATE   <- "2022-02-28"

# Get data from quantmod API
getSymbols("TLKM.JK", src = "yahoo")
TLKM <- log(na.omit(Cl(last(TLKM.JK,"5 years"))))
autoplot(TLKM)

# Create chart for slide #23
chart_Series(TLKM)
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)

# Create chart of log return for slide #24
TLKM.logreturn <- na.omit(diff(log(TLKM)))
autoplot(TLKM.logreturn)
# Histogram
ggplot(TLKM.logreturn, aes(TLKM.JK.Close, fill='pink'))  + 
  geom_histogram(aes(y = ..density..), binwidth = 0.002, colour = "pink") + 
  stat_function(fun = dnorm, args = list(mean = mean(TLKM.logreturn, na.rm = TRUE)
                                         , sd = sd(TLKM.logreturn, na.rm = TRUE))) +
  theme_minimal()

# Descriptive statistics
print(mean(TLKM.logreturn, na.rm = TRUE))
print(sd(TLKM.logreturn, na.rm = TRUE))

# Create samples for forecasting
TLKM.ts <- xts(TLKM)
training <- TLKM.ts['2018/2021']
validation <- TLKM.ts['2022']

# Baseline forecast: Naive forecasting
naive <- naive(training, h=length(validation))
forecast::autoplot(naive)

# ARIMA forecast
arima_model <- auto.arima(training,
                          approximation = FALSE,
                          trace=TRUE,
                          stepwise = FALSE,
                          max.p = 4,
                          max.q = 4,
                          max.D = 5)
print(arima_model)

fit_arima <- Arima(training, order = c(2,1,3))
checkresiduals(fit_arima)
autoplot(forecast(fit_arima, h=length(validation))) +
  autolayer(ts(validation, start=1008), series = "Actual")

# ARIMAX with momentum trading indicator
TLKM.rsi <- xts(lag(RSI(TLKM, n=14)))
TLKM.macd <- xts(lag(MACD(TLKM,fast = 12, slow = 25, signal = 9, maType = "SMA")))
df <- na.omit(merge(TLKM.macd,TLKM.rsi, join = "inner"))

fit_arimax <- auto.arima(last(training['2021'],"100 days"),
                            approximation = FALSE,
                            trace=TRUE,
                            stepwise = FALSE,
                            max.p = 4,
                            max.q = 4,
                            max.D = 5,
                            xreg = last(TLKM.rsi['2021'],"100 days"))
print(fit_arimax)

fcast <- forecast(fit_arimax, h = 59, xreg = last(TLKM.rsi['2022'],"59 days"))
autoplot(fcast) +
  autolayer(ts(last(validation['2022'],"59 days"), start=100), series = "Actual")
