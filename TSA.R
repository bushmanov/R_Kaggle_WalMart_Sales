library(TSA)
data(co2)
class(co2)
frequency(co2)

# step1
plot(co2) # need to remove drift

# step2
series = diff(co2)
plot(series)
acf(series) # still have a problem: need to remove seasonality

# step 3
series = diff(diff(co2), lag=12)
plot(series) # ok now, reminds white noise, this can be modelled
acf(series)
pacf(series)
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma')
pacf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma')

# model fitting
mod <- arima(co2, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))

# model diagnostics
mod # statistical significance
plot(mod$residuals, type='o') # plot of residuals
Box.test(mod$residuals, lag=36, type="L") # iid of residuals
acf(as.vector(mod$residuals), lag.max=36)

library(car)
qqPlot(mod$residuals) # normality of residuals

