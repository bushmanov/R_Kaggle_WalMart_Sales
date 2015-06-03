kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
plot(kingstimeseries)
plot(stl(demandTS, s.window='periodic'))

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot(birthstimeseries)
plot(decompose(birthstimeseries))

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenirtimeseries)
plot(log(souvenirtimeseries))

library(TTR)
par(mfrow=c(1,2))
plot(kingstimeseries)
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot(kingstimeseriesSMA3)
par(mfrow=c(1,1))
plot(kingstimeseries)
lines(SMA(kingstimeseries,n=10), col='blue')

head(data)
plot(data[,1], type='l')
lines(SMA(data[,1], n=10), col='red')


plot(kingstimeseries)
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
> kingstimeseriesarima

plot(train$units, type='l')
lines(SMA(train$units, 10), col='red')

trainTS <- ts(train$units, frequency = 7)
trainTS
plot(trainTS)
plot(decompose(trainTS, type='multiplicative'))

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
rainseries
plot(rainseries, type='l')

rainseriesforecasts <- HoltWinters(rainseries,
                                   beta=FALSE,  # exponential smoothing
                                   gamma=FALSE) # non-seasonal model
plot(rainseriesforecasts)

rainseriesforecasts$fitted

HoltWinters(rainseries, beta=FALSE, gamma=FALSE,
            l.start=23.56) # specify 1st value

rainseriesforecasts2 <- forecast(rainseriesforecasts, h=10)
plot(rainseriesforecasts2)

acf(rainseriesforecasts2$residuals, lag.max=20)
plot(rainseriesforecasts2$residuals)
hist(rainseriesforecasts2$residuals)


skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries,
                                     beta=F,
                                     gamma=FALSE)
plot(skirtsseriesforecasts)

skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.forecast(skirtsseriesforecasts2)

acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
x <- rnorm (100)
Box.test (x, lag = 20)

plot(skirtsseriesforecasts2$residuals)
hist(skirtsseriesforecasts2$residuals)


logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
plot(souvenirtimeseriesforecasts)

souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
plot(souvenirtimeseriesforecasts)

souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=12)
plot(souvenirtimeseriesforecasts2)

acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)

plot(kingstimeseries)
acf(kingstimeseries)
pacf(kingstimeseries)

kingstimeseriesarima <- arima(kingstimeseries, order = c(0,1,1))
kingstimeseriesarima

kingstimeseriesarima.auto <- auto.arima(kingstimeseries)
kingstimeseriesarima.auto

y <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12)
fit <- tslm(y ~ trend + season)
plot(forecast(fit, h=20))
