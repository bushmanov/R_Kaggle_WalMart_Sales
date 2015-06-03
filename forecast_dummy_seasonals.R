#Setup a fake time series
set.seed(1)
library(lubridate)
index <- ISOdatetime(2010,1,1,0,0,0)+1:8759*60*60
month <- month(index)
hour <- hour(index)
usage <- 1000+10*rnorm(length(index))-25*(month-6)^2-(hour-12)^2
usage <- ts(usage,frequency=24)

#Create monthly dummies.  Add other xvars to this matrix
xreg <- model.matrix(~as.factor(month))[,2:12]
colnames(xreg) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

#Fit a model
library(forecast)
model <- Arima(usage, order=c(0,0,0), seasonal=list(order=c(1,0,0), period=24), xreg=xreg)
plot(usage)
points(fitted(model),col=2)
summary(model)

#Benchmark against other models
model2 <- tslm(usage ~ as.factor(month)+as.factor(hour))
model3 <- tslm(usage~as.factor(month))
model4 <- rep(mean(usage),length(usage))
model5 <- tslm(usage ~ trend + season)
model6 <- tslm(usage ~ trend + season + as.factor(month)+as.factor(hour))

plot(usage)
points(model6$fitted.values, col=2)

acf(model6$residuals, lag.max=120)
pacf(model6$residuals, lag.max=120)

#Compare the 4 models
library(plyr) #for rbind.fill
ACC <- rbind.fill(  data.frame(t(accuracy(model))),
                    data.frame(t(accuracy(model2))),
                    data.frame(t(accuracy(model3))),
                    data.frame(t(accuracy(model5))),
                    data.frame(t(accuracy(model6))),
                    data.frame(t(accuracy(model4,usage)))
)
accuracy(model)

ACC <- round(ACC,2)
ACC <- cbind(Type=c('Arima','LM1','Monthly Mean','Mean'),ACC)
ACC[order(ACC$MAE),]
