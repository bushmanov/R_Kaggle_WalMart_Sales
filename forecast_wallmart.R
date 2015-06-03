library(data.table)
library(xts)
options(datatable.print.topn=5)
options(datatable.print.nrows=5)
tr <- fread("../MachineLearning/walmart/train.csv",
            sep = ',', header=T)
tr[,date:=as.Date(date, "%Y-%m-%d")]

te <- fread("../MachineLearning/walmart/test.csv", sep = ',', header=T)
te[,date:=as.Date(date, "%Y-%m-%d")]


setkey(tr, store_nbr, item_nbr)
setkey(te,  store_nbr, item_nbr)

xreg <- data.table(xreg)
setkey(xreg, station_nbr)

store_nbr <- 2
item_nbr <- 5
station_nbr <- 14

train <- xts(tr[.(2, 5), units], order.by = tr[.(2,5), date])
test  <- xts(order.by= te[.(2,5), date])
ds <- merge(train, test)
dim(ds)

xr <- xts(xreg[.(14), 3:27, with=F], order.by = xreg[.(14), date])
dat <- merge(ds, xr, join = 'left')
dim(dat)

# modeling timeseries
dat1 <- dat["2012::2013-03"]
dim(dat1)
units <- ts(dat1$train, frequency = 7)
head(dat1)
covariates <- as.matrix(dat1[,-1])
head(covariates)
dim(covariates)

# step1
plot(units) # no trend, only seasonality
acf(units)
# step2
plot(diff(units, lag=7))
abline(h=0, col=2) # monthly seasonality ?
acf(diff(units, lag=7))
pacf(diff(units, lag=7))

library(car)
library(forecast)
colnames(covariates)
mod <- Arima(units, order=c(0,0,1), seasonal = list(order=c(0,0,1), period=7),
             xreg=covariates[,c('rain', 'snow', 'bizday')])
summary(mod)

acf(mod$resid)
qqPlot(mod$resid)
Box.test(mod$resid, lag=30, "L")
plot(units, type='l')
lines(fitted(mod), col=2)
plot(mod$residuals)


colnames(covariates)
mod2 <- auto.arima(units, xreg=covariates[,'bizday'])
summary(mod2)
qqPlot(mod2$residuals)
Box.test(mod2$resid, lag=30, "L")


df <- data.frame(uni = ts(dat1$train, frequency=7), dat1[,-1])
class(df$uni)
mod3 <- tslm(uni ~., data=df)
summary(mod3)


