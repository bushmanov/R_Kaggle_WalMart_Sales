library(data.table)
options(datatable.print.topn=5)
options(datatable.print.nrows=5)
tr <- fread("../MachineLearning/walmart/train.csv",
            sep = ',', header=T)

tr[,date:=as.Date(date, "%Y-%m-%d")]

te <- fread("../MachineLearning/walmart/test.csv", sep = ',', header=T)
te[,date:=as.Date(date, "%Y-%m-%d")]


setkey(tr, store_nbr, item_nbr)
setkey(te,  store_nbr, item_nbr)

# tr[.(1), sum(units), by=item_nbr]
#
# train <- tr[.(1,9), list(date, units)]
# tail(train)
# test <- te[.(1,9), list(date, units=NA)]
#
#
#
# plot(train[,units], type='l')
#
# train_xts  <- xts(train[,units], order.by = train$date)
# head(train_xts)
# test_xts   <- xts(test [,units],  order.by = test$date)
# head(test_xts)
#
#
# data <- rbind(train_xts, test_xts)
# names(data) <- 'units'
# rm(te, tr)
# class(data)
#
#
# plot(data["2013-05::2013-09", 'units'])
# abline(v=100)
#
#
# acf(train$units, lag.max=30)
# units <- ts(train$units, frequency = 7)
#
# acf(units)
# pacf(units)
# mod_arima <- Arima(units, order=c(3,0,3), seasonal=c(1,0,1))
# summary(mod_arima)
# acf(mod_arima$residuals)
# pacf(mod_arima$residuals)
# hist(mod_arima$residuals)
# plot(units)
# lines(fitted(mod_arima), col=2)
