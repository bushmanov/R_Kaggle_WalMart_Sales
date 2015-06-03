library(xlsx)
demand <- read.xlsx("./data/ch08/SwordForecasting.xlsm",
                    sheetName='Timeseries',
                    rowIndex = c(1:37),
                    colIndex = c(1,2))
head(demand)
tail(demand)

plot(demand$Demand, type='l')
dem <- ts(demand$Demand, frequency = 7)
dem
preds <- HoltWinters(dem)
plot(preds)
preds2 <- forecast(preds, h=12)
plot(preds2, ylim=c(0, 350))

preds3 <- tslm(dem ~ trend+season)
summary(preds3)

plot(dem)
lines(preds3$fitted.values, col='blue')

mod <- auto.arima(dem)
summary(mod)

plot(forecast(mod, h=12), col='blue')
