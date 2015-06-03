library(data.table)

test    <- fread("../MachineLearning/walmart/test.csv")
train   <- fread("../MachineLearning/walmart/train.csv")
temp <- fread("../MachineLearning/walmart/weather.csv")

test[, date := as.Date(date)]
train[, date:=as.Date(date)]

# make weather
temp[, date:= as.Date(date)]
temp[, tavg:= round((as.numeric(tmax) +as.numeric(tmin))/2, digits=1)]
temp[, tavgNorm:= tavg-65]
temp[, RHA:= 100 - (25/9) * (tavg - as.numeric(dewpoint))]
temp[, snowInch:= ifelse(!(is.na(as.numeric(snowfall))), as.numeric(snowfall), 0)]
temp[, preciptotal:= ifelse(!(is.na(as.numeric(preciptotal))), as.numeric(preciptotal), 0)]
temp[, windChill:= 35.74 + .6215 * tavg -
    35.75 * ((as.numeric((avgspeed)))^.16) +
    .4275 * tavg * ((as.numeric((avgspeed)))^.16)]

# make event
snow_inch = 2
snow  = grepl('SN|SG', temp$codesum) & temp$snowInch >= snow_inch
rain_inch = 1
rain  = grepl('RA|SN', temp$codesum) & temp$preciptotal >= rain_inch
temp[, `:=`(snow=as.numeric(snow), rain=as.numeric(rain))]
temp[, eventFlag:=as.numeric( temp$snow==1 | temp$rain ==1)]

weather <- temp[,list(station_nbr,date,tavgNorm,RHA,snowInch,preciptotal,snow,rain,eventFlag)]

# add store_nbr
key   <- fread("../MachineLearning/walmart/key.csv")
train <- merge(train, key, by='store_nbr', all.x=T)
test  <- merge(test, key, by='store_nbr', all.x=T)


# make model
library(glmnet)
library(randomForest)
tr <- merge(train, weather, by = c('date', 'station_nbr'), all.x = T)
te <- merge(test, weather, by = c('date', 'station_nbr'), all.x = T)
setkey(tr, store_nbr, item_nbr)
setkey(te, store_nbr, item_nbr)
stores   <- length(unique(tr[,store_nbr]))
products <- length(unique(tr[,item_nbr]))
submission <- matrix(list(), nrow = stores, ncol=products)

for (store in 1:stores) {
        for (product in 1:products) {
#           store=19;product=83
#           store=2;product=5
            data <- tr[.(store, product)]
            eventDates <- data[eventFlag ==1, date]
            window <-  -3:3
            wdw <- data.table(date = sort(unique(eventDates + rep(window, length(eventDates)))))
            result <- merge(wdw, data, by='date', all.x=T)
            ind <- complete.cases(result)
            y <- result[ind,units]
            x <- as.matrix(result[ind,list(tavgNorm,snowInch,preciptotal,snow)])
            xPred <- as.matrix(te[.(store,product), list(tavgNorm,snowInch,preciptotal,snow)])
            dt <- te[.(store,product), date]
            if (sum(y) > .1 & sum(abs(x)) >.1) {
                mod_glmnet2 <- glmnet(x=x, y=y, family = 'gaussian', lambda=.4)
                pred <- predict(mod_glmnet2, xPred, type='response')
                sub <- data.table(preds=pred[,1], store_nbr = store, item_nbr = product, date= dt)
                submission[[store, product]] = sub
            } else {
                sub <- data.table(preds=rep(0, nrow(xPred)), store_nbr = store, item_nbr = product, date= dt)
                submission[[store, product]] = sub
            }
    }
}


# submit
sss <- data.frame()
for (store in 1:stores) {
    for (product in 1:products) {
        sub <- submission[[store,product]]
        sss <- rbind(sss, sub)
    }
}

setkey(sss, store_nbr, item_nbr)
testSSS <- merge(test, sss, by = c('store_nbr', 'date', 'item_nbr'), all.x=T)
sub777 <- testSSS[,list(id=paste(store_nbr, item_nbr, date, sep='_'), units=preds)]


sub777[is.na(units), units:=0]
sub777[units <=0, units:=0]

write.csv(sub777, file='sub.csv', quote=F, row.names = F)
