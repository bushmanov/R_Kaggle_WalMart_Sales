library(data.table)
library(TTR) # SMA
library(zoo) # na.locf
library(timeDate)
source("./makeTime2Event.R")
source("./makeAve.R")

test    <- fread("../MachineLearning/walmart/test.csv")
train   <- fread("../MachineLearning/walmart/train.csv")
temp    <- fread("../MachineLearning/walmart/weather.csv")

test [, date:=as.Date(date)]
train[, date:=as.Date(date)]

# make weather
temp[, date:= as.Date(date)]
temp[, tavg:= round((as.numeric(tmax) +as.numeric(tmin))/2, digits=1)]
temp[, tavgNorm:= tavg-65]
temp[, RHA:= 100 - (25/9) * (tavg - as.numeric(dewpoint))]
temp[, dry:=ifelse(RHA<25,1,0)]
temp[, wet:=ifelse(RHA>60,1,0)]
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

# make bizday
temp[, bizday:= as.numeric(isBizday(timeDate(temp$date, format = "%Y-%m-%d")))]

weather <- temp[,list(station_nbr,date,tavgNorm,
                      dry,wet,snowInch,preciptotal,snow,rain,windChill,eventFlag, bizday)]

# add store_nbr
key   <- fread("../MachineLearning/walmart/key.csv")
train <- merge(train, key, by='store_nbr', all.x=T)
test  <- merge(test, key, by='store_nbr', all.x=T)


# make model
library(glmnet)
library(randomForest)
library(gbm)
tr <- merge(train, weather, by = c('date', 'station_nbr'), all.x = T)
te <- merge(test,  weather, by = c('date', 'station_nbr'), all.x = T)
setkey(tr, store_nbr, item_nbr)
setkey(te, store_nbr, item_nbr)
stores   <- length(unique(tr[,store_nbr]))
products <- length(unique(tr[,item_nbr]))
submission <- matrix(list(), nrow = stores, ncol=products)
lamda      <- matrix(list(), nrow = stores, ncol=products)

for (store in c(1:34,36:stores)) {
#for (store in c(1:5)) {
    for (product in 1:products) {
#       store=25;product=9
#       add time to/after Train event
        dataTrain <- tr[.(store, product)]
        resultTrain <- makeTime2Event(dataTrain)

#       add time to/after Test event
        dataTest <- te[.(store, product)]
        resultTest <- makeTime2Event(dataTest)

#       fill NA's with locf
        naCols <- c('tavgNorm','dry','wet','windChill')
        resultTrain[,  (naCols) := lapply(.SD, na.locf), .SDcols=naCols]
        resultTest [,  (naCols) := lapply(.SD, na.locf), .SDcols=naCols]

#       add MA7 (dataTrain, dataTest, n=7)
        movingAverage <- makeAve(resultTrain, resultTest)
        resultTrain   <- merge(resultTrain, movingAverage, by="date", all.x=T)
        resultTest    <- merge(resultTest,  movingAverage, by="date", all.x=T)

#       check if we still have some NA's
        ind <- complete.cases(resultTrain)
        y <- resultTrain[ind,units]

#       Choose predictors
        x     <- model.matrix(~.-1 -snow-rain-eventFlag-dry-wet+time:bizday, data = resultTrain[ind, 6:17, with=F])
        xPred <- model.matrix(~.-1 -snow-rain-eventFlag-dry-wet+time:bizday, data = resultTest [, 5:16, with=F])

        dt <- resultTest[, date]
        cat("\n Store =", store, ", Product =", product, "\n")
        if (sum(y) > .1 & sum(abs(x)) >.1) {
            alpha=0.2
            mod_glmnet  <- cv.glmnet(x=x, y=y, family = 'gaussian', alpha=alpha)

            cat("Lambda min=", mod_glmnet$lambda.min, "Lambda 1se =", mod_glmnet$lambda.1se, "\n")
            mod_glmnet2 <-    glmnet(x=x, y=y, family = 'gaussian', lambda=mod_glmnet$lambda.min, alpha=alpha)
            cat("Deviance explined =", mod_glmnet2$dev.ratio, "\n")

            lamda[[store, product]] = c(mn = mod_glmnet$lambda.min,
                                        se1 = mod_glmnet$lambda.1se,
                                        betas = mod_glmnet2$beta,
                                        dev = mod_glmnet2$dev.ratio)

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
sub777[,units:=round(units,0)]

write.csv(sub777, file='sub.csv', quote=F, row.names = F)
