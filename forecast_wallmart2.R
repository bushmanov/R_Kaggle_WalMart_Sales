# store #2, station # 14, item nbr 1
reg <- as.data.table(reg)

head(te)
test <- te[.(2,5)]

train <- tr[.(2,5)]
head(train)
train[,mean(units)]

predictors <- reg[reg$station_nbr==14,]

options(digits = 2)

tte <- merge(test, predictors, by = 'date')
ttr <- merge(train, predictors, by= 'date')

eventDates <- ttr[eventFlag ==1, date]
eventDates
length(eventDates)
window <-  -3:3
wdw <- data.table(date = sort(unique(eventDates + rep(window, length(eventDates)))))
wdw

result <- merge(merge(train, wdw, by='date'), predictors, by='date')
print(result, nrow=120)
result[,mean(units)]
result[, sqrt(mean(log(mean(units)+1) - log(units+1))^2)]


library(randomForest)
ind <- complete.cases(result)
y <- result[ind,units]
x <- as.matrix(result[ind,6:16, with=F])
mod_rf <- randomForest(y =y, x=x[,c('tavg','heat','snowInch','preciptotal','snow')])
mod_rf
mod_rf$importance

library(glmnet)
mod_glmnet <- cv.glmnet(x=x, y=y, nfolds=5)
mod_glmnet$lambda.1se

mod_glmnet2 <- glmnet(x,y, family = 'gaussian', lambda=.4)
mod_glmnet2

pred_glmnet <- predict(mod_glmnet2, x)
pred_rf <- mod_rf$predicted
plot(pred_rf, pred_glmnet)
