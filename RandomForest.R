x <- model.matrix(~. -snow-rain-eventFlag-dry-wet+time:bizday, data = resultTrain[ind, 6:17, with=F])
mod_rf <- randomForest(x=x, y=y, ntree=300)
mod_rf$


alpha=0.2
mod_glmnet  <- cv.glmnet(x=x, y=y, family = 'gaussian', alpha=alpha)
mod_glmnet2 <- glmnet(x=x, y=y, family = 'gaussian', lambda=mod_glmnet$lambda.min, alpha=alpha)
mod_glmnet2$dev.ratio

