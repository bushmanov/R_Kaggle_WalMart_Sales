options(digits=3)

x <- model.matrix(~.-1 -snow-rain-eventFlag-dry-wet + time:bizday, data = resultTrain[ind, 6:17, with=F])
alpha=0.2
mod_glmnet  <- cv.glmnet(x=x, y=y, family = 'gaussian', alpha=alpha)
mod_glmnet2 <-    glmnet(x=x, y=y, family = 'gaussian', lambda=mod_glmnet$lambda.min, alpha=alpha)
mod_glmnet2$dev.ratio
mod_glmnet2$a0
mod_glmnet2$beta

library(corrplot)
corrplot(cor(x[,-1]))
