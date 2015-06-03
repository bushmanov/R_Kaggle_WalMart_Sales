library(forecast)
# create some artifical data
modelfitsample <- data.frame(Customer_Visit=rpois(49,3000),Weekday=rep(1:7,7),
                             Christmas=c(rep(0,40),1,rep(0,8)),Day=1:49)

str(modelfitsample)

# Create matrix of numeric predictors
xreg <- cbind(Weekday=model.matrix(~as.factor(modelfitsample$Weekday)),
              Day=modelfitsample$Day,
              Christmas=modelfitsample$Christmas)

as.factor(modelfitsample$Weekday)
contrasts(as.factor(modelfitsample$Weekday))

# Remove intercept
xreg <- xreg[,-1]

# Rename columns
colnames(xreg) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Day","Christmas")
head(xreg)

# Variable to be modelled
visits <- ts(modelfitsample$Customer_Visit, frequency=7)

# Find ARIMAX model
modArima <- auto.arima(visits, xreg=xreg)
summary(modArima)
