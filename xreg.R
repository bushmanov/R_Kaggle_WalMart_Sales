temp <- read.csv("../MachineLearning/walmart/weather.csv",
                  header=T, stringsAsFactors = F)

##############################################################################
#
#               Weather
#
##############################################################################


w           <- data.frame(date = as.Date(temp$date))

w$station_nbr <- temp$station_nbr

w$tavg      <- round((as.numeric(temp$tmax) +as.numeric(temp$tmin))/2, digits=1)

w$RHA       <- 100 - (25/9) *
    (w$tavg - as.numeric(temp$dewpoint)) # dew point to humidity

w$heat     <- ifelse(w$tavg <=65, 65 - w$tavg, 0)

w$cool     <- ifelse(w$tavg > 65, w$tavg - 65, 0)

w$snowInch  <- ifelse(!(is.na(as.numeric(temp$snowfall))),
                      as.numeric(temp$snowfall), 0)

w$preciptotal<- ifelse(!(is.na(as.numeric(temp$preciptotal))) ,
                      as.numeric(temp$preciptotal), 0)

w$windChill <- 35.74 + .6215 * w$tavg -
    35.75 * ((as.numeric((temp$avgspeed)))^.16) +
    .4275 * w$tavg * ((as.numeric((temp$avgspeed)))^.16)

# w$codesum      <- temp$codesum

##############################################################################
#
#               EVENTS
#
##############################################################################

snow_inch = 2
snow  = grepl('SN|SG', temp$codesum) & w$snowInch >= snow_inch

rain_inch = 1
rain  = grepl('RA|SN', temp$codesum) & w$preciptotal >= rain_inch

event <- data.frame(snow = as.numeric(snow),
                    rain=as.numeric((rain)),
                    eventFlag = as.numeric( snow==1 | rain ==1))

eventWindow <-

##############################################################################
#
#               Calendar
#
##############################################################################
library(caret)
library(timeDate)
bizday <- as.numeric(isBizday(timeDate(temp$date, format = "%Y-%m-%d")))
fs <- data.frame(month = months(w$date))
dummyCalend <- dummyVars(~ month, data=fs, fullRank = F)
calend <- predict(dummyCalend, fs)
calend <- data.frame(bizday = bizday, calend)
colnames(calend) <- c('bizday', 'apr', 'aug', 'dec', 'feb',
                   'jan', 'jul', 'jun', 'mar', 'may', 'nov', 'oct', 'sep')

calend <- data.frame(bizday = bizday)


##############################################################################
#
#               Xreg
#
##############################################################################

reg <- cbind(w, event, calend)
dim(temp)
dim(reg)
str(reg)

#
# ##############################################################################
# #
# #               Check
# #
# ##############################################################################
#
# options(digits=2)
# cor(xreg[3:15], use='na')
#
# mat <- cor(xreg[4:27], use='na')
# library(corrplot)
# corrplot(mat)
#
# head(xreg, 30)
#
# summary(xreg)
# for (id in unique(xreg$station_nbr)) {
#     cat('Summary for station #',id,"\n")
#     print(summary(xreg[xreg$station_nbr == id,]))
# }
#
# head(temp[temp$station_nbr=="8",], 15)
# head(xreg[xreg$station_nbr == 5,], 100)
# head(xreg, 100)
# temp[1:100, 1:12]
# xreg[1:30, 1:12]
#
#
