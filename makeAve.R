require(TTR)

makeAve <- function(dataTrain, dataTest, n=7, ar=7) {
    dt <- merge(dataTrain, dataTest, by=c('date','time'), all=T)
    un <- na.locf(dt$units)
    dt[, ave:=SMA(un,n=n)]
#   dt[, AR7:= c(rep(NA,ar), units[1:(nrow(dt)-ar)])]
    for (i in 2:nrow(dt)) {
        if (dt$time[i] != 'no_event') {
            dt$ave[i] = dt$ave[i-1]
        }
    }
    return(dt[,list(date,ave)])
}


# d <- makeAve(resultTrain, resultTest)
# resultTrain   <- merge(resultTrain, d, by="date", all.x=T)
# resultTest    <- merge(resultTest,  d, by="date", all.x=T)
# options(digits=1)
# print(resultTrain[,(4:17), with=F], topn=100)
# print(resultTest[,(4:16), with=F], topn=100)

