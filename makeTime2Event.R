makeTime2Event <- function(data) {

    data$time   <- 'no_event'

    if (sum(data$eventFlag, na.rm=T) == 0 ) {
        return(data[,time:=factor(time, levels=c("no_event", "b3","b2","b1","ev","a1","a2","a3"))])
    }

    trainLength <- nrow(data)


    if (nrow(data[eventFlag == 1] >= 1)) {

        for (i in 1:trainLength) {
            if (data$eventFlag[i] == 1) {
                data$time[i] = 'ev'}
        }

        if (trainLength >= 2) {
            for (i in 2:trainLength) {
                if (data$eventFlag[i] == 1 & data$time[i-1] == 'no_event') {
                    data$time[i-1] = 'b1'
                }
            }
        }

        if (trainLength >= 3) {
        for (i in 3:trainLength) {
            if (data$eventFlag[i] == 1 & data$time[i-2] == 'no_event') {
                data$time[i-2] = 'b2'
            }
        }
        }

        if (trainLength >= 4) {
        for (i in (4:trainLength)) {
            if (data$eventFlag[i] == 1 & data$time[i-3] == 'no_event') {
                data$time[i-3] = 'b3'
            }
        }
        }

        if (trainLength >= 2) {
        for (i in 1:(trainLength - 1)) {
            if (data$eventFlag[i] == 1 & data$time[i+1] == 'no_event') {
                data$time[i+1] = 'a1'
            }
        }
        }

        if (trainLength >= 3) {
        for (i in 1:(trainLength - 2)) {
            if (data$eventFlag[i] == 1 & data$time[i+2] == 'no_event') {
                data$time[i+2] = 'a2'
            }
        }
        }

        if (trainLength >= 4) {
        for (i in 1:(trainLength - 3)) {
            if (data$eventFlag[i] == 1 & data$time[i+3] == 'no_event') {
                data$time[i+3] = 'a3'
            }
        }
        }
    }

    return(data[,time:=factor(time, levels=c("no_event", "b3","b2","b1","ev","a1","a2","a3"))])
}

# dataTest
# print(makeTime2Event(dataTest), topn=50)
