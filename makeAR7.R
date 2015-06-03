makeAR7 <- function(data, n=7) {
    data[, AR7:= c(rep(NA,n), units[1:(nrow(data)-n)])]
    return(data)
}
