eve <- data.frame(date = w$date, station = w$station_nbr,
                  event = event$snow == 1 | event$rain == 1,
                  w[,c('snow', 'snowInch', 'rain', 'preciptotal', 'codesum')])

eve1 <- eve[eve$station == 5 & eve$event == T,]
eve1

library(ggplot2)
ggplot(data=eve, aes(x=date, y = event, color=event)) +
    geom_point() +
    facet_grid(station ~ .)


