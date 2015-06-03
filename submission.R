sub <- read.table("../MachineLearning/walmart/enterCompetition.csv", sep = ',', header=T)
head(sub)
sb <- data.frame(id = sub$id, units = 0.9868756)
write.csv(file = '/home/sergey/MachineLearning/walmart/sub0.csv', sb, row.names = F, quote=F)
