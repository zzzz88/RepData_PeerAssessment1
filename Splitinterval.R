data <- read.csv("activity.csv")
completes <- data[complete.cases(data)==TRUE,]
Splitinterval <- split(completes, completes$interval, drop = TRUE)
intervalavg <- sapply(Splitinterval, function(x) mean(x$steps))
plot(intervalavg, type = "l")
abline(v = which.max(intervalavg), lty = 1, col ="red", lwd =2)
text(which.max(intervalavg),max(intervalavg), label = paste("max =", 
                                                    as.character(round(max(intervalavg))))
max(intervalavg)