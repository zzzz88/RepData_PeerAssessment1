
data <- read.csv("activity.csv")
completes <- data[complete.cases(data)==TRUE,]
Splitinterval <- split(completes, completes$interval, drop = TRUE)
intervalavg <- sapply(Splitinterval, function(x) mean(x$steps))
count <- 0
for(i in 1:nrow(data)){
        if(is.na(data[i,1])){
                data[i,1] <- intervalavg[as.character(data[i,3])]
                count <- count +1
        }
}
data$ww <- as.Date(as.character(data$date),format = "%Y-%m-%d")
data$wd <-weekdays(data$ww)
for (i in 1:nrow(data)){
        if (data[i,5] %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
                data[i,6] <- "Weekday"
        }
                else {
                        data[i,6] <- "Weekend"
                }
}
as.factor(data$V6)
splitwk <-split(data, data$V6)
splitwkday <- as.data.frame(splitwk[1])
splitwkend <- as.data.frame(splitwk[2])
splitinter1 <- split(splitwkday,splitwkday$Weekday.interval)
splitinter2 <- split(splitwkend,splitwkend$Weekend.interval)
splitinter1avg <- sapply(splitinter1, function(x) mean(as.numeric(x$Weekday.steps)))
splitinter2avg <- sapply(splitinter2, function(x) mean(as.numeric(x$Weekend.steps)))
panel.smooth(splitinter1avg,unique(splitwkday$Weekday.interval),col = "red",type ="l")
panel.smooth(splitinter2avg,unique(splitwkday$Weekend.interval), col = "blue", type= "l")
newData <- data
names(newData)[6] <- "day"
with(stepsByDay, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))
with(stepsByDay[stepsByDay$day == "Weekday",], lines(steps ~ interval, type="l", col="chocolate"))  
with(stepsByDay[stepsByDay$day == "Weekend",], lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("chocolate", "16"), legend = c("weekday", "weekend"), seg.len=3)