
data <- read.csv("activity.csv")
sum(complete.cases(data)==FALSE)
completes <- data[complete.cases(data)==TRUE,]
splitByDay<-split(completes,completes$date, drop=TRUE)          # split the complete cases by date  
dailySteps<-sapply(splitByDay, function(x) sum(x$steps))        # build a numeric vector w/ daily sum of steps  

Splitinterval <- split(completes, completes$interval, drop = TRUE)
intervalavg <- sapply(Splitinterval, function(x) mean(x$steps))
count <- 0
for(i in 1:nrow(data)){
        if(is.na(data[i,1])){
                data[i,1] <- intervalavg[as.character(data[i,3])]
                count <- count +1
        }
}
newsplitByDay<-split(data,data$date)       
newdailySteps<-sapply(newsplitByDay, function(x) sum(x$steps)) 
par(mfrow= c(1,2),mar = c(4,4,4,4))
hist(dailySteps, main="Hist Total Steps per Day", xlab="# Steps", col="bisque3") # plot a histogram  
abline(v=mean(dailySteps), lty=3, col="blue")   
hist(newdailySteps, main = "Hist New Total Steps", col = "blue")
abline(v= mean(newdailySteps),lty =2, col = "red")