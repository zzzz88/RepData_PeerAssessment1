data <- read.csv("activity.csv")
completes<- subset(data,complete.cases(data)==TRUE)              # build a subset of the complete values

splitByDay<-split(completes,completes$date, drop=TRUE)          # split the complete cases by date  
dailySteps<-sapply(splitByDay, function(x) sum(x$steps))        # build a numeric vector w/ daily sum of steps  
hist(dailySteps, main="Hist Total Steps per Day", xlab="# Steps", col="bisque3") # plot a histogram  
abline(v=mean(dailySteps), lty=3, col="blue")                   # draw a blue line thru the mean  
abline(v=median(dailySteps), lty=4, col="red")                  # draw a red line thru the median  
text(mean(dailySteps),25,labels="mean", pos=4, col="blue")      # label the mean  
text(mean(dailySteps),23,labels="median", pos=4, col="red")     # label the median  
rug(dailySteps, col="chocolate") 