---
title: "Activity Monitoring EDA"
author: "Alf Sanchez"
output: html_document
---
  
  
This is a submission of the first course project for the course, Reproducible Research. The assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  
  
Below are the results:  
  
  

```r
## Loading and preprocessing the data

library(plyr)
library(reshape2)
library(lattice)

# Read in the activity.csv dataset. NA denotes missing values
 
activitydata  <- read.csv("activity.csv", na.strings="NA", header=TRUE)
head(activitydata) # check first few rows
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
attach(activitydata)
```

```
## The following objects are masked from weekdata:
## 
##     date, interval, steps
## 
## The following objects are masked from activitydata (pos = 4):
## 
##     date, interval, steps
```

```r
summarizeddata <- aggregate(activitydata,by=list(interval),FUN=mean,na.rm=TRUE)[c(2,4)]
head(summarizeddata) # check first few rows
```

```
##       steps interval
## 1 1.7169811        0
## 2 0.3396226        5
## 3 0.1320755       10
## 4 0.1509434       15
## 5 0.0754717       20
## 6 2.0943396       25
```
  
## What is the mean total number of steps taken per day?  


```r
# Create a histogram of the total number of steps taken each day

png(filename="histogram_noimputation.png",width=480,height=480,units="px",bg="white")

hist(activitydata$steps,col="Light blue", main="Histogram of Total Number of Steps", xlab="Number of Steps")
abline(v=mean(activitydata$steps,na.rm=TRUE),col="Blue",lwd=2, lty=2)
abline(v=median(activitydata$steps,na.rm=TRUE),col="Red",lwd=2, lty=2)
legend("topright",lty=c(2,2),lwd=c(1,1),col=c("Blue","Red"),bty="n",legend=c("Mean","Median"))

dev.off()
```

```
## pdf 
##   2
```

```r
#Calculate and report the mean and median total number of steps taken per day
mean(activitydata$steps,na.rm=TRUE) # Show the value of the mean (blue dashed line)
```

```
## [1] 37.3826
```

```r
median(activitydata$steps,na.rm=TRUE) # Show the value of the median (red dashed line)
```

```
## [1] 0
```
  
## What is the average daily activity pattern?  
  

```r
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)

png(filename="timeseries.png",width=480,height=480,units="px",bg="white")

plot(summarizeddata$interval,summarizeddata$steps,type="l",col="Blue",main="Average Number of Steps Taken", xlab="5-minute interval", ylab="Average # of Steps")
abline(v=summarizeddata[which.max(summarizeddata$steps),2],col="Red",lwd=2, lty=2)

dev.off()
```

```
## pdf 
##   2
```

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max(summarizeddata$steps) # Show maximum average number of steps
```

```
## [1] 206.1698
```

```r
summarizeddata[which.max(summarizeddata$steps),2] # Show the 5-minute interval yielding the maximum
```

```
## [1] 835
```
  
## Imputing missing values  
  
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  
  

```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NROW(activitydata)-NROW(na.omit(activitydata)) # Show number of rows with NA  
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
  

```r
# This step imputes the average number of steps of the corresponding 5-minute interval for missing data

imputedata <- summarizeddata
colnames(imputedata) <- c("avesteps","interval")

mergeddata <- mutate(merge(activitydata,imputedata,by="interval"),newsteps=0)

for (i in 1:length(mergeddata$steps)){  
	if (is.na(mergeddata$steps[[i]])) {mergeddata$newsteps[[i]]=mergeddata$avesteps[[i]]}
	else {mergeddata$newsteps[[i]]=mergeddata$steps[[i]]}
	}

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

newactivitydata <- mergeddata[c(1,3,5)]
colnames(newactivitydata) <- c("interval","date","steps") # Retain original header names
head(newactivitydata)
```

```
##   interval       date    steps
## 1        0 2012-10-01 1.716981
## 2        0 2012-11-23 0.000000
## 3        0 2012-10-28 0.000000
## 4        0 2012-11-06 0.000000
## 5        0 2012-11-24 0.000000
## 6        0 2012-11-15 0.000000
```

```r
# Make a histogram of the total number of steps taken each day 

png(filename="histogram_withimputation.png",width=480,height=480,units="px",bg="white")

hist(newactivitydata$steps,col="Light blue", main="Histogram of Total Number of Steps (With Imputation)", xlab="Number of Steps")
abline(v=mean(newactivitydata$steps,na.rm=TRUE),col="Blue",lwd=2, lty=2)
abline(v=median(newactivitydata$steps,na.rm=TRUE),col="Red",lwd=2, lty=2)
legend("topright",lty=c(2,2),lwd=c(1,1),col=c("Blue","Red"),bty="n",legend=c("Mean","Median"))

dev.off()
```

```
## pdf 
##   2
```

```r
# Comparing with and without imputation

png(filename="histogram_comparison.png",width=480,height=480,units="px",bg="white")

par(mfrow=c(2,1))

hist(activitydata$steps,col="Light blue", main="Total Number of Steps (No Imputation)", xlab="Number of Steps")
abline(v=mean(activitydata$steps,na.rm=TRUE),col="Blue",lwd=2, lty=2)
abline(v=median(activitydata$steps,na.rm=TRUE),col="Red",lwd=2, lty=2)
legend("topright",lty=c(2,2),lwd=c(1,1),col=c("Blue","Red"),bty="n",legend=c("Mean","Median"))

hist(newactivitydata$steps,col="Light blue", main="Total Number of Steps (With Imputation)", xlab="Number of Steps")
abline(v=mean(newactivitydata$steps,na.rm=TRUE),col="Blue",lwd=2, lty=2)
abline(v=median(newactivitydata$steps,na.rm=TRUE),col="Red",lwd=2, lty=2)
legend("topright",lty=c(2,2),lwd=c(1,1),col=c("Blue","Red"),bty="n",legend=c("Mean","Median"))

dev.off()
```

```
## pdf 
##   2
```

```r
# Calculate and report the mean and median total number of steps taken per day 

mean(newactivitydata$steps,na.rm=TRUE) # Show the value of the mean (blue dashed line)
```

```
## [1] 37.3826
```

```r
median(newactivitydata$steps,na.rm=TRUE) # Show the value of the median (red dashed line)
```

```
## [1] 0
```
# Do these values differ from the estimates from the first part of the assignment? 
  
The actual values remain the same given the strategy employed. The imputation made use of the average of the corresponding 5-minute interval. Thus, when the average is computed at the aggregate level, it is expected that the mean will be the same in both approaches.  
  
Nevertheless, if a different imputation strategy is used, mean and median results can possibly vary depending on the treatment/s done on the data entries.  
  
  
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
The difference is not evident just by looking at the graphs themselves. Nonetheless, if one looks at the y-axis values, the difference becomes more apparent. Given that there are more values represented, the frequencies are seen to be higher. Just to nore, the impact could be more drastic, given the employed imputation strategy, if the missing values are localized in a particular interval.  
  
## Are there differences in activity patterns between weekdays and weekends?  
  

```r
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

weekdata <- mutate(newactivitydata,weekday="",weekflag="")

for (i in 1:length(weekdata$date)){
	weekdata$weekday[[i]]=weekdays(as.Date(weekdata$date[[i]]))
	if (weekdata$weekday[[i]]=="Saturday"||weekdata$weekday[[i]]=="Sunday") {
		weekdata$weekflag[[i]]="weekend"
		}
	else {
		weekdata$weekflag[[i]]="weekday"
		}
	}

weekdata$weekflag <- as.factor(weekdata$weekflag)
class(weekdata$weekflag)
```

```
## [1] "factor"
```

```r
head(weekdata) # check first few rows
```

```
##   interval       date    steps  weekday weekflag
## 1        0 2012-10-01 1.716981   Monday  weekday
## 2        0 2012-11-23 0.000000   Friday  weekday
## 3        0 2012-10-28 0.000000   Sunday  weekend
## 4        0 2012-11-06 0.000000  Tuesday  weekday
## 5        0 2012-11-24 0.000000 Saturday  weekend
## 6        0 2012-11-15 0.000000 Thursday  weekday
```

```r
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

attach(weekdata)
```

```
## The following objects are masked from activitydata (pos = 3):
## 
##     date, interval, steps
## 
## The following objects are masked from weekdata (pos = 4):
## 
##     date, interval, steps, weekday, weekflag
## 
## The following objects are masked from activitydata (pos = 5):
## 
##     date, interval, steps
```

```r
summarizeddata2 <- aggregate(weekdata,by=list(interval,weekflag),FUN=mean,na.rm=TRUE)[c(1,2,5)]
colnames(summarizeddata2) <- c("interval","weekflag","steps") # Retain original header names
head(summarizeddata2) # check first few rows
```

```
##   interval weekflag      steps
## 1        0  weekday 2.25115304
## 2        5  weekday 0.44528302
## 3       10  weekday 0.17316562
## 4       15  weekday 0.19790356
## 5       20  weekday 0.09895178
## 6       25  weekday 1.59035639
```

```r
png(filename="timeseries_weekcomparison.png",width=480,height=480,units="px",bg="white")

xyplot(steps ~ interval | weekflag, data = summarizeddata2, type="l", xlab="Interval", ylab="Number of steps", layout = c(1, 2))

dev.off()
```

```
## pdf 
##   2
```
