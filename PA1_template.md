# Reproducible Research: Peer Assessment 1

Loading library to use it after

```r
library(ggplot2)
library(lubridate)
```
defined a function, to give a time format with an interval 

```r
interval2timeformat <- function(interval) paste(interval %/% 100, ":" , interval %% 100,sep="") 
```

## Loading and preprocessing the data

```r
if(!file.exists('activity.csv')){
        unzip('activity.zip')
}
activity <- read.csv('activity.csv')
```
add some transformations 

```r
activity$shortDate <- format.Date(activity$date, format= "%m-%d")
activity$time <- formatC(activity$interval / 100, 2, format='f')
activity$hour <- activity$interval %/% 100 
activity$DateTime <- as.POSIXct(paste(activity$date, activity$time),
                                format='%Y-%m-%d %H.%M',
                                tz='GMT')
```

## What is mean total number of steps taken per day?

```r
activity.totalByDay <- tapply(activity$steps,activity$shortDate
                              ,FUN=sum, na.rm=TRUE)
```
1. Make a histogram of the total number of steps taken each day

```r
barplot(activity.totalByDay
        ,xlab= "experience days between 2012-10-01 and 2012-11-30"
        ,ylab="number of steps"
        ,main="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
2. Calculate and report the mean and median total number of steps taken per day

```r
mean(activity.totalByDay, na.rm=TRUE)
```

```
## [1] 9354
```

```r
median(activity.totalByDay, na.rm=TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

```r
activity.average.daily <- aggregate(steps ~ interval, data = activity, 
                                    FUN = mean, na.rm=TRUE)

ggplot(data=activity.average.daily, aes(x=interval, y=steps)) + geom_line()  + 
        labs(x="5 minute interval", y="the average number of steps taken") + 
        ggtitle("the average number of steps by interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps

```r
most <- which.max(activity.average.daily$steps)
interval2timeformat(activity.average.daily$interval[most])
```

```
## [1] "8:35"
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset 

```r
nrow(activity[is.na(activity$steps),]) 
```

```
## [1] 2304
```
create a new dataFrame
defined interval mean

```r
activity.stepsMeanByHour <- aggregate(steps ~ hour, data = activity, FUN = mean)
```
Create a new dataset that is equal to the original dataset but with the missing 
data filled in with mean value of its 5-minute interval by hour.

```r
activityWithOutNA <- activity
fill.step.value<- function(s,h) ifelse(test=is.na(s),
                                       yes = activity.stepsMeanByHour$steps[h], 
                                       no=s)

activityWithOutNA$steps = fill.step.value(activityWithOutNA$steps,
                                          activityWithOutNA$hour)
```
Make a histogram of the total number of steps taken each day 

```r
activityWithOutNA.totalByDay <- tapply(activityWithOutNA$steps,
                                       activityWithOutNA$shortDate,FUN=sum)

barplot(activityWithOutNA.totalByDay
        ,xlab= "experience days between 2012-10-01 and 2012-11-30"
        ,ylab="number of steps"
        ,main="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
Calculate and report the mean and median total number of steps taken
per day

```r
mean(activityWithOutNA.totalByDay)
```

```
## [1] 10844
```

```r
median(activityWithOutNA.totalByDay)
```

```
## [1] 11162
```
and Calculate and report the mean and median total number of steps taken per day.
Do these values differ from the estimates from the first part of the assignment? 

```r
mean(activityWithOutNA.totalByDay ) - mean(activity.totalByDay, na.rm=T)
```

```
## [1] 1490
```

```r
median(activityWithOutNA.totalByDay) - median(activity.totalByDay, na.rm=T) 
```

```
## [1] 767
```
What is the impact of imputing missing data on the estimates of the total daily
number of steps?

```r
#defined the average daily without NA
activityWithOutNA.average.daily <- aggregate(steps ~ interval, 
                                             data = activityWithOutNA,
                                             FUN = mean, na.rm=TRUE)


activityWithOutNA$correction <-activityWithOutNA.average.daily$steps - activity.average.daily$steps
average.correction <- aggregate(correction ~ interval, data= activityWithOutNA,FUN=mean)
ggplot(average.correction,aes(interval,correction)) +
        geom_line() +
        ggtitle("average of the corrections of the steps between original activity and activity without NA.")
```

![plot of chunk correction](figure/correction.png) 

## Are there differences in activity patterns between weekdays and weekends?

Note : I prefered used wday (lib lubridate) because I don't changes my laptops language
and this method is very nice in this case 

1. Create a new factor variable in the dataset with two levels – “weekday” 
and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityWithOutNA$day <- sapply(activityWithOutNA$date,FUN = day.type)
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). The plot should look
something like the following, which was creating using simulated data:

```r
averages <- aggregate(steps ~ interval + day, data=activityWithOutNA, mean)
ggplot(averages, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps") +
        ggtitle("the average number of steps taken, averaged
across all weekday days or weekend days")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 
