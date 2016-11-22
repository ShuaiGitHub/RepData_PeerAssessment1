# Reproducible Research: Peer Assessment 1, by Shuai Wang

## Loading and preprocessing the data

```r
raw_data<-read.csv('activity.csv')
summary(raw_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
## What is mean total number of steps taken per day?
1. ignore the missing data using na.omit
2. use reshape2 library to sum by date
3. plot the histogram

```r
complete_data<-na.omit(raw_data)
complete_data$date<-as.Date(complete_data$date)
complete_data$interval<-as.factor(complete_data$interval)
aggdata <-aggregate(steps~date,complete_data,FUN=sum)
#print(aggdata)
```
Plot the histogram

```r
hist(aggdata$steps, main="Histogram of steps taken per day",xlab="Number of Steps", ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
So we can easily report mean and median by  

```r
print(mean(aggdata$steps))
```

```
## [1] 10766.19
```

```r
print(median(aggdata$steps))
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgIntervalData<-aggregate(steps~interval,complete_data,FUN=mean)
plot(avgIntervalData$interval,avgIntervalData$steps)
lines(avgIntervalData$interval,avgIntervalData$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
print(avgIntervalData[which.max( avgIntervalData[,2] ),])
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
print(sum(is.na(raw_data$steps)))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
In this problem, we use the mean for 5-minute intervals that don't have steps (marked as missing)

```r
#myFinder function will find the value
raw_data$interval<-as.factor(raw_data$interval)
myFinder<-function(x){
  if(is.na(x[1])) {
    tmp<-x[3]
    target<-subset(avgIntervalData,interval == tmp)
    newRow<-data.frame(c(as.numeric(target['steps']),x[2],x[3]))
    row.names(newRow)<-row.names(x)
    return (newRow)
  } else {
    newRow<-x
    row.names(newRow)<-row.names(x)
    return (newRow)# otherwise make no change
 }
}
myChecker<-function(x){
  return (x['date'])
}
newData<-as.data.frame(t(as.data.frame(apply(raw_data,1,myFinder))))
newData$steps<-as.numeric(as.character(newData$steps))
head(newData,5)
```

```
##                                                  steps       date interval
## c.as.numeric.target..steps.....x.2...x.3..   1.7169811 2012-10-01        0
## c.as.numeric.target..steps.....x.2...x.3...1 0.3396226 2012-10-01        5
## c.as.numeric.target..steps.....x.2...x.3...2 0.1320755 2012-10-01       10
## c.as.numeric.target..steps.....x.2...x.3...3 0.1509434 2012-10-01       15
## c.as.numeric.target..steps.....x.2...x.3...4 0.0754717 2012-10-01       20
```

```r
summary(newData)
```

```
##      steps                date          interval    
##  Min.   :  0.00   2012-10-01:  288   0      :   61  
##  1st Qu.:  0.00   2012-10-02:  288   10     :   61  
##  Median :  0.00   2012-10-03:  288   100    :   61  
##  Mean   : 37.38   2012-10-04:  288   1000   :   61  
##  3rd Qu.: 27.00   2012-10-05:  288   1005   :   61  
##  Max.   :806.00   2012-10-06:  288   1010   :   61  
##                   (Other)   :15840   (Other):17202
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Please see the "newData" dataframe
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newData$steps<-as.numeric(newData$steps)
aggSimulateData <-aggregate(steps~date,newData,FUN=sum)
hist(aggSimulateData$steps, main="Histogram of steps taken per day",xlab="Number of Steps", ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
5. Are there differences in activity patterns between weekdays and week- ends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

```r
#solution from this thread: http://stackoverflow.com/questions/26441700/how-to-determine-if-date-is-a-weekend-or-not-not-using-lubridate
library(chron)
newData$wday <-as.factor(is.weekend(newData$date))
library(plyr)
x<-levels(newData$wday)
revalue(x, c("FALSE"="weekday", "TRUE"="weekend"))
```

```
## [1] "weekday" "weekend"
```

```r
levels(newData$wday)<-x
```
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


## Are there differences in activity patterns between weekdays and weekends?
