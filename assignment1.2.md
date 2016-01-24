#Reproducible Research: Peer Assessment 1
    
##Loading and preprocessing the data
1. Load the data

```r
unzip("activity.zip")
rawdata <-read.csv("activity.csv")
```
    
2. Process/transform the data

```r
#remove missing values
completedata<-complete.cases(rawdata)
#subsetting data
data <-rawdata[completedata,]
```
  
##What is the mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
stepSums<-aggregate(data$steps,by=list(data$date),sum,na.rm=TRUE)
## and give it some meaningful column names
colnames(stepSums)<-c("date","sum")
```
  
2. Make a histogram of the total number of stpes taken per day

```r
hist(stepSums$sum, xlab="Steps Taken Per Day", 
     main="Histogram of Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
meanperday <- mean(stepSums$sum)
medianperday <- median(stepSums$sum)
```

The mean of he total number of steps per day is 1.0766189 &times; 10<sup>4</sup> and the median is ` r medianperday`

##What is the average daily activity pattern?

1.Make a time series plot of the 5-minute interval (x-axis) and the average number of days (y-axis).

```r
intervalMeans<-aggregate(data$steps,by=list(data$interval),mean,na.rm=TRUE)
colnames(intervalMeans)<-c("interval","steps")
plot(intervalMeans$interval,intervalMeans$steps,type="l", 

     xlab="5-Minute Interval",ylab="Mean Steps Taken",

     main="Steps during the Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
2.Which 5-minute interval, on average across all days in the statset, contains the maximum number of steps?

```r
maximalinterval <-intervalMeans$interval[which.max(intervalMeans$steps)]
```
The maximum number of steps was during the time inverval 835

##Inputing missing values

1.Calculate and report the total number of missing values in the dataset

```r
numberna <- sum(!complete.cases(data))
numberna
```

```
## [1] 0
```

2. Fill in missing values

```r
data2<-data

iMeans<-by(data$steps,data$interval,mean,na.rm=TRUE)

data2[!complete.cases(data2),1]<-

        iMeans[as.character(data2[!complete.cases(data2),3])]
```

3. Create a new dataset with the filling in missing values

```r
stepSums2<-aggregate(data2$steps,by=list(data2$date),sum)
colnames(stepSums2)<-c("date","sum")
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(stepSums2$sum, xlab="Steps Taken Per Day", 

     main="Histogram of Steps Taken Per Day (with imputed missing data)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
#Calculating the mean and median
meanperdayfilled <- mean(stepSums2$sum)
print(meanperdayfilled)
```

```
## [1] 10766.19
```

```r
medianperdayfilled <-median(stepSums2$sum)
print(medianperdayfilled)
```

```
## [1] 10765
```

The total number of of missing values is 0
The mean total number of steps per day after the mising values are filled in is 1.0766189 &times; 10<sup>4</sup> and the median is 1.0765 &times; 10<sup>4</sup>

The mean values do not change between the data when the missing values are filled in because the mean was used to fill in the misisng values. The median change slighty. The standard variation was reduced after the adding the missing values.

#Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -"weekday" and "weekend" indicating whether a given data is a weekday or weend day.


```r
days<-weekdays(as.Date(data2$date))
weekends<-days=="Saturday"|days=="Sunday"
days[weekends]<-"weekend"
days[!weekends]<-"weekday"
data2$dayType<-factor(days)
aggMeans<-aggregate(data2$steps,by=list(data2$interval,data2$dayType),mean)
colnames(aggMeans)<-c("interval","dayType","steps")
```

2. Make a panel plot containing a time sireis plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays or weekend days (y-axis).


```r
library(lattice)
xyplot(aggMeans$steps ~ aggMeans$interval|aggMeans$dayType,aggMeans,type="l",
       layout=c(1,2),xlab="Interval",ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
There is similar peak activity between weekdays and weekends between time intercal 700 and 900, but there is  more activity on the weekends after 1000. 
