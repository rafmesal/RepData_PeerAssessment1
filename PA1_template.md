1.  Load libraries:

``` r
library(ggplot2)
library(lattice)
```

1.  Load data:

``` r
activity <- read.csv("activity.csv", header = TRUE,stringsAsFactors = FALSE)

#View a data summary before graphing
summary(activity)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

What is mean total number of steps taken per day?
=================================================

1.  Make a histogram of ttal steps taken by day:

``` r
hist(tapply(activity$steps, activity$date, sum, na.rm = FALSE), main = "Number of steps by day",xlab = "Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median total number of steps taken per day

``` r
noNAs_activity <- na.omit(activity)
mean_totalSteps <- mean(tapply(na.omit(noNAs_activity$steps),noNAs_activity$date,sum,na.rm=FALSE))
median_totalSteps <- median(tapply(na.omit(noNAs_activity$steps),noNAs_activity$date,sum,na.rm=FALSE))

print(mean_totalSteps)
```

    ## [1] 10766.19

``` r
print(median_totalSteps)
```

    ## [1] 10765

If we don't cosider the NA values the mean is: 10,766.19 steps per day; and the median is: 10,765 steps per day.

What is the average daily activity pattern?
===========================================

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
weekAverage <- aggregate(steps ~ interval, data = activity,FUN = "mean")
plot(weekAverage$interval, weekAverage$steps, type = "l", xlab = "Intervalo",ylab = "Number of steps",main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
intervalMaxNsteps <- weekAverage$interval[which.max(weekAverage$steps)]
print(intervalMaxNsteps)
```

    ## [1] 835

The interval that contains the maximum number of steps is 835.

Imputing missing values
=======================

1.  Calculate and report the total number of missing values in the dataset

``` r
missingVals <- length(activity$steps[which(is.na(activity$steps)==TRUE)])
print(missingVals)
```

    ## [1] 2304

The total number of missing values in the data set is 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.

I will to replace the missing values with the value of the mean number of steps for each 5-minute interval

``` r
replaceMissings <- as.data.frame(tapply(activity$steps, activity$interval, mean,na.rm=TRUE))
names(replaceMissings) <- c("steps")
```

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
activity[is.na(activity)] = replaceMissings$steps
activity$steps<-round(activity$steps,0)
summary(activity)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 27.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
hist(tapply(activity$steps,activity$date,sum,na.rm=FALSE),main="Distribution of number of steps taken each day", xlab="Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
#New mean and median values
mean_noMissVal<-mean(tapply(na.omit(activity$steps),activity$date,sum,na.rm=FALSE))
median_noMissVal<-median(tapply(na.omit(activity$steps),activity$date,sum,na.rm=FALSE))
```

After we changed the NA values for the mean values, the mean is 10,765.64 and the median is 10,762. After to replace the missing values with the mean value, did not change significatively the mean and the median values. In the histogram we can see a distribution with more frequency in the interval where the median is.

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

``` r
activity$date <- as.Date(activity$date)
activity$weekday <- weekdays(activity$date)
activity$weekday[activity$weekday=="lunes" | activity$weekday=="martes" | activity$weekday=="miércoles" | activity$weekday=="jueves" | activity$weekday=="viernes"] <- "weekday"
activity$weekday[activity$weekday=="sábado" | activity$weekday=="domingo"] <- "weekend day"
activity$weekday <- factor(activity$weekday)
new_weekAverage <- aggregate(steps ~ interval + weekday, data = activity, FUN= "mean" )

#Comparative graph
xyplot(new_weekAverage$steps ~ new_weekAverage$interval | new_weekAverage$weekday, type="l", layout=c(1,2), xlab="Interval", ylab="Number of Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

We can see that in weekends people walks more than in the weekdays. But in the weekdays we can see a peak in the interval between 800 to 950 and people starts to walk earlier than during the weekend.
