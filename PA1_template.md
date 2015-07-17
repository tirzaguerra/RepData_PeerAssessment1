# Reproducible Research: Peer Assessment 1
Tirza Guerra  

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data. 
 
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data description

The data for this assignment can be downloaded from the course web site:
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

* Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file, and there are a total of 17,568 observations in it. 

## Loading and preprocessing the data

Here, I will assume that the dataset file (activity.csv) is available in the working directory.


```r
activity <- read.csv("activity.csv")
summary(activity)
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

* Calculate the total number of steps taken per day.
* Make a histogram of the total number of steps taken each day.
* Calculate and report the mean and median of the total number of steps taken per day.


```r
stepsPerDay <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
hist(stepsPerDay, main = "Mean Total Number of Steps Taken Per Day", 
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/steps_per_day-1.png) 

```r
mean(stepsPerDay, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
avgPattern <- aggregate(steps ~ interval, data = activity, FUN = mean, 
                        na.rm = TRUE)
plot(steps ~ interval, data = avgPattern, type = "l", col = "blue", 
     main = "Average Daily Activity Pattern",
     xlab = "5-Minute Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/average_pattern-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgPattern[which.max(avgPattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Inputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean of the 5-minute interval across all days to fill the missing values.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fillNAs <- function(steps, interval) {
        value <- NA
        if (!is.na(steps))
                value <- c(steps)
        else
                value <- (avgPattern[avgPattern$interval == interval, "steps"])
        return(value)
}

new_activity <- activity
new_activity$steps <- mapply(fillNAs, new_activity$steps, new_activity$interval)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of inputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDay2 <- tapply(new_activity$steps, new_activity$date, FUN = sum)
hist(stepsPerDay2,
     main = "Mean Total Number of Steps Taken Per Day (no Missing Values)", 
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/steps_per_day2-1.png) 

```r
mean(stepsPerDay2, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay2, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.




```r
new_activity$date <- as.Date(new_activity$date)
new_activity$daytype <- as.factor(ifelse(weekdays(new_activity$date) %in% 
                                                 c("Saturday", "Sunday"), 
                                         "weekend", "weekday"))
summary(new_activity)
```

```
##      steps             date               interval         daytype     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608  
##  Median :  0.00   Median :2012-10-31   Median :1177.5                  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```


```r
library(lattice)
avgPattern2 <- aggregate(steps ~ interval + daytype, data = new_activity, 
                         FUN = mean, na.rm = TRUE)
xyplot(steps ~ interval | daytype, data = avgPattern2, type = "l", 
       main = "Average Daily Activity Pattern",
       xlab = "5-Minute Interval", ylab = "Average Number of Steps", 
       col = "blue", layout = c(1,2))
```

![](PA1_template_files/figure-html/plot_daytype-1.png) 
