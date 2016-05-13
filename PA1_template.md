# Reproducible Research: Peer Assessment 1
### Reproducible Research, Project 1 --- by Catherine Weiss



##Introduction

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals throughout the day. The data 
consists of two months of data from an anonymous individual collected during the 
months of October and November 2012 and include the number of steps taken in 
5 minute intervals each day.


## 1.  Loading and preprocessing the data

The original dataset, 'Activity monitoring data', is loaded into R.  
There are three variables in the dataset: steps, date and interval. To 
pre-process the data, the date variable is converted into date format. 


```r
library(knitr)
opts_chunk$set(message=FALSE)
```


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/activity.zip", method = "curl")  
```

```
## Warning in download.file(fileUrl, destfile = "./data/activity.zip", method
## = "curl"): download had nonzero exit status
```

```r
unzip("activity.zip", exdir = "./")
```

```
## Warning in unzip("activity.zip", exdir = "./"): error 1 in extracting from
## zip file
```

```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data, 15)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## 11    NA 2012-10-01       50
## 12    NA 2012-10-01       55
## 13    NA 2012-10-01      100
## 14    NA 2012-10-01      105
## 15    NA 2012-10-01      110
```



## 2.  What is mean total number of steps taken per day?

Ignoring the missing values in the dataset, the total number of steps taken each day are shown in the following table:


```r
library(dplyr)
dataTbl <- tbl_df(data)
byDate <- group_by(data, date)
dailySteps <- summarize(byDate, totalSteps = sum(steps), na.rm=TRUE)


# Alternatively,
#dailySteps <- aggregate(steps~date, data, sum)

names(dailySteps) <- c("date", "dailySteps")
dailySteps
```

```
## Source: local data frame [61 x 3]
## 
##          date dailySteps    NA
##        (date)      (int) (lgl)
## 1  2012-10-01         NA  TRUE
## 2  2012-10-02        126  TRUE
## 3  2012-10-03      11352  TRUE
## 4  2012-10-04      12116  TRUE
## 5  2012-10-05      13294  TRUE
## 6  2012-10-06      15420  TRUE
## 7  2012-10-07      11015  TRUE
## 8  2012-10-08         NA  TRUE
## 9  2012-10-09      12811  TRUE
## 10 2012-10-10       9900  TRUE
## ..        ...        ...   ...
```

The total number of steps taken each day are shown in the following histogram:


```r
# plot histogram to html document
hist(dailySteps$dailySteps, xlab="Total Daily Steps", main="Histogram of Total Daily Steps")
```

![](PA1_template_files/figure-html/histogramTotalDailySteps-1.png)

Calculate and report the mean and median of the total number of steps taken per day:


```r
mean <- mean(dailySteps$dailySteps, na.rm=TRUE)
median <- median(dailySteps$dailySteps, na.rm=TRUE)

mean
```

```
## [1] 10766.19
```

```r
median
```

```
## [1] 10765
```

The mean and median of Total Daily Steps (disregarding NA's) are shown above.



## 3.  What is the average daily activity pattern?

Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  


```r
intervalMean <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(unique(data$interval), intervalMean, 
        type="l",
        main="Time Series of Average Number of Steps Taken", 
        xlab="Interval", 
        ylab="Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/averageDailyActivity-1.png)


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalData <- data.frame(unique(data$interval), intervalMean)
names(intervalData) <- c("interval", "AverageSteps")
maxSteps <- max(intervalData$AverageSteps)
maxInterval <- intervalData[intervalData$AverageSteps==maxSteps, ]

# Alternatively,
# ordered <- intervalData[rev(order(intervalData$AverageSteps)),]
# ordered[1,]
```

The 5-minute interval that contains the maximum number of steps and the maximum number of steps are (interval, steps): 
835, 206.1698113



## 4.  Imputing missing values

There are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.  Calculate the total numer of missing values in the dataset.


```r
missingValues <- length(data$steps[is.na(data$steps)])
```

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is 2304.


The strategy for filling in, or imputing, all of the missing values in the dataset will be to use the mean for the corresponding 5-minute interval.  A new dataset, 'imputedData', is created with the missing data filled in.  The column labeled 'steps' represents a combination of original data values and imputed values.  The column labeled 'AverageSteps' is the average number of steps taken across all days during that interval.


```r
imputedData <- merge(data, intervalData, by="interval" )

imputedData$steps[which(is.na(imputedData$steps))] <-                        imputedData$AverageSteps[which(is.na(imputedData$steps))]

ordered <- imputedData[order(imputedData$date, imputedData$interval), ]

head(ordered, 15)
```

```
##     interval     steps       date AverageSteps
## 1          0 1.7169811 2012-10-01    1.7169811
## 63         5 0.3396226 2012-10-01    0.3396226
## 128       10 0.1320755 2012-10-01    0.1320755
## 205       15 0.1509434 2012-10-01    0.1509434
## 264       20 0.0754717 2012-10-01    0.0754717
## 327       25 2.0943396 2012-10-01    2.0943396
## 376       30 0.5283019 2012-10-01    0.5283019
## 481       35 0.8679245 2012-10-01    0.8679245
## 495       40 0.0000000 2012-10-01    0.0000000
## 552       45 1.4716981 2012-10-01    1.4716981
## 620       50 0.3018868 2012-10-01    0.3018868
## 716       55 0.1320755 2012-10-01    0.1320755
## 770      100 0.3207547 2012-10-01    0.3207547
## 840      105 0.6792453 2012-10-01    0.6792453
## 880      110 0.1509434 2012-10-01    0.1509434
```

Make a histogram of the total number of steps taken each day.


```r
dailyStepsImputed <- aggregate(steps~date, ordered, sum)

# plot histogram to html document
hist(dailyStepsImputed$steps, 
        xlab="Total Daily Steps", 
        main="Histogram of Total Daily Steps with Imputed Values")
```

![](PA1_template_files/figure-html/histogramTotalDailyStepsImputed-1.png)


Calculate and report the mean and median total number of steps taken per day.  Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
meanImputed <- mean(dailyStepsImputed$steps, na.rm=TRUE)
medianImputed <- median(dailyStepsImputed$steps, na.rm=TRUE)

meanImputed
```

```
## [1] 10766.19
```

```r
medianImputed
```

```
## [1] 10766.19
```

Using imputed values, the mean and median of Total Daily Steps are shown above.  We see that by substituting NA values with the mean for that 5-minute interval, the mean and median are changed only a negligible amount (no change for mean and only one step per day difference for median).


## 5.  Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. (For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.)


```r
ordered$day <- weekdays(ordered$date)
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")

ordered$day[which(ordered$day %in% weekday)] <- "weekday"
ordered$day[which(ordered$day %in% weekend)] <- "weekend"
ordered <- transform(ordered, day=factor(day))
```

Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(dplyr)
activity <- tbl_df(ordered)
byDayInterval <- group_by(activity, day, interval)
per_interval <- summarize(byDayInterval, averageSteps = mean(steps))

# plot xyplot to html document
library(lattice)
xyplot(averageSteps ~ interval | day, data = per_interval, type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/averageIntervalStepsWeekdayWeekend-1.png)
