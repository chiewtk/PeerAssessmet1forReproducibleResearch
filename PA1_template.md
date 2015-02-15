# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
dat<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

### 1. Total footsteps per day over trial period:


```r
fssum<-tapply(dat$steps,dat$date,sum,na.rm=T)
fssum
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

### 2. Histogram of daily footsteps taken:


```r
hist(fssum)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### 3. Brief Summary of the daily footsteps over trial period:


```r
fsmean<-mean(fssum); fsmean
```

```
## [1] 9354.23
```

```r
fsmedian<-median(fssum); fsmedian
```

```
## [1] 10395
```

- Mean   = 9354.2295082
- Median = 10395

## What is the average daily activity pattern?

### 1. Time Series Plot of time_within_the_day:


```r
steps_per_5_min_slots<-tapply(dat$steps,dat$interval,mean,na.rm=T)
barplot(steps_per_5_min_slots)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### 2. Finding the time interval with maximum activity:


```r
names(which(steps_per_5_min_slots==max(steps_per_5_min_slots)))
```

```
## [1] "835"
```

Hence the peak footstep activity happens at **0835H**.

## Imputing missing values

### 1. Counting missing values:

```r
missingCnt<-sum(is.na(dat));
missingCnt;
```

```
## [1] 2304
```

- There are 2304 missing values in the dataset.

### 2. Strategy for missing data replacement:

- if there is activity on that day, take the mean footsteps.
- if there is no activity on that day, set to mean of whole trial period.
- filler array contains substitution values.


```r
overall_mean_foot_steps<-mean(dat$steps,na.rm=T)
overall_mean_foot_steps
```

```
## [1] 37.3826
```

```r
mean_foot_steps_of_the_day<-tapply(dat$steps,dat$date,mean,na.rm=T)
mean_foot_steps_of_the_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        NaN  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
## 38.2465278        NaN 44.4826389 34.3750000 35.7777778 60.3541667 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
## 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
## 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
## 53.5208333        NaN 36.8055556 36.7048611        NaN 36.2465278 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 28.9375000 44.7326389 11.1770833        NaN        NaN 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
## 37.3784722 25.4722222        NaN  0.1423611 18.8923611 49.7881944 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
## 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500 
## 2012-11-30 
##        NaN
```

```r
mean_foot_steps_of_the_day[is.nan(mean_foot_steps_of_the_day)]<-overall_mean_foot_steps
mean_foot_steps_of_the_day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
## 37.3825996  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
## 38.2465278 37.3825996 44.4826389 34.3750000 35.7777778 60.3541667 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
## 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
## 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
## 53.5208333 37.3825996 36.8055556 36.7048611 37.3825996 36.2465278 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 28.9375000 44.7326389 11.1770833 37.3825996 37.3825996 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
## 37.3784722 25.4722222 37.3825996  0.1423611 18.8923611 49.7881944 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
## 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500 
## 2012-11-30 
## 37.3825996
```

```r
filler<-mean_foot_steps_of_the_day[dat$date];
```

### 3. Create new data without missing values -> dat1


```r
dat1<-dat;
sprintf("Number of NAs before tidying: %d", sum(is.na(dat1)))
```

```
## [1] "Number of NAs before tidying: 2304"
```

```r
dat1[is.na(dat1),"steps"]<-filler[is.na(dat1)];
sprintf("Number of NAs after tidying: %d", sum(is.na(dat1)))
```

```
## [1] "Number of NAs after tidying: 0"
```

### 4. Finding statistics of dat1 and comparing them with dat

Results below shows that the imputing missing data **does not** affect the statistics.


```r
fssum1<-tapply(dat$steps,dat$date,sum,na.rm=T)
fssum1
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

```r
hist(fssum1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
fsmean1<-mean(fssum1); c(fsmean1,fsmean)
```

```
## [1] 9354.23 9354.23
```

```r
fsmedian1<-median(fssum1); c(fsmedian1,fsmedian)
```

```
## [1] 10395 10395
```

```r
fshist1<-hist(fssum1,plot=F)
fshist <-hist(fssum ,plot=F)
cbind(fshist1$counts, fshist$counts)
```

```
##      [,1] [,2]
## [1,]   13   13
## [2,]   12   12
## [3,]   28   28
## [4,]    6    6
## [5,]    2    2
```

## Are there differences in activity patterns between weekdays and weekends?

### 1. A new ["weekday" "weekend"] factor:


```r
isWeekend<-grepl("Saturday|Sunday",weekdays(as.Date(dat1$date)))
WEWD<-1:length(isWeekend)
WEWD[isWeekend]<-"weekend"
WEWD[isWeekend==F]<-"weekday"
dat2<-cbind(dat1,factor(WEWD))
dimnames(dat2)[[2]][4]<-"weekday_weekend"
names(dat2)
```

```
## [1] "steps"           "date"            "interval"        "weekday_weekend"
```

```r
head(dat2)
```

```
##     steps       date interval weekday_weekend
## 1 37.3826 2012-10-01        0         weekday
## 2 37.3826 2012-10-01        5         weekday
## 3 37.3826 2012-10-01       10         weekday
## 4 37.3826 2012-10-01       15         weekday
## 5 37.3826 2012-10-01       20         weekday
## 6 37.3826 2012-10-01       25         weekday
```

```r
tail(dat2)
```

```
##         steps       date interval weekday_weekend
## 17563 37.3826 2012-11-30     2330         weekday
## 17564 37.3826 2012-11-30     2335         weekday
## 17565 37.3826 2012-11-30     2340         weekday
## 17566 37.3826 2012-11-30     2345         weekday
## 17567 37.3826 2012-11-30     2350         weekday
## 17568 37.3826 2012-11-30     2355         weekday
```

### 2. Plot of 2-panel time-series:


```r
toplot<-tapply(dat2$steps, list(dat2$interval,dat2$weekday_weekend), mean, na.rm = T)
op <- par(mfcol = c(2, 1)) 
c11 <- plot(dimnames(toplot)[[1]],toplot[,"weekday"],xlab="time-of-day at 5-min intervals", ylab="Weekday Mean Steps",mfg=c(1, 1),type="l")
c21 <- plot(dimnames(toplot)[[1]],toplot[,"weekend"],xlab="time-of-day at 5-min intervals", ylab="Weekend Mean Steps",mfg=c(2, 1),type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
par(op); #Restore graphics parameters
```

Step activities throughout the days vary differently in weekdays and weekends.
During weekdays, activitiy starts earlier, probably corresponding to work commutes.
There is distinct peak at around 8:30 am to support such phenomenum.
Activities then decreases to below half the peak and stays until 8:00 pm.

During weekends, step acitivities begins later, and sustains high level throughout the days.

Overall, level of activities in weekends is higher then that in the weekday. Probably this reflects the increasing sedantary nature of work. Weekends see an increase in step activities, which is desirable for a healthy lifestye. 
