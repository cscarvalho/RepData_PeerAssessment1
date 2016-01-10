## Reproducible Research: Peer Assessment 1
 
# Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
file="activity.csv"
activity=read.csv(file)
```
# What is mean total number of steps taken per day? [For this part of the assignment, you can ignore the missing values in the dataset.]

1. Make a histogram of the total number of steps taken each day


```r
sumsteps=aggregate(steps ~ date, data = activity, FUN = sum)
```

```r
barplot(sumsteps$steps, names.arg = sumsteps$date, xlab = "date", ylab = "no. of steps")
```

![plot of chunk sim_2_plot](figure/sim_2_plot-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean_sumsteps=mean(sumsteps$steps)
median_sumsteps=median(sumsteps$steps)
```
mean=`mean_sumsteps`
median=`median_sumsteps`

# What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meansteps=aggregate(steps ~ interval, data = activity, FUN = mean)
```

```r
plot(meansteps,type="l",ylab="mean no. of steps")
```

![plot of chunk sim_4_plot](figure/sim_4_plot-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_maxstep=meansteps$interval[which.max(meansteps$steps)]
```
interval=`interval_maxstep`

# Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)


```r
n_na=sum(is.na(activity))
```
Number of NA's=n_na

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_2=merge(activity, meansteps, by = "interval", suffixes = c("", ".mean"))
nas=is.na(activity_2$steps)
activity_2$steps[nas]=activity_2$steps.mean[nas]
activity_2=activity_2[, c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sumsteps_2=aggregate(steps ~ date, data = activity_2, FUN = sum)
```

```r
barplot(sumsteps_2$steps, names.arg = sumsteps_2$date, xlab = "date", ylab = "no. of steps")
```

![plot of chunk sim_8_plot](figure/sim_8_plot-1.png) 

```r
mean_sumsteps_2=mean(sumsteps_2$steps)
median_sumsteps_2=median(sumsteps_2$steps)
```
mean_2=`mean_sumsteps_2`
median_2=`median_sumsteps_2`
Answer: Impact is small.

# Are there differences in activity patterns between weekdays and weekends? [For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.]

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
daytype=function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity_2$daytype=as.factor(sapply(activity_2$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    meansteps_2=aggregate(steps ~ interval, data = activity_2, 
      subset = activity_2$daytype==type, FUN = mean)
    plot(meansteps_2, type = "l", main = type)
}
```

![plot of chunk sim_10](figure/sim_10-1.png) 
