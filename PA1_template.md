---
title: "PA1_template.Rmd"
output:
  html_document: default
  pdf_document: default
---



## R Markdown
Loading Data


```r
library(knitr)
library(dplyr)
library(ggplot2)
library(data.table)
library(scales)
library(lubridate)

activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(xx, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'
```

```
## Warning in strptime(x, f, tz = tz): unknown timezone '%Y-%m-%d'
```

```
## Warning in as.POSIXct.POSIXlt(as.POSIXlt(x, tz, ...), tz, ...): unknown timezone '%Y-%m-%d'
```

```r
weekday <- weekdays(activity$date)
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
activity <- cbind(activity,weekday)
```

## Including Plots

You can also embed plots, for example:


```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

![plot of chunk pressure](figure/pressure-1.png)

```
## [1] 9354.23
```

```
## [1] 10395
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



```r
## What is the average daily activity pattern?
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```

```
## [1] 835
```


```r
## Imputing missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
names(total_steps_imputed) <- c("date", "daily_steps")
hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
mean(total_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```


```r
## Are there differences in activity patterns between weekdays and weekends?

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
```

```
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'
```

```r
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

