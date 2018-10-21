---
title: "Reproducible Research Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Loading and Preprocessing data
```{r}
setwd("C:/Coursera/Reproducible research")
library(dplyr)
activity <- read.csv("./activity.csv")
str(activity)
```

Total number of steps taken per day
```{r}
steps.day <- aggregate(steps~date, activity, sum)
steps.day

```

Make a histogram of the total number of steps taken each day
```{r}
hist(steps.day$steps, main = "Total number of steps by day", col="red", xlab="Number of steps",breaks = 50)
```

The mean of the total number of steps taken per day
```{r}
mean(steps.day$steps)

```
The median of the total number of steps taken per day
```{r}
median(steps.day$steps)
```

The average daily activity pattern
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps.interval <-  aggregate(steps~interval, activity, mean)
plot(steps.interval$interval,steps.interval$steps, type="l", 
     xlab = "Interval", ylab="Number of Steps", main="Average number of steps per day",col="red" )
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max.interval <- steps.interval[which.max(steps.interval$steps),1]
max.interval
```
Calculate and report the total number of missing values

```{r}
sum(is.na(activity$steps))
```

```{r}
mean.interval <- mean(steps.interval$steps)
mean.interval
```

Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r}
imputed.data <- transform(activity, steps = ifelse(is.na(activity$steps),steps.interval$steps[match(activity$interval,steps.interval$interval)],  activity$steps))
imputed.data[as.character(imputed.data$date) == "2012-10-01", 1] <- 0
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r}
steps.day2 <- aggregate(steps ~ date, imputed.data, FUN=sum)
hist(steps.day2$steps, main = paste("Total Steps per Day"), col="red", xlab="Number of Steps",breaks = 50)
mean(steps.day2$steps)
median(steps.day2$steps)
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
imputed.data$date <- as.Date(imputed.data$date)
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday" )
imputed.data$week = as.factor(ifelse(is.element(weekdays(as.Date(imputed.data$date)),weekday), "Weekday", "Weekend"))
steps.interval2 <- aggregate(steps~interval+week,imputed.data,mean)
xyplot(steps.interval2$steps~steps.interval2$interval|steps.interval2$week,main="Average steps per day",xlab = "Interval",ylab = "Steps",layout=c(1,2),type="l")
```





