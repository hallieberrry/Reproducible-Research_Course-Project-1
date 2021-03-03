---
title: "Reproducible Research_Course Project 1"
author: "Hallieberrry"
date: "3/2/2021"
output: html_document, file:///Users/halliezimmerman/Dropbox/My%20Mac%20(hallie-zimmermans-macbook.local)/Desktop/Reproducible-Research_Course-Project-1.html
---
1) Loading And Preprocessing The Data
```{r}
library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)


setwd("/Users/halliezimmerman/Dropbox/My Mac (hallie-zimmermans-macbook.local)/Desktop/")
activity <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

activity$date <- ymd(activity$date)

activity1 <- na.omit(activity)

activity$monthly <- as.numeric(format(activity$date, "%m"))
activity$daily <- as.numeric(format(activity$date, "%d"))

# Quick check
summary(activity1)

str(activity1)

head(activity1)

tail(activity1)
```

2) What is the average daily activity pattern?
```{r}
##Calculate the total number of steps taken per day
activity2 <- summarize(group_by(activity1,date),daily.step=sum(steps))
mean.activity <- as.integer(mean(activity2$daily.step))
median.activity <- as.integer(median(activity2$daily.step))

##Make a histogram of the total number of steps taken each day
plot.steps.day <- ggplot(activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.activity, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.activity, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
plot.steps.day

```

```{r}
## Calculate and report the mean and median of the total number of steps taken per day
mean.activity
median.activity
```

3) What is the average daily activity pattern?
```{r}
# Prepare data for ggplot
activity3 <- activity1 %>% group_by(interval) %>% summarize(mean.step=mean(steps))

# Plot average number of steps by 5-minute interval
plot.step.interval <- ggplot(activity3, aes(x=interval,y=mean.step)) + 
  geom_line(color="red") + 
  labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot.step.interval
```
```{r}
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

optimal <- which.max(activity3$mean.step)
optimal.step <- activity3$interval[optimal]
sprintf("Maximum number of steps is coming from %gth 5-min interval", optimal.step)
```

4) Imputing Missing Values

```{r}
# Total # of missing values in dataset
sum(is.na(activity))

```

```{r}
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

impute.activity <- activity
impute.activity$steps[is.na(impute.activity$steps)] <- mean(impute.activity$steps,na.rm=TRUE)
impute.activity$steps <- as.numeric(impute.activity$steps)
impute.activity$interval <- as.numeric(impute.activity$interval)
colSums(is.na(impute.activity))

##Create a new dataset that is equal to the original dataset but with the missing data filled in.
summary(impute.activity)
```
```{r}
##Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
impute.activity2 <- summarize(group_by(impute.activity,date),daily.step=sum(steps))

mean.impute   <- as.integer(mean(impute.activity2$daily.step))
mean.impute

median.impute <- as.integer(median(impute.activity2$daily.step))
median.impute

# Plot histogram
plot.steps.day <- ggplot(impute.activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.impute, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.impute, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
plot.steps.day
```
5) Are there differences in activity patterns between weekdays and weekends?
```{r}
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
impute.activity$day <- ifelse(weekdays(impute.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")
```

```{r}
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:
# Preparing data for ggplot
impute.df <- impute.activity %>% group_by(interval, day) %>% summarise(mean.step=mean(steps))

# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval
```
