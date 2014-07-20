# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
setwd("C:/Users/Feng/Downloads/data science")

data <- read.csv("./reproducible research/RepData_PeerAssessment1/activity.csv")


## What is mean total number of steps taken per day?


#check pattern of missing data
#data$missing <- rep(0, times=length(data$steps))
#data$missing[is.na(data$steps)] <- 1
#daily_missing <- tapply(data$missing, data$date, sum)
#table(daily_missing)
#no missing records within a day, either all filled or all missing

#ignore missing days
data1 <- data[complete.cases(data),]

daily_steps <- tapply(data1$steps, data1$date, sum)
#sum(is.na(daily_steps))

hist(daily_steps)

mean <- mean(daily_steps, na.rm=TRUE)

median <- median(daily_steps, na.rm=TRUE)

## What is the average daily activity pattern?
data1$time <- factor(data1$interval)
interval_steps <- tapply(data1$steps, data1$time, mean)
plot(interval_steps, type="l")
which.max(interval_steps)

## Imputing missing values
missing <- is.na(data$steps)
#1.the total number of missing values in the dataset
sum(missing)
#2.create a new data set "imputed"
#filling in all of the missing values in the dataset
#using the mean for that 5-minute interval
data$time <- factor(data$interval)
imputed <- data
for (i in 1:nrow(data)){
    if (is.na(imputed[i,"steps"]) ) {
        imputed[i,"steps"] <- interval_steps[imputed[i,"time"]]
    }
}
#3.histogram, mean median
daily_steps_imputed <- tapply(imputed$steps, imputed$date, sum)
hist(daily_steps_imputed)

mean(daily_steps_imputed)

median(daily_steps_imputed)
#impact



## Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable with two levels - "weekday" and "weekend"
imputed$d <- weekdays(as.Date(imputed$date, "%Y-%m-%d"))
imputed$weekday <- ifelse(imputed$d == "Saturday" | imputed$d == "Sunday",
                          "weekend", "weekday")

#subset data into weekdays and weekends
weekend <- imputed[imputed$weekday=="weekend",]
weekday <- imputed[imputed$weekday=="weekday",]

#calculate seperately average number of steps taken at given time interval
weekend_steps <- tapply(weekend$steps, weekend$time, mean)


weekday_steps <- tapply(weekday$steps, weekday$time, mean)


steps <- c(weekend_steps, weekday_steps)
day <- c(rep("weekend",length=length(weekend_steps)),
               rep("weekday",length=length(weekday_steps)))
a <- unique(imputed$time)
time <- c(a,a)
figure <- data.frame(steps, time, day)

#plotting the average number of steps taken by time intervals, 
#averaged across all weekday days or weekend days 
library(lattice)
xyplot(steps ~ time | day, data=figure, type='l',layout=c(1,2))
