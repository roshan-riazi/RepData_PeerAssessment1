## Loading and preprocessing the data
unzip("./activity.zip")
activity <- read.csv("./activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d", tz = "GMT")
intervalMin <- activity$interval %% 100
intervalHour <- floor(activity$interval / 100)
activity$time <- paste(intervalHour, intervalMin, sep = ":")
activity$time <- as.POSIXct(strptime(activity$time, "%H:%M", tz = "GMT"))
activity$dateTime <- paste(activity$date, intervalHour, intervalMin, sep = " ")
activity$dateTime <- as.POSIXct(strptime(activity$dateTime, format = "%Y-%m-%d %H %M", tz = "GMT"))
#activity$interval <- as.factor(activity$interval)

## What is mean total number of steps taken per day?
library(dplyr)
daySteps <- group_by(activity, date) %>% summarize(totalSteps = sum(steps, na.rm = T))
hist(daySteps$totalSteps, breaks = 10, xlab = "Total number of steps per day", main = "Histogram of total number of steps per day")
meanStepsPerDay <- mean(daySteps$totalSteps)
medianStepsPerDay <- median(daySteps$totalSteps)

## What is the average daily activity pattern?
intervalSteps <- group_by(activity, time) %>%
    summarize(averageSteps = mean(steps, na.rm = T))
plot(intervalSteps, type = "l", xlab = "Time", ylab = "Average Number of Steps", main = "Average number of steps during the day")
maxSteps <- intervalSteps[which.max(intervalSteps$averageSteps), 1]
maxSteps <- as.POSIXlt(maxSteps$time)
paste(maxSteps$hour, maxSteps$min, sep = ":")

## Imputing missing values
library(lubridate)
sapply(activity, function(x){sum(is.na(x))})
activity <- mutate(activity, wday = wday(dateTime))
imputing <- group_by(activity, wday, interval) %>% summarize(mean = mean(steps, na.rm = T))
activityImputed <- activity
imputeIndex <- which(is.na(activityImputed$steps))
for(i in imputeIndex){
    activityImputed[i, "steps"] <- imputing[(activityImputed$wday[i] == imputing$wday) & (activityImputed$interval[i] == imputing$interval), 3]
}

dayStepsImputed <- group_by(activityImputed, date) %>% summarize(totalSteps = round(sum(steps), digits = 0))
hist(dayStepsImputed$totalSteps, breaks = 10, xlab = "Total number of steps per day", main = "Histogram of total number of steps per day")
meanStepsPerDayImputed <- mean(dayStepsImputed$totalSteps)
medianStepsPerDayImputed <- median(dayStepsImputed$totalSteps)

## Are there differences in activity patterns between weekdays and weekends?
activity$dayType <- factor(rep("weekday", nrow(activity)), levels = c("weekday", "weekend"))
activity$dayType[activity$wday == 7 | activity$wday == 1] <- "weekend"
library(lattice)
intervalSteps2 <- group_by(activity, time, dayType) %>%
    summarize(averageSteps = mean(steps, na.rm = T))
intervalSteps2$hours <- hour(intervalSteps2$time) + minute(intervalSteps2$time)/60
xyplot(averageSteps ~ hours | dayType, data = intervalSteps2, layout = c(1, 2), type = "l", xlab = "Time (hours)", ylab = "Number of Steps", xlim = c(0, 24))