library(dplyr)
library(ggplot2)

#Setup
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)

#Mean total number of steps taken per day
totalSteps_day <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(sum = sum(steps))
hist(totalSteps_day$sum, breaks = 20, xlab = "Number of Steps", main = "Histogram of Total Steps per Day")

totalSteps_day_mean <- mean(totalSteps_day$sum)
print(totalSteps_day_mean)

totalSteps_day_median <- median(totalSteps_day$sum)
print(totalSteps_day_median)

#Avg Daily Pattern
avgSteps_day <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(avg = mean(steps))

avgSteps_day <- as.data.frame(avgSteps_day)

plot(avgSteps_day$interval, avgSteps_day$avg, type = "l", xlab = "Interval", ylab = "Avg Steps", main = "Avg Steps per Interval")
lines(avgSteps_day$interval, avgSteps_day$avg)

maxSteps <- max(avgSteps_day$avg)
print(maxSteps)

#Missing Values
isna <- activity %>%
        filter(is.na(steps)) %>%
        nrow()
print(isna)

activity2 <- activity
for(interval in unique(activity2$interval)) {
        activity2[(is.na(activity2$steps) & activity2$interval == interval),]$steps <- avgSteps_day[avgSteps_day$interval == interval,]$avg
}
totalSteps_day2 <- activity2 %>%
        group_by(date) %>%
        summarise(sum = sum(steps))
hist(totalSteps_day2$sum, breaks = 20, xlab = "Number of Steps", main = "Histogram of Total Steps per Day")

totalSteps_day_mean2 <- mean(totalSteps_day2$sum)
print(totalSteps_day_mean2)

totalSteps_day_median2 <- median(totalSteps_day2$sum)
print(totalSteps_day_median2)

#Weekday
activity2$day <- weekdays(activity2$date)
activity2$is_weekday <- ifelse(activity2$day == c("Saturday", "Sunday"), "Weekdays", "Weekend")

avgSteps_day2 <- activity2 %>%
        group_by(interval, is_weekday) %>%
        summarise(avg = mean(steps))

g <- ggplot(avgSteps_day2, aes(interval, avg)) + geom_line(aes(group = 1)) + facet_grid(is_weekday~.) + ylab("Number of Steps") + xlab("Interval") + title("Avg Steps by Interval")



