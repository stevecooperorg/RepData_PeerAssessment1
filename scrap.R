rm(list=ls())

library(dplyr); library(knitr); library(lattice)

if (!file.exists("./activity.csv")) { unzip('./activity.zip'); }
activity <- read.csv("./activity.csv", colClasses = c("numeric","POSIXct","numeric"))  


stepsPerDay <- activity %>%
    group_by(date) %>%
    summarise(totalStepsPerDay = sum(steps, na.rm = TRUE)) %>%
    mutate(totalStepsPerDay = ifelse(is.na(totalStepsPerDay), 0, totalStepsPerDay))

print(hist(stepsPerDay$totalStepsPerDay, col = "red", breaks = 20, main = "Histogram of total steps per day", xlab = "total steps per day"))

meanTotalStepsPerDay <- mean(stepsPerDay$totalStepsPerDay)
medianTotalStepsPerDay <- median(stepsPerDay$totalStepsPerDay)

fiveMinuteIntervals <- activity %>%
    group_by(interval) %>%
    summarise(mean = mean(steps,na.rm = TRUE))

print(plot(x = fiveMinuteIntervals$interval, y=fiveMinuteIntervals$mean,  type="l", main="mean number of steps in each interval", xlab="interval (time of day)", ylab="mean"))

whichIntervalHasTheMax <- which(fiveMinuteIntervals$mean == max(fiveMinuteIntervals$mean))
whichIntervalIsIt <- fiveMinuteIntervals[whichIntervalHasTheMax, "interval"]

print(whichIntervalIsIt)

numberOfMissingValues <- sum(is.na(activity$steps))

activityWithMeanValues <- activity %>% 
    dplyr::inner_join(fiveMinuteIntervals, by='interval') %>%
    mutate(imputedSteps = ifelse(is.na(steps), mean, steps)) %>%
    select(imputedSteps, date, interval)

stepsPerDayImputed <- activityWithMeanValues %>%
    group_by(date) %>%
    summarise(totalStepsPerDay = sum(imputedSteps, na.rm = TRUE)) %>%
    mutate(totalStepsPerDay = ifelse(is.na(totalStepsPerDay), 0, totalStepsPerDay))

print(hist(stepsPerDayImputed$totalStepsPerDay, col = "red", breaks = 20, main = "Histogram of total steps per day\nwith imputed values", xlab = "total steps per day"))

meanTotalStepsPerDayImputed <- mean(stepsPerDayImputed$totalStepsPerDay)
medianTotalStepsPerDayImputed <- median(stepsPerDayImputed$totalStepsPerDay)

impactOnMean = abs(meanTotalStepsPerDay - meanTotalStepsPerDayImputed) / meanTotalStepsPerDay
impactOnMedian = abs(medianTotalStepsPerDay - medianTotalStepsPerDayImputed) / medianTotalStepsPerDay

withWeekdayFactor <- function(dt) { 
    dt %>% mutate(weekday = factor(ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")))
} 

activityWithWeekdays <- activityWithMeanValues %>% withWeekdayFactor %>% group_by(weekday, interval) %>% summarise(totalSteps=sum(imputedSteps))

print(xyplot(activityWithWeekdays$total ~ activityWithWeekdays$interval | activityWithWeekdays$weekday, type="l", 
             layout = c(1,2),    
             main="mean number of steps in each interval", xlab="interval (time of day)", ylab="mean"))
