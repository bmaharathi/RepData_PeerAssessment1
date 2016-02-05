# reproducible research w1 assignment 
#Load the data (i.e. read.csv())

### load required library
library(dplyr)
library(lattice)

#Loading and preprocessing the data

###Show any code that is needed to
###Load the data (i.e. read.csv())
###Process/transform the data (if necessary) into a format suitable for your analysis
#reading the activity data and pre-processing 
activity <- read.csv('activity.csv', header = TRUE, sep = ",")
activity$date <- as.Date(activity$date)


##What is mean total number of steps taken per day?
###For this part of the assignment, you can ignore the missing values in the dataset.
###Calculate the total number of steps taken per day
###If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
###Calculate and report the mean and median of the total number of steps taken per day

activity2 <- group_by(activity,date)
activity2 <- na.omit(activity2)
dailysteps <- summarise(activity2,total_steps = sum(steps))
summary(dailysteps)
hist(dailysteps$total_steps,main = "Total steps taken per day",
     xlab = "Total number of steps",col = "blue",
     breaks= length(dailysteps$total_steps)-1,plot = TRUE)

mean(dailysteps$total_steps)
median(dailysteps$total_steps)



##What is the average daily activity pattern?
activity3 <- group_by(activity, interval)
activity3 <- na.omit(activity3)

avgsteps_interval <- summarise(activity3, avg_steps = mean(steps)) 
plot(avgsteps_interval,type = 'l', col = 'brown',lwd=2,
     main = "Average number of steps taken across all days in 5min intervals",
     xlab = "5min intervals",ylab = "average number of steps taken")

maxsteps <- filter(avgsteps_interval, avg_steps == max(avg_steps))
cat(sprintf("At %sth interval, the maximum number of %f steps are found. \n", maxsteps$interval, maxsteps$avg_steps))

##Imputing missing values
missing_steps <- sum(is.na(activity$steps))
missing_date <- sum(is.na(activity$date))
missing_interval <- sum(is.na(activity$interval))
#
activity4<- activity
avgsteps <- summarise(activity3, mean_steps = mean(steps), median_steps = median(steps))

activity4$steps[is.na(activity4$steps)] <- tapply(activity4$steps, activity4$interval, mean, na.rm = TRUE)
#
activity5 <- group_by(activity4, date)
dailysteps_afms <- summarise(activity5,total_steps = sum(steps))

hist(dailysteps_afms$total_steps,breaks=length(dailysteps_afms$total_steps)-1, 
     col = 'red', main = "Histogram of total number of steps taken each day",
     xlab = "Total number of Steps",plot = T)
#
new_mean <-mean(dailysteps_afms$total_steps)
new_median <-median(dailysteps_afms$total_steps)


#
activity6<- activity5
activity6$wdays <- weekdays(as.Date(activity6$date))

activity6 <-mutate(activity6, wk.factor = factor((wdays == "Sunday" | wdays == "Saturday"),levels = c(FALSE,TRUE),
                                     labels = c("weekdays","weekends")))

MeanStep_byIntervalByWkday <- aggregate(steps ~ interval + wk.factor, data=activity6, FUN="mean")

xyplot(steps ~ interval | wk.factor, data=MeanStep_byIntervalByWkday, type="l", grid=T, layout=c(1,2),
       main="Plot for comparision of number of steps on weekdays and weekends",
       xlab = "5min. intervals", ylab = "Number of steps",lwd = 2,col = "blue")
