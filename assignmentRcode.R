setwd("/home/adrien/Work/Training/data-scientist-specialization/05-Reproducible-research/RepData_PeerAssessment1/")
rm(list = ls())

unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date( activity$date, "%Y-%m-%d")
# activity$datetime <- strptime( paste(activity$date, sprintf("%04d", activity$interval)), "%Y-%m-%d %H%M")
activity$intervalID <- sapply( activity$interval, function(x){ 
  t <- sprintf("%04d", x)
  ( as.numeric(substr(t, 1, 2))*60 + as.numeric(substr(t, 3, 4)) )/5 + 1
})

str(activity)
summary(activity)
head(activity)

#######################################################################################
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
#######################################################################################

# Calculate the total number of steps taken per day
#----------------------------------------------------
library(plyr)
#data <- ddply( activity[,c("steps", "date")], "date", summarize, steps=sum(steps, na.rm=T) ) # Wrong: set days with NA to have 0 steps
data <- ddply( activity[,c("steps", "date")], "date", summarize, steps=sum(steps) )

# Equivalent to:
naDays  <- unique(activity$date[ is.na(activity$steps) ]) # All days for which at least one step value is missing
data2   <- activity[ !(activity$date %in% naDays), ]     # Remove rows of days with at least on NA for steps
data2   <- ddply( data2[,c("steps", "date")], "date", summarize, steps=sum(steps) )
dataComp <- data[ !is.na(data$steps), ]
cbind(dataComp$date-data2$date, dataComp$steps-data2$steps)

# Make a histogram of the total number of steps taken each day
#-----------------------------------------------------------------
pp <- ggplot( data, aes(x=steps) ) + geom_histogram( binwidth=5000, colour='black', fill='blue' )
pp <- hist( data$steps, col="red", main="", xlab="total number of daily steps")


# Calculate and report the mean and median of the total number of steps taken per day
#---------------------------------------------------------------------------------------
mean(data$steps, na.rm=T)
median(data$steps, na.rm=T)

#######################################################################################
# What is the average daily activity pattern?
#######################################################################################

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
#-------------------------------------------------------------------------------------
pp <- ggplot(activity, aes(intervalID, steps))
pp <- pp + stat_summary(fun.y = mean, geom="line", na.rm=T)
pp

# dataCheck <- activity
# dataCheck$interval <- as.factor(dataCheck$interval)
# dataCheck <- ddply( dataCheck[,c("steps", "interval", "date")], "interval", summarize, steps=mean(steps, na.rm=T) )
# pp2 <- ggplot( dataCheck, aes(as.numeric(interval), steps) ) + geom_line()
# pp2 <- plot(x=as.numeric(dataCheck$interval), y=dataCheck$steps, "l")


# Which 5-minute interval, on average across all the days 
# in the dataset, contains the maximum number of steps?
#-------------------------------------------------------------
dataMeanInt <- ddply( activity[,c("steps", "interval", "intervalID", "date")]
                  , "interval", summarize
                  , steps=mean(steps, na.rm=T)
                  )
#                 , intervalID = unique(intervalID)

# Maximum number of steps and corresponding interval
max(dataMeanInt$steps)
dataMeanInt$interval[which.max(dataMeanInt$steps)]



#######################################################################################
# Imputing missing values
# Note that there are a number of days/intervals where there are 
# missing values (coded as NA). The presence of missing days may
# introduce bias into some calculations or summaries of the data.
#######################################################################################

# Calculate and report the total number of missing values 
# in the dataset (i.e. the total number of rows with NAs)
#-------------------------------------------------------------------------------------
sum(is.na(activity$steps))

temp <- activity[, c("intervalID", "steps", "date")]
temp$isna <- F
temp$isna[is.na(temp$steps)] <- T
temp$steps[is.na(activity$steps)] <- -100
pp <- ggplot(temp, aes(intervalID, steps)) + geom_line(aes(colour=isna)) + facet_wrap(~date)
pp

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.
#
# For each missing value, fill it with average nb of steps for 
# the corresponding 5-minutes interval
#--------------------------------------------------------------------------------------
activity2 <- activity
indsNA <- which(is.na(activity2$steps))
for(i in indsNA){
  activity2$steps[i] <- dataMeanInt$steps[ dataMeanInt$interval == activity2$interval[i] ]
}

pp <- ggplot(activity2, aes(intervalID, steps)) + geom_line() + facet_wrap(~date)
pp

# Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
#--------------------------------------------------------------------------------------
data2 <- ddply( activity2[,c("steps", "date")], "date", summarize, steps=sum(steps) )
pp <- ggplot( data2, aes(x=steps) ) + geom_histogram( binwidth=5000 )
pp <- hist( data2$steps, col="red", main="", xlab="total number of daily steps")

mean(data2$steps, na.rm=T)
median(data2$steps, na.rm=T)

#######################################################################################
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
#######################################################################################

# Create a new factor variable in the dataset with two levels 
#  "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#--------------------------------------------------------------------------------------
activity2$dayType <- "weekday"
activity2$dayType[ weekdays(activity2$date) %in% c("Saturday", "Sunday") ] <- "weekend"

# Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of
# what this plot should look like using simulated data.
#--------------------------------------------------------------------------------------
pp <- ggplot(activity2, aes(intervalID, steps))
pp <- pp + stat_summary(fun.y = mean, geom="line", na.rm=T)
pp <- pp + facet_wrap( ~dayType )
pp

