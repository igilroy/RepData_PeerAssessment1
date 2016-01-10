# Script: run_analysis_withNA.R

# The purpose of this script is to read a CSV data file of 
# human activity as measured by the embedded gyroscope and accelerometer 
# a personal activity monitoring device.

# This device collects data at 5 minute intervals through out the day. 
# The data consists of two months of data from an anonymous individual collected 
# during the months of October and November, 2012 and include the number of steps 
# taken in 5 minute intervals each day.
# 
# The variables included in this dataset are:
#       - steps: Number of steps taking in a 5-min interval (missing values are coded as 𝙽𝙰)
#       - date: The date on which the measurement was taken in YYYY-MM-DD format
#       - interval: Identifier for the 5-min interval in which measurement was taken
# The dataset is stored in a comma-separated-value (CSV) file and there are a total of 
# 17,568 observations in this dataset.

# This script:
#       - reads the CSV file into a table
#       - calculates and plots a histogram of the total number of steps taken each day
#       - calculate and report the mean and median of the total number of steps taken per day
#       - creates a time series plot 𝚕 of the 5-minute interval (x-axis) and the average
#         number of steps taken, averaged across all days (y-axis)
#       - determine which 5-minute interval, on average across all the days in the dataset, 
#         contains the maximum number of steps

## NOTE:  This script ignores NA values.  A separate script (run_analysis_imputeNA.R) 
##      imputes values for those time intervals that have "NA" for number of steps

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## load packages and libraries

install.packages("dplyr")
library(dplyr)

## Read in the Activity data
activity.data <- read.table("../data/activity.csv",sep=",", 
                            colClasses=c("integer","Date","integer"),
                            header=TRUE)

## Calculate the total number of steps taken each day
### Group by day and sum each group
daily.activity<-select(activity.data,date,steps)  ## subset data to just the columns we need

daily.activity <- group_by(daily.activity,date)  ##  Group data by date

daily.activity <- summarise_each(daily.activity,funs(sum(steps)))  ## sum the data for each date

## Create a histogram plot with minimal formatting
par(mfrow = c(1, 1))  ## Specify that plots will be arranged in 1 rows of 1 plots

png(filename="figure/Plot1-Hist-Total.png")   ## Open the output file

with(daily.activity,hist(steps,col="red",main="Histogram of Total Steps", 
     xlab="Steps", breaks=100 ))

with(daily.activity,rug(steps))

### Calculate mean and median

mean.steps <- colMeans(daily.activity[,2],na.rm=TRUE)
median.steps <- apply(X = daily.activity[,2], MARGIN=2, FUN = median, na.rm = TRUE)

### Add to plot 

abline(v=mean.steps, col="navy",lwd=5)
abline(v=median.steps,col="purple",lwd=5,lty=2)

legend("topright", pch = "-", lwd=3, col = c("navy", "purple"), bty="n",
       legend = c("Mean", "Median"))

dev.off()    ##  Close the output file

## Print the mean and median total steps taken
print(paste("Mean total daily steps taken: ",as.numeric(mean.steps),sep=" "))
print(paste("Median total daily steps taken: ",as.numeric(median.steps),sep=" "))

## Create a time series plot of the 5-minute interval (x-axis) and the average
## number of steps taken, averaged across all days (y-axis)

### Group data based on interval

interval.activity<-select(activity.data,interval,steps)  ## subset data to just the columns we need

interval.activity <- group_by(interval.activity,interval)  ##  Group data by 5-minute time interval

### Calculate the mean for each interval

interval.activity <- summarise_each(interval.activity,funs(mean(steps, na.rm=TRUE)))  ## sum the data for each date

### Plot time series graph

png(filename="figure/Plot2-Time-Mean.png")   ## Open the output file

with(interval.activity,plot(interval,steps,col="red", type="l",
                            main="Average (Mean) Steps per 5-minute Time Interval", 
                            xlab="Time Interval", ylab="Mean Steps"  ))

dev.off()    ##  Close the output file

## Determine which 5-minute interval contains the maximum number of steps

max.steps <-max(interval.activity[,2])
max.ints<-interval.activity[,2] == max.steps
max.interval<-interval.activity[max.ints,1]

print(paste("Maximum average number of steps:", max.steps,sep=" "))
print(paste("Occurred during interval(s):",max.interval,sep=" "))


