# Script: run_analysis_imputeNA.R

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
#       - calculates and report the total number of missing values in the dataset 
#         (i.e. the total number of rows with 𝙽𝙰s)
#       - creates a new dataset by filling in all of the missing values in the dataset.
#         This is achieved by using the mean for that 5-minute interval.
#       - produces a histogram of the total number of steps taken each day.
#       - calculates and reports the mean and median total number of steps taken per day. 
#       - using the modified data set:
#               - creates a new factor variable in the dataset with two 
#                 levels: “weekday” and “weekend” indicating whether a given 
#                 date is a weekday or weekend day.
#               - produces a panel plot containing a time series plot of the 
#                 5-minute interval (x-axis) and the average number of steps taken, 
#                 averaged across all weekday days or weekend days (y-axis). 

## load packages and libraries

install.packages("dplyr")
library(dplyr)
install.packages(lattice)
library(lattice)
## Read in the Activity data
activity.data <- read.table("../data/activity.csv",sep=",", 
                            colClasses=c("integer","Date","integer"),
                            header=TRUE)

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with 𝙽𝙰)

# missing.steps<-is.na(activity.data[,1])

print(paste("Number of missing values is:",sum(is.na(activity.data[,1])),sep=" "))

## Create a new dataset by filling in all of the missing values in the dataset.
## This is achieved by using the mean for that 5-minute interval.

### Calculate the mean values for each interval
### Group data based on interval

interval.activity<-select(activity.data,interval,steps)  ## subset data to just the columns we need

interval.activity <- group_by(interval.activity,interval)  ##  Group data by 5-minute time interval

### Calculate the mean for each interval

interval.activity <- summarise_each(interval.activity,funs(mean(steps, na.rm=TRUE)))  ## sum the data for each date

###  Copy original data table to new table

activity.data.imputed <- activity.data

### For each entry where "steps" value is missing, assign the mean vale for that interval
##  (There is probably a more elegant way to do this but this will work to start with)

for ( iptr in 1:nrow(activity.data.imputed)) {  ## for each row in the data file
    
                if (is.na(activity.data.imputed[iptr,1])) { ## if "steps" data is missing ...
               
                index<-interval.activity[,1]==activity.data.imputed[iptr,3]  ## get the interval as index
               
                activity.data.imputed[iptr,1]<-interval.activity[index,2]   ## substitute "NA" with mean for that interval
                
        }
}

## Calculate the total number of steps taken each day

## Group by day and sum each group
daily.activity.imputed<-select(activity.data.imputed,date,steps)  ## subset data to just the columns we need

daily.activity.imputed <- group_by(daily.activity.imputed,date)  ##  Group data by date

daily.activity.imputed <- summarise_each(daily.activity.imputed,funs(sum(steps)))  ## sum the data for each date

## Create a histogram plot with minimal formatting
par(mfrow = c(1, 1))  ## Specify that plots will be arranged in 1 rows of 1 plots
png(filename="figure/Plot3-Hist-Total-Imputed.png")   ## Open the output file

with(daily.activity.imputed,hist(steps,col="red",main="Histogram of Total Steps", 
                         xlab="Steps", breaks=100 ))

with(daily.activity.imputed,rug(steps))

## Calculate mean and median

mean.steps.imputed <- colMeans(daily.activity.imputed[,2],na.rm=TRUE)
median.steps.imputed <- apply(X = daily.activity.imputed[,2], MARGIN=2, FUN = median, na.rm = TRUE)

## Add to plot 

abline(v=mean.steps.imputed, col="navy",lwd=5)
abline(v=median.steps.imputed,col="purple",lwd=5,lty=2)

legend("topright", pch = "-", lwd=3, col = c("navy", "purple"), bty="n",
       legend = c("Mean", "Median"))

dev.off()    ##  Close the output file

## Print the mean and median total steps taken
print(paste("Mean total daily steps taken: ",(as.numeric(mean.steps.imputed)),sep=" "))
print(paste("Median total daily steps taken: ",(as.numeric(median.steps.imputed)),sep=" "))

##  Creates a new factor variable in the dataset with two 
##  levels: “weekday” and “weekend” indicating whether a given 
## date is a weekday or weekend day.

### add a column called "day" based on "date" field
activity.data.imputed<-mutate(activity.data.imputed, day=(weekdays(date)))

### For each entry determine if it occurs on a weekend or week day and label accordingly
##  (There is probably a more elegant way to do this but this will work to start with)

for ( iptr in 1:nrow(activity.data.imputed)) {          ## for each row in the data file
        
        if (grepl("^S",activity.data.imputed[iptr,4])) {     ## is it a Saturday or Sunday?
  
                activity.data.imputed[iptr,4]<- "weekend"       ## if so label as "weekend"
                
        } else {
                activity.data.imputed[iptr,4]<- "weekday"       ## if not label as "weekday"
        }
}

### convert "day" column to a factor
activity.data.imputed<-mutate(activity.data.imputed, as.factor(day))

## Create a panel plot containing a time series plot of the 5-minute interval 
## (x-axis) and the average number of steps taken, averaged across all weekday days 
## or weekend days (y-axis). 

### group each by 5 minute interval and calculate average number of steps per interval

interval.activity.imputed <- select(activity.data.imputed,interval,steps,day)  ## subset data to just the columns we need
interval.activity.imputed <- group_by(interval.activity.imputed,day,interval)  ##  Group data by 5-minute time interval

### Calculate the mean for each interval

interval.activity.imputed <- summarise_each(interval.activity.imputed,funs(mean(steps, na.rm=TRUE)))  ## sum the data for each date

### Create panel plot
xyplot(steps~interval|day,data=interval.activity.imputed,
       layout=c(1,2),type="l",
       xlab="Time Interval",ylab="Mean Steps")
