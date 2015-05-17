Reproducible Research: Peer Assessment 1
========================================

This document is will answer key questions related to activity monitoring data. In order to make this document easy to follow, each of the headers and subheaders below are directly related to the assignment sections and specific questions to be answered.

Data is from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

### Load the data

The first step is to load the data described above.

```{r}
library(utils)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile="activity.zip", method="curl")
unzip(zipfile="./activity.zip") # unzips file "activity.csv"
activity <- read.csv(file="./activity.csv") 
```

### Process/transform the data (if necessary) into a format suitable for your analysis

Now, let's look at the data to see if the data needs to be transformed for analysis.

```{r}
summary(activity)
str(activity)
head(activity)
```

It looks like we need to convert the dates.

```{r}
activity[,2] <- as.Date(activity[,2])
```

Now, we see that date is properly formated.

```{r}
str(activity)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

### Calculate the total number of steps taken per day

Let's aggregate steps for each day ignoring missing values (NAs). Then, we take the mean of steps across days with values. 

```{r}
agg.steps.day <- aggregate(formula= steps ~ date, data=activity, FUN=sum, na.action = na.omit)
mean(agg.steps.day[,2])
```

### Make a histogram of the total number of steps taken each day

Let's create a histogram to graphically show the distribution of the steps per day for this data set.

```{r}
library(graphics)
hist(agg.steps.day[,2], main="Steps Per Day", xlab="Steps", ylab="Number of Days")
```

### Calculate and report the mean and median of the total number of steps taken per day

Now, let's calculate the median number of steps per day.

```{r}
median(agg.steps.day[,2])
```

You should see the following results of the code above:

**- mean steps per day = 10,766.19**

**- median steps per day = 10,765**

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

As we are completing this assignment in the order provided and imputting missing values is a follow-on step, we will continue to ignore missing values in the data set for this section.

We begin by aggregating steps for each interval (across all days) ignoring missing values (NAs). Then we will divide each of the aggregate steps per interval by the total number of days in the dataset (61 days) in order to define the average number of steps taken, averaged across all days. Finally, we create a time-series line plot to show the data graphically.

```{r}
agg.steps.int <- aggregate(formula= steps ~ interval, data=activity, FUN=sum, na.action = na.omit)
agg.steps.int[,3] <- agg.steps.int[,2]/61 # calculates the avg number of steps, averaged across all days and assigns to column 3
plot(agg.steps.int[,1], agg.steps.int[,3], type = "l", main="Average Steps per Interval (across all days)", xlab="Interval", ylab="Steps")
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Let's identify the row in which the maximum value of the average number of steps taken, averaged across all days (column 3 in our new aggregated data set for this problem). Then we can simply look up the interval of the identified row.

```{r}
row <- which.max(agg.steps.int[,3]) # find the row index which contains the maximum avg number of steps, averaged across all days
agg.steps.int[row,1] # return the interval value from our row index
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

This is easily accomplished by using the summary() function on the original data set.

```{r}
summary(activity)
```

We see that the total number of NA's in the data set are 2,304 missing "step" values.

### Devise a strategy for filling in all of the missing values in the dataset.

For each missing value, we will replace it with the average number of steps taken, averaged across all days for that 5 minute interval. This strategy was chosen due to the variability of steps across 5 minute intervals for each day.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

We will loop over our original data set (activity), look for NA values, and replace these with the corresponding average steps value from our aggregate interval data set (agg.steps.int).

```{r}
activity.update <- activity
for (d in 1:61) { # loop over the 61 days
        for (i in 1:288) { # loop over the 288 increments in each day
                a <- (d-1)*288 + i # calculate the row number in the activity data set
                n <- i # set the row number in the aggregate interval data set
                if (is.na(activity.update[a,1])) { # test for NAs
                        activity.update[a,1] <- agg.steps.int[n,3] # replace NAs with values from our aggregate interval data set
                }
        }
}
```

If we look at the new data set using the summary function we see that there are no longer any NA values. Also, we can look at the top few lines of the data set which previously contained NAs and observe that they now contain step values.

```{r}
summary(activity.update)
head(activity.update)
```

### Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

We aggregate step data by day and create a histogram.

```{r}
agg.steps.day.update <- aggregate(formula= steps ~ date, data=activity.update, FUN=sum, na.action = na.omit)
hist(agg.steps.day.update[,2], main="Steps Per Day", xlab="Steps", ylab="Number of Days")
```

Now we calculate the mean and median of our new data set (agg.steps.day.update) which has NA values replaced with imputted values and compare it with our original data set (agg.steps.day) which includes NA values which we ignored.

```{r}
newavg <- mean(agg.steps.day.update[,2]) # updated data set mean
oldavg <- mean(agg.steps.day[,2]) # original data set mean
difavg <- newavg - oldavg # difference
newavg
oldavg
difavg

newmed <- median(agg.steps.day.update[,2]) # updated data set median
oldmed <- median(agg.steps.day[,2]) # original data set median
difmed <- newmed - oldmed # difference
newmed
oldmed
difmed
```

## Are there differences in activity patterns between weekdays and weekends?

For this section we wil use the dataset with the filled-in missing values.

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

We will use the weekdays() function to identify the day of the week for row of data. Then we will define a factor variable to split the data set into weekdays and weekends.

```{r}
library(dplyr) # load the dplpyr package
activity.update[,4] <- weekdays(activity.update[,2]) # identify day of week
colnames(activity.update)[4] <- "weekday" # name column
activity.update<-mutate(activity.update,day=ifelse(activity.update$weekday== c("Saturday", "Sunday"),"Weekend","Weekday")) # define weekend or weekday from day of week
head(activity.update) # review our updated data set
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Now we will calculate average steps for each 5 minute increment for both Weekdays and Weekends and plot these values.

```{r}
ind <- list(activity.update$interval, activity.update$day)
data <- tapply(activity.update$steps, ind, mean)
mydata <- as.data.frame(data)
mydata$increment <- as.numeric(rownames(mydata))

par(mfcol = c(2,1))
plot(mydata$increment, mydata$Weekday, type = "l", main="Average Steps per Interval (across all weekdays)", xlab="Interval", ylab="Steps")
plot(mydata$increment, mydata$Weekend, type = "l", main="Average Steps per Interval (across all weekends)", xlab="Interval", ylab="Steps")
```

Interesting but expected, people are more active taking steps on weekends.