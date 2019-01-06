---
title: "Course Project 1 - R Markdown - lglip"
output: html_document
---



## Step 1: Loading the required packages


```r
require(lubridate, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(dplyr, quietly = TRUE)
```

## Step 2: Downloading and reading the data

Setting the working directory:

```r
wd <- getwd()
filename <- "repdata_data-activity.zip"
```

Downloading the data:

```r
if(!file.exists(file.path(wd, filename))){
      URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(URL, file.path(wd, filename))
      unzip(zipfile = filename)
}
```

Reading the data and transforming the 'date' variable into a proper format for further analysis:

```r
data.activity <- read.csv("activity.csv", header = TRUE, sep = ",")
data.activity$date <- as_date(data.activity$date)
```

## Step 3: Conducting the required analyses
### 1. Average total number of steps taken per day

Calculating the total number of daily steps:

```r
data.grouped <- group_by(data.activity, date) %>%
      summarize(steps = sum(steps, na.rm = TRUE))

print(data.grouped)
```

```
## # A tibble: 61 x 2
##    date       steps
##    <date>     <int>
##  1 2012-10-01     0
##  2 2012-10-02   126
##  3 2012-10-03 11352
##  4 2012-10-04 12116
##  5 2012-10-05 13294
##  6 2012-10-06 15420
##  7 2012-10-07 11015
##  8 2012-10-08     0
##  9 2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```

Constructing the histogram of daily steps:

```r
plot <- ggplot(data = data.grouped, mapping = aes(steps)) +
      geom_histogram(bins = 20) +
      theme_minimal() +
      labs(title = "Histogram of total number of steps per day",
           x = "Steps",
           y = "Count") +
      theme(plot.title = element_text(size = 14, margin = unit(c(.2,0,.2,0), "cm"), hjust = .5),
            axis.title.x = element_text(size = 10, vjust = -2),
            axis.title.y = element_text(size = 10, vjust = 4),
            axis.text = element_text(size = 11),
            plot.margin = unit(c(1,1,1,1), "cm")
      )
print(plot)
```

<img src="figure/steps histogram-1.png" title="plot of chunk steps histogram" alt="plot of chunk steps histogram" style="display: block; margin: auto;" />

Mean and median number of total daily steps:

```r
steps.mean <- round(mean(data.grouped$steps), digits = 0)
steps.median <- median(data.grouped$steps)
```
The average number of steps per day (mean) is 9354 steps.  
The median number of steps per day is 10395 steps.

### 2. Average daily activity pattern

Constructing the time-series plot of the average number of steps by interval:

```r
data.grouped2 <- group_by(data.activity, interval) %>%
      summarize(steps = mean(steps, na.rm = TRUE))

plot2 <- ggplot(data = data.grouped2, mapping = aes(interval, steps)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Interval time series of average number of steps",
           x = "Interval",
           y = "Average number of steps") +
      theme(plot.title = element_text(size = 14, margin = unit(c(.2,0,.2,0), "cm"), hjust = .5),
            axis.title.x = element_text(size = 10, vjust = -2),
            axis.title.y = element_text(size = 10, vjust = 4),
            axis.text = element_text(size = 11),
            plot.margin = unit(c(1,1,1,1), "cm")
      )
print(plot2)
```

<img src="figure/steps line graph and max steps-1.png" title="plot of chunk steps line graph and max steps" alt="plot of chunk steps line graph and max steps" style="display: block; margin: auto;" />

```r
max.steps <- round(max(data.grouped2$steps), digits = 0)
max.interval <- data.grouped2$interval[data.grouped2$steps == max(data.grouped2$steps)]
```

The 5-minute interval containing the maximum average number of steps is '835', representing 206 steps.

### 3. Imputation of missing values

Calculating the number of rows with missing values:

```r
rows.NA <- sum(is.na(data.activity))
```
There are 2304 rows with missing values in the *data.activity* data set.  

Imputing the missing values and constructing a new histogram of the total number of daily steps:

```r
data.act.imp <- data.activity

for(i in seq(0, max(data.act.imp$interval), 5)){
      data.act.imp$steps[is.na(data.act.imp$steps) & data.act.imp$interval == i] <- mean(data.act.imp$steps[data.act.imp$interval == i], na.rm = TRUE)
}

data.grouped.imp <- group_by(data.act.imp, date) %>%
      summarize(steps = sum(steps))

plot3 <- ggplot(data = data.grouped.imp, mapping = aes(steps)) +
      geom_histogram(bins = 20) +
      theme_minimal() +
      labs(title = "Histogram of total number of steps per day",
           subtitle = "whereby missing values are replaced by the mean of the respective interval",
           x = "Steps",
           y = "Count") +
      theme(plot.title = element_text(size = 14, margin = unit(c(.2,0,.2,0), "cm"), hjust = .5),
            plot.subtitle = element_text(size = 10, face = "italic", hjust = .5),
            axis.title.x = element_text(size = 10, vjust = -2),
            axis.title.y = element_text(size = 10, vjust = 4),
            axis.text = element_text(size = 11),
            plot.margin = unit(c(1,1,1,1), "cm")
      )
print(plot3)
```

<img src="figure/missing values bis-1.png" title="plot of chunk missing values bis" alt="plot of chunk missing values bis" style="display: block; margin: auto;" />

```r
steps.mean.imp <- round(mean(data.grouped.imp$steps), digits = 0)
steps.median.imp <- round(median(data.grouped.imp$steps), digits = 0)
```

The average number of steps per day (mean), with imputed missing values, is 1.0766 &times; 10<sup>4</sup> steps.  
The median number of steps per day, with imputed missing values, is 1.0766 &times; 10<sup>4</sup> steps.  

Because the interval mean was used to impute the missing values, both the average and median of the total number of daily steps are higher than their initial values.

### 4. Delta in activity patterns: weekdays vs. weekends

Constructing the panel plot of the time series of the average number of steps (weekdays vs. weekends):

```r
data.act.day <- mutate(data.activity, daytype = if_else(weekdays(data.activity$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

data.grouped3 <- group_by(data.act.day, interval, daytype) %>%
      summarize(steps = mean(steps, na.rm = TRUE))

plot3 <- ggplot(data = data.grouped3, mapping = aes(interval, steps)) +
      geom_line() +
      facet_grid(daytype~.) +
      theme_minimal() +
      labs(title = "Time series of average number of steps",
           subtitle = "subdivided by type of day",
           x = "Interval",
           y = "Average number of steps") +
      theme(plot.title = element_text(size = 14, margin = unit(c(.2,0,.2,0), "cm"), hjust = .5),
            plot.subtitle = element_text(size = 10, face = "italic", hjust = .5),
            axis.title.x = element_text(size = 10, vjust = -2),
            axis.title.y = element_text(size = 10, vjust = 4),
            axis.text = element_text(size = 11),
            plot.margin = unit(c(1,1,1,1), "cm"))
print(plot3)
```

<img src="figure/activity patterns-1.png" title="plot of chunk activity patterns" alt="plot of chunk activity patterns" style="display: block; margin: auto;" />
