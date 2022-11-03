---
title: "PA1_template.Rmd"
author: "Rachel"
date: '2022-11-02'
output: 
  html_document: 
    keep_md: yes
---



# Reproducible Research Course Project #1

Below is my report to complete Coursera's Reproducible Research Course Project #1.

## Setup


```r
# Set seed
set.seed(215)
```

## Loading and preprocessing the data

Show any code that is needed to:

1.  Load the data (i.e. read.csv())


```r
# Loading and preprocessing the data

filename <- "project1.zip"

# Checking if directory already exists.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL, filename, method="curl")
}

# Confirm zipped file is there
dir()
```

```
##  [1] "activity.csv"                         
##  [2] "Course_Project_1.R"                   
##  [3] "PA1_template_files"                   
##  [4] "PA1_template.html"                    
##  [5] "PA1_template.md"                      
##  [6] "PA1_template.Rmd"                     
##  [7] "project1.zip"                         
##  [8] "RepData_PeerAssessment1"              
##  [9] "reportwriting.pdf"                    
## [10] "reproducible_research.Rproj"          
## [11] "Structure of a Data Analysis Part 2.R"
```

```r
# Checking if folder exists
if (!file.exists("activity.csv")) { 
        unzip(filename) 
}

# Read in CSV and save file to object "data" and inspect it
data <- read.csv(file = "activity.csv", header = TRUE)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
# Data contain lots of NAs
# Leave NAs for now

## Confirm there are 17,568 observations
nrow(data) == 17568
```

```
## [1] TRUE
```

2.  Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Convert dates

library("lubridate")
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
data$date <- ymd(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

1.  Make a histogram of the total number of steps taken each day


```r
# Determine the total # of steps taken each day.

library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
sum_data <- data %>%
        group_by(date) %>%
        summarize(sum = sum(steps, na.rm = TRUE))

# Histogram of the total number of steps taken each day:

hist(sum_data$sum, 
        breaks = 25,
        xlab = "Total of Steps", 
        ylab = "Number of Days",
        main = "Total Number of Steps Taken Each Day",
        ylim = c(0,10),
        xlim = c(0,25000),
        )
rug(sum_data$sum)
```

![](PA1_template_files/figure-html/mean1-1.png)<!-- -->

2.  Calculate and report the **mean** and **median** total number of steps taken per day


```r
# The mean for the total number of steps each day is:
mean(sum_data$sum)
```

```
## [1] 9354.23
```

```r
# The median for the total number of steps each day is:
median(sum_data$sum)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Calculate the average number of steps taken for each interval, averaged across all days

int_mean_data <- data %>%
        group_by(interval) %>%
        summarize(avg = mean(steps, na.rm = TRUE))

# Time series plot
with(int_mean_data,
     plot(interval, avg, 
          type = "l",
          xlab = "Interval (in minutes)",
          ylab = "Average Number of Steps Across All Dates",
          main = "Average Number of Steps Across All Dates by Interval",
          ylim = c(0,250),
          xlim = c(0,2500),
     )
)
```

![](PA1_template_files/figure-html/pattern1-1.png)<!-- -->

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#The 5-minute interval on average across all the days in the dataset that contains the maximum number of steps is:

int_mean_data[which.max(int_mean_data$avg),1]
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```

## Imputing missing values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# The total number of rows with NAs is:

sum(rowSums(is.na(data)))
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# My strategy for filling the missing values is to calculate the mean of each interval and impute that value for all missing values of the matching interval. See code below in part #3. 
```

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Create a new dataset with missing valeus imputed
impute <- data %>%
        mutate(
                steps = case_when(
                        is.na(steps) ~ int_mean_data$avg[match(data$interval, int_mean_data$interval)],
                        TRUE ~ as.numeric(steps)
                )
        )

#Verify NAs were removed

any(is.na(impute))
```

```
## [1] FALSE
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Histogram of the total number of steps taken each day

## Calculations
sum_impute <- impute %>%
        group_by(date) %>%
        summarize(sum = sum(steps, na.rm = TRUE))

## Determine range
range(sum_impute$sum)
```

```
## [1]    41 21194
```

```r
## Histogram of the total number of steps taken each day
hist(sum_impute$sum, 
     breaks = 25,
     xlab = "Total of Steps", 
     ylab = "Number of Days",
     main = "Total Number of Steps Taken Each Day",
     ylim = c(0,20),
     xlim = c(0,25000),
)
rug(sum_impute$sum)
```

![](PA1_template_files/figure-html/impute 4-1.png)<!-- -->

```r
# The mean total number of steps each day is:

mean(sum_impute$sum)
```

```
## [1] 10766.19
```

```r
# The median total of steps each day is:

median(sum_impute$sum)
```

```
## [1] 10766.19
```

```r
# These mean and median from the dataset with imputations differs from the original dataset with NAs:

mean(sum_impute$sum) - mean(sum_data$sum)
```

```
## [1] 1411.959
```

```r
median(sum_impute$sum) - median(sum_data$sum)
```

```
## [1] 371.1887
```

```r
# Replacing the NA values in the original data set with the imputed values increases the mean and median by 1411.969 and 371.1887, respectively.
```

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library("stringr")
impute_weekdays <- impute %>%
        mutate(weekday = tolower(weekdays(impute$date))) %>%
        mutate(
                weekday = case_when(
                        str_detect(weekday, "saturday") ~ "weekend",
                        str_detect(weekday, "sunday") ~ "weekend",
                        TRUE ~ "weekday"
                )
        )

impute_weekdays$weekday <- as.factor(impute_weekdays$weekday)
sum_impute_weekdays <- impute_weekdays %>%
        group_by(weekday,interval) %>%
        summarize(avg = mean(steps, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'weekday'. You can override using the
## `.groups` argument.
```

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
weekend <- sum_impute_weekdays[which(sum_impute_weekdays$weekday == "weekend"),]
        
weekday <- sum_impute_weekdays[which(sum_impute_weekdays$weekday == "weekday"),]

par(mfrow = c(2,1))
with(weekend,
     plot(interval, avg, 
          type = "l",
          xlab = "Interval (in minutes)",
          ylab = "Average Number of Steps",
          main = "Average Number of Steps Across by Interval on Weekends",
          ylim = c(0,250),
          xlim = c(0,2500),
     )
)

with(weekday,
     plot(interval, avg, 
          type = "l",
          xlab = "Interval (in minutes)",
          ylab = "Average Number of Steps",
          main = "Average Number of Steps Across by Interval on Weekdays",
          ylim = c(0,250),
          xlim = c(0,2500),
     )
)
```

![](PA1_template_files/figure-html/weekend2-1.png)<!-- -->

## Session Info


```r
# For the purposes of reproducibility, I have included the session info

sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Red Hat Enterprise Linux
## 
## Matrix products: default
## BLAS/LAPACK: /usr/lib64/libopenblasp-r0.3.3.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] stringr_1.4.0   dplyr_1.0.10    lubridate_1.8.0
## 
## loaded via a namespace (and not attached):
##  [1] rstudioapi_0.13  knitr_1.37       magrittr_2.0.3   tidyselect_1.1.2
##  [5] R6_2.5.1         rlang_1.0.6      fastmap_1.1.0    fansi_1.0.3     
##  [9] highr_0.9        tools_4.1.2      xfun_0.30        utf8_1.2.2      
## [13] DBI_1.1.3        cli_3.4.1        jquerylib_0.1.4  htmltools_0.5.2 
## [17] ellipsis_0.3.2   assertthat_0.2.1 yaml_2.3.6       digest_0.6.29   
## [21] tibble_3.1.8     lifecycle_1.0.1  crayon_1.5.2     purrr_0.3.4     
## [25] sass_0.4.2       vctrs_0.4.2      glue_1.6.2       evaluate_0.15   
## [29] rmarkdown_2.12   stringi_1.7.6    compiler_4.1.2   bslib_0.3.1     
## [33] pillar_1.7.0     generics_0.1.3   jsonlite_1.8.0   pkgconfig_2.0.3
```
