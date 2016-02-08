# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
We begin in a working directory that contains the activity file and read in the contents of the file. 


```r
setwd("~/Desktop/R Data/RepData_PeerAssessment1")

dat <- read.csv("activity.csv")
```

Take a peek at the contents of dat.

```r
head(dat)
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
tail(dat)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The activity CSV contains a number of steps during a given five-minute period. For each date, the interval variable is the beginning of the five-minute portion of the day (in military time). 
## What is mean total number of steps taken per day?
To begin, we have to find the mean sum of day's steps.

```r
library(dplyr)
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
library(ggplot2)
dat2 <- dat %>% group_by(date) %>% summarize(Daily_Steps=sum(steps))
dat2
```

```
## Source: local data frame [61 x 2]
## 
##          date Daily_Steps
##        (fctr)       (int)
## 1  2012-10-01          NA
## 2  2012-10-02         126
## 3  2012-10-03       11352
## 4  2012-10-04       12116
## 5  2012-10-05       13294
## 6  2012-10-06       15420
## 7  2012-10-07       11015
## 8  2012-10-08          NA
## 9  2012-10-09       12811
## 10 2012-10-10        9900
## ..        ...         ...
```

```r
g <- ggplot(data=dat2, aes(x= date, y= Daily_Steps))+
        geom_bar(stat="identity")+
        theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
           axis.text.x  = element_text(angle=90, vjust=0.5, size=8))
print(g)
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

From here, it's simple to find the average of all the days.


```r
mean(dat2$Daily_Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
