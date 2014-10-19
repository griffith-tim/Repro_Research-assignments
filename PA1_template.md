###Analysis of Activity (Steps) for a Single Person During a 2 Month Period

The purpose of this report is to analyze the daily activity patterns, measured in steps, of a single anonymous person during a two month period from Oct. 1 to Nov. 30, 2012.

####Data
Raw data was downloaded from the [course web site](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), unzipped and placed in the R working directory. Additional columns were added that designated the time of day (hours and minutes as fractions), the day of the week, and whether a measure was taken on a week day or weekend day.  

```r
activity<-read.csv("activity.csv")
activity$intervalF<-as.factor(activity$interval) #converts interval to a factor
# HM converts $interval values into hours and decimal minutes
HM<-function (x) {
      T<-rep(NA, length(x))
      for (i in 1:length(x)) {
          H<-as.integer(x[i]/100)
          M<-(x[i]/100 - H)*0.6
          T[i]<-H+M
      }
      return(T)
}
activity$time<-as.factor(HM(activity$interval))
# columns with days and week/weekend catagories
activity$wday<-weekdays(as.Date(activity$date))
weekend.days<-c("Saturday", "Sunday")
for (i in 1:length(activity$wday)) {
      if (activity$wday[i] %in% weekend.days) {activity$wkend[i]<-"weekend"}
      else {activity$wkend[i]<-"weekday"}
}
```

####Analysis

#####Number of steps per day
The histogram below shows the distribution of number of steps per day during the 61 day period of the study.  

```r
#Sumation of number of steps in each day
StepsDay<-tapply(activity$steps, activity$date, sum)
# histogram
hist(StepsDay, breaks=8, col="red", 
     main="Histogram of Steps Walked per Day", xlab="Number of steps/day")
MeanSteps=mean(StepsDay, na.rm=T)
MedSteps=median(StepsDay, na.rm=T)
abline(v=MeanSteps, lwd=2)
abline(v=MedSteps, lwd=2)
histtab<-hist(StepsDay, breaks=8, plot=F)
text(x=MedSteps, y=max(histtab$counts), pos=2, labels="Median")
text(x=MeanSteps, y=max(histtab$counts), pos=4, labels="Mean")
```

![plot of chunk hist_dailySteps](figure/hist_dailySteps.png) 

The mean number of steps was 1.0766 &times; 10<sup>4</sup> steps per day while the median number of steps was only slightly lower at 10765 steps.  Note that these values are so similar that they are not distinguishable on the histogram. 

#####Pattern of Steps During the Day
The activity patterns of the study subject fluctuated substantially over the course of the day, as would be expected. The figure below details the pattern steps, in 5 minute increments, over the course of an average day.   

```r
# calculate mean number of steps for each 5 min. interval during a day
MinMeans<-as.data.frame(tapply(activity$steps, activity$interval, mean, na.rm=T))
# add time and time interval columns to data frame
origin<-strptime("00:00", format="%H:%M")
Tmin<-data.frame(time=(seq(0,(60*60*24-300), by=300) + origin))
MinMeans$time<-Tmin$time
MinMeans$interval<-unique(activity$interval)
names(MinMeans)<-c("means5min", "time", "interval")
# plot
plot(MinMeans$time, MinMeans$means5min, type="l", 
     main="Daily Step Patterns", 
     xlab="time of day (5 min. intervals)", ylab="average number of steps")
```

![plot of chunk dailyPattern](figure/dailyPattern.png) 

#####Imputing missing values
Unfortunately, analysis of this data set is complicated by missing values.

```r
# calculate the total number of NA values in the data set
stepsNA<-is.na(activity$steps)
sumNA<-sum(stepsNA)
```
First, note that there are 2304 missing values.

However, missing values are not distributed randomly throughout the data set.  

```r
activityNA<-activity[stepsNA,]
t.NAdate<-table(activityNA$date)
t.NAdateOnly<-t.NAdate[t.NAdate>0]
NAdates<-length(t.NAdateOnly)
```
Instead, there are just 8 days with missing values. The dates with missing values are indicated in the figure below:

```r
StepsDay<-tapply(activity$steps, activity$date, sum)
# to create x-axis labels
dates<-row.names(StepsDay)
l<-length(StepsDay)
SL<-as.vector(rep(NA, l))
missingDates<-row.names(t.NAdateOnly)
for (i in 1:l) {if(dates[i] %in% missingDates) {SL[i]<-dates[i]}}
barplot(StepsDay, names.arg=SL, main="Number of Steps per Day", xlab="Date", sub="(labeled days have missing values)", ylab="Total steps", las=2, cex.axis=0.7, cex.names=0.5)
```

![plot of chunk plot_missing.days](figure/plot_missing.days.png) 

Because missing values are limited to a few days, further analysis can be strengthened my replacing these missing values.  Missing values were imputed by replacing each NA value with the average number of steps taken during that 5 minute interval.  

```r
# Creation of a new data frame with imputed values
activityI<-activity
for(i in 1:length(activityI$steps)) {
      if(is.na(activityI$steps[i])==T) {
            int<-activityI$interval[i]
            activityI$steps[i]<-MinMeans[MinMeans$interval==int,]$means5min
      }
}
```

#####Number of steps per day (with imputed data)
The histogram below shows the distribution of number of steps per day after NA values have been replaced with imputed values. 

```r
StepsDayI<-tapply(activityI$steps, activityI$date, sum)
hist(StepsDayI, breaks=8, col="red", main="Histogram of Steps Walked per Day (with imputed values)", xlab="Number of steps/day")
MeanStepsI=mean(StepsDayI, na.rm=T)
MedStepsI=median(StepsDayI, na.rm=T)
abline(v=MeanStepsI, lwd=3)
abline(v=MedStepsI, lwd=3)
histtabI<-hist(StepsDayI, breaks=8, plot=F)
text(x=MeanStepsI, y=max(histtabI$counts), pos=2, labels="Mean")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

Because the method of replacing missing values relied on means for each time increment, the overall mean and median numbers of steps per day were only slightly affected.  After imputing missing values, the mean number of steps remained unchanged at 1.0766 &times; 10<sup>4</sup> while the median number of steps increased slightly to 1.0766 &times; 10<sup>4</sup> (i.e. the mean number of steps).  

#####Weekday v. Weekend activity patterns
The final comparison involved activity patterns of week days versus weekends.


```r
# calculate mean number of steps each 5 min. during week and weekend days
activityIWkday<-activityI[activityI$wkend=="weekday",]
activityIWkend<-activityI[activityI$wkend=="weekend",]
MinMeans$WkdayMeans<-tapply(activityIWkday$steps, activityIWkday$interval, 
                                    mean, na.rm=T)
MinMeans$WkendMeans<-tapply(activityIWkend$steps, activityIWkend$interval, 
                                    mean, na.rm=T)
#plot
plot(MinMeans$time, MinMeans$WkdayMeans, type="l", col="blue", 
     main="Comparison of Daily Step Patterns: Weekdays v. Weekends", 
     xlab="time of day (5 min. intervals)", ylab="average number of steps")
lines(MinMeans$time, MinMeans$WkendMeans, col="red")
legend("topright", lwd=2, col=c("blue", "red"), 
       legend=c("weekdays", "weekends"), cex=0.7)
```

![plot of chunk weekday.v.weekend](figure/weekday.v.weekend.png) 

Clearly, the the subject started weekend days a little later than week days.  However, there was no evidence of late night partying.  Alas.
