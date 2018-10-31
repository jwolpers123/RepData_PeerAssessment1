---
title: "homework"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r librarys, systemtime}
library(dplyr)
library(chron)
library(ggplot2)
Sys.setlocale("LC_TIME", "C")
```

```{r preprocessing}
actna<-read.csv("activity.csv")
actna$date<-as.POSIXct(strptime(actna$date, "%Y-%m-%d"))
actna$weekday <- weekdays(as.Date(actna$date))
stepsPerDay <- actna %>% group_by(date)  %>%summarize(steps = sum(steps))
act<-na.omit(actna)
```

```{r histogramm steps per day}
hist(stepsPerDay$steps, main = "Steps per day", xlab = "steps")
```

```{r mean/median}
cat("mean", mean(na.omit(stepsPerDay$steps)))
cat("median", median(na.omit(stepsPerDay$steps)))
```

```{r average activity}
stepsPerInterval <- act %>% group_by(interval)  %>% summarize(steps = mean(steps))
plot(stepsPerInterval$interval, stepsPerInterval$steps, type="l", ylab ="steps", xlab = "interval", main = "Average activity")
```

```{r max steps}
maxStepInterval<- which.max(stepsPerInterval$steps)
cat("maxInterval", stepsPerInterval$interval[maxStepInterval])
```

My strategy to replace missing values(NA) is to take the mean of each corresponding interval.

```{r imputing missing values}
act2<-read.csv("activity.csv")
act2$date<-as.POSIXct(strptime(act2$date, "%Y-%m-%d"))
numberOfNAs<- length(act2$steps)-length(na.omit(act2$steps))
cat("numberOfNAs", numberOfNAs)

MeanStepsPerInterval <-as.data.frame( act %>% group_by(interval)  %>%summarize(steps = mean(steps))) 
for (i in 1:length(act2$steps))
{
  if (is.na(act2$steps[i])) {
    replaceValue<-filter(MeanStepsPerInterval, interval==act2$interval[i])[,2]
    act2$steps[i]<-replaceValue
      }
}
act2$weekday <- weekdays(as.Date(act2$date))
stepsPerDay2 <- act2 %>% group_by(date)  %>%summarize(steps = sum(steps)) 
hist(stepsPerDay2$steps, main="With/Without NA replaced", col = "blue")
hist(stepsPerDay$steps, add = TRUE, col = "red")
legend("topright", c("withNA", "withMean"), col=c("blue", "red"), lwd=10)
cat("mean", mean(stepsPerDay2$steps))
cat("median", median(stepsPerDay2$steps))
```
As expected, the mean stays the same, the median now is the mean, as it propably was an NA befor. The impact of replacing the NAs is a more flat curve.


```{r weekdays}
act2$daytype<-factor(ifelse(act2$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday" ))
stepsPerInterval2 <- act2 %>% group_by(interval,daytype)  %>% summarize(steps = mean(steps))
par(mfrow = c(2,1))
ggplot(stepsPerInterval2, aes(x=interval, y=steps, fill=daytype))+geom_line()+ facet_grid(daytype~.)
```