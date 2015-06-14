# Reproducible Research: Peer Assessment 1
Sandyn Skudneski  
Sunday, June 14, 2015

### Loading libraries to be used
```{r}
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
```

**Loading and processing the data**
*Load*
```{r}
zipfile <- "repdata_data_activity.zip"
file <- "activity.csv"
setwd("~/R/Coursera/RepResearch/Project1/")
unzip(zipfile)
df <- tbl_df(read.csv(file, stringsAsFactors = FALSE))
```

*Process*
```{r}
df$steps <- as.numeric(df$steps)
df$date <- ymd(df$date)
# create character vector for levels
df$intervalA <- sprintf("%04d", df$interval)
df$intervalA <- format(strptime(df$intervalA, format="%H%M"), 
                       format = "%H:%M")
# Weekday column
df <- mutate(df, weekday = wday(date, label = TRUE, abbr = TRUE)) 
```

**What is mean total number of steps taken per day?**
*Remove missing values, bin by steps/day, calculate mean and median steps/day*  
*and display all in a single histogram*
```{r}
df1 <- df[complete.cases(df), ] # df1 is only complete cases

result1 <-
    df1 %>%
    group_by(date) %>%
    summarize(totalDailySteps = sum(steps))
meanResult1 <- round(mean(result1$totalDailySteps))
medResult1 <- round(median(result1$totalDailySteps))

g <- ggplot(result1, aes(x=totalDailySteps))
g + geom_bar(binwidth = 5000, fill = "blue", color = "black") + 
    xlim(0, 25000) +
    labs(x = "Steps per Day", 
         y = "Total Days") +
    ggtitle("Total Days by_group Steps per Day") + 
    theme(plot.title = element_text(size = 20, colour="blue")) +
    annotate("text", x = 21000, y = 29, 
             label = paste("   mean steps/day: ", meanResult1)) + 
    annotate("text", x = 21000, y = 27, 
             label = paste("median steps/day: ", medResult1))
```

**What is the average daily activity pattern?**  
*create line plot of daily activity per 5 minute interval and show max() interval*
```{r}
result2 <-
    df1 %>%
    group_by(interval) %>%
    summarize(meanStepsPerInterval = mean(steps))
maxInterval <- result2[result2$meanStepsPerInterval == 
                           max(result2$meanStepsPerInterval), ]
maxText <- paste("max interval:", as.character(maxInterval[1, 1]),  
                  ", steps:", as.character(round(maxInterval[1, 2]), digits = 0))

g2 <- ggplot(result2, aes(x = interval, y = meanStepsPerInterval))
g2 + geom_line(colour = "red", size = .7) + 
     labs(x = "Time Interval", y = "Steps per Time Interval") +
     annotate("text", x = 1500, y = 195, 
             label = maxText) + 
     ggtitle("Mean Daily Activity Pattern")
```

**Imputing missing values**  
*Total number of missing values in the source dataset (df)*
```{r}
incompleteCases <- sum(!complete.cases(df))
```

*Fill in missing values using [Amelia] (http://cran.r-project.org/web/packages/Amelia/Amelia.pdf) (pretty badly)*  
*and create new dataset*
```{r}
library(Amelia)
# create an imputed dataset
naData <- select(df, -intervalA)
naSet <- amelia(naData, m = 3, ts = "date", noms = "weekday")
naSet <- as.data.frame(naSet$imputations)

# Add a swag guess
naSet <- mutate(naSet, swag = (abs(imp1.steps) + 
                               abs(imp2.steps) + 
                               abs(imp3.steps)) / 3)
# Pop it onto df
df$swag <- naSet$swag 
```
**Histogram of results**

```{r}
result3 <-
    df %>%
    group_by(date) %>%
    summarize(swagDailySteps = sum(swag))
meanResult3 <- round(mean(result3$swagDailySteps))
medResult3 <- round(median(result3$swagDailySteps))

g <- ggplot(result3, aes(x=swagDailySteps))
g + geom_bar(binwidth = 5000, fill = "dark grey", color = "dark blue") + 
    xlim(0, 25000) +
    labs(x = "Steps per Day", 
         y = "Total Days") +
    ggtitle("Total Days by_group Steps per Day (imputed)") + 
    theme(plot.title = element_text(size = 20, color="dark blue")) +
    annotate("text", x = 21000, y = 29, color="dark blue", 
             label = paste("   mean steps/day: ", meanResult3)) + 
    annotate("text", x = 21000, y = 27, color="dark blue", 
             label = paste("median steps/day: ", medResult3))
```

**Comparing complete cases versus imputed values** 
```{r}
# Ratio of mean complete vs imupted
print(paste(round(meanResult1, digits = 2), " -vs- ", round(meanResult3, digits = 2)))

# Ratio of median complete vs imupted
print(paste(round(medResult1, digits = 2), " -vs- ", round(medResult3, digits = 2)))

```

**Are there differences in activity patterns between weekdays and weekends?**
*add "dayType" column and compute weekend vs weekday*
```{r}
library(timeDate)
df1$dayType <- "weekday" # add "weekends" to all and...
df1$dayType[isWeekend(df1$date)] <- "weekend" # change what needs it
df1$dayType <- as.factor(df1$dayType) # set them up as a factor
```

*graph results*
```{r}
result4 <-
    df1 %>%
    group_by(interval, dayType) %>%
    summarize(totalPerDayType = sum(steps))

g4 <- ggplot(result4, aes(interval, totalPerDayType, facets = dayType))
g4 + geom_line(aes(colour = dayType), size = .7) +
     labs(x = "Interval", y="Steps per Interval") + 
     facet_wrap(~ dayType, ncol = 1) +
     ggtitle("Comparative Daily Activity Pattern")
```




