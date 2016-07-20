#reproducible research
#assignment 1
library(sqldf)
#reset all variables
rm(list=ls(all=T))

#get the file and unzip
archiveDF <- "activity.zip"

if(!file.exists(archiveDF)) {
  archiveURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url=archiveURL,destfile=archiveDF,method="curl")
}

if(file.exists(archiveDF)){
  unzip(archiveDF)
}

# Code for reading in the dataset and/or processing the data

df <- read.csv("activity.csv", header=T)
print(summary(df))

# Histogram of the total number of steps taken each day
daily_total_steps <- aggregate(steps~date, df, FUN=sum, na.rm=T)
View(daily_total_steps)
print(summary(daily_total_steps))
print(str(daily_total_steps))

hist(daily_total_steps$steps)


# Mean and median number of steps taken each day
print(mean(daily_total_steps$steps))
print(median(daily_total_steps$steps))

# Time series plot of the average number of steps taken
daily_avg_steps<- aggregate(steps ~ interval, data=df, FUN=mean, na.rm=T)
#View(daily_avg_steps)
str(daily_avg_steps)
plot(daily_avg_steps, type="l")

# The 5-minute interval that, on average, contains the maximum number of steps
print(daily_avg_steps$interval[which.max(daily_avg_steps$steps)])

# Code to describe and show a strategy for imputing missing data
print(sum(is.na(df)))

df_tmp <- merge(df, daily_avg_steps, by = "interval", suffixes = c("",".y"))
#pick all NAs in steps

df_nas <- is.na(df$steps)
str(df_tmp)
df_bkup <- df
df$steps[df_nas] <- trunc(df_tmp$steps.y[df_nas])

jjj<-cbind(df_bkup, df)
names(jjj)<-c("a_steps","a_date", "a_interval","b_steps","b_date", "b_interval")

yyy<-sqldf("select a_steps, b_steps, a_interval, b_interval, a_date, b_date
           from jjj  where a_date = '2012-10-08'")
View(yyy)

#View(df)
#write.csv(imputed_df,file="imputed_df.csv")

# Histogram of the total number of steps taken each day after missing values are imputed

daily_total_steps_imputed <- aggregate(steps~date, df, FUN=sum, na.rm=T)
hist(daily_total_steps_imputed$steps)
print(mean(daily_total_steps_imputed$steps))
print(median(daily_total_steps_imputed$steps))
View(daily_total_steps_imputed)

xxx<-sqldf('select a.*, b.* from daily_total_steps_imputed a left outer join daily_total_steps b 
           on (a.date = b.date)')
View(xxx)

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# All of the R code needed to reproduce the results (numbers, plots, etc.) in the report 
