#1.     Code for reading in the dataset and/or processing the data
folder_location <-"/Users/ethanschein/r-programming/RepData_PeerAssessment1"
setwd(folder_location)
unzip(zipfile = paste(folder_location, "/", 'activity.zip', sep = ""))
activity_DS <-read.csv(file = 'activity.csv', header = T)
activity_DS$date <- as.Date(x = activity_DS$date, "%Y-%m-%d" )

good_act <- activity_DS[complete.cases(activity_DS), ]

#2. Histogram of the total number of steps taken each day
dta.sum <- aggregate(x = good_act['steps'], FUN = sum, by = list(Group.date = good_act$date))
hist(dta.sum$steps, main = "Steps")

#3. Mean and median number of steps taken each day
round(mean(dta.sum$steps))

round(median(dta.sum$steps))

#4. Time series plot of the average number of steps taken
dta.mean <- aggregate(x = good_act['steps'], FUN = mean, by = list(Group.date = good_act$date))

#per day#
plot(x = dta.mean$Group.date, y = dta.mean$steps, type = 'l')

#per interval#

interval <- aggregate(good_act['steps'], by = list(good_act$interval), FUN = mean)


# The 5-minute interval that, on average, contains the maximum number of steps

interval[which.max(interval$steps),] 

# Code to describe and show a strategy for imputing missing data

for(i in 1:length(activity_DS$steps)){
        if(is.na(activity_DS$steps)[i]){
         row <- activity_DS[i,]
         int <- row[,which(colnames(row)=='interval')]
         impute <- mean(activity_DS[which(activity_DS$interval==int),]$steps, na.rm = T)
         activity_DS[i,which(colnames(activity_DS)=='steps')] <- impute
        }
}

# Histogram of the total number of steps taken each day after missing values are imputed

dta.sum2 <- aggregate(x = activity_DS['steps'], FUN = sum, by = list(Group.date = activity_DS$date))
hist(dta.sum2$steps, main = "Steps")

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

        index1 <- which(weekdays(activity_DS$date)=="Saturday" | weekdays(activity_DS$date)=='Sunday')
        DS_weekend <- activity_DS[index1,]
        DS_weekday <-activity_DS[activity_DS!=index1,]
        
        DS_weekday_agg <- aggregate(x = DS_weekday['steps'], FUN = mean, by = list(DS_weekday$interval))
        DS_weekend_agg <- aggregate(x = DS_weekend['steps'], FUN = mean, by = list(DS_weekend$interval))

        par(mfrow=c(1,2))
        hist(x = DS_weekday_agg$steps, main = "Histogram of weekday steps")
        hist(x = DS_weekend_agg$steps, main = "Histogram of weekend steps")
