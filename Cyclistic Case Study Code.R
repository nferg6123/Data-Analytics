install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forcats)

setwd("D:/OneDrive/Documents/Data Analytics/DAC Case Study 1")

mar_2021 <- read.csv("divvy trips CSV/202103-divvy-tripdata.csv")
apr_2021 <- read.csv("divvy trips CSV/202104-divvy-tripdata.csv")
may_2021 <- read.csv("divvy trips CSV/202105-divvy-tripdata.csv")
jun_2021 <- read.csv("divvy trips CSV/202106-divvy-tripdata.csv")
jul_2021 <- read.csv("divvy trips CSV/202107-divvy-tripdata.csv")
aug_2021 <- read.csv("divvy trips CSV/202108-divvy-tripdata.csv")
sep_2021 <- read.csv("divvy trips CSV/202109-divvy-tripdata.csv")
oct_2021 <- read.csv("divvy trips CSV/202110-divvy-tripdata.csv")
nov_2021 <- read.csv("divvy trips CSV/202111-divvy-tripdata.csv")
dec_2021 <- read.csv("divvy trips CSV/202112-divvy-tripdata.csv")
jan_2022 <- read.csv("divvy trips CSV/202201-divvy-tripdata.csv")
feb_2022 <- read.csv("divvy trips CSV/202202-divvy-tripdata.csv")

# colnames(mar_2021)
# colnames(apr_2021)
# colnames(may_2021)
# colnames(jun_2021)
# colnames(jul_2021)
# colnames(aug_2021)
# colnames(sep_2021)
# colnames(oct_2021)
# colnames(nov_2021)
# colnames(dec_2021)
# colnames(jan_2022)
# colnames(feb_2022)
# 
# str(mar_2021)
# str(apr_2021)
# str(may_2021)
# str(jun_2021)
# str(jul_2021)
# str(aug_2021)
# str(sep_2021)
# str(oct_2021)
# str(nov_2021)
# str(dec_2021)
# str(jan_2022)
# str(feb_2022)

all_trips <- bind_rows(mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021,oct_2021,nov_2021,dec_2021,jan_2022,feb_2022)

# colnames(all_trips)
# nrow(all_trips)
# dim(all_trips)
# head(all_trips)
# str(all_trips)
# summary(all_trips)

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length <- as.numeric(all_trips$ride_length)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
table(all_trips_v2$day_of_week)


ggplot(data = all_trips_v2,aes(x=fct_infreq(day_of_week),fill = day_of_week)) + geom_bar() +
  labs(x = "day_of_week", title = "Total Rides Per Day of the Week") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
 
ggsave("day_of_week_mode.png", plot = last_plot())

# looking for differences between casual riders and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


#ordering by day of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarize(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# casual vs member number of rides
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# casual vs member ride duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#saving data as as csv
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'D:\\OneDrive\\Documents\\Data Analytics\\DAC Case Study 1\\avg_ride_length.csv')


ggplot(data = all_trips_v2,aes(x=fct_infreq(month),fill = month)) + geom_bar() +
  labs(x = "Month", title = "Total Rides Per Month") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

ggsave("month_mode.png", plot = last_plot())

#casual vs member by month
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

ggsave("casual_member_by_month.png", plot = last_plot())

write.csv(all_trips_v2, file = 'D:\\OneDrive\\Documents\\Data Analytics\\DAC Case Study 1\\all_rides_v2.csv')
