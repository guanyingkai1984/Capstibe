library(dplyr)
library(readr)
library(tidyverse)
library(skimr)
library(lubridate)
library(janitor)
library(scales)
library(mapview)

## Combine all CSV

df <- list.files(path="/Users/admin/Desktop/Capstone/Google_Capstone/CSV", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

## Write CSV to a file
write_csv(df,"All_Trips.csv")

## Investigate df
str(df)
summary(df)
glimpse(df)

## Investigate the NA values in the data frame
colSums(is.na(df))

## Create df1 by removing any rows with NA and Investigate df1
df1 <- drop_na(df)
glimpse(df1)


## Create df2 by adding new columns for grouping and Investigate df2
df2 = df1 %>%
  mutate(
    hour_start = hour(started_at),
    weekday = wday(started_at, label = T, abbr = F),
    month = month(started_at, label = T, abbr =F),
    day = day(started_at),
    week = strftime(started_at, format = "%V"),
    trip_time = difftime(ended_at, started_at, units = "mins")
  )
glimpse(df2)

## Check for duplicates
df2 %>%
get_dupes() %>%
tally()

## Rename columns for visual simplification
df2 = df2 %>%
  rename(
    bikes = rideable_type,
    users = member_casual
  )

## Create a new data frame with time variables for analysis
trips_time_df = df2 %>%
  select(
    ride_id, users, bikes, hour_start, weekday, month, day, week, trip_time
  )


## Create a data frame with location variables for further analysis
trips_location_df = df2 %>%
  select(
    ride_id, start_station_name, end_station_name, start_lat, start_lng,
    end_lat, end_lng, users, trip_time
  )

## Create new sumarisation variables nr_rides, average_trip, total trip grouped by users type and start hour.
ride_hours =
  trips_time_df %>%
  group_by(
    users, hour_start
  ) %>%
  summarise(
    nr_rides = n(),
    average_trip = mean(trip_time),
    total_trip = sum(trip_time)
  )

## Visualise number of trips by hours an Segmented by users type
ride_hours %>%
  ggplot(aes(hour_start, nr_rides, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous()+
  labs(
    title = "Number of Trips per Hour",
    subtitle = "Number of trips for every hour and by users",
    caption = "Figure 1",
    x = "hour of the day",
    y = "number of rides",
  )

## Visualise average number of trips by hours
ride_hours %>%
  ggplot(aes(hour_start, average_trip, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous()+
  labs(
    title = "Average Number of Trips per Hour",
    subtitle = "Number of trips for every hour segmented by users",
    caption = "Figure 2",
    x = "hour of the day",
    y = "average trips duration",
  )

## Visualise total_trip time by hours
ride_hours %>%
  ggplot(aes(hour_start, total_trip, fill = users))+
  geom_col(show.legend = TRUE, position = "dodge")+
  scale_y_continuous()+
  labs(
    title = "Total trip Duration per Hour",
    subtitle = "Total duration for every hour segmented by users",
    caption = "Figure 3",
    x = "hour of the day",
    y = "total duration",
  )

## Analysis of days of the week
ride_week = trips_time_df %>%
  group_by(
    users, weekday
  ) %>%
  summarise(
    nr_rides_week = n(),
    avg_rides_week = mean(trip_time),
    total_duration_week = sum(trip_time)
  )

## Visualise number of trips by weekday

ride_week %>%
  ggplot(aes(weekday, nr_rides_week, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Trips Time by Week Days and Segmented by Users",
    subtitle = "Number of trips for every week of the year",
    caption = "Fig 4",
    x = "day of the week",
    y = "number of trips"
  )

## Visualise average trips time by day of the week
ride_week %>%
  ggplot(aes(weekday, avg_rides_week, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Average Trips Time by Week Days and Segmented by Users",
    subtitle = "Average Number of trips for every week of the year",
    caption = "Fig 5",
    x = "day of the week",
    y = " avg number of trips"
  )


## Visualise total trips time by day of the week
ride_week %>%
  ggplot(aes(weekday, total_duration_week, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Time Trips by Week Days and Segmented by Users",
    subtitle = "Total Trips Time for every week of the year",
    caption = "Fig 6",
    x = "day of the week",
    y = " total time trips"
  )

## Analysis of trip time by month
ride_month = trips_time_df %>%
  group_by(
    users, month
  ) %>%
  summarise(
    nr_rides_month = n(),
    avg_rides_month = mean(trip_time),
    total_time_month = sum(trip_time)
  )

## Visualise number trips by month
ride_month %>%
  ggplot(aes(month, nr_rides_month, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Number of Trips by Month and Segmented by Users",
    subtitle = "Number Trips Time for every Month",
    caption = "Fig 7",
    x = "month",
    y = " number of trips"
  )

## Visualise Average trips time by month
ride_month %>%
  ggplot(aes(month, avg_rides_month, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Average Trips Time by Month and Segmented by Users",
    subtitle = "Average Trips Time for every Month",
    caption = "Fig 8",
    x = "month",
    y = "average trips time"
  )

## Visualise Total trips time by month
ride_month %>%
  ggplot(aes(month, total_time_month, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Trips Time by Month and Segmented by Users",
    subtitle = "Total Trips Time for every Month",
    caption = "Fig 8",
    x = "month",
    y = "total trips time"
  )


## Group Start Location

pop_start_station = trips_location_df %>% 
  group_by(
    users, start_station_name, start_lat, start_lng
  ) %>% 
  summarise(
    nr_rides_start = n()
  ) %>% 
  arrange(-nr_rides_start)


## Visualise the most popular 10 start stations
pop_start_station[1:10, ] %>%
  ggplot(aes(start_station_name, nr_rides_start, fill = users))+
  geom_col(position = "dodge")+
  coord_flip()+
  labs(
    title = "Most Popular Start Stations",
    subtitle = "Top 10 most popular start stations",
    caption = "Paul Juverdeanu",
    x = "station name",
    y = "number of trips"
  )

## Mapview of the most popular 30 start stations
pop_start_station[1:30, ] %>%
  mapview(
    xcol = "start_lng",
    ycol = "start_lat",
    cex = "nr_rides_start",
    alpha = 0.9,
    crs = 4269,
    color = "#8b0000",
    grid = F,
    legend = T,
    layer.name = "30 Most Popular Start Stations"
  )


## Group number of rides by bike type
ride_bikes = 
  trips_time_df %>% 
  group_by(
    users, bikes
  ) %>% 
  summarise(
    nr_bike_ride = n(),
    average_trip = mean(trip_time),
    total_trip = sum(trip_time)
  )


## Visualise number of rides by bike type
ride_bikes %>% 
  ggplot(aes(bikes,nr_bike_ride, fill = users))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Number of Trips per Bike Type and Segregated by Users",
    subtitle = "Number of trips per bike type",
    caption = "Fig 15",
    x = "bike type",
    y = "number of trips"
  )

