# Capstone_Project


- Google Analytics Capstone Project
- Ambika Muscarello
- June 19, 2023

## Introduction
This is a case study for the Google Analytics Professional Certification course. Cyclistic, a fictional bike-share company located in Chicago, is looking to increase their membership numbers.

## Scenario
Cyclistic has more than 5,800 bicycles and over 600 docking stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. The marketing director believes that maximizing the number of annual members will be key to future growth and is looking at a campaign to convert casual riders into members. You are a junior data analyst in the company and tasked with how Cyclistic members and casual riders use their bikes.

## 1. Ask

### Business Objective
#### Increase market share by converting casual riders into member through a targeted marketing campaign.


### Stakeholders
- Lily Moreno: Director of marketing, responsible for the development of campaigns.
- Cyclistic marketing analytics team: The team responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy.
- Cyclistic executive team: The executive team who will decide whether to approve the recommended marketing program.

### Business Task
#### How do annual members and casual riders use Cyclistic bikes differently?



## 2. Prepare

### Data Source
The data provided has been made available by Motivate International Inc. under this license.
For this project, a year’s data will be used, provided by Motivate in csv format.

### ROCCC process:

- R (reliable) - Collected by Motivate
- O (original) - Yes
- C (comprehensive) - Consisting of all rides taken
- C (current) - current 12 months
- C (cited) - Yes

#### Licensing, privacy, security, and accessibility
Data-privacy issues prohibit the use of riders’ personal information which means that casual riders might not all be from the Chicago service area.

#### Install required packages and libraries
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

### Collect Data

####Upload Divvy datasets (csv files) here
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

### ** Wrangle data and combine into a single file**

#### Compare column names of every file
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)


#### Rename columns to make them consistent
(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))



#### Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


#### Convert ride_id and rideable_type to character so that they can stack correctly
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


#### Stack individual quarter’s data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#### Remove lat, long, birthyear, and gender fields
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))


### 3. Process

#### Clean data to prepare for analysis
#### Processing the data, several inconsistencies were found such as missing and duplicate information, which were addressed.

#### Inspect the new table that has been created
colnames(all_trips)  #List of column names

nrow(all_trips)  #How many rows are in data frame?

dim(all_trips)  #Dimensions of the data frame?

head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)

str(all_trips)  #See list of columns and data types (numeric, character, etc)

summary(all_trips)  #Statistical summary of data. Mainly for numerics


#### There are a few problems we will need to fix:
#### Start by seeing how many observations fall under each usertype
table(all_trips$member_casual)


#### In the “member_casual” column, there are two names for members (“member” and “Subscriber”) and two names for casual riders (“Customer” and “casual”). We will need to replace “Subscriber” with “member” and “Customer” with “casual”
#### N.B.: “Level” is a special property of a column that is retained even if a subset does not contain any values from a specific level

#### Reassign to the desired values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))

#### Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)


#### The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data – such as day, month, year – that provide additional opportunities to aggregate the data.
#### Add columns that list the date, month, day, and year of each ride
#### This will allow us to aggregate ride data for each month, day, or year.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#### Add a calculated field for length of ride since the 2020Q1 data did not have the “tripduration” column. We will add “ride_length” to the entire dataframe for consistency.
#### Add a “ride_length” calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#### Inspect the structure of the columns
str(all_trips)


#### Convert “ride_length” from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#### Remove “bad” data
#### There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons or ride_length was negative; that we want to delete.

#### We will create a new version of the dataframe (v2) since some data will be removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


## 4. Analyze

#### Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)

median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths

max(all_trips_v2$ride_length) #longest ride

min(all_trips_v2$ride_length) #shortest ride


#### Condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

#### Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


#### See the average ride time for each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#### The days of the week are out of order. It requires fixing
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#### Run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#### Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
     ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts



#### Create a visualizattion of the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
     ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title="Number of rides by type")



#### Create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
     ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Average duration of rides")


## 5. Share
Create a csv file that we will visualize in Excel, Tableau, or the presentation software of choice.
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write.csv(all_trips_v2, file='all_trips_v3.csv')


## 6. Act

### Conclusion

#### How do casual riders and members differ?

#### Casual Riders

- As the name suggest, casual riders use the service for leisure, recreational activities.
- The service peaks during the summer months from May - July and for all periods, mostly weekends.
- Docked bikes are preferred over classic.
- The average time used is higher, nearly doubled, compared to members.

#### Members

- Members use the service for going to and from work.
- They are consistent in the use of the service the entire week.
- Members prefer classic bikes.
- The duration of use is nearly half that of casual riders.
- More bikes are used by riders.

#### Recommendations

- Offer a one-year promotional membership for first time members.
- Special summer offers since summer is the peak period for casual riders.
-Tie in cycling not only with exercise but saving the planet.

The marketing team can design their campaigns around these findings. They can allocated more resources during the summer months, weekends, at stations close to leisure spots etc.
