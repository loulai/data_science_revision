library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
########################################
# READ AND TRANSFORM THE DATA
########################################
setwd('~/Desktop/programming/ds3/week1')
# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
#trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown", "Male", "Female")))

# convert birth years to numerics
trips = mutate(trips, age_in_2017 = 2017 - birth_year)
trips$birth_year <- as.numeric(trips$birth_year)


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
num_trips <- nrow(trips) # 224,736

# find the earliest and latest birth years (see help for max and min to deal with NAs)
earliest_birth_year <- max(trips$birth_year, na.rm=TRUE) # 20 years old
latest_birth_year <- min(trips$birth_year, na.rm=TRUE) # 118 years old lol

# find number of people that are the oldest and youngest age
num_youngest <- trips %>% filter(birth_year == earliest_birth_year) %>% count() #251
num_oldest <- trips %>% filter(birth_year == latest_birth_year) %>% count() #9!

# show the distribution of age
hist(trips$age_in_2017)

# find the mean, median of age
mean_age = mean(trips$age_in_2017, na.rm=TRUE) #41.5
median_age = median(trips$age_in_2017, na.rm=TRUE) #39

# find the average age of males & females
trips %>% group_by(gender) %>% summarise(avg_age_in_2017_gender = mean(age_in_2017, na.rm=TRUE)) #m=41.5, f=40.5

# show the most frequently occuring age
trips %>% group_by(age_in_2017) %>% summarise(num=n()) %>% arrange(desc(num)) %>% head() #32 years old, 9305

# show the ages within one standard deviation of the mean
std_diev = mySd(trips$age_in_2017) #11.42
group_one_sd = trips %>% filter(age_in_2017 >= (mean_age - std_diev) & age_in_2017 <= (mean_age + std_diev))

# find the % of people within one standard deviation of the mean
percent_within_one_sd = (nrow(group_one_sd) * 100 ) / nrow(na.omit(trips))
# 63.7% Usual is 68%, so pretty good

# use filter and grepl to find all trips that either start or end on broadway


# do the same, but find all trips that both start and end on broadway

# find all unique station names
unique(trips$start_station_name)

# count the number of trips by gender
num_male = nrow(filter(trips, gender == 'Male')) # 176,526
num_female = nrow(filter(trips, gender == 'Female')) # 41,479

trips %>% group_by(gender) %>% count() # <<<<<<<<<<<< come back

# compute the average trip time by gender
trips$tripduration <- as.numeric(trips$tripduration)
males <- subset(trips, gender=='Male')
females <- subset(trips, gender=='Female')
avg_tripduration_male = mean(males$tripduration)
avg_tripduration_female = mean(females$tripduration)

# comment on whether there's a (statistically) significant difference
## come back

# find the most frequent start station
trips %>% group_by(start_station_name) %>% summarize(starts=n()) %>% arrange(desc(starts)) # Lafayette St & E 8 St, 2920

# find the most frequent end station
trips %>% group_by(end_station_name) %>% summarize(ends=n()) %>% arrange(desc(ends)) # 2622

# find the 10 most frequent station-to-station trips
trips %>% group_by(start_station_name, end_station_id) %>% summarize(this_route_trips = n()) %>% ungroup() %>% arrange(desc(this_route_trips))

# find the top 3 end stations for trips starting from each start station

# find the top 3 most common station-to-station trips by gender

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?










