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
View(trips)

# convert birth years to numerics
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

# show the distribution of age. Mean, median, standard deviation

# use filter and grepl to find all trips that either start or end on broadway

# do the same, but find all trips that both start and end on broadway

# find all unique station names

# count the number of trips by gender

# compute the average trip time by gender
# comment on whether there's a (statistically) significant difference

# find the 10 most frequent station-to-station trips

# find the top 3 end stations for trips starting from each start station

# find the top 3 most common station-to-station trips by gender

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?


