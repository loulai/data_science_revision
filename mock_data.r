# ---- Creating data

# Event-level data: an attendance log for every student in a school district
# date | student_id | attendance

total_days = 2
total_students = 3
library(lubridate)

create_event_df <- function(start_date, days, students){
  num_rows = days * students
  
  # initializing vectors for efficiency
  start_date <- as.Date(start_date)
  date <- rep(start_date, num_rows)
  student_id <- numeric(num_rows)
  attendance <- rep('Y', num_rows)
  
  initial_df <- data.frame(date, student_id, attendance)
  
  # DATE vector creation
  day(start_date) <- day(start_date) - 1
  for (i in 1:num_rows){
    if(i%%students == 1){
      day(start_date) = day(start_date) + 1
    }
    date[i] <- start_date
  }
  
  # lol
  initial_df <- data.frame(date, student_id, attendance)
  initial_df
}

event_df2 <- data.frame(date=c('2017-01-08', '2017-01-08', '2017-01-08'), student_id=c(1001, 1002, 1003), attendance=c('Y','Y','N'))




# Dimension-level data: a summary table with demographics for each student in the district
# student_id | school_id | grade_level | date_of_birth | hometown

# ---- Questions

# What was the overall attendance rate for the school district yesterday?

# Which grade level currently has the most students in this school district?

# Which school had the highest attendance rate? The lowest?


# ---- Further Mock Questions

# Given timestamps of logins, figure out how many people on Facebook were active all seven days of a week on a mobile phone.

# How do you determine what product in Facebook was used most by the non-employee users for the last quarter? [Required parameters will be given]
