# ---- Creating data

# Event-level data: an attendance log for every student in a school district
# date | student_id | attendance

library(lubridate)

create_event_df <- function(start_date, days, students){
  num_rows = days * students
  
  # initializing vectors for efficiency
  start_date <- as.Date(start_date)
  date <- rep(start_date, num_rows)
  student_id <- numeric(num_rows)
  attendance <- rep('Y', num_rows)
  
  # DATE 
  day(start_date) <- day(start_date) - 1
  for (i in 1:num_rows){
    if(i%%students == 1){
      day(start_date) = day(start_date) + 1
    }
    date[i] <- start_date
  }
  
  # STUDENT_ID 
  student_id <- rep(c(1:students), days)
  
  # ATTENDANCE 
  attendance <- sample(c('Y', 'N'), num_rows, replace=T)
  
  # assembling dataframe
  final_df <- data.frame(date, student_id, attendance)
  final_df
}

# Dimension-level data: a summary table with demographics for each student in the district
# student_id | school_id | grade_level | date_of_birth | hometown

create_dimension_data <- function(num_students){
  student_id <- sample(1:num_students, num_students, replace=T)
  school_id <- sample(1:5, num_students, replace=T)
  grade_level <- sample(c('Freshman', 'Sophomore', 'Senior'), num_students, replace=T)
  date_range <- seq(as.Date('1999-01-01'), by = "day", length.out = 1080)
  date_of_birth <- sample(date_range, num_students, replace=T)
  hometown <- sample(c('New York', 'New Jersey', 'California', 'Texas', 'Georgia', 'Orlando', 'Boston', 'Cambridge'), num_students, replace = T)
  
  final_df <- data.frame(student_id, school_id, grade_level, date_of_birth, hometown)
  final_df
}

# Create event database:
num_students = 30
num_days = 1
day_log = create_event_df('2016-01-01', num_days, num_students)

# Create dimension database:
dimension = create_dimension_data(300)

# ---- Questions

# What was the overall attendance rate for the school district yesterday?
nrow(filter(dlog, attendance == 'Y')) / nrow(dlog) # 0.3667

# Which grade level currently has the most students in this school district?
dimension %>% group_by(grade_level) %>% summarize(students=n()) %>% arrange(desc(students))

# Which school had the highest attendance rate? The lowest?

student_merge = merge(x = day_log, y = dimension, by = "student_id")
student_merge1 <- student_merge %>% group_by(school_id, attendance) %>% mutate(attendance_count = n()) 
studnet_merge2 <- student_merge1 %>% group_by(school_id) %>% mutate(attendance_rate = nrow(attendance == 'Y')/nrow(attendance=="N"))
student_merge1 <- student_merge1 %>% group_by(school_id) %>% mutate(attendance_rate)

dimension$hometown[which(dimension$hometown == "New York")]

# ---- Further Mock Questions

# Given timestamps of logins, figure out how many people on Facebook were active all seven days of a week on a mobile phone.

# How do you determine what product in Facebook was used most by the non-employee users for the last quarter? [Required parameters will be given]
