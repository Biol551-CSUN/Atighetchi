### Lecture 5b: Data Wrangling: lubridate####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-23 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(lubridate)
### Load data ######
CondData <- read_csv(here("Week_05", "Data", "CondData.csv"))
### Functions ######
now() #what time is it now?
now(tzone = "EST") # what time is it on the east coast?
now(tzone = "GMT") # what time is it in GMT?
today() # tells you date
today(tzone = "GMT")
am(now()) #is it morning?
leap_year(now()) # is it a leap year?
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")
ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")

# make a character string/vector of dates
datetimes <- c("02/24/2021 22:22:20",
               "02/25/2021 11:21:10",
               "02/26/2021 8:01:52")
datetimes <- mdy_hms(datetimes) # convert to datetimes
month(datetimes, label = TRUE, abbr = FALSE) #extract the months from the character string and state the month name
day(datetimes) #extract day
wday(datetimes, label = TRUE) #extract day of the week

hour(datetimes) #extract hour
minute(datetimes) # extract minute
second(datetimes) # extract second

# lets add 4 hours to all the datetimes

datetimes + hours(4) #adds 4 hours

datetimes + days(2) # this adds 2 days

round_date(datetimes, "minute") #round to nearest minute

round_date(datetimes, "5 mins") # round to nearest 5 minute

# Using CondData and converting the data column to a datetime
# We do this to have R treat this column as dates rather than just a character

glimpse(CondData)
view(CondData)

CondData %>%
  mutate(Date = mdy_hm(Date))



 
  