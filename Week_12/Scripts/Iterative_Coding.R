### Iterative Coding ####
### Created by: Cameron Atighetchi #############
### Updated on: 2022-04-24 ####################
#### Load Libraries ######
library(here)
library(tidyverse)
#### Functions ###################################
# For loops - Simple
print(paste("The year is", 2000))

# Put it in a for loop

years <- c(2015:2021)
years<-c(2015:2021)
for (i in years){ # set up the for loop where i is the index
  print(paste("The year is", i)) # loop over i
}

# let's say we want to save a new vector with all the years

#Pre-allocate space for the for loop

# empty matrix
year_data<-data.frame(matrix(ncol = 2, nrow = length(years)))
# add column names
colnames(year_data)<-c("year", "year_name")

year_data

# Now we can add in the column that is going to have all the names in it
for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over i
}
year_data

# Now we can add the year column in as well

for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over year name
  year_data$year[i]<-years[i] # loop over year
}
year_data

# Using loops to read in multiple .csv files

testdata<-read_csv(here("Week_12", "Data","011521_CT316_1pcal.csv"))
glimpse(testdata)

# List files in a directory

# point to the location on the computer of the folder
CondPath<-here("Week_12", "Data", "cond_data")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv")
files

# Pre allocate space for a loop

# make an empty dataframe that has one row for each file and 3 columns
cond_data<-data.frame(matrix(nrow = length(files), ncol = 3))
# give the dataframe column names
colnames(cond_data)<-c("filename","mean_temp", "mean_sal")
cond_data


# Use the for loop on it

raw_data<-read_csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works
head(raw_data)

mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp


#First, add in the filename for each row

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
} 
cond_data

# Add in means

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data

#### PURRRRRRR ###################################

# Using map functions - lets calculate the mean from a set of random numbers and do it 10 times

# create a vector from 1:10

1:10

# for each time 1:10 make a vector of 15 random numbers based on a normal distribution

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) # calculate 15 random numbers based on a normal distribution in a list

# Calculate the mean from each list

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

# make your own function!!!

1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

# use a formula when you want to change the arguments within a function

1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

# Bring in files using purrr instead of a for loop

# find path to files
# point to the location on the computer of the folder
CondPath<-here("Week_12", "Data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")
files

# full name of our path
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files

# Read in the files

data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") # map everything to a dataframe and put the id in a column called filename
data

# Calculate means
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
data



















