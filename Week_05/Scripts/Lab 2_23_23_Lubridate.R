### Lab 5b: Lubridate####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-23 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(lubridate)
library(ggthemr)
library(ggthemes)
### Load Data ########
CondData <- read_csv(here("Week_05", "Data", "CondData.csv")) # Load CondData
DepthData <- read_csv(here("Week_05", "Data", "DepthData.csv")) # Load DepthData
### Functions ########

CondData <- CondData %>%  #assigns CondData
  mutate(date = mdy_hms(date), # changes character to POSIX variable
         date = round_date(date, "10 seconds"))  # rounds time to nearest 10 seconds

DepthData <- DepthData %>% # assigns DepthData
  mutate(date = ymd_hms(date)) # changes character to datetime format
  
DepthandCond <- inner_join(DepthData,  
                           CondData) %>%  # joins DepthData and CondData and drops NA values
  mutate(date = round_date(date, "1 minute")) %>% # rounds and averages dates by minute
  group_by(date) %>% # group by date
  summarise(mean_depth = mean(Depth, na.rm = TRUE), # calculate mean depth
            mean_temp = mean(Temperature, na.rm = TRUE), # calculate mean temp
            mean_salinity = mean(Salinity, na.rm = TRUE)) %>% # calculate mean salinity
  ggplot(aes(x= date,  # assigns date to x variable
             y = mean_temp)) +  # assigns mean temp to y variable
  geom_line(color = "#636d7d") + # gives line with color
  geom_point(size = 1, color = "#a9bbd9") + # gives points with size 1 and color 
  theme_classic() + # classic theme
  labs(x = "Time on January 15th, 2021", # x, y, and plot title labels
       y = "Mean Temperature",
       title = "Average Temp as a Function of Time") +
  theme(panel.background = element_rect(fill= "beige"), # background beige
        panel.grid.major = element_line(color = "gray", # add dashed gray gridlines
                                        linetype = "dashed"))
  DepthandCond
  ggsave(here("Week_05", "Output", "Lubridate_Lab.png"), width = 7, height = 5) #saves and exports plot as a png

 