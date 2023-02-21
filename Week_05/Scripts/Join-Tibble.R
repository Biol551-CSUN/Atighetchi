### Lecture 5a: Data Wrangling: joins####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-21 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
### Load data ######
# Environmental data from each site
EnviroData <- read.csv(here("Week_05", "Data", "site.characteristics.data.csv"))

#Thermal performance data
TPCData <- read_csv(here("Week_05", "Data", "Topt_data.csv"))
glimpse(EnviroData)
glimpse(TPCData)

### Functions ######
EnviroData_wide <- EnviroData %>%
  pivot_wider(names_from = parameter.measured, 
              values_from = values) %>%
  arrange(site.letter)
view(EnviroData_wide)

FullData_left <- left_join(TPCData, EnviroData_wide) %>% ## Joining with by = join_by(site.letter)
  relocate(where(is.numeric), .after = where(is.character)) #relocate all the numeric data after the character data
head(FullData_left)

### Calculating the mean, variance of data by site #####
FullData_left %>%
  pivot_longer(cols = E:substrate.cover, 
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(site.letter, Variables) %>%  #Groups by site letter !!!!!
  summarise(mean_vals = mean(Values, na.rm = TRUE), 
            var_vals = var(Values, na.rm = TRUE))

##Could also use ####

FullData_left %>%
  group_by(site.letter) %>%
  summarise_at(vars(E:substrate.cover), .funs = list(mean = mean, var = var))

### Creating a Tibble #######

# Make 1 tibble
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

# make another tibble
T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))
T2

### Left join versus Right join ####

left_join(T1, T2) ### Keeps the T1 data untouched and adds T2 data if it matches
##joins by site.id

right_join(T1, T2) ### Keeps the T2 data untouched and adds T1 data if it matches
##joins by site.id

#### Inner join versus Full join #####
inner_join(T1, T2)

full_join(T1,T2)

### Semi join versus anti join ######

semi_join(T1, T2)  #only keeps first column

anti_join(T1, T2) # saves all rows in the first data set that do not match anything in the second data set allows for finding missing data


