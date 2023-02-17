### Lecture 4b: Today we are going to practice tidy with biogeochemistry data from Hawaii ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-16 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
### Load data ######
ChemData<-read_csv(here("Week_04",
                        "data", 
                        "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

### Functions ###############
### Another way to remove all NA's ############
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) #filters out anything that is not a complete row
view (ChemData_clean) ### View Chem data 
  
### Separate #####################
ChemData_clean <- ChemData %>%
  drop_na() %>% #filters out anything that is not a complete row
  separate(col = Tide_time, #chooses column name
           into = c("Tide", "Time"), #separate it into two columns names Tide and Time
           sep = "_") #separate by _
head (ChemData_clean)

### Separate but keep the original column ######################
ChemData_clean <- ChemData %>%
  drop_na() %>% #filters out anything that is not a complete row
  separate(col = Tide_time, #chooses column name
           into = c("Tide", "Time"), #separate it into two columns names Tide and Time
           sep = "_", #separate by _
           remove = FALSE) #keep the original tide_time column
head (ChemData_clean)

### Unite columns #########
ChemData_clean <- ChemData %>%
  drop_na() %>% #filters out anything that is not a complete row
  separate(col = Tide_time, #chooses column name
           into = c("Tide", "Time"), #separate it into two columns names Tide and Time
           sep = "_", #separate by _
           remove = FALSE) %>% #keep the original tide_time column
  unite(col = "Site_Zone", # the name of the NEW column
        c(Site, Zone), # the columns to unite
        sep = ".", # puts a . in the middle
        remove = FALSE) #keep the original
head (ChemData_clean)

### Pivoting ###################

### Pivot_longer ##########
ChemData_long <- ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the columns you want to pivot. this says select the temp to percent SGD cols
  names_to = "Variables", # the names of the new columns with all the column names
  values_to = "Values") # names of the new column with all the values

view(ChemData_long)

### You can calculate the mean and variance for all variables at each site ############
ChemData_long %>%
  group_by(Variables, Site) %>%  #group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), # get mean
            Param_vars = var(Values, na.rm = TRUE)) # get variance

### Calculate mean, variance, and standard deviation for all variables by site, zone, and tide #############
ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>% # group by everything we want 
  summarise(Param_means = mean(Values, na.rm = TRUE), # get mean 
            Param_vars = var(Values, na.rm = TRUE), # get variance
            Param_stdev = sd(Values, na.rm = TRUE)) # get standard deviation

### Facet Wrap with Long Data ##############
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+
  facet_wrap(~Variables, scales = "free") #### scales = "free" releases both the x and y axes

### Pivot_Wide ##################
ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns
              values_from = Values) # column with the values
view(ChemData_wide)

### Calculate and export summary statistics ####################
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns Tide and time
           sep = "_", # separate by _
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Variables", # the names of the new cols with all the column names
               values_to = "Values") %>% # names of the new column with all the values
  group_by(Variables, Site, Time) %>%
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variables,
              values_from = mean_vals) %>% # notice it is now mean_vals as the col name
write_csv(here("Week_04","Output","SummaryforLecture.csv"))  # export as a csv to the right folder







