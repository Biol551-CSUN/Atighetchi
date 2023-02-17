### Lab - 2/16/23 ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-16 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
### Load data ######
ChemData<-read_csv(here("Week_04",
                        "data", 
                        "chemicaldata_maunalua.csv"))  # Loads csv into ChemData Dataframe
View(ChemData)
glimpse(ChemData)

### Functions #################
ChemData_clean <- ChemData %>%
  drop_na() %>% #filters out anything that is not a complete row
  separate(col = Tide_time, #chooses column name Tide_time
           into = c("Tide", "Time"), #separate into two columns named Tide and Time
           sep = "_") %>% #separate it by _
  filter(Season == "SPRING") %>% # Give me all values that were measured in Spring
 
view (ChemData_clean)

ChemData_long <- ChemData_clean %>%  # creates new name for dataframe
  pivot_longer(cols = Temp_in:percent_sgd, # selects columns from Temp_in to Percent_sgd
               names_to = "Variables", # names the new column with all measurements
               values_to = "Values") # name of new column with all measurement values
view(ChemData_long)

ChemData_long %>% 
  group_by(Variables, Season) %>% # groups by Variables and Season (Spring)
  summarise(Param_means = mean(Values, na.rm = TRUE)) %>% # Gives us the mean of variables in Spring
  mutate(Param_means = round(Param_means, digits = 2)) %>% # Gives the param_means new rounded values
  ggplot(aes(x = Variables, y = Param_means)) + # plots variables versus param_means
  facet_wrap(~Variables, # facet wraps based on variables
             scales = "free") + # opens scales for x and y axes for each facet
  geom_col() + # creates bar charts 
  labs(x = "", 
       y = "",  # Gets rid of x and y axes labels and gives a title
       title = "Average Variables Measured in Spring") + 
  geom_text(aes(label = Param_means), # assigns the Param_means value to each variable and puts on chart
            position = position_stack(vjust = 0.5), # moves position of value into the middle of bar
            color = "white", #changes text color of value
            size = 4) + # changes size of value
  theme_base() + # changes theme
  theme(plot.title = element_text(hjust = 0.3,
                                  size = 15)) + # moves horizontally and makes the plot title larger
  theme(plot.title = element_text(margin = margin(b = 15))) # moves plot title vertically
ggsave(here("Week_04", "Output", "2/16/23_plot.png"), width = 10, height = 7) # saves and exports plot
write_csv(here("Week_04","Output","2/16/23- HW.csv")) # exports as csv
 
  
  
  
