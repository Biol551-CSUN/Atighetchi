### Homework for Data Wrangling ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-14 ####################

## Write a script that:

###### 1.calculates the mean and variance of body mass by species, island, and sex without any NAs

###### 2. filters out (i.e. excludes) male penguins, then calculates the log body mass, 
#then selects only the columns for species, island, sex, and log body mass, 
#then use these data to make any plot. Make sure the plot has clean and clear labels 
#and follows best practices. Save the plot in the correct output folder.


#### Load Libraries ######
library(tidyverse)
library(here)
library(palmerpenguins)
library(ggthemes)
library(beyonce)

#### Load Data #####
head(penguins)

#### Functions #######

#1.calculates the mean and variance of body mass by species, island, and sex without any NAs

mean_data <- penguins %>%   #assign mean_data to this filtered penguin data
  group_by(species, 
           island, 
           sex) %>%  #group by species, island, and sex
  drop_na (species, 
           island, 
           sex) %>%  # drop all NA in species, island, and sex
  summarise(mean_body_mass = mean(body_mass_g, 
                                  na.rm = TRUE),  #compute mean body mass
            variance_body_mass = var(body_mass_g, 
                                  na.rm = TRUE)) #compute variance body mass

view (mean_data) #view data

###### 2. filters out (i.e. excludes) male penguins, then calculates the log body mass, 
#then selects only the columns for species, island, sex, and log body mass, 
#then use these data to make any plot. Make sure the plot has clean and clear labels 
#and follows best practices. Save the plot in the correct output folder.

penguins %>%
  filter(sex == "female") %>%  #filters only females into dataset
  mutate(log_body_mass = log(body_mass_g)) %>%  #adds a log body mass column
  select (species,  # selects species, island, sex, and log columns
          island, 
          sex, 
          log_body_mass) %>%
  ggplot(aes(x = species, #starts plot and assigns x and y values to species and log body respectively
             y = log_body_mass, 
             group = species, # assigns species to a group
             color = species)) +. #assigns color to species and colors species x values
  geom_point() +   #assigns points to value of graph
  labs(title = "Species and Log Body Mass",  ###labels for each axis, title, and legend 
       x = "Species", 
       y = "Log Body Mass",
       color = "Species") + ## Assigns new label for legend
  theme_economist() + #imported ggtheme theme
  theme(panel.grid.major = element_line(color = "gray",   #creates major grid lines at y-breaks
                                        linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", 
                                        linetype = "dashed"), #adds minor grid lines in between y-breaks
        axis.title.x = element_text(vjust = -3.5), #adjusts x axis down
        axis.title.y = element_text(vjust = 3.5), #adjusts y axis to the left
        axis.title = element_text(face = "bold"), #bolds axis titles
        plot.title = element_text(hjust = 0.4), #adjusts plot title to the right
        panel.background = element_rect(fill = "beige")) + #beige background color
  scale_color_manual(values = beyonce_palette(101)) #assigns beyonce palette to the points
  
  ggsave(here("Week_04", "Output", "HWDATAWRANG.png"), width = 8, height = 6) #saves plot as png
  
  

  








  





