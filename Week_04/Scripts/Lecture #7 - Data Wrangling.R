### Today we are going to plot penguin data ####
### Created by: Cameron Atighetchi #############
### Updated on: 2022-02-14 ####################
#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)
### Load data ######
# The data is part of the package and is called penguins ##
glimpse(penguins)
head(penguins)
## Adding Filters ######
filter(.data = penguins, sex =="female")
### filter with multiple conditions -- females greater than 5000g ####
filter(.data = penguins, sex == "female", body_mass_g > 5000)
### Using Boolean ######
filter(.data = penguins, sex == "female" & body_mass_g > 5000) ### this means the same thing as the one above ###
### Boolean practice ######
#### Penguins that were collected in either 2008 or 2009 ####
filter(.data = penguins, year == 2009 | year == 2008 )
### Penguins that are not from the island Dream ####
filter (.data = penguins, island!= "Dream")
### Penguins in the species Adelie and Gentoo #####
filter (.data = penguins, species == "Adelie" | species == "Gentoo")
filter(.data = penguins, species!="Chinstrap")
filter(.data = penguins, species %in% c("Adelie","Gentoo"))

## Mutate function - adds columns #####

data2 <- mutate(.data = penguins, body_mass_kg = body_mass_g/1000)
view(data2)

### Change Multiple columns at once ####
data2 <- mutate(.data = penguins, 
                body_mass_kg = body_mass_g/1000, 
                bill_length_depth = bill_length_mm/bill_depth_mm)
view(data2)

### Do Conditional tests with mutate ####

data2 <- mutate(.data = penguins, 
                after_2008 = ifelse(year>2008, 
                                    "After 2008", 
                                    "Before 2008"))
view (data2)

##### Ifelse activity ######

data2 <- mutate(.data = penguins,
                flip_and_body = body_mass_g + flipper_length_mm,
                greater4000 = ifelse(body_mass_g>4000, "Big", "Small"))
view(data2)

### The Pipe #####

###### Use %>% as an "and then" connector ####

penguins %>% ## Use penguin dataset
  filter(sex =="female") %>%    ## select females
  mutate(log_mass = log(body_mass_g)) %>% ##calculate log biomass
  select (Species=species, island, sex, log_mass) ## select certain columns and rename species to Species
  
## Summarize ####
penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE)) ## Compute a table of data, calculate mean flipper and exclude any NA's

# Calculate mean and mean flipper length #####
penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE), ## mean flipper length
            min_flipper = min(flipper_length_mm, na.rm = TRUE)) ### min flipper length

## Calculate and group_by means #####
penguins %>%
  group_by(island) %>%. ### Groups by island
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max (bill_length_mm, na.rm =TRUE))

### Remove NA's ####

penguins %>%
    drop_na(sex) ### Drops all NA's in sex column
  
penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

## Pipe into ggplot ####
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x=sex, y= flipper_length_mm)) +
  geom_boxplot()

            