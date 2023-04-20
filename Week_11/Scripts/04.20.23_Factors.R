### Working with Factors ####
### Created by: Cameron Atighetchi #############
### Updated on: 2022-04-20 ####################
#### Load Libraries ######
library(here)
library(tidyverse)
#### Load Data ###############
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')
#### Functions ################
fruits<-factor(c("Apple", "Grape", "Banana"))
fruits

# Factor Booby traps

# Let's say you had a typo in a column of what was 
#suppose to be numbers. R will read everything in as characters. 
#If they are characters and you try to covert it to a number, 
#the rows with real characters will covert to NAs

test<-c("A", "1", "2")
as.numeric(test)

# Lets test with a factor 
test<-factor(test) # covert to factor
as.numeric(test)

##### USING FORCATS ###############################
glimpse(starwars)

starwars %>%
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE)


# there are 38 unique species but most are really rare. lets say we
# wanted to lump together species that have less than 3 individuals
# we could use fct_lump which converts the data into a factor and lumps it together

star_counts<-starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>%
  count(species)
star_counts

# Reordering factors

star_counts %>%
  ggplot(aes(x = species, y = n))+
  geom_col()

# Reorder factors in ascending order

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n))+ # reorder the factor of species by n
  geom_col()

# Descending order

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ # reorder the factor of species by n
  geom_col() +
  labs(x = "Species")

# We will make a plot of total income by year and quantile across all dollar types

total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor

# Basic Line Plot
total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+
  geom_line()

# We can reorder the line plots by using fct_reorder2 which reorders data by 2 variables

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")

# reorder levels directly in a vector the way we want it

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1

# you can set the specific order of the levels

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1

# Subset Data with Factors

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) # only keep species that have more than 3

starwars_clean

levels(starwars_clean$species)

#Only the data that we subsetted are in the dataframe, but all the levels from all possible factors are still there... this causes all sorts of problems when you go to plot. Using either fct_drop() within mutate() or droplevels() (the latter is in base R), we can remove any extra levels not included in the dataframe

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() # drop extra levels

levels(starwars_clean$species)

# Recode levels If you want to rename (or recode) a level. For example, lets recode Human to Humanoid.

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels 
  mutate(species = fct_recode(species, "Humanoid" = "Human"))
starwars_clean




