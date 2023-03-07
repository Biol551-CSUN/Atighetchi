### Lecture 7a: Mapping Intro ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-03-07 ####################

#### Load Libraries ######
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

### Load data ######
popdata<-read_csv(here("Week_07","data","CApopdata.csv"))
stars<-read_csv(here("Week_07","data","stars.csv"))

### Functions ###############

# get data for the entire world
world <- map_data("world")
head(world)
# get data for the USA
usa <- map_data("usa")
head(usa)
# get data for italy
italy<-map_data("italy")
head(italy)
# get data for US states
states<-map_data("state")
head(states)
# get data for counties
counties<-map_data("county")
head(counties)

# make a map of the world
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group))   #group = group is very important, if you dont have it then it will be broken 

# add colors to lines and fill
ggplot()+
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               color = "black")

# add colorful fills and lines
ggplot()+
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = region),
               color = "black") +
  guides(fill = FALSE) +  # gets rid of color legend for each country
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_map(projection = "mercator", # changes the projection of map in the plot
            xlim = c(-180,180)) # sets a limit for x values

### LETS MAKE A MAP OF CALIFORNIA #########

# using the states dataset
head(states)

CA_data<-states %>%
  filter(region == "california")
ggplot() +
  geom_polygon(data = CA_data, aes(x = long,
                                   y = lat,
                                   group = group),
                                   fill = "navy", 
               color = "black") +
                coord_map() +
                 guides(fill = FALSE) +
                 theme_minimal()

# Adding multiple layers of data

# We want to plot population of counties in California
# Look at the county data
head(counties)[1:3,] # only showing the first 3 rows for space
# Look at the county data
head(popdata)

# We have to join the data by subregion and county (change subregion to county)

CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% # rename the county col
  inner_join(counties) %>%
  filter(region == "california") # some counties have same names in other states

head(CApop_county)

# Now plot CA population by county

ggplot() + 
  geom_polygon(data = CApop_county,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  coord_map() +
  theme_minimal() +
  scale_fill_gradient(trans = "log10") # use log10 to make it easier to interpret

# Add a layer of points
head (stars)

ggplot() + 
  geom_polygon(data = CApop_county,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars, 
             aes(x = long, # adds a point for all starfish sites
                 y = lat,
                 size = star_no)) +
  coord_map() +
  theme_minimal() +
  scale_fill_gradient(trans = "log10") + # use log10 to make it easier to interpret
  labs(size = "# stars/m2") # makes a better legend title

ggsave(here("Week_07","Output","CApop.pdf"))
