### Lecture 8a: Advanced Plotting####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-03-28 ####################
#### Load Libraries ######

library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)

### Functions ######

### USING PATCHWORK #######

# plot 1
p1 <- penguins %>%
  ggplot(aes(x = body_mass_g,
             y = bill_length_mm,
             color = species)) +
  geom_point()
p1

# plot 2 
p2<-penguins %>%
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)
p2

## Bring the plots together using simple operations

p1 + p2

## Group the legends together

p1 + p2 +
  plot_layout(guides = 'collect')

## Add labels (A,B)
p1 + p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') 

## Put one plot on top of another

p1/p2 +
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')

######### USING ggrepel ############################

## Create a plot
view(mtcars)

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() + # creates a text label
  geom_point(color = 'red')

## Repel the labels

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text_repel() + # repel them
  geom_point(color = 'red')

## Use the label function

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_label_repel() + # repel them
  geom_point(color = 'red')

####### USING gganimate ################################

## Create a static plot

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()

## Animate the plot by year

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )

## Change the ease aesthetic/change the transition

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )+
  ease_aes("bounce-in-out")

## Add a transition title

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )+
  ease_aes("sine-in-out") +
  ggtitle('Year: {closest_state}')

## Save it as a GIF

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )+
  ease_aes("sine-in-out") +
  ggtitle('Year: {closest_state}') +
  anim_save(here("Week_08","Output","mypengiungif.gif"))

############ USING magick ########################

## Read in an image of a penguin

penguin<-image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")
penguin

## To put the penguin picture into the plot first save it as an image

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() 

ggsave(here(here("Week_08", "Output", "penguinplot.png")))

penplot<-image_read(here("Week_08","Output","penguinplot.png"))
out <- image_composite(penplot, penguin, offset = "+70+30")
out