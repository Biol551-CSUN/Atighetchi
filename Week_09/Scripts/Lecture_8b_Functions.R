### Lecture 8b: Functional Programming ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-04-04 ####################
#### Load Libraries ######
library(tidyverse)
library(palmerpenguins)
library(PNWColors)
### Functions #################################

# lets create a random df using rnorm
df <- tibble::tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df)

# lets rescale every column individually
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)))

# now we have to do that for every row without making a mistake
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

# THIS IS IMPRACTICAL AND CAN BE REPLACED WITH FUNCTIONS
rescale01 <- function(x) {
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
}
df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

# Lets make a function to convert degrees to fahrenheit
fahren_to_cel <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

fahren_to_cel(32)

fahren_to_cel(212)

# Write a function that converts celcius to Kelvin
cel_to_kelvin <- function(temp_C) {
  temp_K <- (temp_C + 273.15)
  return(temp_K)
}
cel_to_kelvin(20)

# Making Plots into a function
library(palmerpenguins)
library(PNWColors) # for the PNW color palette 
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
  theme_bw()

# Lets turn this into a function (Lets make it broad so that we can plug it in and out)
myplot<-function(data, x, y){ 
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+ # curly-curly brackets look within the dataframe for x and y
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
} # curly-curly brackets look within dataframes 
myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)

# Adding Defaults so that you dont have to always assign a value to data (here we assign data = penguins as the default)
myplot<-function(data = penguins, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
myplot(x = body_mass_g, y = flipper_length_mm)
# You can also layer the plot by adding labels and text and edit the plot
myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

# You can add flexibility using if-else statements
a <- 4
b <- 5
if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2
  f <- 10
}
f

# Now we can incorporate an if-else statment to make lines show up if it meets some sort of parameters
myplot<-function(data = penguins, x, y, wewantlines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(wewantlines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}
# this plot should be lines
myplot(x = body_mass_g, y = flipper_length_mm)
# this plot shouldn't have lines
myplot(x = body_mass_g, y = flipper_length_mm, wewantlines = FALSE)





