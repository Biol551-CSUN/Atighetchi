### Penguin Lab Plot Data ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-09 ####################
#### Load Libraries ######
library (palmerpenguins)
library (tidyverse)
library (here)
library (beyonce)
library (ggthemes)
### Load Data ##############
glimpse (penguins)
summary (penguins)

###### Data Analysis/Plotting ##################

plot2 <- ggplot(data=penguins, 
       mapping = aes(x = sex,
                     y = body_mass_g,
                    fill = sex)) + 
  geom_boxplot() + 
  labs(x = "Sex", y = "Body Mass (g)", title = "Quantifying Total Body Mass Per Sex of Penguins") + 
  facet_wrap (~species) +
  scale_fill_manual(values = beyonce_palette(18)) + 
  theme_stata() + 
  theme(axis.title = element_text (size = 15), panel.background = element_rect (fill = "white"))
                                                                                                                                            
                                                                          