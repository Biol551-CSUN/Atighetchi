library(tidyverse)
library(ggthemes)
library(palmerpenguins)
library(here)

tibble::tribble(
  ~species,     ~island, ~bill_length_mm, ~bill_depth_mm, ~flipper_length_mm, ~body_mass_g,     ~sex, ~year,
  "Adelie", "Torgersen",            39.1,           18.7,               181L,        3750L,   "male", 2007L,
  "Adelie", "Torgersen",            39.5,           17.4,               186L,        3800L, "female", 2007L,
  "Adelie", "Torgersen",            40.3,             18,               195L,        3250L, "female", 2007L,
  "Adelie", "Torgersen",              NA,             NA,                 NA,           NA,       NA, 2007L,
  "Adelie", "Torgersen",            36.7,           19.3,               193L,        3450L, "female", 2007L,
  "Adelie", "Torgersen",            39.3,           20.6,               190L,        3650L,   "male", 2007L,
  "Adelie", "Torgersen",            38.9,           17.8,               181L,        3625L, "female", 2007L,
  "Adelie", "Torgersen",            39.2,           19.6,               195L,        4675L,   "male", 2007L,
  "Adelie", "Torgersen",            34.1,           18.1,               193L,        3475L,       NA, 2007L,
  "Adelie", "Torgersen",              42,           20.2,               190L,        4250L,       NA, 2007L,
  "Adelie", "Torgersen",            37.8,           17.1,               186L,        3300L,       NA, 2007L,
  "Adelie", "Torgersen",            37.8,           17.3,               180L,        3700L,       NA, 2007L,
  "Adelie", "Torgersen",            41.1,           17.6,               182L,        3200L, "female", 2007L,
  "Adelie", "Torgersen",            38.6,           21.2,               191L,        3800L,   "male", 2007L,
  "Adelie", "Torgersen",            34.6,           21.1,               198L,        4400L,   "male", 2007L,
  "Adelie", "Torgersen",            36.6,           17.8,               185L,        3700L, "female", 2007L,
  "Adelie", "Torgersen",            38.7,             19,               195L,        3450L, "female", 2007L,
  "Adelie", "Torgersen",            42.5,           20.7,               197L,        4500L,   "male", 2007L,
  "Adelie", "Torgersen",            34.4,           18.4,               184L,        3325L, "female", 2007L,
  "Adelie", "Torgersen",              46,           21.5,               194L,        4200L,   "male", 2007L
  )


torgersen_penguins <- penguins %>%
  filter(island == "Torgersen")

plot1 <- ggplot(data=torgersen_penguins,
                x = bill_depth_mm, y = bill_length_mm, group = species, color = species) + 
  geom_point()+ 
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)"
  ) +
  theme_economist() +
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill = "beige", color = "blue"))

