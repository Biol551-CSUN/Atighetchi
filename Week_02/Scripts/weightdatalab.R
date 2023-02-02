##This is my first script. I am learning how to import data 
## Created by: Cameron Atighetchi
# Created on: 2023-02-02
#############################################################################
### Load libraries ##########################################################
library (tidyverse)
library (here)
## Read in Data ######
Weightdata <- read_csv (here("Week_02", "Data", "weightdata.csv"))
#### Data Analysis #########################################################
head (Weightdata)
tail (Weightdata)
View (Weightdata)