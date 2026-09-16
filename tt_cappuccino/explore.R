# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")
library(dplyr)


tuesdata <- tidytuesdayR::tt_load('2026-09-08')


cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index


# What can be visulaized?
# Need a coffe jar viz to show the continents

#1.Divide the countries into continents(Asia,Africa,europe etc)
  #a)Breakup of cafes in urban ,rural,suburban by continent ans their show the difference in wages and price