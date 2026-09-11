# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")
library(dplyr)


tuesdata <- tidytuesdayR::tt_load('2026-09-08')


cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index
