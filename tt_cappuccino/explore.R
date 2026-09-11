# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-09-08')
## OR
tuesdata <- tidytuesdayR::tt_load(2026, week = 36)

cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index