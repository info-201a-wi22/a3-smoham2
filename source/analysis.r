# Assignment Overview ----------------------------------------------------------------
# Assignment 3: Incarceration
# 
# (Acknowledgement: Vera Project https://github.com/vera-institute/incarceration-trends#documentation.)
#
# Introduction
#
# As you may know black people in this country are incarcerated at very high rates. The Vera Project, is volunteer run research project
# that seeks to track, report, and most importantly visualize in a human readable format (e.g using graphs) the ways in which 
# black people are incarcerated in this country. In this assignment I will seek the answer to the following questions about
# the incarceration rate of black people using the Vera Projects data available here https://github.com/vera-institute/incarceration-trends#documentation.
# 
# 1. What county (County, State) has the most black people in jail as of the most recent year?
#   a. This will be stored in county_highest_black_rate
# 2. What is the percentage of total jail population vs the total number of black people jailed across all counties as of the most recent year?
#   a. This will be stored in black_jail_ratio
# 3. What is the percentage of total jail population vs the total number of white people jailed across all counties as of the most recent year?
#   a. This will be stored in white_jail_ratio
# 4. In the last 5 years (since the Trump Presidency) have the national rates of incarceration for black people gone up vs the last 10 years?
#   a. This will be stored in last_5_years_black_rate
# 5. What state has the lowest incarceration rate for black people
#   a. This will be stored in state_lowest_black_rate
# 6. What state has the highest incarceration rate for black people
#   a. This will be stored in state_highest_black_rate

# This will clear environment variables
rm(list = ls())
# Setting working directory
setwd("~/a3-smoham2/source")

# Load helper functions for data visualization
source("helperfunctions.R")

# Load the tidyverse and dyplr packages
install.packages("dplyr")
library(dplyr)
old.packages()
# Loading data ------------------------------------------------------------
national_data <- load_incarceration_trends()
jail_jurisdiction_data <- load_incarceration_trends_jail_jurisdiction()

# Add a location column to the data set to better understand what counties are related to each state
national_data <- national_data %>%
  mutate(location = paste(county_name, state, sep = ", "))

# 1. What county (County, State) has the most black people in jail as of the most recent year?
county_highest_black_rate <- national_data %>% 
  filter(year == max(year)) %>% 
  drop_na(black_jail_pop) %>% 
  filter(black_jail_pop == max(black_jail_pop)) %>% 
  pull(location)

# 2. What is the percentage of total population vs the total number of black people jailed across all counties as of the most recent year?
total_black_jail_pop <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(black_jail_pop) %>% 
  pull()

total_jail_population <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(total_jail_pop) %>% 
  pull()

black_jail_percentage <- (total_black_jail_pop/total_jail_population) * 100

# 3. What is the percentage of total jail population vs the total number of white people jailed across all counties as of the most recent year?
total_white_jail_pop <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(white_jail_pop) %>% 
  pull()

white_jail_percentage <- (total_white_jail_pop/total_jail_population) * 100 

# Calculate total number of black people in jail every year
national_black_jail_pop <- national_data %>% 
  drop_na(black_jail_pop) %>% 
  group_by(year) %>% 
  summarise(total_black_jail_pop = sum(black_jail_pop))

# Calculate total number of white people in jail every year
national_white_jail_pop <- national_data %>% 
  drop_na(white_jail_pop) %>% 
  group_by(year) %>% 
  summarise(total_white_jail_pop = sum(white_jail_pop))

national_black_jail_pop <- national_black_jail_pop %>% 
  mutate(new_black_jail_pop = total_black_jail_pop - lag(total_black_jail_pop))

national_white_jail_pop <- national_black_jail_pop %>% 
  mutate(new_white_jail_pop = total_white_jail_pop - lag(total_white_jail_pop))

window_size <- 7
national_black_jail_pop <- moving_avg_counts(national_black_jail_pop, window_size)
moving_avg_black_plot <- plot_moving_avg_jail_pop(national_black_jail_pop, window_size)
print(moving_avg_black_plot)
