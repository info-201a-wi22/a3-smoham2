# Assignment Overview ----------------------------------------------------------------
# Assignment 3: Incarceration
# 
# (Acknowledgement: Vera Project https://github.com/vera-institute/incarceration-trends#documentation.)
#

# This will clear environment variables
rm(list = ls())
# Setting working directory
setwd("~/a3-smoham2/source")

# Load helper functions for data visualization
source("helperfunctions.R")

# Load packages
library("tidyverse")
library("maps")
library("dplyr")

# Loading data ------------------------------------------------------------
national_data <- load_incarceration_trends()
jail_jurisdiction_data <- load_incarceration_trends_jail_jurisdiction()

# Add a location column to the data set to better understand what counties are related to each state
national_data <- national_data %>%
  mutate(location = paste(county_name, state, sep = ", "))

# 1. What is the total number of black people incarcerated in jail as of the most recent year?
# To calculate this first we must filter the data to the most recent year.
# Then we tally the total number of black people in local jails.
total_black_jail_pop <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(black_jail_pop) %>% 
  pull()

# 2. What is the percentage of total population vs the total number of black people jailed across all counties as of the most recent year?

# Calculates total number of people in jail as of the most recent year
total_jail_population <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(total_jail_pop) %>% 
  pull()

# Divide the previously calculated black jail population by the new total jail population
black_jail_percentage <- (total_black_jail_pop/total_jail_population) * 100

# 3. What is the percentage of total jail population vs the total number of white people jailed across all counties as of the most recent year?
total_white_jail_pop <- national_data %>% 
  filter(year == max(year)) %>% 
  tally(white_jail_pop) %>% 
  pull()

# Divide the new total jail population by the previously calculated total jail population
white_jail_percentage <- (total_white_jail_pop/total_jail_population) * 100 


# 4. What is the census division with the lowest percentage of black people in jail
division_lowest_black_rate <- national_data %>% 
  # First we filter to the most recent year
  filter(year == max(year)) %>% 
  # Then we group by census division
  group_by(division) %>% 
  drop_na(total_jail_pop) %>% 
  drop_na(black_jail_pop) %>% 
  # Then we create two new columns to track the total amount people in jail and black people in jail
  summarise(jail_pop = sum(total_jail_pop), total_black_jail_pop = sum(black_jail_pop)) %>% 
  # We add a new column to track the percentage
  mutate(black_ratio = (total_black_jail_pop/jail_pop) * 100) %>% 
  # Then we filter to the lowest
  filter(black_ratio == min(black_ratio)) %>% 
  pull(division)
  
  
# 5. What is the census division with the highest percentage of black people in jail
division_highest_black_rate  <- national_data %>% 
  # First we filter to the most recent year
  filter(year == max(year)) %>% 
  # Then we group by census division
  group_by(division) %>% 
  drop_na(total_jail_pop) %>% 
  drop_na(black_jail_pop) %>% 
  # Then we create two new columns to track the total amount people in jail and black people in jail
  summarise(jail_pop = sum(total_jail_pop), total_black_jail_pop = sum(black_jail_pop)) %>% 
  # We add a new column to track the percentage
  mutate(black_ratio = (total_black_jail_pop/jail_pop) * 100) %>% 
  # Then we filter to the lowest
  filter(black_ratio == max(black_ratio)) %>% 
  pull(division)

# Calculate total number of white and black people in jail every year
national_jail_pop <- national_data %>% 
  drop_na(white_jail_pop) %>% 
  drop_na(black_jail_pop) %>% 
  group_by(year) %>% 
  summarise(total_black_jail_pop = sum(black_jail_pop), total_white_jail_pop = sum(white_jail_pop))

# Adds a new column that tracks how many more black people were in jail every year
national_jail_pop <- national_jail_pop %>% 
  mutate(new_black_jail_pop = total_black_jail_pop - lag(total_black_jail_pop))

# Adds a new column that tracks how many more white people were in jail every year
national_jail_pop <- national_jail_pop %>% 
  mutate(new_white_jail_pop = total_white_jail_pop - lag(total_white_jail_pop))

# Shows black jail population growth over time 
window_size <- 10
national_jail_pop <- moving_avg_counts(national_jail_pop, window_size)
moving_avg_black_plot <- plot_moving_avg_jail_pop_by_race(national_jail_pop, window_size, "Black", national_jail_pop$rolling_avg_black_pop)
print(moving_avg_black_plot)

# Shows white and black jail population growth over time 
window_size <- 10
national_jail_pop <- moving_avg_counts(national_jail_pop, window_size)
moving_avg_white_and_black_plot <- plot_moving_avg_jail_pop(national_jail_pop, window_size)
print(moving_avg_white_and_black_plot)


# Map
# Get county fips data
data(county.fips)
# Get county data and add new column to match the polyname 
counties <- map_data("county") %>% 
  mutate(polyname = paste(region, subregion, sep = ","))
# left join county data with county fips data
counties <- left_join(counties, county.fips, by = "polyname")

map_data <- left_join(national_data, counties, by = "fips") %>%  
  filter(year == max(year))

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

black_jail_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white", # show state outlines
    size = .1        # thinly stroked
    ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_distiller(palette = "RdPu") +
  labs(fill = "Black Jail Population") +
  blank_theme +
  labs(
    x = "",
    y = "",
    title = "Current Black Jail Populations Across Counties",
    subtitle = "",
    caption = "",
    alt = ""
  )

