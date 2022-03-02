# helperfunctions.R
# -------------------------------------------------------------------------
# Functions to help visualize data for Assignment 3

# Note: It is okay to use these functions or borrow code for
# completing your assignment. 

# This file contains functions for computing rolling windows
# on New York Times datasets. For more on rolling windows see:
# https://dplyr.tidyverse.org/articles/window-functions.html

# install.packages("RcppRoll")
library("tidyverse")
library("RcppRoll")

# Data loading functions -----

# Get the most recent national incarceration data from the Vera Project
load_incarceration_trends <- function() {
  filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
  df <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

# Get the most recent jail jurisdiction trends for the united states
load_incarceration_trends_jail_jurisdiction <- function() {
  filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv"
  df <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

# Get the most recent county data from the New York Times
load_county_data <- function() {
  filename <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  df <- read.csv(filename)
  return(df)
}

# Data wrangling functions -----
# This function computes rolling averages for the case
# and death counts. A rolling average has a window size.
#
# NOTE: Daily counts must have been computed before calling
#       this function
moving_avg_counts <- function(national, window_size = 7) {
  
  # Check that daily counts have been computed and added to national
  if ("new_cases" %in% colnames(national) == FALSE ||
      "new_deaths" %in% colnames(national) == FALSE) {
    message("INFO-201: A2: Error: new_cases and new_deaths columns unassiged in national data frame.")
    stop("See assignment questions 2.k and 2.l.")
  }
  
  # If the window size is less than one, set it to 1. This is an
  # odd case because there is no average at all! 
  if (window_size < 1) {
    warning("INFO-201: A2: Setting window_size to 1.")
    window_size <- 1
  }
  
  # If the window size is larger than the data frame, set it
  # to the size of the data, which means we will compute
  # the average of the all rows - this is also an odd case!
  # (Later we will learn how "throw an error message" and stop.)
  if (window_size > nrow(national)) {
    warning("INFO-201: A2: Setting window_size to size of data frame.")
    window_size <- nrow(national)
  }
  
  # New cases - Use the function `roll_mean()` to compute the rolling
  # averages.  The rolling averages are put into a vector called
  # `avg_cases`.  Read the documentation on this function to learn about
  # these three parameters -- see `?roll_mean()`
  avg_cases <- round(
    roll_mean(
      national$new_cases,
      n = window_size,
      na.rm = TRUE
    ),
    1
  )
  
  # Do the same for new deaths
  avg_deaths <- round(
    roll_mean(
      national$new_deaths,
      n = window_size,
      na.rm = TRUE
    ),
    1
  )
  
  # If you study how `roll_mean()` works, you will discover that
  # it produces a vector with FEWER elements than its input vector.
  # So this code "pads" the beginning of the vector.  We want
  # the rolling average vectors to have EXACTLY the same number
  # of elements so that we can put them into the `national`
  # dataset.
  #    1. The function `rep()` is like `seq()`
  #        See 7.1 of book and R documentation with `?rep`
  #    2. The function `append()` puts two vectors together
  #
  pad_vector <- rep(0, window_size - 1)
  avg_cases_padded <- append(pad_vector, avg_cases)
  avg_deaths_padded <- append(pad_vector, avg_deaths)
  
  # Now, assign the rolling averages to the `natioal` data frame
  national$rolling_avg_cases <- avg_cases_padded
  national$rolling_avg_deaths <- avg_deaths_padded
  return(national)
}

# Plotting charts ----

# This function creates a chart of the rolling averages of
# COVID-19 cases in the United States
plot_moving_avg_cases <- function(national, window_size) {
  
  # We initialize the labels of the chart. Each of these labels
  # will be placed on the chart in standard positions
  plot_x_axis <- ""
  plot_y_axis <- ""
  plot_title <- "United States COVID-19 Cases"
  plot_subtitle <- paste0(window_size, "-day Running Average (January 2020-22)")
  plot_alt <- "Daily US COVID Cases from January 2020-22. Data from New York Times."
  plot_caption <- paste0(
    "Educational exercise (INFO-201: Winter 2022: UW Information School).\n Data from:",
    "The New York Times. (2022). ",
    "Coronavirus (Covid-19)\n",
    "Data in the United States. Retrieved [January, 2022],\n",
    "https://github.com/nytimes/covid-19-data."
  )
  
  # This is the code for plotting the chart
  p <- ggplot(
    
    # This is the data frame, containing data that we want to plot
    national,
    
    # This is how we map columns in the `national` data frame to coordinates on the plot
    # Notes:
    #   (1) We need to turn the `date` string into a `date` data type
    #   (2) Note that x is the date dimension and y the counts
    
    aes(x = as.Date(date), y = rolling_avg_cases)
  ) +
    
    # This is how to draw the curve. Size is the thickness of the line.
    # For standard names of colors, see: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    geom_line(
      color = "red",
      size = 0.75
    ) +
    
    # This is how to fill in under the curve
    geom_area(fill = "pink") +
    
    # %B, \n, and %Y are special characters for formatting dates
    # They refer to Month, newline (aka line break or <br>), and Year respectively
    scale_x_date(date_labels = "%B\n%Y") +
    
    # Show the y-scale with commas - e.g., 200,000 (instade of 2-E06)
    scale_y_continuous(labels = scales::comma) +
    
    # The strings for the chart are initialized above. We draw them on the plot
    # as follows.
    labs(
      x = plot_x_axis,
      y = plot_y_axis,
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption,
      alt = plot_alt
    )
}

# COVID-19 deaths in the United States - Cumulative graph
# This plot follows the same structure as plot_moving_avg_cases but shows a 
# different variable, namely `deaths`, which holds a cumulative sum of
# deaths due to COVID.
plot_cumulative_death_counts <- function(national) {
  
  # We initialize the labels of the chart. Each of these labels
  # will be placed on the chart in standard positions
  plot_x_axis <- ""
  plot_y_axis <- ""
  plot_title <- "United States COVID-19 Cases"
  plot_subtitle <- paste0("Cumulative Death Counts (January 2020-22)")
  plot_alt <- "Daily US COVID Cases from January 2020-22. Data from New York Times."
  plot_caption <- paste0(
    "Educational exercise (INFO-201: Winter 2022: UW Information School).\n Data from:",
    "The New York Times. (2022). ",
    "Coronavirus (Covid-19)\n",
    "Data in the United States. Retrieved [January, 2022],\n",
    "https://github.com/nytimes/covid-19-data."
  )
  p <- ggplot(national, aes(x = as.Date(date), y = deaths)) +
    geom_line(
      color = "red",
      size = 0.75
    ) +
    geom_area(fill = "pink") +
    scale_x_date(date_labels = "%B\n%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = plot_x_axis,
      y = plot_y_axis,
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption,
      alt = plot_alt
    )
}


# Testing functions -----

test_scripts <- function() {
  
  # Some basic tests of the function `moving_avg_counts()`
  # Check for small windows (extreme cases)
  test0 <- moving_avg_counts(national, 0)
  test1 <- moving_avg_counts(national, 1)
  
  # Check for the expected cases
  test_expected_window <- moving_avg_counts(national, 3)
  
  # Check for extreme large cases
  test_big_window <- moving_avg_counts(national, nrow(national) + 1)
  
  View(test0)
  View(test1)
  View(test_expected_window)
  View(test_big_window)
  
  # Test  moving averages 
  window_size <- 7
  national <- moving_avg_counts(national, window_size)
  p <- plot_moving_avg_cases(national, window_size)
  p
  
  # Test cumulative sum plot
  p <- plot_cumulative_death_counts(national)
  p
  
}
