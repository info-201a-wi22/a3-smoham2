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


# Data wrangling functions -----
# This function computes rolling averages for the prison population counts. A rolling average has a window size.
#
# NOTE: Yearly counts must have been computed before calling
#       this function
moving_avg_counts <- function(national_data, window_size = 7) {
  
  # Check that daily counts have been computed and added to national
  if ("new_black_jail_pop" %in% colnames(national_data) == FALSE ||
      "new_white_jail_pop" %in% colnames(national_data) == FALSE) {
    message("Error: new_black_jail_pop and new_white_jail_pop columns unassiged in national data frame.")
    stop()
  }
  
  # If the window size is less than one, set it to 1. This is an
  # odd case because there is no average at all! 
  if (window_size < 1) {
    warning("Setting window_size to 1.")
    window_size <- 1
  }
  
  # If the window size is larger than the data frame, set it
  # to the size of the data, which means we will compute
  # the average of the all rows - this is also an odd case!
  # (Later we will learn how "throw an error message" and stop.)
  if (window_size > nrow(national_data)) {
    warning("Setting window_size to size of data frame.")
    window_size <- nrow(national_data)
  }
  
  # new_black_jail_pop - Uses the function `roll_mean()` to compute the rolling
  # averages.  The rolling averages are put into a vector called
  # `avg_black_jail_pop `.
  avg_black_jail_pop <- round(
    roll_mean(
      national_data$new_black_jail_pop,
      n = window_size,
      na.rm = TRUE
    ),
    1
  )
  
  # Do the same for new deaths
  avg_white_jail_pop <- round(
    roll_mean(
      national_data$new_white_jail_pop,
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
  avg_black_padded <- append(pad_vector, avg_black_jail_pop)
  avg_white_padded <- append(pad_vector, avg_white_jail_pop)
  
  # Now, assign the rolling averages to the `national_data` data frame
  national_data$rolling_avg_black_pop <- avg_black_padded
  national_data$rolling_avg_white_pop <- avg_white_padded
  return(national_data)
}

# Plotting charts ----

# This function creates a chart of the rolling averages of
# black and white jail populations
plot_moving_avg_jail_pop <- function(national_data, window_size) {
  
  # We initialize the labels of the chart. Each of these labels
  # will be placed on the chart in standard positions
  plot_x_axis <- "Year"
  plot_y_axis <- "Jail Population"
  plot_title <- "United States Black Jail Population Growth"
  plot_subtitle <- paste0(window_size, "- Year Avg Cumulative Black Jail Population Counts (1970-2018)")
  plot_alt <- "Incarceration Trends from 1970-2018 Data from The Vera Project."
  plot_caption <- paste0(
    "UW INFO 201 Assignment 3 .\n Data from:",
    "The Vera Project. (2022). ",
    "Incarceration Trends\n",
    "Data in the United States. Retrieved [January, 2022],\n",
    "https://github.com/vera-institute/incarceration-trends#documentation."
  )
  
  # This is the code for plotting the chart
  p <- ggplot(
    
    # This is the data frame, containing data that we want to plot
    national_data,
    
    # This is how we map columns in the `national` data frame to coordinates on the plot
    # Notes:
    #   (1) We need to turn the `date` string into a `date` data type
    #   (2) Note that x is the date dimension and y the counts
    
    aes(x = as.Date(ISOdate(year, 1, 1)), y = rolling_avg_black_pop)
  ) +
    
    # This is how to draw the curve. Size is the thickness of the line.
    # For standard names of colors, see: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    geom_line(
      color = "black",
      size = 0.75
    ) +
    
    # This is how to fill in under the curve
    geom_area(fill = "grey") +
    
    # %B, \n, and %Y are special characters for formatting dates
    # They refer to Month, newline (aka line break or <br>), and Year respectively
    scale_x_date(date_labels = "%Y") +
    
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
