# Assignment Overview ----------------------------------------------------------------
# Assignment 2: Incarceration
# 
# (Acknowledgement: Vera Project https://github.com/vera-institute/incarceration-trends#documentation.)
#
#
# This figure shows a rough sketch of a "cumulative sum" time series. 
#
# A time series means data collected sequentially, usually at regular
# time intervals; for example, every 15 minutes, every 24 hours, or 
# some other appropriate unit of time. A "cumulative sum" means that
# at each point something changes and that the time series is the sum
# of all previous changes. 
#
# Each "x" in the figure represents a data point in a time series, 
# with 1 being the earliest recording and N being the most recent. 
# The y-axis, represents the cumulative sum of things being counted. 
# Sometimes there are large changes, sometimes there are no changes. 
#
# When data is structured this way, the counts strictly increase. 
# In other words, this relationship is, in theory, ALWAYS true:
#     f(t+1) >= f(t)
#
# (In practice, there might be occasions when f(t+1) < f(t); for 
# example, when data collection errors are corrected. Such 
# declines, however, would indicate a problem of some kind - and,
# ideally, one would want to update all necessary data points 
# so that declines are never present.)
#
# In a cumulative sum time series, the most recent point is also 
# the total count (objects made, tasks completed, events that 
# have occurred, etc.).

## 2.0 Introduction ----
# In this assignment you will work with three datasets, each  
# is a cumulative sum time series. The time interval is 
# one day (24 hours) and two things are being recorded during
# each daily interval: 
#
#     cases -  the number of COVID infections 
#     deaths - the number of deaths due to COVID. 
#
# Further, the case and death counts are collected at three 
# different geographic levels: 
#     national    The U.S. Nation as a whole. 
#     state       U.S. States (50 official U.S. States, plus District
#                 of Columbia, plus other territories).
#     county      Each state is divided into counties. There are around 3,000 
#                 counties in the U.S. And, FYI, Washington State has 
#                 39 counties.*
#
#                 *See County, United States (2022, January 25). In Wikipedia. 
#                          https://en.wikipedia.org/wiki/County_(United_States)
#
# These geographic levels differ in granularity. A state is made up of 
# counties and the nation as a whole is made of states.  Thus, in theory
# the county data should add up to the state data and the state data should
# add up to the national data. 
#
# The data for Assignment 2 come from the New York Times:  
#     The New York Times. (2022). Coronavirus (Covid-19) Data in the 
#          United States. Retrieved [January, 2022], 
#          from https://github.com/nytimes/covid-19-data."
#
# The charts that you might have seen in the New York Times 
# are created from this data: 
#      https://www.nytimes.com/interactive/2021/us/covid-cases.html 

# This will clear environment variables
rm(list = ls())
# Setting working directory
setwd("~/a3-smoham2/source")

# Load helper functions for data visualization
source("helperfunctions.R")

# 1.a Load the tidyverse and dyplr packages
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

# Loading data ------------------------------------------------------------
national_data <- load_incarceration_trends()
jail_jurisdiction_data <- load_incarceration_trends_jail_jurisdiction()

