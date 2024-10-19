# Problem 2 - get_temp()
# write a function get_temp()

library(tidyverse)
library(dplyr)

nnmaps <- readr::read_delim("chicago-nmmaps.csv") 

#' Get temperature with date and data and etc. 
#'
#' You can obtain the average temperature for a given month and year from your data
#' You have the option to change the mean to another function  
#'
#' @param month_input either a numeric 1-12 or a string
#' @param year_input a numeric
#' @param data the data set to obtain data from
#' @param celsius logical, default = FALSE
#' @param average_fn a function, default = mean
#'
#' @return a numeric vector of length 1.
get_temp <- function(month_input, year_input, data, celsius = FALSE, average_fn = mean) {
  # check whether month_input is either a numeric 1-12 or a string for months
  if (is.character(month_input)) {
    month_input <- month_input %>% substr(1,3) %>% tolower
    month_represent <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    if(!month_input %in% month_represent) {
      stop("month_input must be a character representing a month")
    }
  }
  else if (is.numeric(month_input)) {
    if(!month_input %in% 1:12) {
    stop("month_input must be a numeric from 1 to 12")
    }
  }
  else {
    stop("month_input must be a numeric from 1 to 12, or a character representing a month")
  }
  
  # check whether year_input is a peirod whithin a data
  if (is.numeric(year_input)) {
    first_year <- data %>% 
      select(year) %>% 
      filter(year == min(year)) %>%
      distinct()  # first year of a data
    last_year <- data %>% 
      select(year) %>% 
      filter(year == max(year)) %>%
      distinct() # last year of a data
    first_year <- as.numeric(first_year)
    last_year <- as.numeric(last_year)
    if(!year_input %in% first_year:last_year) {
      stop("year_input must be a period whithin a data")
    }
  }
  else {
    stop("year_input must be a numeric")
  }
  
  # calculate an average temperature or a temperature using a given function
  if (is.character(month_input)) {
      data %>%
      group_by(month, year) %>%
      summarize(avg_temp = average_fn(temp)) %>%
      filter(year == year_input, tolower(month) == tolower(month_input)) %>%
      mutate(avg_temp = ifelse(celsius, (avg_temp - 32) * 5/9, avg_temp)) %>%
      ungroup
  }
  else if (is.numeric(month_input)) {
    data %>%
      group_by(month_numeric, year) %>%
      summarize(avg_temp = average_fn(temp)) %>%
      filter(year == year_input, month_numeric == tolower(month_input)) %>%
      mutate(avg_temp = ifelse(celsius, (avg_temp - 32) * 5/9, avg_temp)) %>%
      ungroup
  } 
}

get_temp("Apr", 1999, data = nnmaps)
get_temp("Apr", 1999, data = nnmaps, celsius = TRUE)
get_temp(11, 1999, data = nnmaps, celsius = TRUE, average_fn = median)
get_temp(13, 1998, data = nnmaps)
get_temp(2, 2005, data = nnmaps)
get_temp("November", 1999, data =nnmaps, celsius = TRUE,
         average_fn = function(x) {
           x %>% sort -> x
           x[2:(length(x) - 1)] %>% mean %>% return
         })
