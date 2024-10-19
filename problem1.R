install.packages("nycflights13")

# Problem 1 - tidyverse
# a. Generate a table (which can just be a nicely printed tibble) reporting the mean and median departure/arrival delay per airport
# a-1. departure delay table 

library(nycflights13)
library(tidyverse)
library(dplyr)

flights_departure <- flights %>%
  left_join(airports, by = c("origin" = "faa")) # table join for the airport infomation

departure_delay <- flights_departure %>%
  group_by(name) %>%
  summarize(number_flights = n(),
            mean_depature_delay = mean(dep_delay, na.rm = TRUE),
            median_depature_delay = median(dep_delay, na.rm = TRUE)) %>%
  filter(number_flights >= 10) %>%
  arrange(desc(mean_depature_delay)) %>%
  ungroup # produce the mean and median data of arrival delay for each airport

departure_delay_table <- departure_delay %>% 
  select(!number_flights)

departure_delay_table

# a-2. arrival delay table 

flights_arrival <- flights %>%
  left_join(airports, by = c("dest" = "faa")) # table join for the airport infomation

arrival_delay <- flights_arrival %>%
  group_by(dest, name) %>%
  summarize(number_flights = n(),
            mean_arrival_delay = mean(arr_delay, na.rm = TRUE),
            median_arrival_delay = median(arr_delay, na.rm = TRUE)) %>%
  filter(number_flights >= 10) %>%
  arrange(desc(mean_arrival_delay)) %>%
  ungroup  # produce the mean and median data of arrival delay for each airport


arrival_delay_table <- arrival_delay %>% 
  select(!(number_flights | dest)) # arrival delay table only with mean and median

arrival_delay_table <- drop_na(arrival_delay_table) # Remove data without airport names for the destination codes ; "BQN", "SJU", "STT" and "PSE"

print(arrival_delay_table, n = Inf) 

# b. flights with the fastest average speed model

fastest_model <- planes %>% 
  select(tailnum, 
         model,
         speed) %>%
  filter(speed == max(speed, na.rm = TRUE)) # find the fastest model
  
fastest_flights <- flights %>%
  select(flight, tailnum) %>%
  inner_join(fastest_model, by = "tailnum") # find the flight with the fastest model

fastest_flights_table <- fastest_flights %>%
  group_by(model) %>% 
  reframe(average_speed = speed, 
          number_of_flight = n()) %>% 
  distinct() %>%
  ungroup # summary table for the fastest flights

fastest_flights_table
