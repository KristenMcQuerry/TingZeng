##my edit to the file
##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights

# part a: arr_delay>=2
flights %>% 
  filter(arr_delay>=2)

# part b: flew to Houston, IAH or HOU
flights %>% 
  filter(dest==c("IAH", "HOU"))

# part c: operated by UA,AA,DL
flights %>% 
  filter(carrier %in% c("UA","AA","DL"))

# part d: departed in summer
flights %>% 
  filter(month %in% c(7,8,9))

# part e: arr_delay>2,but didn't leave late
flights %>% 
  filter(arr_delay>2,dep_delay<=0)

# part f: were delayed at least 1 hr but make up 30 mins in flight
flights %>% 
  filter(dep_delay>=1,dep_delay-arr_delay>=30)

# part g: departed b/t midnight and 6 a.m.(inclusive)
flights %>% 
  filter((dep_time<=600 | dep_time==2400))

# part h: sort,find the most delayed flights.
flights %>% 
  arrange(desc(dep_delay))

# Find the flights that left earliest.
flights %>% 
  arrange(dep_delay)

# part i:sort to find the fastest flights
 flights %>% 
   mutate(speed=arr)


# part j: which flights traveled the longest?
flights %>% 
  arrange(desc(distance))