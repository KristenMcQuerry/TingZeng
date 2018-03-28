##new branch with my edits

## part1: clone repository
## part2: create a new branch with edits to the R file

##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)
library(ggplot2)

## dataset for assignment
flights

## part3: 
# missing values represent cancelled flights
(cancelled <- flights %>% 
    filter(is.na(dep_delay),is.na(arr_delay)))

# number of flights cancelled per day
(cancelled_by_day <- flights %>% 
    mutate(date=paste(year,month,day,sep="-")) %>% 
    filter(is.na(dep_delay),is.na(arr_delay)) %>%
    group_by(year,month,day) %>% 
    summarize(num_cancelled=n()))
  
# plot the trend of the number of flights cancelled per day
plot(x=c(1:358),y=cancelled_by_day$num_cancelled,main = "Number of Cancelled Flights per Day",
     xlab = "Day",ylab = "No. of Cancelled Flights",type ="l",xaxt="n")
axis(1,at=1:358)
# Pattern: 5 peaks appear on the plot around 2/8-2/10,3/6-3/9,5/20-5/24,9/10-9/15,12/3-12/20.
# We can see obvious seasonal patterns.


# proportion of cancelled flights & average delay
(prop_avgdelay <-flights %>% 
  group_by(year,month,day) %>% 
  summarize(cancel_prop=100*sum(is.na(dep_delay)&is.na(arr_delay))/n(),avg_delay=mean(dep_delay,na.rm=TRUE)))

# cancellation proportion VS average departure delay
ggplot(prop_avgdelay,aes(x=cancel_prop,y=avg_delay))+
  geom_point()
# As we can see from the plot, there is a positive relationship between the cancellation proportion and average delay


## part 4:
# carrier with the highest delay proportion has the worst delays
(carrier_worst_delays <- flights %>%
    group_by(carrier) %>% 
    summarize(delay_prop=100*mean(dep_delay>0,na.rm=TRUE)) %>% 
    arrange(desc(delay_prop)))
# Carrier WN (Southwest Airlines Co.) has the highest delay proportion
# so it has the worst delay.


# Chanllenge: Can you disentangle the effects of bad airports vs bad carriers?
# Why or why not?
(carrier_airport <- flights %>% 
    group_by(carrier,dest) %>% 
    summarize(total=n(),delay_count=sum(dep_delay>0,na.rm=TRUE),delay_prop=100*mean(dep_delay>0)) %>% 
    arrange(desc(delay_prop)))

# plot the effects of bad aiports vs bad carriers
ggplot(carrier_airport,aes(x=carrier,y=dest))+
  geom_point(aes(size=delay_prop),alpha=1/3,na.rm=TRUE)

#As the plot shows, carrier DL(Delta), EV(ExpressJet Airlines), OO(SkyWest Airlines),UA(United Air Lines)
#have most of the big delay proportion. Whereas, there is no such pattern for airport.


## part 5
(delays_bf <- flights %>% 
    group_by(tailnum) %>% 
    summarise(num_flights = ifelse(min(which(dep_delay>60),na.rm=T)==Inf,
                                   n(),
                                   min(which(dep_delay>60),na.rm=T)-1)))
#if no dep_delay>60,return total num of flight;
#else return the position of the 1st flight with dep_delay>60 minus 1


## part 6
(worst_plane <- flights %>% 
    group_by(tailnum) %>% 
    summarize(delay_prop=100*mean(dep_delay>0,na.rm=TRUE)) %>% 
    arrange(desc(delay_prop)))
# the plane has worst on-time record means the plane has the highest delay proportion


## part 7
(avoid_delay <- flights %>% 
    group_by(hour) %>% 
    summarize(delay_prop=100*mean(dep_delay>0,na.rm=TRUE)) %>% 
    arrange(delay_prop))
# Break a day into 24 hours.To avoid delay as much as possible,
# then fly around the hour with the lowest delay proportion.
# We should fly around 6am or 7 am.


## part 8
(dest_total_delay <- flights %>% 
    group_by(dest) %>%
    summarize(total_delay=sum(dep_delay*(dep_delay>0),na.rm=TRUE)))
#only sum up the dep_delays that are positive to get the total delay for each destination

(dest_total_delay2 <- flights %>% 
    group_by(dest) %>%
    mutate(total_delay=sum(dep_delay*(dep_delay>0),na.rm=TRUE)) %>% 
    arrange(flight,dest))

(flight_dest_prop <- flights %>% 
    group_by(flight) %>% 
    mutate(flight_total_delay=sum(dep_delay*(dep_delay>0),na.rm=TRUE)) %>% 
    arrange(flight,dest))

(prop_total_delay <- flights %>% 
    mutate(prop_total_delay=flight_dest_prop$flight_total_delay/dest_total_delay2$total_delay) %>% 
    arrange(flight,dest))

## part 9
(dest_flight_speed <- flights %>% 
    mutate(speed=distance/air_time) %>% 
    arrange(dest,desc(speed)) %>% 
    select(dest,flight,tailnum,distance,air_time,speed))

(most_delay <- flights %>% 
    group_by(origin,dest) %>% 
    mutate(compare_time=air_time/min(arr_time-dep_time))%>%
    arrange(desc(compare_time)) %>% 
    select(flight,tailnum,compare_time,origin,dest))
