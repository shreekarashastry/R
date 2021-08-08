library(tidyverse)

bike = read_csv('bikeshare.csv')

# A) Lets calculate average bike rentals by the hours of the day and plot the line graph

hours = seq(0,23,1)
bike_rental_avg = rep(0,24)

for (i in hours){
  bike_rentals_each_hour = bike %>% filter(hr == i)
  bike_rental_avg[i+1] = sum(bike_rentals_each_hour$cnt)/sum(bike_rentals_each_hour$hr==i)
}

df = data.frame(bike_rental_avg, hours)

# Average number of bike rentals for the whole day
average_bike_rentals = mean(bike_rental_avg)

ggplot(df)+
  geom_line(aes(x=hours,y=bike_rental_avg))+ 
  scale_x_continuous("Hours of the day (0 = midnight)", labels = as.character(hours), breaks = hours) +
  labs(x="Hours of the day",
       y="Average Bike rentals",
       title="Bike share rentals in Washington D.C (2011 and 2012)",
       caption="Starting from midnight Average bike rentals decreases till 4am and it picks up and reaches 
                a morning maximum at 8am then it decreases during lunch hours and 
                reaches the daily peak at 5pm and decreases for the rest of the day")+
        theme(plot.caption = element_text(hjust = 0.5))

# B) faceted line plots based on working day

bike_working = bike %>%
                filter(workingday == 1)
bike_nonworking = bike %>%
                filter(workingday == 0)

bike_rental_avg_workingday = rep(0,24)
bike_rental_avg_nonworkingday = rep(0,24)

for (i in hours){
  bike_rentals_each_hour_working = bike_working %>% filter(hr == i)
  bike_rental_avg_workingday[i+1]=sum(bike_rentals_each_hour_working$cnt)/sum(bike_rentals_each_hour_working$hr == i)
  
  bike_rentals_each_hour_nonworking = bike_nonworking %>% filter(hr == i)
  bike_rental_avg_nonworkingday[i+1]=sum(bike_rentals_each_hour_nonworking$cnt)/sum(bike_rentals_each_hour_nonworking$hr == i)
}

working = rep(1, 24)
nonworking = rep(0, 24)

df_b = data.frame(bike_rent_avg = bike_rental_avg_workingday, working, hours)
df_c = data.frame(bike_rent_avg = bike_rental_avg_nonworkingday, working = nonworking, hours)

df_final = rbind(df_b, df_c)

ggplot(df_final)+
  geom_line(aes(x=hours,y=bike_rent_avg))+
  facet_wrap(~working)+
  scale_x_continuous("Hours of the day (0 = midnight)", labels = as.character(hours), breaks = hours) +
  labs(x="Hours of the day",
       y="Average Bike rentals",
       title="Bike share rentals in Washington D.C (2011 and 2012) during non working and working day",
       subtitle="0-non working day 1-workingday",
       caption="As expected during non working days there is very limited bike rentals in the 
                morning till 6am and then it increases and reaches a peak at 1pm and decreases for the rest of the day
                , whereas during working days the morning peak happens at 8am and the daily peak happens at 5pm since its the time 
                  when the people go to the office and leave from the office respectively, after which it decreases for the rest of the day")+
  theme(strip.background = element_blank(), strip.placement = "outside", plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
# C) faceting based on weather conditions at 8am

bike_8am = bike %>%
            filter(hr == 8)

ggplot(bike_8am)+
  geom_col(aes(x=weathersit,y=cnt/dim(bike_8am)[1],color=weathersit))+
    facet_wrap(~workingday)+
    labs(x="Weather situation",
     y="Average Bike rentals",
     title="Bike share rentals in Washington D.C (2011 and 2012) during non working and working day",
     subtitle="0-non working day 1-workingday",
     caption="1: Clear, Few clouds, Partly cloudy, Partly cloudy.
              2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist.
              3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds.
              
              Clear days have the highest bike rentals, people enjoy biking outside on clear days")+
      theme(strip.background = element_blank(), strip.placement = "outside",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5))

