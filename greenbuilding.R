library(tidyverse)

# construction cost is $100 million, 5% expected premium for green certification
buildings = read.csv('greenbuildings.csv')

buildings_green = buildings %>%
  filter(green_rating == 1)
buildings_nongreen = buildings %>%
  filter(green_rating == 0)

rent_diff_mean = mean(buildings_green$Rent) - mean(buildings_nongreen$Rent)
rent_diff_median = median(buildings_green$Rent) - median(buildings_nongreen$Rent)

# analyzing the buildings with less than 10 percent before thinking of scrubbing them off the dataset
buildings_lessthan10 = buildings %>% filter(leasing_rate <= 10)

ggplot(buildings_lessthan10)+
  geom_point(aes(x=age, y=leasing_rate))

ggplot(buildings)+
  geom_histogram(aes(leasing_rate), binwidth = 1)

# Lets find the mean squared error for both mean and median as averages
MSE_mean_green = sum((mean(buildings_green$Rent)-buildings_green$Rent)^2)/dim(buildings_green)[1]
MSE_Median_green = sum((median(buildings_green$Rent)-buildings_green$Rent)^2)/dim(buildings_green)[1]

MSE_mean_nongreen = sum((mean(buildings_nongreen$Rent)-buildings_nongreen$Rent)^2)/dim(buildings_nongreen)[1]
MSE_Median_nongreen = sum((median(buildings_nongreen$Rent)-buildings_nongreen$Rent)^2)/dim(buildings_nongreen)[1]

buildings_green = buildings_green %>%
                    mutate(error_mean=(mean(buildings_green$Rent)-buildings_green$Rent), 
                           error_median=(median(buildings_green$Rent)-buildings_green$Rent))

buildings_nongreen = buildings_nongreen %>%
                      mutate(error_mean=(mean(buildings_nongreen$Rent)-buildings_nongreen$Rent), 
                        error_median=(median(buildings_nongreen$Rent)-buildings_nongreen$Rent))

ggplot(buildings_green)+
  geom_histogram(aes(error_mean), binwidth=1)+
    labs(x="deviation from average",
     y="Frequency",
     title="Deviation when Mean is used as Average")

ggplot(buildings_green)+
  geom_histogram(aes(error_median), binwidth=1)+
  labs(x="deviation from average",
       y="Frequency",
       title="Deviation when Median is used as Average")

ggplot(buildings_nongreen)+
  geom_histogram(aes(error_mean), binwidth=1)+
  labs(x="deviation from average",
       y="Frequency",
       title="Deviation when Mean is used as Average")

ggplot(buildings_nongreen)+
  geom_histogram(aes(error_median), binwidth=1)+
  labs(x="deviation from average",
       y="Frequency",
       title="Deviation when Median is used as Average")
ggplot(buildings)+
  geom_histogram(aes(size))+
    xlim(0,2000000) 

# Lets predict the size of a 15 story buildings

plot = ggplot(buildings)+
  geom_point(aes(x=stories,y=size))+
    labs(x="Number of stories in the building",
     y="Size of the building",
     title="Size of the building vs Number of stories")
plot 

model = lm(size ~ stories, data=buildings)

plot + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color='red')

predict(model, data.frame(stories=15))
