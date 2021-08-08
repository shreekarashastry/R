library(tidyverse)

# Reading the data set
data = read.csv('creatinine.csv')
head(data)

# Initial plotting of the data set
plot = ggplot(data)+
  geom_point(aes(x=age,y=creatclear))+
  labs(x="Age of patient",
       y="Patients creatinine clearance rate",
       title="Patients creatinine rate at different Ages")

plot

# Fitting a linear regression model to the data
model = lm(creatclear ~ age , data=data)

# Plotting the model to visualize
plot + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color='red')
  
# A) Estimated creatinine clearance rate for 55 years old is 
predict(model, data.frame(age = 55))
# Predicted Answer is 113.723

Intercept = coef(model)[1]
Slope = coef(model)[2]
# B) creatinine clearance rate changes with age according to this equation 
# CCR = Slope*age + Intercept , CCR = (-0.62*age + 148)

# C) for a 40 year old with ccr=135 and 60 year old with ccr=112
P40=predict(model, data.frame(age=40))
P60=predict(model, data.frame(age=60))

# Lets compare by finding the standard errors
SE40 = P40-135
SE60 = P60-112

# From the SE calculation it can be seen that the person who is 60 years of age 
# and has a ccr of 112 is more healthier than the person with ccr 135 and age=401
