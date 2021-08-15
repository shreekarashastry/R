library(tidyverse)

stock = read.csv('stocks_bonds.csv')
head(stock)

I = sum(stock$SP500 <= -0.1)
N = dim(stock)[1]

mean = I/N
se = sqrt(mean*(1-mean)/N)

lower_bound = mean - 1.96*se
upper_bound = mean + 1.96*se

lower_bound
upper_bound
