library(tidyverse)

sum = 422
N = 30
l = sum/N

SE = sqrt(l/N)

lower_bound = l - 1.96*SE
upper_bound = l + 1.96*SE

lower_bound
upper_bound

dist = rpois(30, l)

boot1 = do(1000)*{
  xsim_bootstrap = resample(dist)
  mean(xsim_bootstrap)
}

confint(boot1)
