library(tidyverse)

data = read.csv('predimed.csv')

head(data)

xtabs(~group+event, data=data) %>% prop.table %>% round(2)

data_updated = data %>%
  mutate(updated_group = ifelse(group == "Control", yes="Control", no="Med Diet Any"))

xtabs(~updated_group+event, data=data_updated) %>% prop.table %>% round(4)

N = sum(data_updated$updated_group == "Control")
M = sum(data_updated$updated_group == "Med Diet Any")

p = 0.0153
q = 0.0245

delta = p - q

var = p*(1-p)/N + q*(1-q)/M
se = sqrt(var)

lower_bound = (p-q) - 1.96*se
upper_bound = (p-q) + 1.96*se

lower_bound
upper_bound

# SE = 0.00359

# Bootstrap
x_sim = rbinom(N, 1, p)
y_sim = rbinom(M, 1, q)

boot1 = do(1000)*{
  xsim_bootstrap = resample(x_sim)
  p_hat_boot = sum(xsim_bootstrap)/N
  
  ysim_bootstrap = resample(y_sim)
  q_hat_boot = sum(ysim_bootstrap)/M
  
  p_hat_boot-q_hat_boot
}

confint(boot1)
hist(boot1$result)

# Null hypothesis testing(one sided) for delta = 0

z = (delta - 0)/se
p_value = pnorm(-abs(z))
p_value


