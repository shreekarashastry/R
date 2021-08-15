library(mosaic)

GasPrices = read.csv("GasPrices.csv", header=TRUE)
head(GasPrices)


sub1 = subset(GasPrices, Highway == 'Y')
sub2 = subset(GasPrices, Highway == 'N')

m1 = mean(sub1$Price)
m2 = mean(sub2$Price)
v1 = var(sub1$Price)
v2 = var(sub2$Price)

delta = m1-m2
se = sqrt(v1/dim(sub1)[1] + v2/dim(sub2)[1])

lower_bound = delta - 1.96*se
upper_bound = delta + 1.96*se

lower_bound
upper_bound

hist(sub1$Price)
hist(sub2$Price)

boot1 = do(1000)*{
  xsim_bootstrap = resample(sub1)
  p_hat_boot = mean(xsim_bootstrap$Price)
  
  ysim_bootstrap = resample(sub2)
  q_hat_boot = mean(ysim_bootstrap$Price)
  
  p_hat_boot-q_hat_boot
}

confint(boot1)

# Null hypothesis testing(one sided) for delta = 0

z = (delta - 0)/se
p_value = pnorm(-abs(z))
p_value

