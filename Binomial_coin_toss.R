library(mosaic)
#From the section A of the Question Number it is established that 
#For Binomial Distribution, E(x)=Np V(x)=Npq

p=0.5 #Given
q=1-p

N=5 #In first case

#Let's simulate a coin flip to get one value of X5 where X5 denotes the 
#number of times tossed result was heads 

#setting a seed will make it easy to replicate this result
set.seed(357)

#coin toss with heads=1, tails=0
num_coin=c(1,0)

#Now create a monte carlo simulation with 1000 trials
simulation=do(1000)*{
  #Here size defines the number of coin flips
  flip=sample(num_coin, size=N, replace=TRUE)
  
  #Number of heads
  head_freq=cumsum(flip==1)
  #Last frequency of cumulative sum give the total heads
  head_prob=head_freq[N]
  Xn=head_prob
  Xn/N
}

head(simulation)

Expected_Theoretical_Mean=p
Expected_Theoretical_Variance=(p*q/N)^0.5

Monte_carlo_Mean=mean(simulation$result)
Monte_carlo_SD=sd(simulation$result) #square root of variance

# Simulating the above result for different values of N
Th_sd=rep(0,5)
MC_Mean=rep(0,5)
MC_sd=rep(0,5)
Number=c(5,10,25,50,100)
count=1

for (num in Number){
  simulation=do(1000)*{
    #Here size defines the number of coin flips
    flip=sample(num_coin, size=num, replace=TRUE)
    
    #Number of heads
    head_freq=cumsum(flip==1)
    #Last frequency of cumulative sum give the total heads
    head_prob=head_freq[num]
    Xn=head_prob
    Xn/num
  }
  Th_sd[count]=(p*q/num)^0.5
  MC_Mean[count]=mean(simulation$result)
  MC_sd[count]=sd(simulation$result)
  count=count+1
}

plot(Number, Th_sd, type='l', col='blue',
  	xlab="Number of coin flips",
    ylab="standard deviation",
    main="Standard deviation vs Number of coin flips",
    las=1, cex.axis = 0.85,
    ylim=c(0,0.25))
legend('topright',
 	legend=c("Theoretical Values of SD", "Monte Carlos values of sd"),
	lwd=1, col=c("blue", "red"))
lines(Number, MC_sd, type='l', col='red')
