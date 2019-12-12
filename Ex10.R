# Devin North - Exercise 10

# set initial values
N0 = 100
M0 = 1
rn = 0.1 # normal growth rate
rN = -0.1 #growth rate with drug
rm = 0.1
K = 1000000
timesteps = 500

# create vector to store Ns
N = numeric(length=timesteps)
N[1] = N0

# create vector to store Ms
M = numeric(length=timesteps)
M[1] = M0

# simulate
for(t in 1:(timesteps-1)){
  if((N[t]-N[t-1]) = 0){ 
    N[t+1] <- N[t]+rn*N[t]*(1-(N[t]+M[t])/K) # rate is normal
    M[t+1] <- M[t]+rm*M[t]*(1-(N[t]+M[t])/K)  
  }
  else{
    N[t+1] <- N[t]+rN*N[t]*(1-(N[t]+M[t])/K) # rate is reversed
    M[t+1] <- M[t]+rm*M[t]*(1-(N[t]+M[t])/K)
  }
}
# note about loop: the if statement forces the loop to change as soon as the
# values hit equilibrium by checking that the previous value is not the same 
# as the current one 

# plot simulation
library(ggplot2)
sim <- data.frame(time=1:length(N),N=N,M=M)
ggplot(data = sim, aes(x = time, y = N)) +
  geom_line(aes(x = time, y = N), col = "skyblue") +
  geom_line(aes(x = time, y = M), col = "firebrick2") +
  labs(title = "Normal vs. Mutated Tumor Cell Growth", x = "time (days)",
       y = "N (number of cells)")
              
            

