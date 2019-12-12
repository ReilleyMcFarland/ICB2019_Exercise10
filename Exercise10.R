#Exercise10
#Reilley Knott, Intro to Biocomputing

#initial values and parameters
N0=100
M0=1
r=0.1
rN=-0.1
rM=0.05
K=1000000
timesteps=1000
#create 2 vectors to store Ns and Ms and set initial N0/M0
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

#Establish models, with the change occuring at t=200 to the alternative models
for(t in 1:(timesteps-1)) {
  if(t < 200) {
    Ns[t+1] = Ns[t] + r*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1] = Ms[t] + r*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }
  else {
    Ns[t+1] = Ns[t] + rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1] = Ms[t] + rM*Ms[t]*(1-((Ms[t]+Ns[t])/K))
  }
}

#plot simulation, distinguish between Mutant and Non-mutant cell types
library(ggplot2)
sim<-data.frame(time=1:length(Ns),N=Ns, M=Ms)
ggplot(data=sim, aes(x=time)) + geom_line(aes(x=time, y=N, col='red')) + geom_line(aes(x=time, y=M, col='blue')) + theme_classic()



