setwd ("/Users/ashlynwang/Biocomputing2020_Tutorial13")

#define parameters
rN=0.1
rM=0.1
rND=-0.1
rMD=0.05
K=1000000

#set timesteps to simulate
times<-1:500

#create place to store output
output<-matrix(data=NA,nrow=length(times),ncol=3)
output[,1]=times

#initial population size of both populations
# first output is for non-mutant cells
# last output is for mutant cells
output[1,2]=100
output[1,3]=1

#for loop for code, include equations of both populations
for (i in times[-1]){
  if (i<150){
  output[i,2]=output[(i-1),2]+rN*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/K)
  output[i,3]=output[(i-1),3]+rM*output[(i-1),3]*(1-(output[(i-1),2]+output[(i-1),3])/K)
  } else if (i>149) {
    output[i,2]=output[(i-1),2]+rND*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/K)
    output[i,3]=output[(i-1),3]+rMD*output[(i-1),3]*(1-(output[(i-1),2]+output[(i-1),3])/K)
  }
}

#plot results, load the package
library(ggplot2)

#create outputs for each population
outputN <- data.frame(time=output[,1],nonmutantGrowth=output[,2])
outputM <- data.frame(time=output[,1],mutantGrowth=output[,3])

#create line graphs
ggplot()+
  geom_line(data=outputN,aes(x=time, y=nonmutantGrowth), color='blue')+
  geom_line(data=outputM,aes(x=time, y=mutantGrowth), color='red')+
  xlab('Time')+
  ylab('# of Cells')+
  theme_classic()