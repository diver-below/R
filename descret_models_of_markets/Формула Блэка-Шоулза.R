RET_DAILY<-diff(log(CLOSE_DAILY,1))

BS<-function(S,K,r,s,t)
{
  d1<-(log(S/K)+r*t+0.5*s^2*t)/(s*t^0.5)
  d2<-(log(S/K)+r*t-0.5*s^2*t)/(s*t^0.5)
  return(S*pnorm(d1)-K*exp(-r*t)*pnorm(d2))
}
BS(100,100,0.01,0.02,30)

t0<-1500
tau<-150
T<-30
K<-vector(length=ncol(CLOSE_DAILY))
for(j in 1:ncol(CLOSE_DAILY))
{
  K[j]<-0.9*CLOSE_DAILY[t0,j]
}
r<-0.05/240

TRAIN<-CLOSE_DAILY[(t0-tau):t0,]
EXAM<-CLOSE_DAILY[(t0+1):(t0+T),]

vol<-matrix(nrow=nrow(EXAM),ncol=ncol(CLOSE_DAILY))
COP<-matrix(nrow=nrow(EXAM),ncol=ncol(CLOSE_DAILY))
for(t in 1:nrow(vol))
{
  for(j in 1:ncol(CLOSE_DAILY))
  {
    vol[t,j]<-sd(diff(log(CLOSE_DAILY[(t0-tau):(t0+t-1),j])))
    COP[t,j]<-BS(CLOSE_DAILY[t0+t-1,j],K[j],r,vol[t,j],T-t)
  }
}
vol
COP


