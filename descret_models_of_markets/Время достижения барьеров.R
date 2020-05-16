t0<-1000
T<-200
Y<-log(CLOSE_DAILY[(t0-T):t0,])

RETURNS<-diff(Y,1)
h_u<-vector(length=ncol(Y))
h_d<-vector(length=ncol(Y))
p<-vector(length=ncol(Y))
q<-vector(length=ncol(Y))
for(j in 1:ncol(Y))
{
  h_u[j]<-mean(RETURNS[which(RETURNS[,j]>=0),j])
  h_d[j]<-mean(RETURNS[which(RETURNS[,j]<=0),j])
  p[j]<-length(RETURNS[which(RETURNS[,j]>=0),j])/length(RETURNS[,j])
  q[j]<-length(RETURNS[which(RETURNS[,j]<=0),j])/length(RETURNS[,j])
}
h_u;h_d
p;q
p+q

Y_exam<-log(CLOSE_DAILY)[(t0+1):nrow(CLOSE_DAILY),]
which(Y_exam[,5]>=Y[T,5]+k*h_u[5])[1]
RIGHT<-matrix(nrow=length(c(4:10)),ncol=ncol(CLOSE_DAILY))
LEFT<-matrix(nrow=length(c(4:10)),ncol=ncol(CLOSE_DAILY))
TIME<-matrix(nrow=length(c(4:10)),ncol=ncol(CLOSE_DAILY))
TIME_MODEL<-matrix(nrow=length(c(4:10)),ncol=ncol(CLOSE_DAILY))
m1<-4
m2<-10
for(k in m1:m2)
{
  for(j in 1:ncol(Y))
  {
  RIGHT[k-m1+1,j]<-which(Y_exam[,j]>=Y[T,j]+k*h_u[j])[1]
  LEFT[k-m1+1,j]<-which(Y_exam[,j]<=Y[T,j]+k*h_d[j])[1]
  ifelse(is.na(RIGHT[k-m1+1,j]),RIGHT[k-m1+1,j]<-Inf,RIGHT[k-m1+1,j]<-RIGHT[k-m1+1,j])
  ifelse(is.na(LEFT[k-m1+1,j]),LEFT[k-m1+1,j]<-Inf,LEFT[k-m1+1,j]<-LEFT[k-m1+1,j])
  TIME[k-m1+1,j]<-min(RIGHT[k-m1+1,j],LEFT[k-m1+1,j])
  TIME_MODEL[k-m1+1,j]<-k/(q[j]-p[j])-(2*k)/(q[j]-p[j])*(1-(q[j]/p[j])^k)/(1-(q[j]/p[j])^(2*k))
  }
}
RIGHT;LEFT
TIME
TIME_MODEL
RMT<-rowMeans(TIME)
RMT_MODEL<-rowMeans(TIME_MODEL)
RESULT<-cbind(RMT,RMT_MODEL)
cor(RESULT)

