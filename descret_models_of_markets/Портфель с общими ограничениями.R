
RET_DAILY<-diff(log(CLOSE_DAILY))        # логарифмические доходности
cols<-c(3,7,9)                           # выбор активов
n<-length(cols)                          # количество активов
RETURNS<-RET_DAILY[,cols]                # доходности выбранных активов
covar<-cov(RETURNS)                      # ковариации доходностей выбранных активов
means<-colMeans(RETURNS)                 # ожидаемые доходности выбранных активов

# имеем ограничения:  wi>l , w1<0.1, w1+w2+w3=1
l<-rep(0,n)                                          # ограничения на короткие позиции
m1<-1                                                # количество ограничений в виде равенств
m2<-1                                                # количество дополнительных ограничений в виде неравенств

                      # ФОРМИРОВАНИЕ ВЕЛИЧИН ДЛЯ ФОРМУЛИРОВКИ ЗАДАЧИ

a<-c(2*covar%*%l,rep(0,m2))                                            
A1<-matrix(nrow=m1,ncol=n)
A1[1,]<-rep(1,n)
A2<-matrix(nrow=m2,ncol=length(cols))
A2[1,]<-c(-1,rep(0,n-1))
nul_m1_m2<-matrix(0,nrow=m1,ncol=m2)
nul_n_m2<-matrix(0,nrow=n,ncol=m2)
nul_m2_N<-matrix(0,nrow=m2,ncol=n+m2)
A<-rbind(cbind(A1,nul_m1_m2),cbind(A2,-diag(m2)))
B1<-vector(length=m1);B1[1]=1
B2<-vector(length=m2);B2[1]<--0.1
B<-c(B1,B2)
A;B                                                 # матрица и столбец условия AX=B
sigma<-rbind(cbind(covar,nul_n_m2),nul_m2_N);sigma
p<-c(means,rep(0,m2));p

grad_w<-matrix(nrow=n+m2,ncol=n+m2+m1+m2)           # матрица производных по переменным

for(i in 1:(n+m2))
{
  grad_w[i,]<-c(-2*sigma[i,],A[,i])
}
grad_lambda<-matrix(nrow=m1+m2,ncol=n+m2+m1+m2)     # матрица производных по множителям Лагранжа

for(i in 1:(m1+m2))
{
  grad_lambda[i,]<-c(A[i,],rep(0,m1+m2))
}
G<-rbind(grad_w,grad_lambda)                        # матрица-градиент функции Лагранжа
G                                                   



nul_N_M<-matrix(0,nrow=n+m2,ncol=m1+m2)
ID<-cbind(diag(n+m2),nul_N_M)
GG<-array(dim=c(n+m2+m1+m2,n+m2+m1+m2,2^(n+m2)))

for(i in 1:2^(n+m2))
{
  for(j in 1:(n+m2))
  {
    ifelse(BOOLEAN[[n+m2]][i,j]==1, GG[j,,i]<-ID[j,],GG[j,,i]<-G[j,])
  } 
  for(j in (n+m2+1):(n+m2+m1+m2))
  {
    GG[j,,i]<-G[j,]
  }
}
GG                                                 # совокупность матриц системы Куно-Таккера


K<-seq(from=0,to=1.72,by=0.01)                      # последовательность значений углового коэффициента

rights<-matrix(nrow=length(K),ncol=n+m2+m1+m2)            # правые части системы уравнений Лагранжа

RIGHTS<-array(dim=c(n+m2+m1+m2,ncol=2^(n+m2),length(K)))  # совокупность правых частей системы Куно-Таккера
SOLVE<-array(dim=c(n+m2+m1+m2,ncol=2^(n+m2),length(K)))   # совокупность решений системы Куно-Таккера


PORT<-array(dim=c(n+m2,2^(n+m2),length(K)))               
LAMBDA<-array(dim=c(m1+m2,2^(n+m2),length(K)))
PORT_INV<-matrix(nrow=n,ncol=length(K))



for(k in 1:length(K))
{
  rights[k,]<-c(-K[k]*p+a,B)
  for(i in 1:2^(n+m2))
  {
    for(j in 1:(n+m2))
    {
      ifelse(BOOLEAN[[n+m2]][i,j]==1,RIGHTS[j,i,k]<-0,RIGHTS[j,i,k]<-rights[k,j])
    }
    for(j in (n+m2+1):(n+m2+m1+m2))
    {
      RIGHTS[j,i,k]<-B[j-n-m2]
    }
  ifelse(det(GG[,,i])!=0, SOLVE[,i,k]<-solve(GG[,,i])%*%RIGHTS[,i,k],SOLVE[,i,k]<-NA)
  }
  PORT[,,k]<-SOLVE[1:(n+m2),,k]
  LAMBDA[,,k]<-SOLVE[(n+m2+1):(n+m2+m1+m2),,k]
}
rights
RIGHTS
PORT
LAMBDA


GR<-array(dim=c(n+m2,2^(n+m2),length(K)))
MIN_PORT<-matrix(nrow=2^(n+m2),ncol=length(K))
MAX_GR<-matrix(nrow=2^(n+m2),ncol=length(K))

for(k in 1:length(K))
{
  for(i in 1:nrow(MIN_PORT))
  {
  MIN_PORT[i,k]<-min(PORT[,i,k])
  GR[,i,k]<-K[k]*p-a-2*sigma%*%PORT[,i,k]+t(A)%*%LAMBDA[,i,k]
  MAX_GR[i,k]<-max(GR[,i,k])
  }
}
MIN_PORT
for(k in 1:length(K))
{
  for(i in 1:2^(n+m2))
  {
    ifelse(!is.na(MIN_PORT[i,k]),MIN_PORT[i,k]<-MIN_PORT[i,k],MIN_PORT[i,k]<--1)
    ifelse(!is.na(MAX_GR[i,k]),MAX_GR[i,k]<-MAX_GR[i,k],MAX_GR[i,k]<-1)
  }
}
MAX_GR
MIN_PORT


num<-matrix(nrow=2^(n+m2),ncol=length(K))
number<-vector(length=length(K))
for(k in 1:length(K))
{
  for(i in 1:2^(n+m2))
  {
    ifelse(MAX_GR[i,k]<=10^(-18)&MIN_PORT[i,k]>=0,num[i,k]<-i,num[i,k]<-0)
  }
  number[k]<-max(num[,k])
}
num
number
PORT_K<-matrix(nrow=n,ncol=length(K))
for(k in 1:length(K))
{
  PORT_K[,k]<-PORT[1:n,number[k],k]
}
PORT_K
K[63]


mu_K<-vector(length=length(K))
sigma_K<-vector(length=length(K))
for(k in 1:length(K))
{
  mu_K[k]<-sum(PORT_K[,k]*means)
  sigma_K[k]<-(PORT_K[,k]%*%covar%*%PORT_K[,k])^0.5
}
mu_K
sigma_K
plot(sigma_K,mu_K,type="l")

means
