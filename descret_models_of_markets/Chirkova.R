m<- 22
  dim (MODEL_PRICES)
  
DATA<-MODEL_PRICES[,,m]
DATA
R<-matrix(nrow=nrow(DATA)-1, ncol=ncol(DATA))
R[,1]<-diff(log(DATA[,1]))
R[,2]<-diff(log(DATA[,2]))
R

shapiro.test(R[,1])
shapiro.test(R[,2])

install.packages("ghyp")
install.packages("copula")
library("ghyp")
library("copula")

model<-fit.ghypmv(R)

model

cdf<-pobs(R)
cdf
plot(cdf)

tCopula<-tCopula(dim=2)
t_Copula<-fitCopula(data=cdf, copula=tCopula)
t_Copula
tAIC<-2*3.968-2*2
tAIC

normalCopula<-normalCopula(dim=2)
normal_Copula<-fitCopula(data=cdf, copula=normalCopula)
normal_Copula
normalAIC<-2*1.095-2*1
normalAIC


TRAIN<-R[1:80,]
EXAM<-R[81:119,]
ks.test(TRAIN[,1],EXAM[,1])
ks.test(TRAIN[,2],EXAM[,2])

alpha<-0.1
v<-function(w)
{
  -quantile(w%%R,alpha)
}
heq<-function(w)
{
  sum(w)-1
}
hin<-function(w)
{
  w
}
w<-seq(from=0,to=1,by=0.01)
VaR_w<-vector(length=length(w))
for(i in 1:length(w))
{
  VaR_w[i]<-quantile(w[i]*R[,1]+(1-w[i])*R[,2],alpha)
}
plot(w,VaR_w,type="l")

w_opt_VaR<-w[which(VaR_w==max(VaR_w))]
w_opt_VaR
VaR_alpha_emp<-VaR_w[which(VaR_w==max(VaR_w))]
VaR_alpha_emp


PORT_OPT_VaR_TRAIN <- w_opt_VaR*TRAIN[,1]+(1-w_opt_VaR)*TRAIN[,2]
PORT_OPT_VaR_EXAM <- w_opt_VaR*EXAM[,1]+(1-w_opt_VaR)*EXAM[,2]

VaR_alpha_emp<-quantile(PORT_OPT_VaR_TRAIN, alpha)

model_PORT<-fit.ghypuv(PORT_OPT_VaR_TRAIN, silent=T)
summary(model_PORT)
BEST_AIC<-stepAIC.ghyp(PORT_OPT_VaR_TRAIN, dist=c("ghyp","t","gauss"))$best.model
summary(BEST_AIC)
VaR_alpha_mod<-qghyp(alpha,BEST_AIC)
VaR_alpha_mod

K_mod<-length(which(PORT_OPT_VaR_EXAM<=VaR_alpha_mod))

T<-nrow(EXAM)
Kupic_emp


TRAIN<-R[1:80,]
Y<-TRAIN[71:80,]
length(Y[,1])
trend<-c(1:10)
lm(Y[,1]~trend)

alpha1<-lm(Y[,1]~trend)$coef[1]
alpha1
alpha2<-lm(Y[,2]~trend)$coef[1]
alpha2
beta1<-lm(Y[,1]~trend)$coef[2]
beta1
beta2<-lm(Y[,2]~trend)$coef[2]
beta2

mean1<-alpha1+beta1*11
mean2<-alpha2+beta2*11
means<-c(mean1, mean2)
means

Q<-cov(TRAIN)
Q

tau<-0.025
V_MARC<-vector(length=length(w))
V_MARC
length(w)
for(i in 1:length(w))
{
  V_MARC[i]<-tau*(w[i]*mean1+(1-w[i])*mean2)-0.5*(Q[1,1]*w[i]^2+2*Q[1,2]*w[i]*(1-w[i])+Q[2,2]*(1-w[i])^2)
}
t<-1:(length(w))
w_MARC<-w[which(V_MARC==max(V_MARC))]
w_MARC
plot(t,V_MARC, type = "p")

