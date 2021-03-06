                                #������������� ����������� �������

install.packages("ghyp")
install.packages("stabledist")
install.packages("fBasics")

library(ghyp)
library(stabledist)
library(fBasics)

                                             #���� ������

CLOSE_DAILY<-cbind(AAPL[,3],AMD[,3],AXP[,3],CAT[,3],CSCO[,3],EBAY[,3],GLD[,3],JPM[,3],MCD[,3],NVDA[,3],SBUX[,3],
                   TRV[,3],UNH[,3],XOM[,3])
RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY)) #������� ����������
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY)) {RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1}
}

t0<-100         #����� ��������� ����
T1<-200         #����� ��������� �������
T2<-floor(T1/2) #����� ������������ ������� (������ �������� ����� ������ ����� ��������� �������)

RETURNS_TRAIN<-RET_DAILY[c((t0+1):(t0+T1)),c(1:14)]
RETURNS_EXAM<-RET_DAILY[c((t0+T1+1):(t0+T2)),c(1:14)]




                            #������������� ����������� (�������� �������������)

m<-10       #����� ���������������� ������� (���������� ������) - �������� �� 1 �� 14
Y<-RETURNS_TRAIN[,m] #����� ���������� ���� �����������
#(������� ���������� �������� j �� 1 �� ���������� �������)
#��� ������������� ����������� ������������, ��� �������, ��� ����������� ���������
# 1   ���������� �������������
# 2   ���������� ��������������� �������������

Y

    #�������� ����������� �������������. ������������� ������� (distr)Fit ������ fBasics
STABLE_Fit<-stableFit(Y, alpha = 1.75, beta = -0.3, gamma = 0.02, delta = 0,
                      type = "mle",doplot=FALSE) #�������� ����������� �������������
## S4 method for signature 'fDISTFIT'
show(STABLE_Fit)


alpha_stable_model<-1.493720747              #��������� ������� (copy-past �� �������)
beta_stable_model<--0.157368858               #��������� ������� (copy-past �� �������)
gamma_stable_model<-0.019034089        #��������� ������� (copy-past �� �������)
delta_stable_model<-0.003007814       #��������� ������� (copy-past �� �������)

               #�������� ������ ����������� ���������������� ������������� (����� ghyp). 
                          #������������ ����� ������������� �������������
ghyp_dist_model <- fit.ghypuv(Y,  silent = TRUE)
summary(ghyp_dist_model)

par_ghyp<-coef(ghyp_dist_model)

lambda_ghyp_model<-par_ghyp$lambda
alpha.bar_ghyp_model<-par_ghyp$alpha.bar
mu_ghyp_model<-par_ghyp$mu
sigma_ghyp_model<-par_ghyp$sigma
gamma_ghyp_model<-par_ghyp$gamma

ghyp_model<-ghyp(lambda=lambda_ghyp_model,alpha.bar=alpha.bar_ghyp_model,mu=mu_ghyp_model,
                 sigma=sigma_ghyp_model,gamma=gamma_ghyp_model)
ghyp_model

vcov(object=ghyp_model)      #��������� ���������� ghyp-�������������
sd(Y)^2                      #������������ ��������� (��� �� ��������� ���������� ����������� �������������)
sd(RETURNS_EXAM[,m])^2       #��������� ������������ �������


#�������� ������������ ��������� �������������          
n<-floor(length(Y)/3) #���������� �������� ���������
delta<-(max(Y)-min(Y))/n #���
breakpoints<-seq(from=min(Y),to=max(Y),by=delta)
EMPIRICAL_DENSITY<-hist(Y,breaks=breakpoints, plot=FALSE)
x<-EMPIRICAL_DENSITY$mids #� ���� ������ ����������� ������������ � ������������� ���������
#�����������
z<-EMPIRICAL_DENSITY$density
print(x)
print(z)

#��������� ����������� ������������� � ����������� ���������� �����������
stable_model_density<-function(x)
{
  dstable(x, alpha=alpha_stable_model, beta=beta_stable_model, gamma=gamma_stable_model, 
          delta=delta_stable_model,pm=0)
}

#��������� ����������� ���������������� ������������� � ����������� ���������� �����������
ghyp_model_density<-function(x)
{
  dghyp(x,object=ghyp(lambda=lambda_ghyp_model,alpha.bar=alpha.bar_ghyp_model,mu=mu_ghyp_model,
                      sigma=sigma_ghyp_model,gamma=gamma_ghyp_model))
}

#��������� ����������� ������������� � ����������� ���������� �����������
norm_model_density<-function(x)
{
  dnorm(x,mean=mean(Y),sd=sd(Y))
}

dif_stable<-sum((z-stable_model_density(x))^2);dif_stable
dif_ghyp<-sum((z-ghyp_model_density(x))^2);dif_ghyp
dif_norm<-sum((z-norm_model_density(x))^2);dif_norm



                                              #����������� �����������

hist(Y,breaks=floor(length(x))/4,probability=TRUE)
lines(x,ghyp_model_density(x),col="BLUE")
lines(x,stable_model_density(x),col="RED")
lines(x,dnorm(x,mean=mean(Y),sd=sd(Y)),col="YELLOW")

#�������� ������� ������

hist(Y,breaks=floor(length(x))/4,ylim=c(0,1),probability=TRUE)
lines(x,ghyp_model_density(x),col="BLUE")
lines(x,stable_model_density(x),col="RED")
lines(x,dnorm(x,mean=mean(Y),sd=sd(Y)),col="YELLOW")

                                  #����� ������ GHYP �� ��������������� ��������

BEST_AKAIKE_MODEL<- stepAIC.ghyp(Y, dist=c("ghyp", "hyp","NIG", "t", "gauss"), silent=TRUE)$best.model
summary(BEST_AKAIKE_MODEL)        

######################################################################################################

                                                        #VaR
alpha<-0.05
VaR_emp<-quantile(Y,alpha)
VaR_stable_best<-qstable(alpha,alpha=alpha_stable_model,beta=beta_stable_model, gamma=gamma_stable_model,
                         delta=delta_stable_model)
VaR_ghyp_best<-qghyp(alpha, object=ghyp_dist_model)
VaR_norm_model<-qnorm(alpha,mean=mean(Y),sd=sd(Y))
VaR_emp
VaR_stable_best
VaR_ghyp_best
VaR_norm_model

VaR_EXAM<-quantile(RETURNS_EXAM[,m],alpha);VaR_EXAM

                                                    #ES SHORTFALL

Shortfall_obs_numbers_emp<-which(RETURNS_EXAM[,m]<VaR_emp);Shortfall_obs_numbers_emp
Shortfall_obs_emp<-vector(length=length(Shortfall_obs_numbers_emp))
for(i in 1:length(Shortfall_obs_numbers_emp)) 
{Shortfall_obs_emp[i]<-RETURNS_EXAM[,m][Shortfall_obs_numbers_emp[i]]}
ES_EXAM_emp<-mean(Shortfall_obs_emp);ES_EXAM_emp

Shortfall_obs_numbers_ghyp<-which(RETURNS_EXAM[,m]<VaR_ghyp_best);Shortfall_obs_numbers_ghyp
Shortfall_obs_ghyp<-vector(length=length(Shortfall_obs_numbers_ghyp))
for(i in 1:length(Shortfall_obs_numbers_ghyp)) 
{Shortfall_obs_ghyp[i]<-RETURNS_EXAM[,m][Shortfall_obs_numbers_ghyp[i]]}
ES_EXAM_ghyp<-mean(Shortfall_obs_ghyp);ES_EXAM_ghyp

Shortfall_obs_numbers_stable<-which(RETURNS_EXAM[,m]<VaR_stable_best);Shortfall_obs_numbers_ghyp
Shortfall_obs_stable<-vector(length=length(Shortfall_obs_numbers_stable))
for(i in 1:length(Shortfall_obs_numbers_stable)) 
{Shortfall_obs_stable[i]<-RETURNS_EXAM[,m][Shortfall_obs_numbers_stable[i]]}
ES_EXAM_stable<-mean(Shortfall_obs_stable);ES_EXAM_stable

Shortfall_obs_numbers_norm<-which(RETURNS_EXAM[,m]<VaR_norm_model);Shortfall_obs_numbers_norm
Shortfall_obs_norm<-vector(length=length(Shortfall_obs_numbers_norm))
for(i in 1:length(Shortfall_obs_numbers_norm)) 
{Shortfall_obs_norm[i]<-RETURNS_EXAM[,m][Shortfall_obs_numbers_norm[i]]}
ES_EXAM_norm<-mean(Shortfall_obs_norm);ES_EXAM_norm


                                                    #���� ������
a0_emp<- length(Shortfall_obs_numbers_emp)/T2
S_emp <- 2*log( (1-a0_emp)^(T2-(length(Shortfall_obs_numbers_emp))) * a0_emp^(length(Shortfall_obs_numbers_emp)) ) - 2*log( (1-alpha)^(T2-(length(Shortfall_obs_numbers_emp))) * alpha^(length(Shortfall_obs_numbers_emp)) )
Kupik_emp <- 1 - pchisq(S_emp, 1)

a0_stable <- length(Shortfall_obs_numbers_stable)/T2
S_stable <- 2*log( (1-a0_stable)^(T2-(length(Shortfall_obs_numbers_stable))) * a0_stable^(length(Shortfall_obs_numbers_stable)) ) - 2*log( (1-alpha)^(T2-(length(Shortfall_obs_numbers_stable))) * alpha^(length(Shortfall_obs_numbers_stable)) )
Kupik_stable <- 1 - pchisq(S_stable, 1)

a0_ghyp <- length(Shortfall_obs_numbers_ghyp)/T2
S_ghyp <- 2*log( (1-a0_ghyp)^(T2-(length(Shortfall_obs_numbers_ghyp))) * a0_ghyp^(length(Shortfall_obs_numbers_ghyp)) ) - 2*log( (1-alpha)^(T2-(length(Shortfall_obs_numbers_ghyp))) * alpha^(length(Shortfall_obs_numbers_ghyp)) )
Kupik_ghyp <- 1 - pchisq(S_ghyp, 1)

a0_norm <- length(Shortfall_obs_numbers_norm)/T2
S_norm <- 2*log( (1-a0_norm)^(T2-(length(Shortfall_obs_numbers_norm))) * a0_ghyp^(length(Shortfall_obs_numbers_norm)) ) - 2*log( (1-alpha)^(T2-(length(Shortfall_obs_numbers_norm))) * alpha^(length(Shortfall_obs_numbers_norm)) )
Kupik_norm <- 1 - pchisq(S_norm, 1)


                                                 #�������� ����������
RESULT<-matrix(nrow=3,ncol=4)
row.names(RESULT)=c("��������� VaR �� ��������","P-VALE","ES")
colnames(RESULT)=c("EMP","STABLE","GHYP","NORM")
RESULT[1,1]<-VaR_EXAM/VaR_emp
RESULT[1,2]<-VaR_EXAM/VaR_stable_best
RESULT[1,3]<-VaR_EXAM/VaR_ghyp_best
RESULT[1,4]<-VaR_EXAM/VaR_norm_model
RESULT[2,1]<-Kupik_emp
RESULT[2,2]<-Kupik_stable
RESULT[2,3]<-Kupik_ghyp
RESULT[2,4]<-Kupik_norm
RESULT[3,1]<-ES_EXAM_emp
RESULT[3,2]<-ES_EXAM_stable
RESULT[3,3]<-ES_EXAM_ghyp
RESULT[3,4]<-ES_EXAM_norm
RESULT


############################################################################################################


