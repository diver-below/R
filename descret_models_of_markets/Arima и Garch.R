install.packages("forecast")
install.packages("fGarch")
library(forecast)
library(fGarch)

                                              # ВВОД ДАННЫХ

#CLOSE_DAILY<-cbind(AAPL[,3],AMD[,3],AXP[,3],CAT[,3],CSCO[,3],EBAY[,3],GLD[,3],JPM[,3],MCD[,3],NVDA[,3],SBUX[,3],
#                  TRV[,3],UNH[,3],XOM[,3])
RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY)) #дневные доходности
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY)) {RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1}
}

k<-3       # выбор актива
Y<-RET_DAILY[,k]

                                              # МОДЕЛЬ ARMA

Arima_model<-Arima((Y-mean(Y))^2,order=c(1,0,1))
summary(Arima_model)

ar1<-Arima_model$coef[1]
ma1<-Arima_model$coef[2]
mean<- Arima_model$coef[3]

ar1;ma1;mean
                                              # МОДЕЛЬ GARCH

Garch_model<-garchFit(formula=~garch(1,1),data=Y,trace=F)
summary(Garch_model)
par<-Garch_model@fit
par$matcoef[2,1]
mu<-par$matcoef[1,1]#8.3711e-04
omega<-par$matcoef[2,1]#5.3498e-06
alpha1<-par$matcoef[3,1]#8.9803e-02
beta1<-par$matcoef[4,1]#8.9941e-01

mu;omega;alpha1;beta1


