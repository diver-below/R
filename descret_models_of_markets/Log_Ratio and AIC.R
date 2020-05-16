                                    

#############################################################################################################

                                           # ПОРОЖДЕННЫЕ ДАННЫЕ

#############################################################################################################


T<-100         # длина выборки (временного ряда)
k<-20          # количество выборок
mu<-0.05       # математическое ожидание
sigma<-1.04    # стандартное отклонение

NORM<-matrix(nrow=T,ncol=k)               # порожденные выборки
for(i in 1:ncol(NORM))
{
  NORM[,i]<-rnorm(T,mean=mu,sd=sigma)
}
means<-colMeans(NORM)                     # выборочные средние
sds<-apply(NORM,2,sd)                     # выборочные стандартные отклонения

    # ЭКСПЕРИМЕНТ - СРАВНЕНИЯ СТАТИСТИЧЕСКИХ ЗНАЧЕНИЙ СО ЗНАЧЕНИЯМИ СТАНДАРТНОГО НОРМАЛЬНОГО РАСПРЕДЕЛЕНИЯ

dif_means<-vector(length=k)
dif_sds<-vector(length=k)
for(i in 1:k)
{
  ifelse(abs(means[i])>=abs(mu),dif_means[i]<-0,dif_means[i]<-1)
  ifelse(abs(sds[i]-1)>=abs(sigma-1),dif_sds[i]<-0,dif_sds[i]<-1)
}
cbind(means,sds,dif_means,dif_sds)  # если значение 1, статистические значения лучше

                                      # ПОСТРОЕНИЕ ФУНКЦИИ ПРАВДОПОДОБИЯ
x<-vector(length=T)
loglikelyhood<-function(x,m,s)
{
  T*(-0.5*log(2*pi)-log(s))-sum((x-m)^2/(2*s^2))
}

                        # ТЕСТЫ ОТНОШЕНИЯ ПРАВДОПОДОБИЯ И ИНФОРМАЦИОННЫЕ КРИТЕРИИ

LOG_LIK_STAT<-vector(length=k)
LOG_LIK_STAND<-vector(length=k)
LOG_RATIO<-vector(length=k)
LOG_RATIO_STAT<-vector(length=k)
AIC_STAT<-vector(length=k)
AIC_STAND<-vector(length=k)
AIC_TEST_STAT<-vector(length=k)
qchisq(0.95,2)
for(i in 1:k)
{
  LOG_LIK_STAT[i]<-loglikelyhood(NORM[,i],mean(NORM[,i]),sd(NORM[,i]))
  LOG_LIK_STAND[i]<-loglikelyhood(NORM[,i],0,1)
  LOG_RATIO[i]<-2*LOG_LIK_STAT[i]-2*LOG_LIK_STAND[i]
  ifelse(LOG_RATIO[i]<=qchisq(0.95,2),LOG_RATIO_STAT[i]<-0,LOG_RATIO[i]<-1)
  AIC_STAT[i]<-2*2-2*LOG_LIK_STAT[i]
  AIC_STAND[i]<--2*LOG_LIK_STAND[i]
  ifelse(AIC_STAT[i]<=AIC_STAND[i],AIC_TEST_STAT[i]<-1,AIC_TEST_STAT[i]<-0)
}    
cbind(LOG_LIK_STAT,LOG_LIK_STAND,LOG_RATIO,LOG_RATIO_STAT,AIC_STAT,AIC_STAND,AIC_TEST_STAT)

##############################################################################################################

                                             # РЕАЛЬНЫЕ ДАННЫЕ

##############################################################################################################

library(ghyp)

                                      # ПОЛУЧЕНИЕ РЯДОВ ДОХОДНОСТЕЙ

RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY)) #дневные доходности
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY)) {RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1}
}
t0<-1001             # выбор начального значения
T<-150               # выбор длины временных рядов
TIME<-c(t0:(t0+T))   # моделируемый отрезок временных рядов

                          # ТЕСТ ОТНОШЕНИЯ ПРАВДОПОДОБИЯ И ИНФОРМАЦИОННЫЙ КРИТЕРИЙ

LOG_RATIO_STUDENT<-vector(length=ncol(RET_DAILY))   
LOG_RATIO_BEST<-vector(length=ncol(RET_DAILY))
AIC_GHYP<-vector(length=ncol(RET_DAILY))
AIC_STUDENT<-vector(length=ncol(RET_DAILY))
AIC_BEST<-vector(length=ncol(RET_DAILY))

for(i in 1:ncol(RET_DAILY))
{
GHYP<-fit.ghypuv(RET_DAILY[TIME,i],silent=TRUE)
STUDENT<-fit.tuv(RET_DAILY[TIME,i],silent=TRUE)
BEST<-stepAIC.ghyp(RET_DAILY[TIME,i],dist = c("ghyp", "hyp", "NIG", "t","gauss"),silent=TRUE)$best.model
LOG_RATIO_STUDENT[i]<-lik.ratio.test(GHYP, STUDENT, conf.level = 0.95)$p.value
LOG_RATIO_BEST[i]<-lik.ratio.test(GHYP, BEST, conf.level = 0.95)$p.value
AIC_GHYP[i]<-AIC(GHYP)
AIC_STUDENT[i]<-AIC(STUDENT)
AIC_BEST[i]<-AIC(BEST)
}
RESULT<-cbind(LOG_RATIO_STUDENT,LOG_RATIO_BEST,AIC_GHYP,AIC_STUDENT,AIC_BEST)
RESULT

                                # ВЫБОР МОДЕЛИ ПО ИНФОРМАЦИОННОМУ КРИТЕРИЮ
m<-4
GHYP<-fit.ghypuv(RET_DAILY[TIME,m],silent=TRUE)
BEST<-stepAIC.ghyp(RET_DAILY[TIME,m],dist = c("ghyp", "hyp", "NIG", "t","gauss"),silent=TRUE)$best.model
BEST

