
                                      # ЗАГРУЗКА СТАТИСТИЧЕСКОГО ПАКЕТА

install.packages(fBasics)
library("fBasics")


                                      # ЗАГРУЗКА ДАННЫХ ФИНАНСОВЫХ РЯДОВ

CLOSE_DAILY

                                       # ПОСТРОЕНИЕ РЯДОВ ДОХОДНОСТЕЙ

RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY))      #прямын дневные доходности
RET_LOG_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY))  # логарифмические дневные доходности
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
colnames(RET_LOG_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")

for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY))
  {
    RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1
    RET_LOG_DAILY[t,j]<-log(CLOSE_DAILY[t+1,j])-log(CLOSE_DAILY[t,j])
  }
}
summary(RET_DAILY/RET_LOG_DAILY)   # сравнение прямых и логарифмических доходностей


 
                                # ОБЗОР ГЛОБАЛЬНЫХ СВОЙСТВ РЯДОВ ДОХОДНОСТЕЙ

m<-7                            # выбор актива для статистического анализа

basicStats(RET_DAILY[,m])       # общая статистика

MAX<-max(RET_DAILY[,m])         # гистограмма и сравнение с нормальной кривой
MIN<-min(RET_DAILY[,m])
step<-(MAX-MIN)/1000
x<-seq(from=MIN,to=MAX,by=step)
hist(RET_DAILY[,m],breaks=100,probability=T,col="YELLOW")
dnormal<-function(x)
{
  dnorm(x,mean=mean(RET_DAILY[,m]),sd=sd(RET_DAILY[,m]))
}
lines(x,dnormal(x),type="l",col="BLUE")
shapiro.test(RET_DAILY[,m])     # тест на нормальность

                                # автокорреляционная функция

lag.max<-10
acf_function<-acf(RET_DAILY[,m],lag.max=lag.max,plot=F)[1:lag.max]
acf_function

alpha<-0.1                      # выбор уровня значимости
t_crit<-qt(p=alpha,df=length(RET_DAILY[,m])-2);t_crit
corr_crit<-(t_crit^2/((t_crit^2)+length(RET_DAILY[,m])-2))^0.5
corr_crit                       # критическое значение корреляции на выбранном уровне значимости



                                   # РАЗБИЕНИЕ ВРЕМЕННЫХ РЯДОВ НА ПОДВЫБОРКИ

length_ts<-150                                # выбираемая длина подвыборки
quant_ts<-floor(nrow(RET_DAILY)/length_ts)   # количество подвыборок
RETURNS<-RET_DAILY[1:(quant_ts*length_ts),]  # удаление "лишних" наблюдений (с конца)

SUBSERIES<-array(dim=c(length_ts,ncol(RETURNS),quant_ts))


for(k in 1:quant_ts)
{
  SUBSERIES[,,k]<-RETURNS[((k-1)*length_ts+1):(k*length_ts),]
}


                                        # ТЕСТИРОВАНИЕ НА СТАЦИОНАРНОСТЬ

length_train<-2*floor(length_ts/3)
length_exam<-length_ts-length_train
SUBSERIES_TRAIN<-array(dim=c(length_train,ncol(RETURNS),quant_ts))
SUBSERIES_EXAM<-array(dim=c(length_exam,ncol(RETURNS),quant_ts))
HOMOGENITY_TEST<-array(dim=c(quant_ts-1,ncol(RETURNS)))
for(k in 1:quant_ts)
{
  SUBSERIES_TRAIN[,,k]<-SUBSERIES[1:length_train,,k]
  SUBSERIES_EXAM[,,k]<-SUBSERIES[(length_train+1):length_ts,,k]
}
for(k in 1:(quant_ts-1))
{
  for(j in 1:ncol(RETURNS))
  {
    ifelse(length(unique(SUBSERIES[,j,k]))==length_ts,
            HOMOGENITY_TEST[k,j]<-ks.test(SUBSERIES_TRAIN[,j,k],SUBSERIES_EXAM[,j,k])$p.value,
            HOMOGENITY_TEST[k,j]<-NaN)    
  }
}
HOMOGENITY_TEST
HOMOGENITY_005<-vector(length=ncol(RETURNS))
HOMOGENITY_010<-vector(length=ncol(RETURNS))
for(j in 1:ncol(RETURNS))
{
  HOMOGENITY_005[j]<-length(which(HOMOGENITY_TEST[,j]>0.05))/length(HOMOGENITY_TEST[,j][!is.na(HOMOGENITY_TEST[,j])])
  HOMOGENITY_010[j]<-length(which(HOMOGENITY_TEST[,j]>0.10))/length(HOMOGENITY_TEST[,j][!is.na(HOMOGENITY_TEST[,j])])
}
HOMOGENITY_TEST_RESULT<-rbind(HOMOGENITY_005,HOMOGENITY_010)
row.names(HOMOGENITY_TEST_RESULT)<-c("ДОЛЯ ДЛЯ УЗ=0.05","ДОЛЯ ДЛЯ УЗ=0.10")
colnames(HOMOGENITY_TEST_RESULT)<-colnames(RET_DAILY)
HOMOGENITY_TEST_RESULT

                                        # ТЕСТИРОВАНИЕ НА НОРМАЛЬНОСТЬ

NORMAL_TEST<-array(dim=c(quant_ts,ncol(RETURNS)))

for(k in 1:quant_ts)
{
  for(j in 1:ncol(RETURNS))
  {
    NORMAL_TEST[k,j]<-shapiro.test(SUBSERIES[,j,k])$p.value
  }
}
NORMAL_TEST
hist(NORMAL_TEST[,3],breaks=20,probability=T,col="BLUE")


NORMAL_005<-vector(length=ncol(RETURNS))
NORMAL_010<-vector(length=ncol(RETURNS))
for(j in 1:ncol(RETURNS))
{
  NORMAL_005[j]<-length(which(NORMAL_TEST[,j]>0.05))/length(NORMAL_TEST[,j][!is.na(NORMAL_TEST[,j])])
  NORMAL_010[j]<-length(which(NORMAL_TEST[,j]>0.10))/length(NORMAL_TEST[,j][!is.na(NORMAL_TEST[,j])])
}
NORMAL_TEST_RESULT<-rbind(NORMAL_005,NORMAL_010)
row.names(NORMAL_TEST_RESULT)<-c("ДОЛЯ ДЛЯ УЗ=0.05","ДОЛЯ ДЛЯ УЗ=0.10")
colnames(NORMAL_TEST_RESULT)<-colnames(RET_DAILY)
NORMAL_TEST_RESULT

                              # КОЛИЧЕСТВЕННЫЕ ХАРАКТЕРИСТИКИ РАСПРЕДЕЛЕНИЙ

covar<-array(dim=c(ncol(RETURNS),ncol(RETURNS),quant_ts))
corr<-array(dim=c(ncol(RETURNS),ncol(RETURNS),quant_ts))
means<-matrix(nrow=quant_ts,ncol=ncol(RETURNS));colnames(means)<-colnames(RET_DAILY)
for(k in 1:quant_ts)
{
  covar[,,k]<-cov(SUBSERIES[,,k])
  corr[,,k]<-cor(SUBSERIES[,,k])
  means[k,]<-apply(SUBSERIES[,,k],2,mean)
}

                           # анализ вариабельности количественных характеристик

m1<-4                     # выбор
m2<-8                     # двух активов
mean1<-means[,m1]         # внутригрупповые средние
mean2<-means[,m2]
sd1<-covar[m1,m1,]^0.5    # внутригрупповые стандартные отклонения
sd2<-covar[m2,m2,]^0.5
cor_m1_m2<-corr[m1,m2,]   # внутригрупповые корреляции


RESULT<-matrix(nrow=2,ncol=5)
RESULT[1,]<-c(mean(mean1),mean(mean2),mean(sd1),mean(sd2),mean(cor_m1_m2))
RESULT[2,]<-c(sd(mean1),sd(mean2),sd(sd1),sd(sd2),sd(cor_m1_m2))
row.names(RESULT)<-c("СРЕДНЕЕ ЗНАЧЕНИЕ","СТАНДАРТНОЕ ОТКЛОНЕНИЕ")
colnames(RESULT)<-c("ДОХОДНОСТЬ 1","ДОХОДНОСТЬ 2","СТОТКЛ 1","СТОТКЛ 2","КОРРЕЛЯЦИЯ 1-2")
RESULT

                                         # разделение на тренд-флэт

STUD_TEST<-matrix(nrow=quant_ts,ncol=ncol(RETURNS))
colnames(STUD_TEST)<-colnames(RETURNS)
for(j in 1:ncol(RETURNS))
{
  for(k in 1:quant_ts)
  {
    STUD_TEST[k,j]<-t.test(SUBSERIES[,j,k])$p.value
  }
}
STUD_TEST
TREND_FLAT<-matrix(nrow=quant_ts,ncol=ncol(RETURNS))
colnames(TREND_FLAT)<-colnames(RETURNS)
for(j in 1:ncol(RETURNS))
{
  for(k in 1:quant_ts)
  {
    ifelse(STUD_TEST[k,j]<=0.1,TREND_FLAT[k,j]<-1,TREND_FLAT[k,j]<-0)
  }
}
TREND_FLAT

                                                  # график

plot(cumsum(SUBSERIES[,1,1]),type="l")

                                      # ТЕСТ НА КОШИ-РАСПРЕДЕЛЕНИЕ

s<-10
t1<-floor(runif(1,min=0,max=length(RET_DAILY[,s])))
t2<-floor(runif(1,min=0,max=length(RET_DAILY[,s])))
ifelse(t1<t2,time<-t1:t2,time<-t2:t1)
time

#TIME<-c(1:length(RET_DAILY[,s]))
ifelse(length(time)>=100,TIME<-time,TIME<-100)
TIME
Y<-unique(RET_DAILY[TIME,s]);Y

mu_normal<-mean(Y)
sd_normal<-sd(Y)
quant0_5<-quantile(Y,0.5)
quant0_8<-quantile(Y,0.8)
mu_cauchy<-quant0_5
gamma_cauchy<-(quant0_8-quant0_5)/tan(0.3*pi)
mu_cauchy
gamma_cauchy

                                        # графическая иллюстрация

MINIM<-min(Y);MAXIM<-max(Y)
MINIM;MAXIM
k<-100;step<-(MAXIM-MINIM)/k
t<-seq(from=MINIM,to=MAXIM,by=step);t
d_norm<-vector(length=length(t));d_cauchy<-vector(length=length(t))

for(i in 1:length(t))
{
d_norm[i]<-dnorm(t[i],mean=mu_normal,sd=sd_normal)
d_cauchy[i]<-dcauchy(t[i],location=mu_cauchy,scale=gamma_cauchy)
}
hist(Y,breaks<-length(Y)/20,probability=TRUE)
lines(t,d_norm,type="l",col="BLUE")
lines(t,d_cauchy,type="l",col="RED")

                                        # тест Колмогорова-Смирнова


ks.test(Y,pnorm(t,mean=mu_normal,sd=sd_normal))
ks.test(Y,pcauchy(t,location=mu_cauchy,scale=gamma_cauchy))








