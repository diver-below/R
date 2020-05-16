install.packages("ghyp")
install.packages("copula")
install.packages("tseries")
library(ghyp)
library(copula)
library(tseries)


                                                  # ВВОД ДАННЫХ

#CLOSE_DAILY<-cbind(AAPL[,3],AMD[,3],AXP[,3],CAT[,3],CSCO[,3],EBAY[,3],GLD[,3],JPM[,3],MCD[,3],NVDA[,3],SBUX[,3],
#                   TRV[,3],UNH[,3],XOM[,3])
RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY)) #дневные доходности
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY)) {RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1}
}
RET_DAILY

                                # МОДЕЛИРОВАНИЕ ПО ВЫБРАННОЙ ПАРЕ АКТИВОВ

Y<-RET_DAILY[c(1500:1600),c(2,7)]   # выбор пары активов и временного отрезка
Y

                               # моделирование в семействе многомерных ОГР

GHYP<-fit.ghypmv(Y,silent=TRUE);GHYP
F_emp<-vector(length=nrow(Y))
F_ghyp<-vector(length=nrow(Y))
for(i in 1:nrow(Y))
{
  F_emp[i]<-length(which((Y[,1]<Y[i,1])&(Y[,2]<Y[i,2])))/nrow(Y) # эмпирическая функция распределения
  F_ghyp[i]<-pghyp(c(Y[i,1],Y[i,2]),object=GHYP)                 # смоделированная фуекция распределения
}
F_emp;F_ghyp
                              
x<-c(1:nrow(Y))                   # графическая иллюстрация
plot(x,F_emp,type="l")
lines(x,F_ghyp,type="l",col="BLUE")

                                          # копула-моделирование
cdf<-pobs(Y)  # псевдонаблюдения. 

norm.cop <- normalCopula(dim = 2)             # объявление копулы
gumbel.cop<-gumbelCopula(dim=2)
t.cop<-tCopula(dim=2) 

normal_Copula<- fitCopula(norm.cop, data=cdf) # подгонка копулы
t_Copula<-fitCopula(t.cop, data=cdf)
gumbel_Copula<-fitCopula(gumbel.cop,data=cdf)

summary(normal_Copula)   # результаты подгонки
summary(t_Copula)
summary(gumbel_Copula)
normal_Copula@estimate   # извлечение слота - оценки параметра копулы
t_Copula@estimate[1]
gumbel_Copula@estimate

# смоделированные копулы
COPULA_MODEL_N<-normalCopula(normal_Copula@estimate)    
COPULA_MODEL_t<-tCopula(t_Copula@estimate[1],df=round(t_Copula@estimate[2]))     # чмсло степеней свободы приходится выбирать целым
COPULA_MODEL_G<-gumbelCopula(gumbel_Copula@estimate)

COPULA_MODEL_N
COPULA_MODEL_t
COPULA_MODEL_G

R1_mod<-fit.ghypuv(Y[,1],silent=TRUE)  # моделирование одномерных распределений в семействе ОГР
R2_mod<-fit.ghypuv(Y[,2],silent=TRUE)
R1_mod;R2_mod

F1<-pghyp(Y[,1],object=R1_mod)      # функции распределения смоделированных
F2<-pghyp(Y[,1],object=R1_mod)      # одномерных распределений
F1;F2

# моделирование двумерных распределений с помощью копул
F_copula_n<-vector(length=nrow(Y))
F_copula_t<-vector(length=nrow(Y))
F_copula_g<-vector(length=nrow(Y))

for(i in 1:nrow(Y))
{
  F_copula_n[i]<-pCopula(c(pghyp(Y[i,1],object=R1_mod),pghyp(Y[i,2],object=R2_mod)),COPULA_MODEL_N)
  F_copula_t[i]<-pCopula(c(pghyp(Y[i,1],object=R1_mod),pghyp(Y[i,2],object=R2_mod)),COPULA_MODEL_t)
  F_copula_g[i]<-pCopula(c(pghyp(Y[i,1],object=R1_mod),pghyp(Y[i,2],object=R2_mod)),COPULA_MODEL_G)
}

F_copula_n
F_copula_t
F_copula_g




                               # МОДЕЛИРОВАНИЕ ПО БОЛЬШОМУ КОЛИЧЕСТВУ АКТИВОВ



set<-matrix(nrow=2,ncol=28)
set<-cbind(c(3,4),c(3,5),c(3,6),c(3,7),c(3,8),c(3,9),c(3,10),c(4,5),c(4,6),c(4,7),c(4,8),c(4,9),c(4,10),
           c(5,6),c(5,7),c(5,8),c(5,9),c(5,10),c(6,7),c(6,8),c(6,9),c(6,10),c(7,8),c(7,9),c(7,10),c(8,9),
           c(8,10),c(9,10))
set

diff_ghyp<-vector(length=ncol(set))        # отличие смоделированных функций распределения
                                           # от эмпирических, определяемое по методу
diff_copula_n<-vector(length=ncol(set))    # наименьших квадратов
diff_copula_t<-vector(length=ncol(set))
diff_copula_g<-vector(length=ncol(set))

active1<-vector(length=ncol(set))
active2<-vector(length=ncol(set))


# Начало цикла
for(j in 1:ncol(set))
{
rows<-c(1001:1100);T<-length(rows)
cols<-c(set[1,j],set[2,j])

RETURNS<-RET_DAILY[rows,cols]
RETURNS
                                      # МОДЕЛИРОВАНИЕ МНОГОМЕРНЫХ РАСПРЕДЕЛЕНИЙ

#NORMAL<-fit.gaussmv(RETURNS);NORMAL
#STUDENT<-fit.tmv(RETURNS,silent=TRUE);STUDENT
GHYP<-fit.ghypmv(RETURNS,silent=TRUE);GHYP


                                                # ФОРМИРОВАНИЕ КОПУЛЫ

cdf<-pobs(RETURNS)  # псевдонаблюдения. 

norm.cop <- normalCopula(dim = 2)
gumbel.cop<-gumbelCopula(dim=2)
t.cop<-tCopula(dim=2)
normal_Copula<- fitCopula(norm.cop, data=cdf)
t_Copula<-fitCopula(t.cop, data=cdf)
gumbel_Copula<-fitCopula(gumbel.cop,data=cdf)
gumbel_Copula@estimate

summary(normal_Copula)
summary(t_Copula)
normal_Copula@estimate
t_Copula@estimate[1]


COPULA_MODEL_N<-normalCopula(normal_Copula@estimate)    
COPULA_MODEL_t<-tCopula(t_Copula@estimate[1],df=round(t_Copula@estimate[2]))     # чмсло степеней свободы приходится выбирать целым
COPULA_MODEL_G<-gumbelCopula(gumbel_Copula@estimate)

COPULA_MODEL_N
COPULA_MODEL_t
COPULA_MODEL_G

                                      # МОДЕЛИРОВАНИЕ ОДНОМЕРНЫХ РАСПРЕДЕЛЕНИЙ


R1_mod<-fit.ghypuv(RETURNS[,1],silent=TRUE)
R2_mod<-fit.ghypuv(RETURNS[,2],silent=TRUE)


F1<-vector(length=nrow(RETURNS))
F2<-vector(length=nrow(RETURNS))

for(i in 1:length(F1))
{
  F1[i]<-pghyp(RETURNS[i,1],object=R1_mod)
  F2[i]<-pghyp(RETURNS[i,2],object=R1_mod)
 
}
F1;F2

                                            # ФУНКЦИИ РАСПРЕДЕЛЕНИЙ

F_emp<-vector(length=T) 
F_ghyp<-vector(length=T)
F_copula_n<-vector(length=T)        
F_copula_t<-vector(length=T)
F_copula_g<-vector(length=T)

for(i in 1:T)
{
  F_emp[i]<-length(which((RETURNS[,1]<RETURNS[i,1])&(RETURNS[,2]<RETURNS[i,2])))/T
  F_ghyp[i]<-pghyp(c(RETURNS[i,1],RETURNS[i,2]),object=GHYP)
  F_copula_n[i]<-pCopula(c(F1[i],F2[i]),COPULA_MODEL_N)
  F_copula_t[i]<-pCopula(c(F1[i],F2[i]),COPULA_MODEL_t)
  F_copula_g[i]<-pCopula(c(F1[i],F2[i]),COPULA_MODEL_G)
}
F_emp
F_ghyp
#F_t
F_copula_n
F_copula_t
F_copula_g

for(i in 1:T)
{
  F_copula_n[i]<-pCopula(c(pghyp(RETURNS[i,1],object=R1_mod),pghyp(RETURNS[i,2],object=R2_mod)),COPULA_MODEL_N)
  F_copula_t[i]<-pCopula(c(pghyp(RETURNS[i,1],object=R1_mod),pghyp(RETURNS[i,2],object=R2_mod)),COPULA_MODEL_t)
  F_copula_g[i]<-pCopula(c(pghyp(RETURNS[i,1],object=R1_mod),pghyp(RETURNS[i,2],object=R2_mod)),COPULA_MODEL_G)
}

F_copula_n
F_copula_t
F_copula_g

                                          # ОЦЕНКИ КАЧЕСТВА МОДЕЛИРОВАНИЯ

diff_ghyp[j]<-sum(F_emp-F_ghyp)^2
#diff_t<-sum(F_emp-F_t)^2
diff_copula_n[j]<-sum(F_emp-F_copula_n)^2
diff_copula_t[j]<-sum(F_emp-F_copula_t)^2
diff_copula_g[j]<-sum(F_emp-F_copula_g)^2


active1[j]<-set[1,j]
active2[j]<-set[2,j]
}

diff_ghyp
#diff_t
diff_copula_n
diff_copula_t
diff_copula_g

RESULT<-cbind(active1,active2,diff_ghyp,diff_copula_n,diff_copula_t,diff_copula_g)
RESULT

MIN_DIF<-vector(length=nrow(RESULT))  # определяет столбец с минимальным различием между эмпирической
                                      # и смоделированной функциями распределениями
for(i in 1:nrow(RESULT))
{
  MIN_DIF[i]<-which.min(RESULT[i,])
}
MIN_DIF
ITOG<-cbind(RESULT,MIN_DIF);ITOG     
