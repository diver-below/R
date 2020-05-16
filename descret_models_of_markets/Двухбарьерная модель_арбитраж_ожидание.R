

step<-1                                 # выбор шага (количество дней)
RET_DAILY<-diff(log(CLOSE_DAILY),step)  # логарифмические доходности

# параметры эксперимента
t0<-900                                 # выбор начальной даты
T<-200                                  # длина обучающей выборки (прошлое)
tau<-200                                # длина экзаменующей выборки (будущее)
l<-7                                    # выбор актива

Y_train<-RET_DAILY[(t0-T):t0,l]
S_exam<-CLOSE_DAILY[(t0+1):(t0+tau),l]
Y_train;S_exam
ks_test<-ks.test(Y_train,Y_exam)$p.value;ks_test   # проверка однородности обучающей и экзаменующей выборок
                                                   # если p_value<0.1, рекомендуется изменить параметры 
                                                   # эксперимента
h_u<-mean(Y_train[which(Y_train>0)])
h_d<-mean(Y_train[which(Y_train<0)])
h_u;h_d
h_exp<-(h_u-h_d)/2;h_exp                # величина скачка

S0<-CLOSE_DAILY[t0,l]                   # начальная цена
m_exp<-4                                    # расстояние до левого барьера
n_exp<-4                                    # расстояние до правого барьера
r<-0.05/240*step                        # безрисковая ставка
K_exp<-S0*exp(-(m_exp+1)*h)                        # цена исполнения опциона
K_exp
S0-K_exp
p_mart<-(exp(r)-exp(-h))/(exp(h)-exp(-h))    # мера выбирается мартингальной (риск-нейтральной)
Su_exp<-S0*(exp(n*h_exp))
Sd_exp<-S0*(exp(-m*h_exp))

C_opt<-function(S,Su,Sd,K,h,p,r)        # функция цены опциона
{
  m<-(1/h)*log(S/Sd)
  n<-(1/h)*log(Su/S)
  L1<-(1+(1-4*p*(1-p)*exp(-2*r))^0.5)/(2*p*exp(-2*r))
  L2<-(1-(1-4*p*(1-p)*exp(-2*r))^0.5)/(2*p*exp(-2*r))
  U_left<-((1-p)/p)^m*(L1^n-L2^n)/(L1^(m+n)-L2^(m+n))
  U_right<-(L1^m-L2^m)/(L1^(m+n)-L2^(m+n))
  return((Sd-K)*U_left+(Su-K)*U_right)
}

# результат, наблюдаемый на экзаменующей выборке
C_opt_mod<-C_opt(S0,Su_exp,Sd_exp,K_exp,h_exp,p_mart,r)

R<-which(S_exam>Su_exp)[1]            # момент достижения правого барьера
L<-which(S_exam<Sd_exp)[1]            # момент достижения левого барьера
R;L
NUMBER<-min(R,L);NUMBER               # момент реализации опциона
C_opt_RESULT<-S_exam[NUMBER]-K_exp
RESULT<-C_opt_RESULT-C_opt_mod        # результат инвестиции
RESULT

                        # ЗАВИСИМОСТЬ ЦЕНЫ ОПЦИОНА ОТ РАССТОЯНИЙ ДО БАРЬЕРОВ
x<-seq(from=h_exp,to=m*h_exp,by=0.1*h_exp)
K_opt<-S0*exp(-(m+1)*h_exp)
C_opt_boards<-vector(length=length(x))
for(i in 1:length(x))
{
  C_opt_boards[i]<-C_opt(S0,S0*exp(x[i]),S0*exp(-x[i]),K_opt,h_exp,p_mart,r)
}
C_opt_boards
plot(x,C_opt_boards)

                          # ЗАВИСИМОСТЬ ЦЕНЫ ОПЦИОНА ОТ ЦЕНЫ ИСПОЛНЕНИЯ
y<-seq(from=0,to=3,by=0.1)
C_opt_K<-vector(length=length(y))
for(i in 1:length(y))
{
  C_opt_K[i]<-C_opt(S0,Su_exp,Sd_exp,Sd_exp-y[i]*h_exp,h_exp,p_mart,r)
}
C_opt_K
plot(y,C_opt_K)
