                                    # СГЕНЕРИРОВАННОЕ БРОУНОВСКОЕ ДВИЖЕНИЕ

Y<-cumsum(rnorm(10000))
h_u_norm<-vector(length=H)
h_d_norm<-vector(length=H)
sigma_norm<-vector(length=H)

                                            # моделирование скачков

H<-20                                   # диапазон длины шага
for(h in 1:H)
{
  NORM_h<-diff(Y,h)                            # "доходности" при длине шага h
  NORM_PLUS_h<-NORM_h[which(NORM_h>=0)]        # наблюдения с положительными доходностями
  NORM_MINUS_h<-NORM_h[which(NORM_h<=0)]       # наблюдения с отрицательными доходностями
  h_u_norm[h]<-mean(NORM_PLUS_h)               # моделирование скачка вверх
  h_d_norm[h]<-mean(NORM_MINUS_h)              # моделирование скачка вниз
  sigma_norm[h]<-sd(NORM_h)                    # выборочная дисперсия
}
h_u_norm
h_d_norm
sigma_norm

                                                  # регрессии

summary(lm(h_u_norm~sigma_norm))
multiply_u<-lm(h_u_norm~sigma_norm)$coef[2]
summary(lm(h_d_norm~sigma_norm))
multiply_d<-lm(h_d_norm~sigma_norm)$coef[2]
summary(lm(h_u_norm~sigma_norm))

multiply_theoretical<-(2/pi)^0.5
h_u_norm-uu*sigma_norm
h_d_norm+uu*sigma_norm
multiply_u/multiply_theoretical
multiply_d/multiply_theoretical

                                                # РЕАЛЬНЫЕ ДАННЫЕ

library("alabama")
time<-c(1001:1200)                      # выбор временного промежутка
PRICES<-CLOSE_DAILY[time,]

H<-20                                   # диапазон длины шага

h_u<-matrix(nrow=H,ncol=ncol(PRICES))   # скачок вверх в биномиальной модели
h_d<-matrix(nrow=H,ncol=ncol(PRICES))   # скачок вниз в биномиальной модели
h_sigma<-matrix(nrow=H,ncol=ncol(PRICES))
for(h in 1:H)
{
  RET_h<-diff(log(PRICES),h)
  for(j in 1:ncol(PRICES))
  {
    ACTIVE_j<-RET_h[,j]                     # аналогично сгенерированным броуновским
    PLUS_j<-ACTIVE_j[which(ACTIVE_j>=0)]
    MINUS_j<-ACTIVE_j[which(ACTIVE_j<=0)]
    h_u[h,j]<-mean(PLUS_j)
    h_d[h,j]<-mean(MINUS_j)
    h_sigma[h,j]<-sd(RET_h[,j])
  }
}
plot(step,h_u[,m],type="p")
plot(step,h_d[,m],type="p")
h_d
h_sigma


m<-2                             # выбор актива для просмотра результатов моделирования
h_u_m<-h_u[,m]
h_d_m<-h_d[,m]
h_sigma_m<-h_sigma[,m]
step<-c(1:H)
step_05<-step^0.5


                                                # регрессии
summary(lm(h_u_m~step_05))
coefs_u<-lm(h_u_m~step_05)$coef
summary(lm(h_d_m~step_05))
coefs_d<-lm(h_d_m~step_05)$coef
summary(lm(h_u_m~h_sigma_m))     
summary(lm(h_d_m~h_sigma_m)) 

# подбор аппроксимирующей степенной функции

                                              # скачки вверх
f_u<-function(x)
{
  x1<-x[1]                           # аддитивный свободный член
  x2<-x[2]                           # множитель при степени
  x3<-x[3]                           # показатель степени
  sum((h_u_m-x1-x2*step^x3)^2)       # сумма квадратов отклонений - минимизируемая величина
}

x1_u<-coefs_u[1]                     # значения взятые из регрессии
x2_u<-coefs_u[2]
optim(c(x1_u,x2_u,0.5),f_u,  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                        "Brent"))
param_u<-optim(c(x1_u,x2_u,0.5),f_u,  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                         "Brent"))$par
param_u                              # оптимальные параметры по методу наименьших квадратов
plot(step,h_u_m)                     # графическая иллюстрация
lines(step,param_u[1]+param_u[2]*step^param_u[3],type="l",col="BLUE")
lines(step,coefs_u[1]+coefs_u[2]*step^0.5,type="l",col="RED")

                                            # cкачки вниз
# все действия аналогичны предыдущим
f_d<-function(y)
{
  y1<-y[1]
  y2<-y[2]
  y3<-y[3]
  sum((h_d_m-y1-y2*step^y3)^2)
}


y1_d<-coefs_d[1]
y2_d<-coefs_d[2]
optim(c(y1_d,y2_d,0.5),f_d,  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                        "Brent"))
param_d<-optim(c(y1_d,y2_d,-0.5),f_d,  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                                                 "Brent"))$par
param_d
plot(step,h_d_m)
lines(step,param_d[1]+param_d[2]*step^param_d[3],type="l",col="BLUE")
lines(step,coefs_d[1]+coefs_d[2]*step^0.5,type="l",col="RED")



