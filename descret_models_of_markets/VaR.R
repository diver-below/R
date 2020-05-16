library("ghyp")

RET_DAILY<-diff(log(CLOSE_DAILY),1)
t0<-800      # âûáîð íà÷àëüíîé äàòû
T_train<-100 # äëèíà îáó÷àþùåé âûáîðêè
T_exam<-50  # äëèíà ýêçàìåíóþùåé âûáîðêè

m<-8         # âûáîð àêòèâà
train<-RET_DAILY[c((t0+1):(t0+T_train)),m]
exam<-RET_DAILY[c((t0+T_train+1):(t0+T_train+T_exam)),m]

# ÏÐÎÑÒÀß VaR

alpha<-0.1
VaR_simple_emp<-quantile(train,alpha)
SHORTFALL_train_emp<-length(train[train<VaR_simple_emp])
ghyp_train<-fit.ghypuv(train,silent=TRUE)
best_model_train<-stepAIC.ghyp(train,dist=c("ghyp","hyp","t","NIG","gauss"),silent=T)$best.model
VaR_simple_model<-qghyp(alpha,object=best_model_train)
SHORTFALL_train_model<-length(train[train<VaR_simple_model])

SHORTFALL_train_emp/length(train)
SHORTFALL_train_model/length(train)

plot(exam,type="p",main="Simple VaR")
abline(h=VaR_simple_emp,col="BLUE")
abline(h=VaR_simple_model,col="RED")

SHORTFALL_exam_emp<-length(exam[exam<VaR_simple_emp])
SHORTFALL_exam_model<-length(exam[exam<VaR_simple_model])
SHORTFALL_exam_emp/length(exam)
SHORTFALL_exam_model/length(exam)

cbind(SHORTFALL_train_emp/length(train),SHORTFALL_train_model/length(train),SHORTFALL_exam_emp/length(exam),
      SHORTFALL_exam_model/length(exam))

# ôóíêöèÿ Êóïèêà
Cupic<-function(x,T,M)
{
  2*log((1-x)^(T-M)*x^M)
}
alpha_emp<-SHORTFALL_exam_emp/length(exam)
alpha_model<-SHORTFALL_exam_model/length(exam)

Cupic_emp<-Cupic(alpha_emp,length(exam),SHORTFALL_exam_emp)-Cupic(alpha,length(exam),SHORTFALL_exam_emp)
Cupic_model<-Cupic(alpha_model,length(exam),SHORTFALL_exam_model)-Cupic(alpha,length(exam),SHORTFALL_exam_model)

Cupic_emp_test<-1 - pchisq(Cupic_emp, 1)
Cupic_emp_test

Cupic_model_test<-1-pchisq(Cupic_model, 1)
Cupic_model_test

                                                # ÊÐÈÂÀß VaR
serie<-c(train,exam)

VaR_curve_emp<-vector(length=T_exam)
VaR_curve_ghyp<-vector(length=T_exam)
VaR_curve_model<-vector(length=T_exam)
for(t in (1:T_exam))
{
  TRAIN<-serie[t:(t+T_train)]
  move_model_ghyp<-fit.ghypuv(TRAIN,silent=T)
  best_move_model<-stepAIC.ghyp(TRAIN,dist=c("ghyp","hyp","t","NIG","gauss"),silent=T)$best.model
  
  VaR_curve_emp[t]<-quantile(TRAIN,alpha)
  VaR_curve_ghyp[t]<-qghyp(alpha,move_model_ghyp)
  VaR_curve_model[t]<-qghyp(alpha,best_move_model)
}
plot(exam,type="l")
lines(VaR_curve_emp,type="l",col="BLUE")
lines(VaR_curve_ghyp,type="l",col="GREEN")
lines(VaR_curve_model,type="l",col="RED")

                          # ÑÐÀÂÍÅÍÈÅ ÓÑÒÎÉ×ÈÂÎÑÒÈ ÝÌÏÈÐÈ×ÅÑÊÎÃÎ È ÌÎÄÅËÈÐÎÂÀÍÍÎÃÎ VaR

alpha<-0.1
m1<-13
SUBSERIES<-array(dim=c(100,20))
for(k in 1:20)
{
  SUBSERIES[,k]<-RET_DAILY[(100*(k-1)+1):(100*k)]
}
SUBSERIES
VaR_emp<-vector(length=20)
VaR_model<-vector(length=20)
for(k in 1:20)
{
  VaR_emp[k]<-quantile(SUBSERIES[,k],alpha)
  GHYP<-fit.ghypuv(SUBSERIES[,k],silent=TRUE)
  VaR_model[k]<-qghyp(alpha,GHYP)
}
VAR<-cbind(VaR_emp,VaR_model)
VAR
sd(VaR_emp)
sd(VaR_model,na.rm=T)
  