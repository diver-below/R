install.package(MASS)
install.package(alabama)
library("MASS")
library("alabama")

                      # В КАЧЕСТВЕ ОПЫТНЫХ ДАННЫХ ВЫБЕРЕМ ПАРАМЕТРЫ РЕАЛЬНЫХ ЦЕНОВЫХ РЯДОВ

RET_DAILY<-diff(log(CLOSE_DAILY),1)
m<-vector(length=ncol(CLOSE_DAILY))
for(j in 1:length(m)) {m[j]<-mean(RET_DAILY[,j])}
Q<-matrix(nrow=ncol(CLOSE_DAILY),ncol=ncol(CLOSE_DAILY))
for(i in 1:nrow(Q))
{
  for(j in 1:ncol(Q)) {Q[i,j]<-cov(RET_DAILY[,i],RET_DAILY[,j])}
}
Q                   # матрица ковариации и вектор доходности, используемые для постановки задачи
m                   # оптимизации

tau<-seq(from=0.02,to=0.08,by=0.005)          # толерантность к риску
cc<-seq(from=0,to=0.0003,by=0.0001)           # транзакционные издержки
port<-array(dim=c(length(tau),length(m),length(cc)))


for(i in 1:length(cc))
{
  for(k in 1:length(tau))
  {
  w<-vector(length=length(m))
  V<-function(w)
    {
    -tau[k]*(sum(w*m)-2*cc[i]*sum(abs(w)))+0.5*w%*%Q%*%w
    }
  heq<-function(w) {sum(w)-1}
  port[k,,i]<-constrOptim.nl(par=rep(1/length(m),length(m)),fn=V,heq=heq)$par
  }
}
port     # оптимальный портфель при различных значениях толерантности к риску и транзакционных издержек




          # ДОХОДНОСТЬ И РИСК ОПТИМАЛЬНОГО ПОРТФЕЛЯ ПРИ РАЗЛИЧНЫХ ЗНАЧЕНИЯХ ТОЛЕРАНТНОСТИ К РИСКУ И
                                           # ТРАНЗАКЦИОННЫХ ИЗДЕРЖЕК

RETURN_PORT<-matrix(nrow=length(tau),ncol=length(cc))
row.names(RETURN_PORT)<-c("tau=0.020","tau=0.025","tau=0.030","tau=0.035","tau=0.040","tau=0.045","tau=0.050",
                          "tau=0.055","tau=0.060","tau=0.065","tau=0.070","tau=0.075","tau=0.080")
colnames(RETURN_PORT)<-c("costs=0.0000","costs=0.0001","costs=0.0002","costs=0.0003")

RISK_PORT<-matrix(nrow=length(tau),ncol=length(cc))
row.names(RISK_PORT)<-row.names(RETURN_PORT)
colnames(RISK_PORT)<-colnames(RETURN_PORT)

for(k in 1:length(tau))
{
  for(i in 1:length(cc))
  {
    RETURN_PORT[k,i]<-port[k,,i]%*%m-2*cc[i]*sum(abs(port[k,,i]))
    RISK_PORT[k,i]<-(port[k,,i]%*%Q%*%port[k,,i])^0.5
  }
}
RETURN_PORT                         
RISK_PORT

plot(RISK_PORT[,1],RETURN_PORT[,1],type="l")
plot(RISK_PORT[,2],RETURN_PORT[,2],type="l")
plot(RISK_PORT[,3],RETURN_PORT[,3],type="l")
plot(RISK_PORT[,4],RETURN_PORT[,4],type="l")



               # НЕПОСРЕДСТВЕННОЕ НАХОЖДЕНИЕ ПОРТФЕЛЯ В ОТСУТСТВИЕ ТРАНЗАКЦИОННЫХ ИЗДЕРЖЕК

I<-rep(1,length(m))

A<-m%*%solve(Q)%*%m   # константы Марковица
B<-sum(solve(Q)%*%m)
C<-sum(solve(Q))

PORT<-matrix(nrow=length(tau),ncol=length(m))            # портфель 
for(k in 1:length(tau))
{
PORT[k,]<-tau[k]*solve(Q)%*%(m-B/C*I)+(1/C)*solve(Q)%*%I
}
PORT                                             # точное значение портфеля в отсутствии
                                                 # транзакционных издержек


DIFF<-sum(abs(PORT-port[,,1]));DIFF              # разница между результатом оптимизации и
                                                 # точным значением
RET_WITHOUT_COSTS<-vector(length=length(tau))
RISK_WITHOUT_COSTS<-vector(length=length(tau))
for(k in 1:length(tau))
{
  RET_WITHOUT_COSTS[k]<-tau[k]*(A-B^2/C)+B/C
  RISK_WITHOUT_COSTS[k]<-(tau[k]^2*(A-B^2/C)+1/C)^0.5
}

RETURN_PORT[,1]-RET_WITHOUT_COSTS                # разница в доходностях и риске оптимизированного
RISK_PORT[,1]-RISK_WITHOUT_COSTS                 # и точного портфеля
