install.packages("fBasics")
library("fBasics")

                                        #  � � � �       � � � � � �


RET_DAILY<-matrix(nrow=nrow(CLOSE_DAILY)-1,ncol=ncol(CLOSE_DAILY)) #������� ����������
colnames(RET_DAILY)<-c("AAPL","AMD","AXP","CAT","CSCO","EBAY","GLD","JPM","MCD","NVDA","SBUX","TPV","UNH","XOM")
for(j in 1:ncol(RET_DAILY)) 
{
  for(t in 1:nrow(RET_DAILY)) {RET_DAILY[t,j]<-CLOSE_DAILY[t+1,j]/CLOSE_DAILY[t,j]-1}
}
Rf<-0.02/240


                                  #� � � � � � �         � � � � � � � � �

t0<-2600 # ����� ���� ��������������. � ���� ����� ���� �������� ����������, ������ ��� �������� ������� ��� ���.
benchmark<-c(-2.877798e-04,  4.859162e-02,  5.135517e-04,  2.036851e-04,  4.473733e-05,  2.502370e-03,  1.042033e-04,  4.642095e-03,
               1.821281e-04,  9.194339e-01,  9.409484e-05,  1.784419e-02,  6.314337e-03,  1.046861e-04)
benchmark             

mu_benchmark<-0.00812
sigma_benchmark<-0.019650


N<-(mu_benchmark-Rf)/sigma_benchmark^2;N

tau<-200
Q<-cov(RET_DAILY[(t0-tau):t0,]);Q
RF<-matrix(nrow=ncol(RET_DAILY),ncol=1)
for(i in 1:nrow(RF)) {RF[i]<-Rf}
RF

                # � � � � � � � � � � �        � � � � � � � �       � � � � � � � � � � �

dim(Q)
length(benchmark)
means_market<-N*Q%*%benchmark+RF
row.names(means_market)<-colnames(RET_DAILY)
colnames(means_market)<-"�������� ����������"
means_market

                      # � � � � � � � �        � � � � � � � � � �        � � � � � �


l<-3 # ���������� ���������� ������
P<-matrix(nrow=l,ncol=ncol(RET_DAILY)) # ��������� �������
P[1,]<-c(1,0,0,0,0,0,0,0,0,0,0,0,0,0)  # ���� ���������
P[2,]<-c(0,1,0,0,0,0,0,0,0,0,0,0,0,0)
P[3,]<-c(0,0,1,0,0,0,0,0,0,0,0,0,0,0)
q<-matrix(nrow=l,ncol=1)
q[1,1]<-0
q[2,1]<-0
q[3,1]<-0



                              # � � � � � �         � � � � �  -  � � � � � � � � �

E<-diag(nrow=ncol(RET_DAILY))
X<-rbind(E,P)
Y<-rbind(means_market,q)


sigma_market<-Q/100
sigma_expert<-P%*%Q%*%t(P)
null<-matrix(0,nrow=ncol(RET_DAILY),ncol=l)
OMEGA<-rbind(cbind(sigma_market,null),cbind(t(null),sigma_expert))
means_BL<-matrix(nrow=ncol(RET_DAILY),ncol=1)
means_BL<-solve(t(X)%*%solve(OMEGA)%*%X)%*%t(X)%*%solve(OMEGA)%*%Y
row.names(means_BL)<-colnames(RET_DAILY)
colnames(means_BL)<-"���������� �����-���������"
means_BL

ESTIMATE_COMPARE<-cbind(means_market,means_BL);ESTIMATE_COMPARE


                            #   � � � � � � � �       � � � � �  -  � � � � � � � � �

w_BL<-matrix(nrow=ncol(RET_DAILY),ncol=1)
w_BL<-solve(Q)%*%(means_BL-RF);w_BL
PORT_BL<-w_BL/sum(w_BL);PORT_BL
colnames(PORT_BL)<-"�������� �����-���������"
ESTIMATE_PORT<-cbind(benchmark,PORT_BL)
colnames(ESTIMATE_PORT)<-c("��������","�������� �����-���������")
ESTIMATE_PORT

                                    # � � � � � � � � �       � � � � � � � � �
test<-3
MARKET<-vector(length=test)
RETURNS<-matrix(nrow=test,ncol=ncol(RET_DAILY))
BL<-vector(length=test)
for(t in 1:test)
{
  RETURNS[t,]<-RET_DAILY[t0+t,]
  MARKET[t]<-sum(RET_DAILY[t,]*benchmark)
  BL[t]<-sum(RET_DAILY[t,]*w_BL)
}
RESULT<-cbind(MARKET,BL)
RESULT