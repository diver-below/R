
RET_1<-diff(log(CLOSE_DAILY),1)    # ������� ���������� (��� ����� 1)


k<-7                                       # ����� ������
RET1<-RET_1[,k][!is.na(RET_1[,k])]
length(RET1)
#trend<-c(1:length(RET1))
#mu<-lm(RET1~trend)$coef[2]
#RET1<-RET1-mu*trend

diapason<-c(-0.1,0.1)                                      # ��������� �������� ��� �������� �����������
                                                           # ��� ���������� �����������
breaks<-seq(from=min(diapason),to=max(diapason),by=0.005)  # ��������� �����������
mids<-vector(length=length(breaks)-1)
for(i in 1:length(mids))
{
  mids[i]<-(breaks[i]+breaks[i+1])/2
}
mids                                                       # �������� ����������

f<-function(h,H)                                                # ������� �����������
{
  RET_h<-diff(log(CLOSE_DAILY),h)
  RETh<-RET_h[,k][!is.na(RET_h)]
  Zh<-RETh/(h^H)
  Z_1<-RET1[which(RET1<=max(diapason)&RET1>=min(diapason))]
 
  Z_h<-Zh[which(Zh<=max(diapason)&Zh>=min(diapason))]
 
  hist<-matrix(nrow=2,ncol=length(mids))
  
  hist[1,]<-hist(Z_1,breaks=breaks,plot=F)$counts
  hist[2,]<-hist(Z_h,breaks=breaks,plot=F)$counts
  return(hist)
}

h<-5                                   # ������� ���� (���������� - 5, ������)
DIF<-function(H)                       # ������� ���������� ����� ������������� (�� ������ ���������� ���������)
{
  RET_h<-diff(log(CLOSE_DAILY),h)
  RETh<-RET_h[,k][!is.na(RET_h)]
  Zh<-RETh/(h^H)
  Z_1<-RET1[which(RET1<=max(diapason)&RET1>=min(diapason))]
  
  Z_h<-Zh[which(Zh<=max(diapason)&Zh>=min(diapason))]
  
  hist<-matrix(nrow=2,ncol=length(mids))
  
  hist[1,]<-hist(Z_1,breaks=breaks,plot=F)$counts
  hist[2,]<-hist(Z_h,breaks=breaks,plot=F)$counts
  
  return(sum((hist[1,]-hist[2,])^2))
}

             # ����������� ������������ ������� (�� ����������� �������� ����� �������������)


HURST<-optimize(DIF,interval=c(0,1))$minimum # ����������� �������� ������������ ����������� (���������� ������)
HURST
DIF(0.5);DIF(HURST)  # ��������� ���������� ��� H=0.5 � H=HURST

Y1<-f(h,0.5)[1,];Y1                         # ������� �������
Y2_05<-f(h,0.5)[2,];Y2_05                  # �������, �������� ���������
Y2_HURST<-f(h,HURST)[2,];Y2_HURST 

plot(mids,Y1,type="l")                    # ����������� ����������� (���������� ��������� ������)
lines(mids,Y2_05,type="p",col="BLUE")
lines(mids,Y2_HURST,type="line",col="RED")

###############################################################################################################
install.packages(pracma)
library("pracma")

hurstexp(RET_1[,k])


S