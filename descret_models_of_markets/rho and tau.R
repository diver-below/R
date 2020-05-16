install.packages(MASS)
install.packages(copula)  
library("MASS")
library("copula")

mu<-c(1,2)
Q<-cbind(c(8,4),c(4,9))
NORM<-mvrnorm(20,mu,Q)
NORM
r<-cor(NORM[,1],NORM[,2]);r

normcop<-normalCopula(dim=2)
pobs<-pobs(NORM)
norm.cop<-fitCopula(normcop,pobs)
norm.cop

tcop<-tCopula(dim=2)
t.cop<-fitCopula(tcop,pobs)
t.cop

gumbelcop<-gumbelCopula(dim=2)
gumbel.cop<-fitCopula(gumbelcop,pobs)
gumbel.cop

claytoncop<-claytonCopula(dim=2)
clayton.cop<-fitCopula(claytoncop,pobs)
clayton.cop


