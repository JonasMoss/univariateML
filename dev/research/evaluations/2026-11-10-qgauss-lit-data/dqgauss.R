dqgauss <- function(x,q=0,mu=0,sig=1) {
qv <- q
if(qv >=3 || qv == 1) print("q value must be < 3 or != 1)")
else{
Z<-(qv-1)/(3-qv)
X<-1+Z/(sig^2)*(x-mu)^2
aq<-function(qv)
ifelse(qv>1,1/Z,1-1/Z)
sqrt(abs(Z)/X^(1+1/Z))/(beta(aq(qv)/2,1/2)*sig)
}
}
