cqgauss <- function(p,q=0,mu=0,sig=1,lower.tail=TRUE){
Q <- p
qv <- q
if(qv >= 3 || qv == 1 ) stop("q value must be < 3 or != 1")
if(Q > 1 || Q <0  ) stop("Quantile must to be between [0,1]))")
Z<-(qv-1)/(3-qv)
x <- array(seq_len(length(Q)))
#library(zipfR) # library de Rbeta
if (lower.tail == TRUE) {
n<- -1
}else{
if (lower.tail == FALSE) n<-0
}
if (qv < 1){ # for Compact Support
a <- 1-1/Z
for(i in seq_len(length(Q))){
if(Q[i]<.5){
x[i] <- mu + (-1)^n*sqrt( (1-Rbeta.inv(2*Q[i],a/2,1/2))*sig^2/(-Z)  )
}else{
x[i] <- mu - (-1)^n*sqrt( (1-Rbeta.inv(2*(1-Q[i]),a/2,1/2))*sig^2/(-Z)  )
}
}
}else{ # for Heavy tail
a <- 1/Z
for(i in seq_len(length(Q))){
if(Q[i]<.5){
x[i] <- mu + (-1)^n*sqrt(  (1/Rbeta.inv(2*Q[i],a/2,1/2)-1)*sig^2/Z  )
}else{
x[i] <- mu - (-1)^n*sqrt(  (1/Rbeta.inv(2*(1-Q[i]),a/2,1/2)-1)*sig^2/Z  )
} 
}
}
return(x) 
}
