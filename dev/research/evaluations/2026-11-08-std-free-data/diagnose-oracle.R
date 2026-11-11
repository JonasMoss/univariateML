pkgload::load_all("univariateML-r",quiet=TRUE)
held_values <- c(mu=.2,sigma=1.3,df=3)
samples <- list(two=c(-1,2),normal=qnorm((1:20-.5)/20),uniform=seq(-1,1,length.out=20))
for(id in names(samples))for(mask in 1:7) {
 x <- samples[[id]]
 held <- names(held_values)[as.logical(intToBits(mask)[1:3])]
 f <- tryCatch(suppressWarnings(do.call(mllst,c(list(x=x),as.list(held_values[held])))),error=identity)
 if(inherits(f,"error")){cat(id,mask,"STOP",conditionMessage(f),"\n");next}
 free <- setdiff(names(held_values),held)
 if(!length(free))next
 encode <- function(p){z<-c(p[1],log(p[2]),log(max(p[3]-1,1e-6)));names(z)<-names(p);z[free]}
 decode <- function(z){p<-held_values;for(j in seq_along(z)){k<-match(free[j],names(p));p[k]<-if(k==1)z[j]else if(k==2)exp(z[j])else 1+exp(z[j])};p}
 objective <- function(z){if(any(!is.finite(z))||any(abs(z)>30))return(1e100);v<-sum(stats::dt((x-decode(z)[1])/decode(z)[2],df=decode(z)[3],log=TRUE)-log(decode(z)[2]));if(is.finite(v))-v else 1e100}
 best <- NULL
 for(shift in c(-1,0,1)){
  start<-held_values;start[1]<-median(x)+shift;start[2]<-max(.2,sd(x))*exp(shift/2);start[held]<-held_values[held]
  o<-suppressWarnings(optim(encode(start),objective,method="Nelder-Mead",control=list(maxit=1800,reltol=1e-11)))
  if(is.null(best)||o$value<best$value)best<-o
 }
 normal_mu<-if("mu"%in%held)held_values["mu"] else mean(x)
 normal_sigma<-if("sigma"%in%held)held_values["sigma"] else sqrt(mean((x-normal_mu)^2))
 cat(id,mask,"fit",format(as.numeric(f),digits=17),"LL",format(as.numeric(logLik(f)),digits=17),"cert",attr(f,"certified"),"oracle",format(decode(best$par),digits=17),"LL",format(-best$value,digits=17),"normalLL",format(sum(dnorm(x,normal_mu,normal_sigma,log=TRUE)),digits=17),"\n")
}
