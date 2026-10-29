set.seed(20261029)
B <- 10000L
sources <- list(
  dnorm_standard=function(n) extraDistr::rdnorm(n),
  dnorm_small_floor=function(n) extraDistr::rdnorm(n,4,.2),
  dgamma_default=function(n) extraDistr::rdgamma(n,2,1),
  dgamma_concentrated=function(n) extraDistr::rdgamma(n,400,100),
  rounded_normal_small=function(n) round(rnorm(n,3.5,.2)),
  poisson_small=function(n) rpois(n,.1))
out <- list()
for (name in names(sources)) for(n in c(20,100,1000)) {
  range <- replicate(B,diff(range(sources[[name]](n))))
  out[[length(out)+1L]] <- data.frame(source=name,n=n,replicates=B,point_mass=sum(range==0),adjacent=sum(range==1),finite=sum(range>=2))
}
result <- do.call(rbind,out)
write.csv(result,'/tmp/umvl-codex/dnormgamma/escape-frequency.csv',row.names=FALSE)
print(result,row.names=FALSE)
cat('7 log(.7) + 3 log(.3) =',7*log(.7)+3*log(.3),'\n')
# CLT path: mean M=k+1-sd*qnorm(p); shape=(M/sd)^2, rate=M/sd^2.
for (s in c(.1,.03,.01,.003)) {
  m <- 4-s*qnorm(.7); a <- (m/s)^2; r <- m/s^2
  p <- pgamma(4,a,rate=r)
  cat('gamma path sd',s,'shape',a,'rate',r,'p(lower cell)',p,'logLik',7*log(p)+3*log1p(-p),'\n')
}
