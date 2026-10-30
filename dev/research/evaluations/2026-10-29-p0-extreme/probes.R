# Run from repository root: Rscript dev/research/evaluations/2026-10-29-p0-extreme/probes.R
library(extraDistr)
out <- 'dev/research/evaluations/2026-10-29-p0-extreme'; dir.create(out,recursive=TRUE,showWarnings=FALSE)
x <- c(1,2,3,4,5); e <- 10^-(1:9)
# Paths stay strictly inside the reference support.
paths <- data.frame(epsilon=e,
 gev_collapse=vapply(e,function(t) sum(dgev(x,mu=1,sigma=5*t,xi=5,log=TRUE)),0.0),
 gpd_collapse=vapply(e,function(t) sum(dgpd(x,mu=1-t,sigma=t,xi=5,log=TRUE)),0.0),
 frechet_collapse=vapply(e,function(t) sum(dfrechet(x,lambda=.2,mu=1-t,sigma=t,log=TRUE)),0.0),
 gpd_endpoint=vapply(e,function(t) sum(dgpd(x,mu=0,sigma=10+t,xi=-2,log=TRUE)),0.0),
 gev_endpoint=vapply(e,function(t) sum(dgev(x,mu=0,sigma=10+t,xi=-2,log=TRUE)),0.0),
 gpd_uniform=vapply(e,function(t) sum(dgpd(x,mu=0,sigma=5+t,xi=-1,log=TRUE)),0.0))
write.csv(paths,file.path(out,'paths.csv'),row.names=FALSE)
print(paths)
cat('Reference endpoint densities at xi=-1 (GPD, GEV):',dgpd(5,0,5,-1),dgev(5,0,5,-1),'\n')
cat('Reference accepts lambda=.2:',dfrechet(1,.2,0,1),'GEV xi=2:',dgev(1,0,1,2),'GPD xi=-2:',dgpd(.1,0,1,-2),'\n')
# With fixed scale, slowly vary shape and location towards the support edge.
L <- c(10,20,40,80,160,320)
# Use x=0:4 to retain representable thresholds even for very large L.
x0 <- 0:4
fixed_scale <- data.frame(L=L,
 frechet=vapply(L,function(z) sum(dfrechet(x0,lambda=log(.5*z)/z,mu=-exp(-z),sigma=1,log=TRUE)),0.0))
print(fixed_scale)
write.csv(fixed_scale,file.path(out,'fixed-scale.csv'),row.names=FALSE)
# GEV: hold location above minimum; increasing sigma moves the lower endpoint
# exponentially close to the minimum while xi increases.
shapes <- c(10,20,30,40,50,60,70)
gev_held_mu <- data.frame(xi=shapes, ll=vapply(shapes,function(v) {
 s<-v/(1-exp(-.4*v)); sum(dgev(1:5,mu=2,sigma=s,xi=v,log=TRUE))
},0.0))
print(gev_held_mu)
write.csv(gev_held_mu,file.path(out,'gev-held-mu.csv'),row.names=FALSE)
# Finite but excluded suprema, including equality of the tie exponent.
e <- 10^-(1:8); tied<-c(1,1,2,3)
finite_faces <- data.frame(epsilon=e,
 gev_xi_minus_one=vapply(e,function(t) sum(dgev(1:5,mu=5+t-2,sigma=2,xi=-1,log=TRUE)),0.0),
 gpd_free_threshold=vapply(e,function(t) sum(dgpd(1:5,mu=1-t,sigma=1,xi=.2,log=TRUE)),0.0),
 gpd_shrinking_uniform=vapply(e,function(t) sum(dgpd(1:5,mu=1-t,sigma=4+2*t,xi=-1,log=TRUE)),0.0),
 gev_equal_ties=vapply(e,function(t) sum(dgev(tied,mu=1+t,sigma=2*t,xi=1,log=TRUE)),0.0),
 frechet_equal_ties=vapply(e,function(t) sum(dfrechet(tied,mu=1-t,sigma=2*t,lambda=1,log=TRUE)),0.0))
print(finite_faces)
write.csv(finite_faces,file.path(out,'finite-faces.csv'),row.names=FALSE)
gev_held_sigma <- data.frame(xi=shapes, ll=vapply(shapes,function(v) {
 mu<-1+1/v-exp(-.4*v)/v; sum(dgev(1:5,mu=mu,sigma=1,xi=v,log=TRUE))
},0.0))
print(gev_held_sigma)
write.csv(gev_held_sigma,file.path(out,'gev-held-sigma.csv'),row.names=FALSE)
