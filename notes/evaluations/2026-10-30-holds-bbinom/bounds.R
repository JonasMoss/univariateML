# Independent checks of the proved held-beta bound and example limit paths.
stopifnot(requireNamespace('extraDistr',quietly=TRUE))
checks <- 0L
for (b in c(.1, 1, 3, 20)) for (x in c(1L,2L,5L,10L))
 for (N in c(2*x, 10*x, 100*x, 1000*x)) {
  H <- sum(1/(b+1+seq.int(0,N-1)))
  bound <- max(2^x/(exp(1)*H), (2^(x+1)-1)*exp(-H))
  for (a in c(.001,.01,.1,.5,1,2,10,100)) {
   p <- extraDistr::dbbinom(x,N,a,b)
   stopifnot(is.finite(p), p <= bound*(1+1e-10)); checks <- checks+1L
  }
 }
for (N in c(1000,10000)) {
 x <- 0:10
 bb <- extraDistr::dbbinom(x,N,2,N*2/3)
 stopifnot(max(abs(bb-dnbinom(x,2,mu=3))) < 2/N)
 # Both concentrations large at a fixed size give the binomial face.
 bb <- extraDistr::dbbinom(0:10,10,1e5,2e5)
 stopifnot(max(abs(bb-dbinom(0:10,10,1/3))) < 1e-5)
 stopifnot(max(abs(dbinom(x,N,3/N)-dpois(x,3))) < 2/N)
 checks <- checks+3L
}
# Exact endpoint faces, including the point-mass subfaces.
for (N in c(2L,7L,12L)) {
  p <- .35; eps <- 1e-7
  masses <- extraDistr::dbbinom(0:N,N,eps*p,eps*(1-p))
  face <- c(1-p,rep(0,N-1),p)
  stopifnot(max(abs(masses-face)) < 1e-5); checks <- checks+1L
  stopifnot(extraDistr::dbbinom(N,N,2,1e-7) > 1-1e-5)
  stopifnot(extraDistr::dbbinom(N,N,1e8,3) > 1-1e-5)
  checks <- checks+2L
}
cat(checks,'bound and limit checks passed\n')
