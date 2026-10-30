# Run from the repository root under /tmp/umvl-cpu/run after just test.
pkgload::load_all("univariateML-r",compile=FALSE,quiet=TRUE)
face_x <- c(100,1e6,1e12,1e12+1e6)
face_rate <- c(100,1e6,1e12,1e12)
stopifnot(isTRUE(all.equal(dskellam(x = face_x, mu1 = face_rate, mu2 = 0, log = TRUE),
                          stats::dpois(face_x,face_rate,log=TRUE),tolerance=1e-12)))
cat("4 large-rate Poisson-face log masses agree with stats::dpois\n")
# Enumerate small samples, independently scan the exact mean-matched
# profile using the internal Bessel-based density.
count <- 0L; multimodal <- 0L; worst <- 0
for(n in 2:5) {
  g <- expand.grid(rep(list(-2:2),n))
  for(i in seq_len(nrow(g))) {
    x <- as.numeric(g[i,]);if(is.unsorted(x)) next
    m <- mean(x); t <- exp(seq(-16,4,length.out=241))
    ll <- vapply(t,function(v)sum(dskellam(x = x, mu1 = v + max(m, 0), mu2 = v + max(-m, 0), log = TRUE)),0.)
    peaks <- which(diff(sign(diff(ll))) < 0)+1L
    multimodal <- multimodal+as.integer(length(peaks)>1)
    f <- mlskellam(x)
    worst <- max(worst,max(ll)-as.numeric(logLik(f)))
    count <- count+1L
  }
}
cat("samples",count,"multiple interior grid peaks",multimodal,"worst oracle gap",worst,"\n")
stopifnot(worst<1e-7)
