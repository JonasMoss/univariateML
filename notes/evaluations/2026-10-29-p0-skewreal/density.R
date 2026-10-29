# Independent implementation of README log densities, in stable log form.
logref <- function(id,x,p) {
 if(id=='sn') {z <- (x-p['xi'])/p['omega']; return(log(2)-log(p['omega'])+dnorm(z,log=TRUE)+pnorm(p['alpha']*z,log.p=TRUE))}
 if(id=='emg') {z <- (x-p['mu'])/p['sigma']; w <- z-p['lambda']*p['sigma']; return(log(p['lambda'])+dnorm(z,log=TRUE)+pnorm(w,log.p=TRUE)-dnorm(w,log=TRUE))}
 if(id=='tnorm') return(extraDistr::dtnorm(x,mean=p['mean'],sd=p['sd'],a=p['a'],b=p['b'],log=TRUE))
 nu <- p['nu']; xi <- p['xi']
 if(id=='snorm') m <- sqrt(2/pi)
 if(id=='sstd') m <- exp(lgamma((nu-1)/2)-lgamma(nu/2)+.5*log((nu-2)/pi))
 if(id=='sged') m <- exp(lgamma(2/nu)-.5*(lgamma(1/nu)+lgamma(3/nu)))
 v <- (1-m*m)*(xi^2+xi^(-2))+2*m*m-1
 z <- (sqrt(v)*(x-p['mean'])/p['sd']+m*(xi-1/xi)); z <- ifelse(z>0,z/xi,z*xi)
 core <- switch(id,snorm=dnorm(z,log=TRUE),sstd={s <- sqrt((nu-2)/nu);dt(z/s,nu,log=TRUE)-log(s)},sged={loga <- .5*(lgamma(1/nu)-lgamma(3/nu)); log(nu)-log(2)-loga-lgamma(1/nu)-exp(nu*(log(abs(z))-loga))})
 .5*log(v)-log(p['sd'])+log(2/(xi+1/xi))+core
}
