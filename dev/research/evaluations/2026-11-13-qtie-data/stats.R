args <- commandArgs(TRUE)
d <- read.csv(args[1])
raw <- read.csv(args[1],colClasses="character")
d$stats <- NA_real_
for (id in unique(d$family)) for (lower in c(TRUE,FALSE)) for (log in c(TRUE,FALSE)) {
 i <- which(d$family==id & d$lower==lower & d$log==log)
 theta <- switch(id,pois=list(lambda=20),binom=list(size=100,prob=.5),geom=list(prob=.25),nbinom=list(size=10,prob=.5),hyper=list(m=50,n=50,k=20))
 d$stats[i] <- do.call(get(paste0("q",id),asNamespace("stats")),c(list(p=d$p[i]),theta,list(lower.tail=lower,log.p=log)))
}
raw$stats <- d$stats
# Retain original 17-digit inputs; write.table rounds numeric columns to 15.
write.csv(raw[d$cpp!=d$stats,],args[2],row.names=FALSE,quote=FALSE)
writeLines(R.version.string,sub("disagreements.csv$","R-version.txt",args[2]))
print(table(d$family))
print(table(d$family[d$cpp!=d$stats]))
stopifnot(all(d$at==1),all(d$before==0))
