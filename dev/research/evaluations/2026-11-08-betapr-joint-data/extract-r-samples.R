pkgload::load_all("univariateML-r", quiet=TRUE)
samples <- list()
for(seed in 1:12){set.seed(seed);samples[[paste0("small",seed)]]<-rbetapr(20,2,3,scale=4)}
for(seed in 1:3){set.seed(seed);samples[[paste0("ordinary",seed)]]<-rbetapr(400,.7,1.2,scale=3)}
set.seed(1);samples$gamma<-rgamma(300,2,rate=.5)
set.seed(1);samples$invgamma<-rinvgamma(300,2,beta=4)
set.seed(1);samples$lnorm<-rlnorm(300,.7,.8)
cat("BEGIN_SAMPLES\n",jsonlite::toJSON(samples,digits=17,auto_unbox=TRUE),"\nEND_SAMPLES\n",sep="")
