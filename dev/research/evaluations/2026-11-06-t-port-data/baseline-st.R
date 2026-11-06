options(pkg.build_extra_flags=FALSE)
pkgload::load_all("build/scratch/t-port-baseline-r",quiet=TRUE,helpers=FALSE)
set.seed(7)
x <- ml_simulate("mlst",80)
base <- mlst(x)
rows <- lapply(c(1e-4,1e4),function(b) {
  fit <- mlst(3*b+b*x)
  pulled <- as.numeric(fit);pulled[1]<-(pulled[1]-3*b)/b;pulled[2]<-pulled[2]/b
  list(scale=b,base=as.numeric(base),pulled=pulled,difference=pulled-as.numeric(base),
       relative_nu_difference=abs(pulled[4]-base[4])/abs(base[4]),
       likelihood_difference_per_observation=(attr(fit,"logLik")+length(x)*log(b)-attr(base,"logLik"))/length(x))
})
result <- list(revision="164d41784756606a48ddc0cedc67752acab4050b",seed=7,n=80,
               R=R.version.string,RNGkind=RNGkind(),comparisons=rows)
jsonlite::write_json(result,"dev/research/evaluations/2026-11-06-t-port-data/baseline-st.json",auto_unbox=TRUE,digits=17,pretty=TRUE)
print(rows)
