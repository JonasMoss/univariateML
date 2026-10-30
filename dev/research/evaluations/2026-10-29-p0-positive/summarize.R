# Summarize saved independent-oracle comparisons; run from repo root.
p <- "dev/research/evaluations/2026-10-29-p0-positive"
d <- do.call(rbind,lapply(list.files(p,"^oracle-.*[.]csv$",full.names=TRUE),read.csv))
d$held[is.na(d$held)]<-""; d$fixed[is.na(d$fixed)]<-""
a<-d[is.finite(d$pkg) & d$held==d$fixed & is.finite(d$oracle),,drop=FALSE]
a$disagrees<-abs(a$delta)>1e-6*a$n
by<-aggregate(cbind(compared=rep(1,nrow(a)),disagrees=as.integer(a$disagrees)),list(id=a$id,held=a$held),sum)
write.csv(by,file.path(p,"comparison-summary.csv"),row.names=FALSE)
print(by,row.names=FALSE)
cat("all rows:",nrow(d),"valid comparisons:",nrow(a),"disagreements:",sum(a$disagrees),"\n")
print(a[a$disagrees,c("id","sample","held","n","oracle","pkg","delta")],row.names=FALSE)
cat("max abs own-formula error:",max(d$own_formula_error,na.rm=TRUE),"\n")
cat("max abs fitted reference versus stored logLik:",max(abs(a$pkg_ref-a$pkg),na.rm=TRUE),"\n")
cat("Partial argument matches returning a fit without requested holds:\n")
print(unique(d[is.finite(d$pkg)&d$held!=d$fixed,c("id","held","fixed")]),row.names=FALSE)
