# Summarize the saved sweeps and check predictions for triangular edge cases.
outdir <- 'dev/research/evaluations/2026-10-29-p0-bounded'
files<-file.path(outdir,c('results-shapes.csv','results-endpoints.csv'))
d<-do.call(rbind,lapply(files,read.csv,na.strings='NA'));corner<-read.csv(file.path(outdir,'corners.csv'),na.strings='NA');e<-read.csv(file.path(outdir,'edges.csv'),na.strings='NA')
cat('finite fitted comparisons by family: count and max |fit - oracle|\n')
print(aggregate(delta~id,d,function(x)c(count=length(x),max=max(abs(x)))))
bad<-d[!is.na(d$delta)&abs(d$delta)>1e-6*d$n,];cat('tolerance failures:',nrow(bad),'\n')
print(corner[!is.na(corner$delta)&abs(corner$delta)>1e-6*corner$n,])
cat('oracle failures:',sum(d$oracle_status=='search-failed'|grepl('oracle-error',d$oracle_status)),'\n')
tri<-e[e$id=='triang',]; classify<-function(row){
 m<-if(grepl('constant-',row$case))as.numeric(sub('constant-','',row$case)) else 0
 M<-if(grepl('constant-',row$case))m else 1
 names<-if(is.na(row$held)||row$held=='') character() else strsplit(row$held,'+',fixed=TRUE)[[1]]
 vals<-if(length(names))as.numeric(strsplit(row$values,'+',fixed=TRUE)[[1]]) else numeric();h<-setNames(vals,names)
 has<-function(n) n %in% names(h)
 if((has('a')&&h['a']>m)||(has('b')&&h['b']<M)||(has('a')&&has('b')&&h['a']>=h['b'])||(has('a')&&has('c')&&h['a']>h['c'])||(has('b')&&has('c')&&h['b']<h['c']))return('error')
 if((has('a')&&h['a']==m&&has('b')&&h['b']==M)||(has('a')&&h['a']==m&&has('c')&&h['c']!=h['a'])||(has('b')&&h['b']==M&&has('c')&&h['c']!=h['b']))return('error')
 if(m==M&&all(h==m))return('error')
 'fit'
}
expected<-apply(tri,1,function(r)classify(as.list(r)));observed<-ifelse(is.na(tri$error)|tri$error=='','fit','error')
cat('triangular edge classification expected vs observed\n');print(table(expected,observed));wrong<-tri[expected!=observed,];write.csv(wrong,file.path(outdir,'edge-disagreements.csv'),row.names=FALSE,na='NA');cat('edge disagreements:',nrow(wrong),'\n')
if(nrow(bad)||nrow(wrong)) print(head(rbind(bad,wrong),10))
