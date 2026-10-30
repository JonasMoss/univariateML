source('dev/research/evaluations/2026-10-29-p0-bounded/oracle.R')
# Independent likelihood searches for boundary modes, endpoints and constants.
x<-c(0,.25,1); z<-rep(.4,5)
cases<-list(
 list(x=x,h=c(a=0)),list(x=x,h=c(b=1)),list(x=x,h=c(a=.25)),list(x=x,h=c(b=.25)),
 list(x=x,h=c(c=-1)),list(x=x,h=c(c=2)),list(x=x,h=c(c=0)),list(x=x,h=c(c=1)),
 list(x=x,h=c(a=0,b=2)),list(x=x,h=c(a=-1,b=1)),list(x=x,h=c(a=0,b=1)),
 list(x=x,h=c(a=0,c=0)),list(x=x,h=c(a=0,c=.4)),list(x=x,h=c(b=1,c=1)),list(x=x,h=c(b=1,c=.4)),
 list(x=x,h=c(a=-1,b=2,c=.4)),list(x=x,h=c(a=0,b=1,c=.4)),
 list(x=z,h=c(a=.4)),list(x=z,h=c(b=.4)),list(x=z,h=c(c=.4)),
 list(x=z,h=c(a=.4,c=.4)),list(x=z,h=c(b=.4,c=.4)),list(x=z,h=c(a=0)),list(x=z,h=c(b=1)),list(x=z,h=c(c=0)),list(x=z,h=c(a=0,b=1)),list(x=z,h=c(a=0,c=.4)),list(x=z,h=c(b=1,c=.4)),list(x=z,h=c(c=-1)),list(x=z,h=c(b=2,c=-1)),list(x=z,h=c(c=2)),list(x=z,h=c(a=-1,c=2)))
rows<-lapply(seq_along(cases),function(i)run('triang',paste0('corner',i),cases[[i]]$x,cases[[i]]$h))
write.csv(do.call(rbind,rows),file.path(outdir,'corners.csv'),row.names=FALSE,na='NA')
print(do.call(rbind,rows)[,c('sample','held','oracle_status','oracle','fit','delta','error')],row.names=FALSE)
