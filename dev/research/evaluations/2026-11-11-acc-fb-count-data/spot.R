options(pkg.build_extra_flags=FALSE)
pkgload::load_all("univariateML-r",quiet=TRUE)
P <- "dev/research/evaluations/2026-11-11-acc-fb-count-data"
cases <- jsonlite::fromJSON(file.path(P,"cases.json"),simplifyVector=FALSE)
ids <- c(927,90,209,317,733,1068,629,1076,1078,1245,1247,1250,1252,1253,1254,1255,1257,1258,1259,1260,1261,1263,1264,1265,1266)
required <- list(bbinom="size",hyper=c("k","N"),nhyper=c("r","N"),tbinom=c("a","b","size"),ztbinom="size",zib="size",zmbinom="size")
rows <- lapply(ids,function(i) {
 c <- cases[[i+1]]; f <- c$family;args <- list(x=as.numeric(unlist(c$data)))
 for (name in required[[f]])args[name] <- list(NULL)
 for (name in names(c$held))args[name] <- c$held[name]
 if (!f %in% c("hyper","nhyper","tbinom")) {
  args[[if(f=="bnbinom")"gradtol" else "reltol"]] <- if(!is.null(c$tol))c$tol else 1e-10
  args$iterlim <- if(!is.null(c$maxit))c$maxit else if(f=="bbinom")500 else 200
 }
 fit <- do.call(get(paste0("ml",f)),args)
 list(index=i,family=f,estimates=as.list(setNames(sprintf("%.17g",as.numeric(fit)),names(fit))),certified=attr(fit,"certified"))
})
jsonlite::write_json(rows,file.path(P,"r-spots.json"),auto_unbox=TRUE,digits=17,pretty=TRUE)
