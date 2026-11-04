pkgload::load_all('univariateML-r',quiet=TRUE)
g <- jsonlite::fromJSON('build/scratch/pareto4-original.json',simplifyVector=FALSE)
f <- mlpareto4(unlist(g$data),min=g$estimates$min)
path <- 'univariateML-cpp/include/cppunivariate/distributions/pareto4/golden.json'
# Keep the original fixture byte-for-byte apart from fitted numbers.
s <- readLines('build/scratch/pareto4-original.json')
for(k in c('shape1','shape2','scale')) {
 i <- which(grepl(paste0('    "',k,'":'),s))[2]
 s[i] <- sprintf('    "%s": %.17g,',k,f[k])
}
i <- which(grepl('  "logLik":',s)); s[i] <- sprintf('  "logLik": %.17g,',attr(f,'logLik'))
writeLines(s,path)
