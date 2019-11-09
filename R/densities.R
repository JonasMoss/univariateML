# densities = unique(c("mlcauchy", "mlgumbel", "mllaplace", "mllogis", "mlnorm",
#                      "mlbetapr", "mlexp", "mlgamma", "mlinvgamma", "mlinvgauss",
#                      "mlinvweibull", "mlllogis", "mllnorm", "mllomax", "mlrayleigh",
#                      "mlweibull", "mllgamma", "mlpareto", "mlbeta", "mlkumar",
#                      "mllogitnorm", "mlunif", "mlpower"))

densities = list.files("R")
densities = densities[sapply(densities, function(x) substr(x, 1, 2) == "ml")]
densities = unname(unlist(sapply(densities, function(x) strsplit(x, ".R"))))
