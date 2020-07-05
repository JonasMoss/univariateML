densities <- list.files("R")
densities <- densities[sapply(densities, function(x) substr(x, 1, 2) == "ml")]
densities <- densities[!grepl("select", densities)]
densities <- unname(unlist(sapply(densities, function(x) strsplit(x, ".R"))))
