context("mltriang")
x <- c(.02, .2, .3, .4, .45, .65, .9)
ll <- function(p, x) sum(dtriang(x = x, a = p[1], b = p[2], c = p[3], log = TRUE))
for (mask in 0:7) {
  held <- c("a", "b", "c")[as.logical(intToBits(mask)[1:3])]
  values <- c(a = 0, b = 1, c = .4)
  fit <- suppressWarnings(do.call(mltriang, c(list(x), as.list(values[held]))))
  expect_identical(attr(fit, "fixed"), held)
  expect_equal(attr(logLik(fit), "df"), 3L - length(held))
  expect_equal(unname(coef(fit)[held]), unname(values[held]))
  expect_equal(as.numeric(logLik(fit)), ll(fit, x), tolerance = 1e-11)
  free <- setdiff(names(values), held)
  if (length(free)) {
    objective <- function(z) {
      p <- coef(fit); p[free] <- z
      if (p[1] >= min(x) || p[2] <= max(x) || p[3] < p[1] || p[3] > p[2]) return(1e50)
      v <- suppressWarnings(ll(p, x))
      if (is.finite(v)) -v else 1e50
    }
    for (offset in c(-.02, 0, .02)) {
      oracle <- optim(coef(fit)[free] + offset, objective,
        method = if (length(free) == 1) "BFGS" else "Nelder-Mead",
        control = list(maxit = 2000, reltol = 1e-12))
      expect_lte(-oracle$value, as.numeric(logLik(fit)) + 1e-7)
    }
  }
}
expect_warning(tie <- mltriang(c(0, 1)), "not unique")
expect_equal(unname(coef(tie)), c(0, 1.5, 0), tolerance = 1e-11)
expect_error(mltriang(rep(1, 4)), "unbounded")
expect_error(mltriang(x, a = .1), "incompatible")
expect_error(mltriang(c(0, 1), a = 0, b = 1), "incompatible")
expect_error(mltriang(x, a = 2, b = 1), "a < b")
expect_error(mltriang(x, c = Inf), "finite")
expect_error(mltriang(x, c = NA_real_), "finite")
expect_error(mltriang(x, a = 0, c = -1), "a <= c")
expect_error(mltriang(x, iterlim = 0), "iterlim")
expect_equal(pml(0, tie), 0)
expect_equal(pml(0, tie, lower.tail = FALSE), 1)
expect_equal(pml(0, tie, log.p = TRUE), -Inf)
# Independent mode grid with a separate endpoint optim at every grid point.
fit <- mltriang(x)
for (mode in seq(0, 1, length.out = 31)) {
  oracle <- optim(c(-.1, 1.1), function(z) {
    if (z[1] >= min(x) || z[2] <= max(x) || mode < z[1] || mode > z[2]) return(1e50)
    -ll(c(z, mode), x)
  }, control = list(maxit = 1000, reltol = 1e-11))
  expect_lte(-oracle$value, as.numeric(logLik(fit)) + 1e-7)
}
# A held endpoint observed in the data forces its corner mode.
corner <- mltriang(c(0, .01, .1, .5), a = 0)
expect_equal(coef(corner)[["c"]], 0)
for (mode in c(-1, 2)) {
  fixed_mode <- mltriang(x, c = mode)
  expect_equal(coef(fixed_mode)[[if (mode < 0) "a" else "b"]], mode)
}
expect_equal(as.numeric(logLik(mltriang(rep(.4, 3), a = 0, b = 1))),
  3 * log(2))
corner_upper <- mltriang(1 - c(0, .01, .1, .5), b = 1)
expect_equal(coef(corner_upper)[["c"]], 1)
expect_warning(known_tie <- mltriang(c(.2, .8), a = 0, b = 1), "not unique")
expect_equal(coef(known_tie)[["c"]], .2)

test_that("constant triangular samples with exterior held mode", {
  for (v in c(.1,.4,1)) for (b in c(NA,3)) {
    x <- rep(v,5); args <- list(x=x,c=-1)
    if (!is.na(b)) args$b <- b
    fit <- do.call(mltriang,args)
    expect_equal(unname(fit["a"]), -1)
    expect_equal(unname(fit["b"]), if (is.na(b)) 2*v+1 else b)
    expect_equal(as.numeric(logLik(fit)), sum(dtriang(x = x, a = fit[1], b = fit[2], c = fit[3], log = TRUE)))
  }
})

test_that("P2 reflected constant exterior-mode maxima", {
  for (held in list(list(c=2),list(a=-1,c=2),list(a=-1,b=3,c=2))) {
    fit <- do.call(mltriang,c(list(x=rep(.4,5)),held))
    expect_equal(as.numeric(logLik(fit)),ll(fit,rep(.4,5)),tolerance=1e-12)
    if (is.null(held$b)) expect_equal(unname(fit["b"]),2)
    if (is.null(held$a)) expect_equal(unname(fit["a"]),-1.2)
  }
})

test_that("P2 triangular subsets dominate independent multistart endpoint searches", {
  skip_unless_tier("full")
  set.seed(8131)
  samples <- list(rtriang(n = 20, a = 0, b = 1, c = 0.4),rtriang(n = 100, a = 0, b = 1, c = 0.4),
                  runif(20),c(.2,.2,.6,.8),c(.2,.8),rep(.4,5))
  values <- c(a=0,b=1,c=.4)
  for (x in samples) for (mask in 0:7) {
    held <- names(values)[as.logical(intToBits(mask)[1:3])]
    if (length(unique(x))==1 && all(values[held]==x[1])) next
    fit <- suppressWarnings(do.call(mltriang,c(list(x=x),as.list(values[held]))))
    # Conditional mode maxima occur at observations or a held mode.
    modes <- if ("c" %in% held) values["c"] else unique(x)
    best <- -Inf
    for (mode in modes) {
      free <- setdiff(c("a","b"),held)
      objective <- function(z) {
        p <- c(a=-.1,b=1.1,c=mode); p[held] <- values[held]
        for (i in seq_along(free)) p[free[i]] <- if (free[i]=="a")
          min(x,mode)-exp(z[i]) else max(x,mode)+exp(z[i])
        if (p[1]>=p[2] || p[3]<p[1] || p[3]>p[2]) return(1e100)
        v <- -sum(dtriang(x = x, a = p[1], b = p[2], c = p[3], log = TRUE))
        if (is.finite(v)) v else 1e100
      }
      if (!length(free)) best <- max(best,-objective(numeric())) else
        for (start in c(-3,0,2)) best <- max(best,-nlminb(rep(start,length(free)),
          objective,lower=rep(-25,length(free)),upper=rep(10,length(free)),
          control=list(iter.max=500,rel.tol=1e-11))$objective)
    }
    expect_lte(best,as.numeric(logLik(fit))+1e-6*length(x))
    # Interior slack searches only approach endpoint modes; compare against
    # the fit from below, with a small numerical allowance.
    expect_lt(as.numeric(logLik(fit))-best,1e-5*length(x))
  }
})

test_that("triang audit-support case 8173 checks the public fit", {
  x <- c(0.03627073533558047,0.036270735335580485,0.03627073533558049,0.03627073533558051,0.036270735335580555)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8194 checks the public fit", {
  x <- c(0.03627073533558047,0.036270735335580485,0.03627073533558049,0.03627073533558051,0.036270735335580555)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8195 checks the public fit", {
  x <- c(2.1270091277547183e+149,5.56117281364277e+149,5.239257282111793e+149,3.8924929029308256e+149,5.514445431996137e+149,5.285874607041478e+149,5.398522144183517e+149,3.251642995048314e+149,6.451318151317536e+149,4.357297092676162e+149,5.2674134634435175e+149,3.8801221367903053e+149)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8268 checks the public fit", {
  x <- c(0.03627073533658046,0.03627073533758046,0.03627073533858047,0.036270735340580465,0.03627073534458047)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465,c=0.3888433021027595),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8289 checks the public fit", {
  x <- c(1.0045256123717767,1.0045256123707769,1.0045256123697768,1.0045256123677768,1.004525612363777)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,b=1.0045256123727768,c=0.3888433021027595),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11117 checks the public fit", {
  x <- c(0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11118 checks the public fit", {
  x <- c(1.0045256123727766,1.0045256123727764,1.0045256123727762,1.0045256123727757,1.0045256123727748)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11119 checks the public fit", {
  x <- c(0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11122 checks the public fit", {
  x <- c(1.0045256123727766,1.0045256123727764,1.0045256123727762,1.0045256123727757,1.0045256123727748)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,b=1.0045256123727768),error=identity))
  expect_true(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11130 checks the public fit", {
  x <- c(1.0045256123727766,1.0045256123727764,1.0045256123727762,1.0045256123727757,1.0045256123727748)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,b=1.0045256123727768,c=0.3888433021027595),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8165 checks the public fit", {
  x <- c(0.5267413463432983,0.5267413463438251,0.5267413463443518,0.5267413463448785,0.526741346345932)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8168 checks the public fit", {
  x <- c(0.999999999998,0.999999999999,1.0,1.000000000001,1.000000000003)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8172 checks the public fit", {
  x <- c(1.0045256123717767,1.0045256123707769,1.0045256123697768,1.0045256123677768,1.004525612363777)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8213 checks the public fit", {
  x <- c(1.0045256123717767,1.0045256123707769,1.0045256123697768,1.0045256123677768,1.004525612363777)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,b=1.0045256123727768),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 8270 checks the public fit", {
  x <- c(0.03627073533558047,0.036270735335580485,0.03627073533558049,0.03627073533558051,0.036270735335580555)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465,c=0.3888433021027595),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triang audit-support case 11127 checks the public fit", {
  x <- c(0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053)
  fit <- suppressWarnings(tryCatch(mltriang(x,reltol=1e-10,iterlim=100,a=0.036270735335580465,c=0.3888433021027595),error=identity))
  expect_false(inherits(fit,"error"))
  if(inherits(fit,"error")) {
    expect_s3_class(fit,"univariateML_numerical")
    expect_match(conditionMessage(fit),"numerical|converge|accuracy|certificate",ignore.case=TRUE)
  } else {
    raw <- sum(dml(x,fit,log=TRUE))
    expect_true(is.finite(raw))
    expect_equal(as.numeric(logLik(fit)),raw,tolerance=1e-11)
  }
})

test_that("triangular endpoint representation agrees with 400-digit profiles", {
  # Saved case 8165: best public loss = 1.82271526249e-10.
  x <- c(0.5267413463432983,0.5267413463438251,0.5267413463443518,0.5267413463448785,0.526741346345932)
  fit <- suppressWarnings(mltriang(x,reltol=1e-10))
  expect_equal(unname(coef(fit)),c(0.5267413463432983,0.5267413463466865,0.5267413463432983),tolerance=0)
  expect_equal(as.numeric(logLik(fit)),132.84792080737714,tolerance=1e-13)
  expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-13)
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 8168: best public loss = 6.80778730019e-11.
  x <- c(0.999999999998,0.999999999999,1.0,1.000000000001,1.000000000003)
  fit <- suppressWarnings(mltriang(x,reltol=1e-10))
  expect_equal(unname(coef(fit)),c(0.999999999998,1.0000000000044325,0.999999999998),tolerance=0)
  expect_equal(as.numeric(logLik(fit)),129.6426460734028,tolerance=1e-13)
  expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-13)
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 8172: best public loss = 7.82594674304e-10.
  x <- c(1.0045256123717767,1.0045256123707769,1.0045256123697768,1.0045256123677768,1.004525612363777)
  fit <- suppressWarnings(mltriang(x,reltol=1e-10))
  expect_equal(unname(coef(fit)),c(1.004525612361764,1.0045256123717767,1.0045256123717767),tolerance=0)
  expect_equal(as.numeric(logLik(fit)),127.65928931238614,tolerance=1e-13)
  expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-13)
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 8173: best public loss = 0.000116161224635.
  x <- c(0.03627073533558047,0.036270735335580485,0.03627073533558049,0.03627073533558051,0.036270735335580555)
  expect_error(mltriang(x,reltol=1e-10),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 8194: best public loss = 0.00352298316833.
  x <- c(0.03627073533558047,0.036270735335580485,0.03627073533558049,0.03627073533558051,0.036270735335580555)
  expect_error(mltriang(x,reltol=1e-10,a=0.036270735335580465),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14,a=0.036270735335580465),class="univariateML_numerical")
  # Saved case 8213: best public loss = 1.26985670694e-10.
  x <- c(1.0045256123717767,1.0045256123707769,1.0045256123697768,1.0045256123677768,1.004525612363777)
  fit <- suppressWarnings(mltriang(x,reltol=1e-10,b=1.0045256123727768))
  expect_equal(unname(coef(fit)),c(1.0045256123615323,1.0045256123727768,1.0045256123717767),tolerance=0)
  expect_equal(as.numeric(logLik(fit)),127.1883141769809,tolerance=1e-13)
  expect_equal(as.numeric(logLik(fit)),sum(dml(x,fit,log=TRUE)),tolerance=1e-13)
  expect_error(mltriang(x,reltol=1e-14,b=1.0045256123727768),class="univariateML_numerical")
  # Saved case 11117: best public loss = 1.79707853612e-05.
  x <- c(0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053)
  expect_error(mltriang(x,reltol=1e-10),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 11118: best public loss = 1.79707853612e-05.
  x <- c(1.0045256123727766,1.0045256123727764,1.0045256123727762,1.0045256123727757,1.0045256123727748)
  expect_error(mltriang(x,reltol=1e-10),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14),class="univariateML_numerical")
  # Saved case 11119: best public loss = 0.00564341002129.
  x <- c(0.03627073533558047,0.03627073533558048,0.036270735335580485,0.0362707353355805,0.03627073533558053)
  expect_error(mltriang(x,reltol=1e-10,a=0.036270735335580465),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14,a=0.036270735335580465),class="univariateML_numerical")
  # Saved case 11122: best public loss = 0.00564341002129.
  x <- c(1.0045256123727766,1.0045256123727764,1.0045256123727762,1.0045256123727757,1.0045256123727748)
  expect_error(mltriang(x,reltol=1e-10,b=1.0045256123727768),class="univariateML_numerical")
  expect_error(mltriang(x,reltol=1e-14,b=1.0045256123727768),class="univariateML_numerical")
})
