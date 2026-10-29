`%||%` <- function(a, b) if (is.null(a)) b else a
suppressMessages({library(yaml); library(jsonlite)})
Sys.setenv(PKG_CXXFLAGS = "-O1 -I/home/jonas/Files/programming/univariateML/cppunivariate/include")
Rcpp::sourceCpp("/tmp/dpqr-probe/shim.cpp", cacheDir = "/tmp/dpqr-probe/cache")
Rcpp::sourceCpp("/tmp/dpqr-probe/cumlse.cpp", cacheDir = "/tmp/dpqr-probe/cache2")
D <- "/home/jonas/Files/programming/univariateML/cppunivariate/include/cppunivariate/distributions"
fm <- function(id) { l <- readLines(file.path(D, id, "README.md"), warn = FALSE)
  e <- which(l == "---"); yaml.load(paste(l[(e[1] + 1):(e[2] - 1)], collapse = "\n")) }
fam <- read.csv("/tmp/dpqr-probe/families.csv"); fam <- fam[fam$probed, ]
args <- commandArgs(TRUE); tag <- args[1]; ids <- args[-1]; if (length(ids)) fam <- fam[fam$id %in% ids, ]
WRONG <- 1e-6
num <- function(v) { v <- unlist(v); if (is.character(v)) v <- as.numeric(v); v }
# relative error on log scale (abs for |log| <= 1), Inf if exactly one side is non-finite or NA
lerr <- function(a, b) { out <- abs(a - b) / pmax(1, abs(b)); same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  out[same] <- 0; out[!same & (is.na(a) | is.na(b) | !is.finite(a) | !is.finite(b))] <- Inf; out }
lse <- function(v) { v <- v[is.finite(v)]; if (!length(v)) return(-Inf); m <- max(v); m + log(sum(exp(v - m))) }
res <- list()
for (k in seq_len(nrow(fam))) {
  id <- fam$id[k]; f <- fm(id); g <- fromJSON(file.path(D, id, "golden.json"))
  pn <- strsplit(fam$params[k], ",")[[1]]
  pars <- lapply(pn, function(p) num(g$sampler_args[[p]] %||% g$estimates[[p]] %||% f$params[[match(p, pn)]]$default))
  names(pars) <- pn; a <- vapply(pars, as.numeric, 0)
  sp <- strsplit(fam$ref[k], "::")[[1]]; dn <- sp[2]
  dref <- getExportedValue(sp[1], dn); pref <- getExportedValue(sp[1], sub("^d", "p", dn))
  qref <- tryCatch(getExportedValue(sp[1], sub("^d", "q", dn)), error = function(e) NULL)
  RD <- function(x, lg) suppressWarnings(do.call(dref, c(list(x), pars, list(log = lg))))
  RP <- function(x, lt, lp) suppressWarnings(do.call(pref, c(list(x), pars, list(lower.tail = lt, log.p = lp))))
  RQ <- function(u, lt, lp) if (is.null(qref)) rep(NA_real_, length(u)) else
    suppressWarnings(do.call(qref, c(list(u), pars, list(lower.tail = lt, log.p = lp))))
  CD <- get(paste0("cd_", id)); CP <- get(paste0("cp_", id)); CQ <- get(paste0("cq_", id))
  # independent log density: reference where finite, else ours (only matters where ref underflows)
  LD <- function(t) { r <- RD(t, TRUE); o <- CD(t, a, TRUE); ifelse(is.finite(r) | !is.finite(o), r, o) }
  disc <- identical(f$support$type, "integer")
  lo_s <- suppressWarnings(as.numeric(num(f$support$lower))); hi_s <- suppressWarnings(as.numeric(num(f$support$upper)))
  sup <- c(if (length(lo_s) && !is.na(lo_s)) lo_s else -Inf, if (length(hi_s) && !is.na(hi_s)) hi_s else Inf)
  # parameter-dependent endpoints: take them from q(0), q(1) (reference, else ours)
  e0 <- RQ(0, TRUE, FALSE); if (!is.finite(e0)) e0 <- CQ(0, a, TRUE, FALSE)
  e1 <- RQ(1, TRUE, FALSE); if (!is.finite(e1)) e1 <- CQ(1, a, TRUE, FALSE)
  if (is.finite(e0) && e0 > sup[1]) sup[1] <- e0
  if (is.finite(e1) && e1 < sup[2]) sup[2] <- e1
  med <- RQ(0.5, TRUE, FALSE); if (!is.finite(med)) med <- CQ(0.5, a, TRUE, FALSE)
  us <- 10^-c(1, 2, 4, 6, 8, 10, 12, 14, 20, 50, 100, 300); um <- seq(0.05, 0.95, 0.05)
  # truth: log P(X <= x) (lt) or log P(X > x)
  truthP <- function(xx, lt) vapply(xx, function(x0) {
    if (!disc && is.finite(x0) && is.finite(med) && ((lt && x0 > med) || (!lt && x0 < med))) {
      o <- truthP1(x0, !lt); return(if (is.na(o)) NA_real_ else if (o > -0.693) log(-expm1(o)) else log1p(-exp(o))) }
    truthP1(x0, lt) }, 0)
  truthP1 <- function(xx, lt) vapply(xx, function(x0) {
    if (!is.finite(x0)) return(NA_real_)
    if (disc) {
      j <- floor(x0) - KS[1] + 1
      if (lt) { if (j < 1) return(-Inf); if (j > length(KS)) return(0); CL[j] }
      else { if (j < 1) return(0); if (j >= length(KS)) return(-Inf); CU[j + 1] }
    } else {
      if (lt && x0 <= sup[1]) return(-Inf); if (!lt && x0 >= sup[2]) return(-Inf)
      # log substitution t = x0 * exp(+-s) towards an infinite end (x0 > 0 upper, x0 < 0 lower)
      # or towards zero (lower tail on (0, inf)); plain integrate otherwise
      sub_up <- !lt && x0 > 0 && is.infinite(sup[2]); sub_lo_neg <- lt && x0 < 0 && is.infinite(sup[1])
      sub_lo_zero <- lt && x0 > 0 && sup[1] == 0
      I <- function(fun, lo, hi) tryCatch(integrate(fun, lo, hi, rel.tol = 1e-11, subdivisions = 5000)$value, error = function(e) NA_real_)
      if (sub_up || sub_lo_neg || sub_lo_zero) {
        sg <- if (sub_lo_zero) -1 else 1
        l1 <- LD(x0) + log(abs(x0)); if (!is.finite(l1)) l1 <- 0
        r <- I(function(s) exp(LD(x0 * exp(sg * s)) + log(abs(x0)) + sg * s - l1), 0, Inf); l1 + log(r)
      } else {
        l0 <- LD(x0); if (!is.finite(l0)) l0 <- 0
        r <- if (lt) I(function(t) exp(LD(t) - l0), sup[1], x0) else I(function(t) exp(LD(t) - l0), x0, sup[2])
        l0 + log(r) } } }, 0)
  if (disc) {
    fq <- function(u) { v <- RQ(u, TRUE, FALSE); if (!is.finite(v)) v <- CQ(u, a, TRUE, FALSE); v }
    hi <- fq(1 - 1e-12); if (!is.finite(hi)) hi <- 200; hi <- min(hi, 3000)
    lo <- if (is.finite(sup[1])) sup[1] else { l <- fq(1e-12); if (!is.finite(l)) l <- -200; max(l, -3000) }
    x <- unique(c(lo - 1, lo:hi, round(hi * c(2, 5)), 1e4))
    top <- if (is.finite(sup[2])) sup[2] else max(x) + 5e6
    KS <- (lo - 2e5 * !is.finite(sup[1])):top; LDK <- LD(KS)
    CL <- cumlse(LDK); CU <- rev(cumlse(rev(LDK)))
    if (is.finite(sup[2])) x <- x[x <= sup[2] + 1]
  } else {
    x <- c(RQ(us, TRUE, FALSE), RQ(um, TRUE, FALSE), RQ(us, FALSE, FALSE))
    alt <- c(CQ(us, a, TRUE, FALSE), CQ(um, a, TRUE, FALSE), CQ(us, a, FALSE, FALSE))
    x[!is.finite(x)] <- alt[!is.finite(x)]; x <- unique(x[is.finite(x)])
  }
  row <- function(chk, xs, ours, ref, truth) {
    eo <- lerr(ours, truth); er <- lerr(ref, truth); eor <- lerr(ours, ref)
    ok <- !is.na(truth) & (!grepl("^p_(lower|upper)$", chk) | truth > -700) & !grepl("^d_", chk)
    w <- if (any(ok & eo > WRONG)) which(ok & eo > WRONG)[which.max(eo[ok & eo > WRONG])] else 1
    res[[length(res) + 1]] <<- data.frame(id = id, src = sp[1], check = chk, n = length(xs), n_truth = sum(ok),
      ours_wrong = sum(ok & eo > WRONG), ref_wrong = sum(ok & er > WRONG), disagree = sum(eor > 1e-8),
      worst_at = signif(xs[w], 6), ours = signif(ours[w], 10), ref = signif(ref[w], 10), truth = signif(truth[w], 10))
  }
  dd <- CD(x, a, TRUE); dr <- RD(x, TRUE); row("d_log", x, dd, dr, dr)
  for (lt in c(TRUE, FALSE)) {
    tp <- truthP(x, lt); side <- if (lt) "lower" else "upper"
    row(paste0("p_", side, "_log"), x, CP(x, a, lt, TRUE), RP(x, lt, TRUE), tp)
    row(paste0("p_", side), x, log(CP(x, a, lt, FALSE)), log(RP(x, lt, FALSE)), tp)
    for (lp in c(FALSE, TRUE)) {
      u <- c(us, um); uin <- if (lp) c(log(u), -800, -1e4) else u; tgt <- if (lp) uin else log(uin)
      qo <- CQ(uin, a, lt, lp); qr <- RQ(uin, lt, lp)
      chk <- paste0("q_", side, if (lp) "_log" else "")
      if (disc) {
        # q correct iff it is the smallest support point reaching the target
        okq <- function(q) vapply(seq_along(q), function(i) { if (!is.finite(q[i])) return(FALSE)
          if (lt) { a1 <- truthP(q[i], TRUE); a0 <- truthP(q[i] - 1, TRUE); a1 >= tgt[i] - 1e-9 * max(1, abs(tgt[i])) && !(a0 >= tgt[i] + 1e-9 * max(1, abs(tgt[i]))) }
          else { a1 <- truthP(q[i], FALSE); a0 <- truthP(q[i] - 1, FALSE); a1 <= tgt[i] + 1e-9 * max(1, abs(tgt[i])) && !(a0 <= tgt[i] - 1e-9 * max(1, abs(tgt[i]))) } }, TRUE)
        go <- okq(qo); gr <- okq(qr)
        w <- if (any(!go)) which(!go)[1] else 1
        res[[length(res) + 1]] <- data.frame(id = id, src = sp[1], check = chk, n = length(uin), n_truth = length(uin),
          ours_wrong = sum(!go), ref_wrong = sum(!gr), disagree = sum(lerr(qo, qr) > 0),
          worst_at = signif(uin[w], 6), ours = qo[w], ref = qr[w], truth = NA)
      } else {
        po <- truthP(qo, lt); pr <- truthP(qr, lt)
        eo <- lerr(po, tgt); er <- lerr(pr, tgt); eo[!is.finite(qo)] <- Inf; er[!is.finite(qr)] <- Inf
        nb <- function(q, dir) ifelse(q == 0, dir * 4.940656e-324, q * (1 + dir * sign(q) * 4 * .Machine$double.eps))
        brk <- function(q) { lo <- truthP(nb(q, -1), lt); hi <- truthP(nb(q, 1), lt)
          !is.na(lo) & !is.na(hi) & is.finite(q) & tgt >= pmin(lo, hi) - 1e-9 * pmax(1, abs(tgt)) & tgt <= pmax(lo, hi) + 1e-9 * pmax(1, abs(tgt)) }
        eo[which(eo > WRONG & brk(qo))] <- 0; er[which(er > WRONG & brk(qr))] <- 0
        okt <- (is.na(po) == FALSE | !is.finite(qo)) & (is.na(pr) == FALSE | !is.finite(qr))
        eo[!okt] <- NA; er[!okt] <- NA
        bo <- !is.na(eo) & eo > WRONG; br <- !is.na(er) & er > WRONG
        w <- if (any(bo)) which(bo)[which.max(eo[bo])] else 1
        res[[length(res) + 1]] <- data.frame(id = id, src = sp[1], check = chk, n = length(uin), n_truth = sum(okt),
          ours_wrong = sum(bo), ref_wrong = sum(br), disagree = sum(lerr(qo, qr) > 1e-8),
          worst_at = signif(uin[w], 6), ours = signif(qo[w], 10), ref = signif(qr[w], 10), truth = signif(po[w], 10))
      }
    }
  }
  cat(id, ""); flush.console()
}
out <- do.call(rbind, res)
write.csv(out, sprintf("/tmp/dpqr-probe/r2-%s.csv", tag), row.names = FALSE)
cat("\ndone", tag, "\n")
