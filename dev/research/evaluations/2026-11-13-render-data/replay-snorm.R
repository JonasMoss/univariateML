# Reproduce the unchanged full-suite assertions that outcome rendering exposes.
# Run from the repository root through /tmp/umvl-cpu/run.
pkgload::load_all("univariateML-r", quiet=TRUE, recompile=FALSE)
x <- c(-1,0,1)
fit <- mlsnorm(x,sd=1.4)
cat("Input: c(-1,0,1); held sd=1.4\n")
print(coef(fit))
cat("logLik:",as.numeric(logLik(fit)),"\n")
cat("fixed:",attr(fit,"fixed"),"\n")
cat("certified:",attr(fit,"certified"),"unique:",attr(fit,"unique"),"\n")
print(attr(fit,"candidates"))
cat("Original diagnostic:",attr(fit,"outcome_message"),"\n")
cat("Held-coordinate assertion:",identical(unname(as.numeric(fit["sd"])),1.4),"\n")
