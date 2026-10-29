# P0 bounded families — TASK-79

The analysis, full hold tables, arguments, proposed YAML and numerical
summary are in `notes/evaluations/2026-10-29-p0-bounded.md`; reproducible
scripts and results are in the same-named directory. Package code, README
files and tests were not changed.

Proved on ordinary positive-finite parameter domains: beta and Kumaraswamy
have an MLE for nonconstant interior samples when both shapes are free, and
for all interior samples with one shape held. Logit-normal reduces to the
normal on logits, with the stated held-mu and held-sigma conditions. Uniform
and triangular existence follows from width bounds and endpoint feasibility.
All held combinations, including all-held endpoint evaluations, are covered.
No R5 restriction is needed for these families; the proposed parameter kinds
agree with the design.

Numerically checked on simbox with fixed seeds, independent reference-density
multistart searches, samples of n=2,3,5,20,100,1000, ties, point-mass and
uniform limits, U-shaped samples, endpoint cases and explicit collapse paths.
All 97 finite implemented package comparisons met 1e-6*n: maximum absolute
log-likelihood difference was 3.59e-10. Own logit-normal density agreed with
the README formula to 3.34e-15. The triangular mode search is approximate
for >100 distinct data values; existence is supported by a separate proof.

Surprises: `mlkumar(x,a=...)` silently matches `a0` and does not hold `a`;
constant data throw a Boost exception. For `x=rep(.4,5), c=-1`, triangular
has the attained MLE `(a,b,c)=(-1,1.8,-1)`, but the fitter reports iteration
failure; it also fails with b=2 held. The failures persist to 10,000
iterations and across tested tolerances. Six duplicated rows among 832
triangular edge probes show the failure; remaining status checks agree.

R7 needs a design decision: stats accepts zero and infinite beta shapes as
point-mass laws, unlike the README. The note classifies those held cases for
interior samples, but a mixed atomic/continuous likelihood needs a specified
dominating-measure convention to compare the `(Inf,Inf)` atom with ordinary
beta laws. Stats uniform and extraDistr triangular also numerically accept
some infinite endpoints that yield improper zero-density limits; R7 should
distinguish those from proper laws.

Still incomplete: the beta all-free classification on the extended R7
parameter domain is not resolved, because the agreed rules do not define how
to compare the reference's atomic and continuous density values. This does
not affect the stated finite-shape proofs or ordinary-domain comparisons.
The large-sample triangular numerical search is not a global certificate;
no other requested ordinary-domain checks are outstanding.
