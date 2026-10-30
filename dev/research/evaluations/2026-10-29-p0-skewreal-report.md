# TASK-79: skew-real P0 — incomplete

Proved: with the current `extraDistr::dtnorm` open-support likelihood,
any freely estimated bound can move toward a sample extreme and improve
the likelihood, but the density is zero at that extreme. Thus no such
subset has an MLE; the design's proposed attained sample-extreme bounds
require a change in endpoint semantics. Fixed-bound parent-parameter
conditions and the complete Azzalini skew-normal hold table are in the note.

Derived: SGED's unbounded shape-to-zero spike in the full-free model;
SSTD's high-multiplicity tie divergence, which can also occur with sd held;
and the mean/sd constraints on the translated half-family faces. SGED's
README domain says nu >= 1 while its density accepts nu > 0. Its spike
argument requires nu to be free, so it does not establish that fixed values
below 1 must be rejected.

Checked on simbox: independent README-formula densities agree with package
densities at default parameters; 1,020 numerical hold/sample searches and
135 tnorm searches are recorded. All 200 returned fits for the five own
families and 27 tnorm fits matched within 1e-6*n. The five-family searches
also use the package estimate as one start and do not certify global maxima
or face classifications; tnorm searches avoid unstable reference tails.

Still incomplete: the complete hold tables and exact existence conditions
for most sstd, sged and several emg cases; proofs that proposed face lists
exhaust all joint escapes; divergent paths for every applicable subset;
complete R7 reference-domain checks; and a certified comparison of every
limit likelihood. The numerical searches cannot establish those facts,
and a proof of the joint boundaries has not been obtained. The proposed
YAML is therefore provisional. No package code, READMEs, tests or goldens
were changed; no commit was made.
