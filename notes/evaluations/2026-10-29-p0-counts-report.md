# TASK-79: P0 count-family existence analysis

The hold-subset tables for all 14 assigned families, reference-domain audit,
arguments and proposed YAML are in `notes/evaluations/2026-10-29-p0-counts.md`.
No generic hold subset has an unbounded discrete likelihood. Held values
that make the observed sample impossible have identically zero likelihood and
are identified separately.

Proved: the NB free-fit variance criterion (published theorem), held-prob NB
concavity, positive conditional mean equations, and the modified-family
likelihood factorization. The prior PIG analysis proves the two-free
variance criterion and the fixed-shape Lévy-face argument. For ZINB,
truncated/modified NB and held-mean PIG, the note gives exact existence
conditions via global one-dimensional likelihood profiles, including ties;
it does not assert that these profiles are unimodal.

R7 surprises: stats NB and extraDistr ZINB accept size=0 as a zero point
mass; actuar PIG accepts shape=0 as that mass and mean=Inf as the proper
Poisson–Lévy law. The READMEs and master fitters exclude these. Actuar PIG
therefore attains the all-zero maximum when shape is free, contrary to the
master error. ExtraDistr ZIP returns NaNs at lambda=0 despite its documented
closed domain; with pi<1 held, an all-zero fit is consequently unattained.
A held p0 equal to the empirical zero share does not make the positive
conditional fit degenerate. All listed parameters are regular; no R5 bound
is indicated. NB size is a real shape, including the actuar truncated
size=0 logarithmic member, not a count of trials.

Fixed-seed independent multistart sweeps on simbox cover every subset,
n=20/100/1000 defaults, ties, n=2/3/5, boundary holds and appropriate
limit-law samples. Of 602 cases, all 197 returned fits with finite oracle
values match within 1e-6 n; the rest are errors, escapes or unsupported
holds. The initial three apparent PIG discrepancies were actuar density
cancellation errors, confirmed by independent 120-digit Bessel calculations.
The scripts and CSVs, including endpoint and limit-path probes, are in
`notes/evaluations/2026-10-29-p0-counts/`.

Incomplete: the exact profile criteria do not yet have a certified numerical
global optimizer. The multistart search and endpoint grid are numerical, not
exhaustive over all held values, and do not certify all profile peaks.
Actuar PIG is unreliable in extreme regions; the Bessel supplement and
high-precision cross-check address the observed examples. These are evidence
limitations rather than missing hold rows; no result is presented as a proof
from multistart alone. No package code, README, test or golden was changed;
no commit was made.
