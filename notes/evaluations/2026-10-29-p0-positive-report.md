# TASK-79 p0-positive report

The 18-family subset inventory, arguments, proposed YAML, R7 probes and
numerical evidence are in `notes/evaluations/2026-10-29-p0-positive.md`.

- Proved the listed positive-continuous-domain classifications by profiling,
  monotonic scores and transformations. Joint Lomax/inverse Pareto uses the
  existing theorem in `notes/ml-behaviour.tex`; shifted Gompertz uses the
  likelihood envelopes in its implementation. Optimizer checks are evidence,
  not proofs of global optimality.
- All 445 fixed-seed family/sample/hold probes are saved in `oracle-*.csv`.
  Of 157 finite package fits with the requested hold actually implemented,
  156 agree with the reference-density oracle to 1e-6*n. The remaining case
  is constant-sample power, which has no finite MLE and whose returned offset
  approximation differs from an increasing likelihood path. Own Nakagami
  and shifted-Gompertz log densities match the README formulas to 3.6e-15
  on the checked samples. `paths.csv` records boundary and collapse paths.
- The reference `extraDistr::dpower` excludes alpha=max(x): there is no MLE
  with free alpha, even with beta held. Master returns max(x)+epsilon. Pareto
  includes its minimum and its endpoint and held-shape results are proved.
- Four apparent holds are silently treated as optimizer starts by partial
  argument matching: weibull and invweibull `shape`, lomax `lambda`, and
  invpareto `scale`. Their returned coefficients are not held. Rayleigh with
  mixed zeros returns a fit with logLik=-Inf.
- R7 probe: actuar accepts inverse Gaussian mean=Inf, the proper Lévy law;
  the held-mean shape MLE is n/sum(1/x). Stats accepts lnorm sdlog=0 as a
  point mass; gamma shape=0 and extraDistr inverse-gamma beta=0 also generate
  point masses at zero. Several other out-of-domain boundary values return
  zero density rather than NaN. The complete probe is `domains.csv`.
- All assigned kinds agree as roles, but power's endpoint is unattained.
  No R5 shape restriction is proposed. C4 is one-to-one and preserves all
  existence conditions. The full master hold-exception YAML is in the note.

Still incomplete: R7 does not say how to compare singular point-mass members
with continuous likelihoods, or whether reference calls returning all-zero
improper densities count as accepted parameter values. The note classifies
ordinary positive continuous domains, states the special held point-mass
behavior, and identifies the policy decision instead of inventing one. The
multistart search does not certify every possible numerical global maximum;
analytic proofs carry the existence claims. No source, README, tests or
 goldens were modified, and no commit was made.
