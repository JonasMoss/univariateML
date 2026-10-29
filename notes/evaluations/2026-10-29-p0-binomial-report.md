# TASK-79 p0-binomial: continuation report

The hold-subset tables, candidate conditions and metadata proposals are in
`notes/evaluations/2026-10-29-p0-binomial.md`. **The requested research is
still incomplete**: the beta-binomial free-size and truncated-binomial
compactifications need independent proofs, and not every limit, reference
boundary, and implemented hold has a complete independent numerical check.
The finite-grid searches do not certify global optima or infinite-size tails.

Proved or source-supported: fixed-design CMB via its finite exponential-family
convex hull; fixed-design truncated-binomial via monotone conditional means;
the C6 compactification and finite integer domains; several fixed-probability and fixed-shape size
profiles; and the binomial variance criterion (DeRiggi 1983, exact citation in
the note). Other entries relying on the compactifications are marked conjectural.

Two material disagreements with master/design:
- `mlbinom(c(2,6,6))` returns size 6 (logLik −6.826661), while size 11 has
  logLik −6.215143. Another independent sample also shows a suboptimal size.
- `dcmb` accepts prob 0/1 and size at most 10^7, contrary to its README.
  Consequently master rejects some attained endpoint fits and the literal R7
  bounded-size existence problem differs from the unlimited-size design.

R7 also needs decisions on infinite tbinom bounds, accepted by the reference
but excluded by the README, and nhyper's n = 0: `extraDistr::dnhyper` crashed
in isolated subprocesses, so the analysis uses the natural point-mass completion.
No R5 shape restriction is justified; proposed kinds agree with the design.

On simbox, saved probes cover default samples at n = 20, 100, 1000 and 2, 3, 5,
ties, and several limiting laws. Independent density multistarts were compared
with master on fixed designs and supported shape holds; all 169 finite
comparisons in the two fixed-design tables met 1e-6*n. The CMB README formula
agreed with `dcmb` on the tested grid within 1.8e-14 in log density. The
free-size comparison separately identified the binomial failures above. The
scripts and result TSVs are in `notes/evaluations/2026-10-29-p0-binomial/`.
