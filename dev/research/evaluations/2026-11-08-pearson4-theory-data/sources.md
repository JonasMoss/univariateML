# Sources and scope of attribution

Read on 8–9 November 2026. The mathematical arguments and rejection-envelope
construction in this bundle are derivations for TASK-161, not theorems
attributed to the sources below. High-precision checks are numerical evidence.

* [Reviewed TASK-160 literature report](../2026-11-08-pearson4-lit-report.md):
  starting bibliography, verified density and Cheng boundary-sign convention;
  also records the limits of its earlier full-text access.
* [Cheng (2011), Using Pearson Type IV and Other Cinderella Distributions in
  Simulation](https://informs-sim.org/wsc11papers/040.pdf), especially §3.1.
  Reopened for this job. It supports the inverse-gamma boundary and inward
  likelihood expansion; this bundle derives its own fixed-(t,a,β) coefficient.
  Its discussion of the whole Pearson family is not a proof that gamma is an
  IV limit.
* [PearsonDS, PearsonIV manual](https://search.r-project.org/CRAN/refmans/PearsonDS/html/PearsonIV.html).
  Reopened: supports argument names, reference density and the existing
  implementation's complex-gamma/series/quadrature choices. Its documented
  random generator requires m>1. This is context, not an error guarantee.
* [Nagahara (1999), publisher record](https://www.sciencedirect.com/science/article/abs/pii/S016771529800265X),
  DOI 10.1016/S0167-7152(98)00265-X. The primary publisher search extract
  exposes the differential equation with (μ,τ,b,δ); that equation determines
  the map in proofs.md. Direct full-text/page retrieval returned HTTP 403.
  No uninspected claims about Nagahara's optimizer or information are used.
* [NIST DLMF §5.11(ii)](https://dlmf.nist.gov/5.11#ii), read for the
  right-half-plane Stirling remainder bound. §5.11.9 supplies the familiar
  large-imaginary-argument asymptotic, independently used in the derivations.
* Heinrich (2004), *A Guide to the Pearson Type IV Distribution*,
  CDF/MEMO/STATISTICS/PUBLIC/6820: the reviewed literature report and
  PearsonDS documentation establish its coordinate convention. Fresh PDF
  attempts at the Fermilab and CiteSeer URLs failed. Formula (7) of
  numerics.md is explicitly a newly derived local primitive, not a claim
  to have rechecked the full Heinrich/Willink branch transformations.

Repository sources read: AGENTS.md; diagnostics R6; dossier README_SCHEMA;
parameter rules R1–R8/A1–A7; argument-name history and the superseding
accuracy contract; work budgets; inference plan; Pearson-system roadmap;
lst README, certificate.hpp and location_certificate.hpp; tlambda theory
report, proofs and subset inventory. No modification of those files was made.
