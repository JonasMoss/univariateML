# Sample-checkable existence conditions for the comparison-form criteria

**Status:** not scheduled (recorded 2026-11-27).

## The idea

The existence atlas (`dev/research/existence-atlas/`) has an iff attainment criterion in 505 of 520 regimes, but
about a fifth of those (roughly 100 regimes, 20 of them default all-free fits) are in **comparison form**: "a
finite MLE exists iff the best finite fit reaches the largest limit-model value". That is a real theorem (the
limit list is proved exhaustive and the tie case settled), but nothing in it can be checked before fitting; in
practice the fitter's certificate decides it sample by sample. The all-free comparison-form families are betapr,
bhatt, burr, dblpareto, emg, expweibull, frechet, ged, ht, invburr, invtrgamma, lst, pgw, sged, slash, sn, snorm,
sstd, std and trgamma (keyword sort of `atlas.csv` criterion claims on 2026-11-27, approximate).

The upgrade, per family: prove the best **sample-checkable conditions on both sides**, a sufficient condition
for attainment (of the kind "nonconstant and sample skewness below $c$") and a sufficient condition for escape to
each limit model, and measure the gap between them. Inside the gap the certificate does the work; outside it the
fitter can decide up front, and the dossier can state the conditions. The binomial size criterion (mean above
variance, DeRiggi; `dev/lean/UnivariateML/Family/BinomialSize.lean`) is the model of a closed criterion. Several
reports already contain one-sided conditions of this kind (the SGED held-mask reports, the generalized-beta limit
reports, the 2026-11-27 atlas math reports): a math job would collect them per family, tighten them, and
quantify the gap (for example its probability under the family's own sampling distribution).

The 15 regimes whose criterion is still only one-sided belong here too: free-size beta-binomial,
beta-negative-binomial, GB2, generalized gamma (all free and held location), skew-t, Huber, the hypergeometric
family (all free, held draws, held marked count), negative hypergeometric, Tukey lambda (all free), beta prime with
a held shape, and GPD with location and shape held. For the hypergeometric family,
`2026-11-27-uniq-discrete-math-report.md#hyper-held-all-free` already splits nonconstant samples by variance
against mean (as for binomial size) and gives a terminating exact procedure; turning that into a criterion row
needs a statement and a review, not new mathematics.

## Trigger to revisit

When the uniqueness round (TASK-412 to TASK-418) is joined into the atlas, or when a fitter wants to report
"no MLE" before fitting.

## Verification

Per family: a theorem with both sufficient conditions, an exact sample in the gap showing each side is not
necessary, and a simulation of the gap's size at a few parameter values. Atlas rows go through the usual
independent review; dossiers gain the conditions in their `existence` text.
