# TASK-393 · Limit detector provenance

Base `4a014db6`, worktree `fam-provenance`. The changes and new tests are uncommitted. The atlas and generated JS modifier metadata were refreshed from the dossiers. The metadata changes cover only normal truncation and negative-binomial inflation.

## Transformed gamma and inverse transformed gamma

At held scale 1 on inverse data `(1, 2)`, the reciprocal delegate supplies a reachable Pareto trace with rate `2/log(2)`. Evaluating an ordinary Pareto density at the threshold replaced that trace by a value one log-likelihood unit higher. The inverse route now transports the likelihood and the candidate bounds with the reciprocal Jacobian and preserves the endpoint penalty. The forward held-scale route already retained the penalty. The asymptotic expansion proves that a finite coefficient point strictly beats the held-endpoint trace in every bounded case.

The all-free inverse switch used to certify a face on the strength of its status. The forward profile cover allows an arithmetic margin and the forward outcome already carries a numerical grade. Both inverse named limits now use that same grade. The regressions replay the held-scale diagnostic with `iterlim=1` and reach each all-free named face. Native baseline cases such as `004060`–`004066` are expected to keep their selected face and lose the certification flag. [Transformed-gamma proof and trace analysis](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

## Inverse Burr with held power

The signed Lomax cover excludes finite competitors on its binary64 input. The exact reciprocal theorem applies to exact powered reciprocals. In the held-power route the observations are rounded first in reciprocation and then again during powering, and the cover does not enclose those original-data transforms. That route keeps the inverse Weibull selection with a numerical grade. The regression uses `(1, 1/2, 1/3, 1/4)` at power 2. Held-power limit rows such as `003601` and `003605` should keep their named outcome and lose certification. [Exact transport and no-witness theorem](../../lean/UnivariateML/Family/ShapeFacesInverseBurr.lean#L86) · [Lomax face comparisons](2026-11-25-burr-math-report.md#face-comparisons-and-equality)

## Huber

A Normal or Laplace status now carries certification only when the global search sets it after closing the compact profile and the eligible tails, or upon proving the zero-residual held-scale Normal case. The numerical proposer can issue the same statuses by comparison margin. The regression distinguishes an unproved status from the public certified Normal result at `(2, 2, 2)` and held scale 1. The existing finite–Normal comparison example at `(1.5242981269692526, 1.5242981269692526)` illustrates the need to retain overlapping witnesses. [Free-tuning certificate](2026-11-07-huber-cert-report.md#closing-the-noncompact-ends) · [Finite–Normal comparison](2026-11-07-huber-cert-report.md#counterexamples-and-reproducible-checks)

At held tuning `c = nextafter(1, 0)`, the sample `(-1, -1, 1, 1)` has a strict maximizing interval that is narrower than the old comparison guard. If the two levels have equal counts, the mean absolute deviation from the median equals the central half-gap. The all-tail score gives the common scale as `c` times that half-gap, and the interval half-width is the half-gap times `1 - c²`. The fitter evaluates the latter with a fused operation independently of the rounded scale and returns the closed interval and its midpoint. Concavity in the inverse chart gives global optimality, and strict concavity in inverse scale gives a common scale at all maxima. The new regression checks both bounds, the scale, and the certified nonunique outcome. A successful fit whose set comparison remains unresolved carries unknown cardinality and an uncertified grade. Expect the balanced near-cutoff regression to gain a certified interval. The public strict face regressions keep their grade. [Huber joint maximizer set](2026-11-25-lean-huber-report.md#theorems) · [All-tail scale score](2026-11-26-huber-flat-set-report.md#maximizer-sets-and-numerical-decisions)

## Two-piece normal

The all-free polynomial scan compares rounded stationary heights. The generic held-moment face path also uses a likelihood allowance. A selected face on either path now carries a numerical grade. The explicit held-mean and held-scale constant-sample bounds and the separated held-scale small-spread comparison still certify their faces. The constant held-scale path attaches both equal faces. The tests cover the numerical right face on `(-1, -1, -1, 1)`, the sharp bound at held mean 0 on `(2, 2, 2)`, and the equal faces at held scale 1. The initial regression caught a missing provenance assignment on the held-mean constant route, and the corrected run passed. All-free limit rows such as `006546`–`006552` should lose certification. Held-moment rows depend on the branch reached. [Half-normal bounds and implementation scope](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md#details)

## GED

The existing runtime already grades the uniform proposal by its completed certificate. Free standard deviation permits the signed shape-tail theorem and strict compact cover. A constant sample away from a held mean also has a strictly increasing profiled objective up to the uniform limit. Held standard deviation has a different contact penalty and an unresolved proposal can reach the finite-shape fallback. The dossier now grades the closed free-scale detector as proved. The regression distinguishes the numerical proposal from the certified cover on 40 equally spaced points in `[-1, 1]` and checks the held-scale contact case `(-sqrt(3), sqrt(3))`. Expect the existing certified free-scale limits, including `002356` and `002373`, to preserve their outcomes. [Free-scale proof](2026-11-25-ged-math-report.md#free-standard-deviation) · [Held-scale contact](2026-11-25-ged-math-report.md#exact-contact-outcomes-and-the-rounded-endpoint)

## Truncated normal

The boundary statuses previously implied certification even at the rounded variance threshold. In natural coordinates concavity makes a strict inward quadratic-score sign sufficient to exclude every finite parent fit, after matching the mean in the two-free-coefficient problem. On a bounded interval, the checked unit-window tilt bracket supplies a variance upper bound. With parent mean held, the test is instead on the uniform second moment about that mean. Only a strict comparison outside the arithmetic margin is certified. A numerical direct selection stays a limit, and a close comparison with an evaluated finite witness retains both witnesses. The regressions exercise the rounded one-sided equality at `(0, 2)` and strict translated, reflected, finite-window and held-mean limits. Route snapshots containing strict comparisons should keep their grade. Rounded equality or a direct selection inside the moment margin should lose certification. [Tilt and coefficient comparison](2026-11-21-tnorm-limit-report.md#coefficient-rule-and-comparison) · [Strict sign and bracket bound](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md#truncated-normal-face-comparisons)

## Zero-inflated negative binomial

A named face now carries provenance from the specific public return site. Size and probability must be free. An ordinary-NB reduction can use its Poisson theorem, including held positive inflation on samples without zeros. Otherwise the signed Poisson tail and complete rectangle cover apply to eligible free or held inflation. With inflation free, certified positive-count exclusion followed by a strict ordinary-NB comparison can also close a face. The legacy proposer does not supply those proofs.

The regression checks both unproved statuses, the Poisson and held-inflation ZIP reductions on `(1, 1, 1)`, and the same-inflation ZIP cover with 50 zeros, 50 fours, and inflation 0.5. The certified public outcomes and their held coefficients should stay the same. [NB Poisson supremum](../../lean/UnivariateML/Family/NegBinomial.lean#L713) · [ZINB coverage and hold scope](2026-11-06-zinb-cert-report.md#implementation-and-mathematical-scope)

## Validation

All runs used the workstation and the shared CPU reservations. Each item passed `just test-changed`. The GED, truncated-normal and ZINB iterations used the CRAN tier, followed by the final development-tier run. The existing truncated-normal and ZINB modifier CTest shards also passed.

| Final check | Result |
| --- | --- |
| `just test-changed --no-long-double` | Native C++ 61/61. Double-only C++ 61/61. R 11,872 passes, 71 tier skips, zero failures and warnings. JS build passed and JS tests 413/413. |
| `just cpp-test` | 136/136 CTest shards. |
| `python3 dev/tools/existence_atlas.py --check` and `python3 dev/validation/existence_keys.py` | Passed. The atlas contains 87 families and 520 base regimes. |
| `just site-check` | Passed. All 86 generated C++17 snippets, family pages, browser checks and internal links resolved. |
| `git diff --check` | Passed. |

The final shell wrapper exited 2 because its cleanup trap had a Bash syntax error after the successful site check. Every validation command had already completed. The regenerated modifier registry was present and verified to differ only for `norm` and `nbinom`. No golden values were changed. The case IDs above are examples from the stored native snapshots and the snapshot effects are predictions.

## Open

- Inverse Burr's held-power Lomax cover needs bounds for the original-data reciprocal and power transforms before it can certify that broader route.
- For held-tuning Huber with more than two observation levels and an unresolved central-gap sign, an exact or enclosing computation of the all-tail deviation and cut is still needed to certify interval cardinality. The balanced two-level proof avoids that comparison.
- A stronger grade for generic transformed-gamma or two-piece normal face selections needs strict exclusion of every competing finite value and face through the arithmetic margin. The numerical selections retain that grade.
