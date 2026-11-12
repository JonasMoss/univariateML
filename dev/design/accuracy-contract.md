# Accuracy contract and uniform controls (decided 2026-11-07)

Decided by Jonas for the 2.0 release: every fitter meets the same accuracy
target, and every fitter takes the same two controls. Compatibility with the
CRAN univariateML arguments and defaults is not a constraint. This supersedes
[decision D3](../roadmap/2026-10-28-cran-argument-names.md) on argument names.

## Contract

A fit that returns status `Ok` has each free coefficient within relative
accuracy `tol` of a maximizer of the likelihood:

    |theta_hat_i - theta_star_i| <= tol * max(|theta_star_i|, s_i)

`s_i` is a floor for coefficients that can be zero or change sign. It is 1
unless the family's dossier declares a natural scale. For example, a location
coefficient uses the fitted scale. The default is `tol = 1e-10`.

The stopping rule must justify the bound. Accepted rules:

- closed forms, accurate to rounding;
- bracket width for monotone one-dimensional scores, with the bracket
  confirmed by computed signs (`beta/mle.hpp`, `decreasing_root`);
- a Newton-step error estimate: at a point where the observed information is
  positive definite, each component of `H^{-1} g` is at most
  `tol * max(|theta_i|, s_i)`, with quadratic convergence established by the
  last steps;
- exact enumeration for integer coefficients.

Step size alone, objective decrease alone, or a gradient norm without the
curvature that converts it into a parameter error do not satisfy the contract.

When double precision cannot reach `tol` because the problem is
ill-conditioned (the score's rounding noise divided by the curvature exceeds
`tol`), the fit does not report `Ok` with a silently worse answer. It reaches
the best accuracy the formulation allows, and either the family reformulates
the problem (beta: mean and precision coordinates) or the fit reports a
numerical-accuracy failure.

Accuracy failures come in two kinds, reported separately with the best
candidate. `tolerance`: the requested `tol` is below the representation or
refinement method's supported resolution (the nearest double to 2/3 is
3.7e-17 away); the report gives the diagnosed resolution floor.
`conditioning`: the score's rounding noise over the curvature exceeds a
representable `tol`, which can happen at the default and calls for a
reformulation. Evaluation breakdown (NaN, overflow) stays `numerical`.

The report's `attainable_tol` is in public coefficient units, held
coordinates excluded: one binary64 spacing at the candidate, which rounding can
always meet, or for a closed ratio its exact distance to the nearest double. A
finer request can still succeed when the maximizer lies close to a double.
When a refinement method imposes a larger floor, `attainable_tol` retains that
floor after conversion to public coefficient units. The shared Newton polish
requires at least four binary64 epsilons in its local coordinates; this is a
method limit, not a proof that no closer representable maximizer exists. A
failed refinement without a noise-over-curvature bound stays a numerical
failure; reaching a cap alone is not conditioning.

The contract is local: it concerns the maximizer the fit reports. Global
optimality is a separate claim with its own evidence (the dossier
`existence:` block and certificates). It therefore covers uncertified fits
too: `certified = FALSE` withdraws only the global claim.

## Log likelihood

A fit's `logLik` is the maximized value the fit establishes: the certified
supremum, or the value at the returned local maximizer. It is accurate to
`1e-10 * max(1, |logLik|)`. Where the likelihood is Lipschitz near the
maximizer, the usual case, it agrees with the summed d/p/q log density at the
returned public coefficients to that tolerance.

Where it is not, no representable coefficient vector attains the supremum.
Examples: a power cusp below one at an observation (SGED and GED with shape
below one, mode on a data point), or a maximizer at a support endpoint reached
through a transformation of the public coefficients. Then the summed density
at the returned coefficients is at most `logLik` (up to rounding) and below it
by at most an allowance the family dossier states; for a cusp of power `nu`
it is about `eps^nu` per cusp observation (1e-4 at `nu = 0.3`). Returned
coefficients never place an observation outside the support.

## d/p/q

Every `dpqr.hpp` function is accurate for the exact binary64 arguments it
receives, over the whole parameter domain: log densities and log
probabilities within `1e-10 * max(1, |v|)` of the exact value `v`, natural
probabilities within relative `1e-10`. A quantile `q_hat` is accepted if it
is within relative `1e-9` of the true quantile, or if its probability
`F(q_hat)`, on the tail and scale of the input, is within relative `1e-12` of
the input. The second test only forgives ill-conditioned quantiles, such as
one that cancels to nearly zero against a nonzero center; in a tail, a
relative quantile error `d` moves the log tail by about `shape * d`. These are
the [truth consumer's](../../univariateML-cpp/tests/test_support.hpp)
tolerances (it still applies the probability test only at discrete jumps and
at an interior zero); the contract extends them from the tabulated points to
every parameter value.

This is forward accuracy, not accuracy relative to the problem's
conditioning. Fitted parameters are exact doubles, and near limit models the
amplifying cancellation can usually be removed: for `x/s` in `[1/2, 2]`,
`x - s` is exact, so `shape * log1p((x - s)/s)` replaces
`shape * (log(x) - log(s))`. A region that cannot meet the contract returns
`NaN` and is named in the dossier. The `stats` functions the R package uses
for its 14 `stats` families are outside this contract; `stats::dweibull` and
`stats::pweibull` miss it at large shapes.

## Controls

Every R fitter `ml<id>(x, <held coefficients>, tol = 1e-10, iterlim = 100)` and
every JS and C++ entry point take only these controls:

- `tol`: the requested relative accuracy above (C++ `tol`);
- `iterlim`: the work budget of [work budgets](work-budgets.md) (C++ `maxit`).

`reltol`, `abstol`, `gradtol` and the CRAN-era defaults (`1.220703e-4`,
`1.490116e-08`) are removed. Start-value arguments (`<param>0`) are removed
unless a family's search genuinely needs a user start, since starts must
affect only speed. Families that need one say why in the dossier.

## Verification

Golden references ([reference.py](../validation/quality/reference.py)) and the
fit audit's stress cases are compared with fits at a uniform test tolerance
of a small multiple of `tol`. A family needs a documented, condition-limited
exception to use a wider one.

## Open

- The scale floor `s_i` per family, declared in the README front matter.
- The R package version (2.0.0) and the NEWS entry describing the breaking
  change.
