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

The contract is local: it concerns the maximizer the fit reports. Global
optimality is a separate claim with its own evidence (the dossier
`existence:` block and certificates). It therefore covers uncertified fits
too: `certified = FALSE` withdraws only the global claim.

## d/p/q

Every `dpqr.hpp` function is accurate for the exact binary64 arguments it
receives, over the whole parameter domain: log densities and log
probabilities within `1e-10 * max(1, |v|)` of the exact value `v`, natural
probabilities within relative `1e-10`, quantiles within relative `1e-9` or
the table's resolution floor. These are the
[truth consumer's](../../univariateML-cpp/tests/test_support.hpp)
tolerances; the contract extends them from the tabulated points to every
parameter value.

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
