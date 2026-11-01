# Covariance and regular inference

The [inference catalogue](/inference/) records availability by parameter
regime and free subset. A covariance method being available does not establish
its statistical validity at a particular fit.

## DQM and information

Write $p_\theta$ for a density relative to a fixed dominating measure.
Differentiability in quadratic mean (DQM) requires an $L^2(p_\theta)$ score
$s_\theta$ such that

$$
\int\left(\sqrt{p_{\theta+h}}-\sqrt{p_\theta}
-\tfrac12 h^T s_\theta\sqrt{p_\theta}\right)^2d\mu
=o(\lVert h\rVert^2).
$$

The model Fisher information is $I(\theta)=E_\theta[s_\theta s_\theta^T]$.
DQM, finite information, full rank, suitability of the pointwise Hessian and
asymptotic normality of the fitted estimator are separate requirements.
Consistency and adequate optimization accuracy also matter. Interior parameter
values alone establish none of these. For example, the direct skew-normal
model is DQM but its information is singular at $\alpha=0$ when location is
free. Laplace location is DQM but its ordinary pointwise Hessian misses the
location information. See the [classification and derivations](../include/cppunivariate/core/inference.md)
and the [inference plan](../../dev/roadmap/2026-10-30-inference-and-dqm.md).

All statements refer to the specified free coordinates with held coefficients
known. A fitted endpoint or integer design coefficient cannot be treated as
known merely by omitting it from the derivative vector. For fitted regular
nuisance parameters, use the appropriate block of the full covariance;
inverting just the target's information block instead conditions on nuisance.

## Observed and sandwich covariance

For independent observations, let $\ell_i(\theta)=\log p_\theta(x_i)$ and
$s_i=\nabla\ell_i(\hat\theta)$ in the free natural coordinates. Where the
pointwise Hessian estimates the appropriate sensitivity, define total matrices

$$
H=-\sum_{i=1}^n\nabla^2\ell_i(\hat\theta),\qquad
J=\sum_{i=1}^n s_i s_i^T.
$$

The implemented choices are

$$
\widehat V_{\mathrm{observed}}=H^{-1},\qquad
\widehat V_{\mathrm{sandwich}}=H^{-1}JH^{-T}.
$$

There is no extra $1/n$ for these totals. Equivalently, for $A=H/n$ and
$B=J/n$, sandwich covariance is $A^{-1}BA^{-T}/n$. The outer product of the
total score is not $J$. Neither matrix is the optimizer's inverse-Hessian
approximation. Under correct specification and regular likelihood conditions,
$H/n$ and $J/n$ estimate $I(\theta)$; their finite-sample values can differ.

Under misspecification, sandwich inference needs a well-defined pseudo-true
parameter, finite score variance, nonsingular sensitivity and a suitable
estimator expansion. Model DQM does not prove these conditions under the true
law. Nonsmooth models may require sensitivity contributions absent from the
pointwise Hessian; the generic procedure excludes unsupported regimes.

R's `vcov(fit)` and JS's `api.gamma.vcov(data, fit)` default to sandwich;
`observed` selects inverse observed information. Standard errors are square
roots of diagonal entries. Held coefficients are omitted; fitted nuisance
coefficients remain free. Unsupported regimes, fitting restrictions and failed
information solves return diagnostics rather than a pseudoinverse. A successful
solve with condition number above $10^6$ warns about numerical sensitivity.
That number depends on coefficient units and cannot certify inferential validity.

## What the numerical evidence establishes

The [C++ validation report](../../dev/research/evaluations/2026-11-01-vcov-scores-report.md)
compares derivatives with independent numerical references and standard errors
with empirical sampling standard deviations. Its initial model-based experiment
used $n=500$, 200 replications per eligible family and recorded seeds. It retained
122 failed replications across six families and skipped seven slow families;
comparisons in incomplete families condition on success. Simbox follow-ups
covered the slow families and increased selected runs to 1,000 replications.
These are pointwise numerical checks, not proofs or Wald coverage experiments.

Near the skew-normal singular member ($\alpha=0.5$ in the experiment), standard
errors were unstable; at $\alpha=3$ agreement improved. Bhattacharjee at
$a=0.7$, $\sigma=1$ had 81 failures out of 200, with poor uncertainty estimates
among successes near its normal limit $a=0$. Beta-negative-binomial examples
showed substantial observed-versus-sandwich differences at $n=500$, including
away from infinite-mean settings; a follow-up at $n=5000$ improved agreement.
These results indicate weak identification and slow finite-sample convergence.
An infinite data moment alone does not prove that score moments diverge.

Neither successful differentiation nor simulation at one model point establishes
coverage, robustness to misspecification or validity in a nearby singular regime.
Consult [/inference/](/inference/) for the conditional scopes and remaining
procedure and evidence gaps before constructing Wald intervals or tests.
