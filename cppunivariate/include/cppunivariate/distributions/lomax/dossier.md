# Lomax

## Description

The Lomax distribution (also known as Pareto Type II) is a
heavy-tailed continuous distribution on the non-negative real line.

## Edge cases

The Lomax converges to an Exponential as $\kappa \to \infty$ with
$\lambda \kappa$ held fixed. The sample-side signature is
$\overline{x^2} < 2\,\bar x^2$: in that regime the joint MLE does
not exist and the fitter reports
`LomaxStatus::MeanVarianceCondition`. Use `mlexp` instead.

## See also

- `mlexp` — the $\kappa \to \infty$ limit; the right model when
  `MeanVarianceCondition` fires.

## References

Kleiber, C. and Kotz, S. (2003), *Statistical Size Distributions in
Economics and Actuarial Sciences*, Wiley, Chapter 3.
