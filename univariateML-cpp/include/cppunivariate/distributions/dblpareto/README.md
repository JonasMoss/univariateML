---
id: dblpareto
name: Double Pareto
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
  - {name: scale, symbol: b, role: scale, domain: '(0, inf)', simulate: 1, kink: true}
  - {name: left_rate, symbol: '\alpha', role: shape, domain: '(0, inf)', simulate: 2}
  - {name: right_rate, symbol: '\beta', role: shape, domain: '(0, inf)', simulate: 1}
r:
  density: ddblpareto
  sampler: rdblpareto
  control:
    scale: Positive kink on the observation scale. NULL estimates it.
    left_rate: Positive left log-scale rate. NULL estimates it.
    right_rate: Positive right log-scale rate. NULL estimates it.
inference_numeric:
  - regime: interior
    available: false
    condition: Positive finite coefficients with any free subset.
    classification: "univariateML-cpp/include/cppunivariate/distributions/dblpareto/README.md#numerical-inference"
    reason: Fitted-Hessian validation is unavailable for this family.
log_density: log(left_rate*right_rate/(left_rate+right_rate)) - log(x) + Piecewise((left_rate*log(x/scale), x <= scale), (-right_rate*log(x/scale), True))
limits:
  - model: pareto
    firmness: proved
    when: The left rate tends to infinity with the right rate and kink fixed.
    detect: The left rate is free and its one-sided profile wins at a kink at or below the sample minimum.
    status: ParetoLimit
    detect_firmness: proved
    proof: ["UnivariateML.DoublePareto.pareto_face", "dev/research/evaluations/2026-11-25-dblpareto-math-report.md#exhaustion-of-the-boundary"]
    detect_proof: ["UnivariateML.DoublePareto.scan_supremum", "dev/research/evaluations/2026-11-25-dblpareto-math-report.md#one-held-rate"]
  - model: power
    firmness: proved
    when: The right rate tends to infinity with the left rate and kink fixed.
    detect: The right rate is free and its one-sided profile wins at a kink at or above the sample maximum.
    status: PowerLimit
    detect_firmness: proved
    proof: ["UnivariateML.DoublePareto.power_face", "dev/research/evaluations/2026-11-25-dblpareto-math-report.md#exhaustion-of-the-boundary"]
    detect_proof: ["UnivariateML.DoublePareto.scan_supremum", "dev/research/evaluations/2026-11-25-dblpareto-math-report.md#one-held-rate"]
  - name: unbounded likelihood
    firmness: proved
    when: Both rates grow without bound at the common observation.
    detect: Both rates are free, all observations coincide, and the kink is free or held at the common value.
    status: Unbounded
    message: unbounded
    detect_firmness: proved
    proof: ["UnivariateML.DoublePareto.constant_unbounded", "UnivariateML.DoublePareto.zero_distances_unbounded"]
    detect_proof: ["UnivariateML.DoublePareto.constant_unbounded", "UnivariateML.DoublePareto.zero_distances_unbounded"]
group: Pareto and Lomax
construction: [base]
relations: {}
fit:
  method: closed-form
  certified: 'yes'
  note: Exact profiling gives an exhaustive comparison at the sample knots. Unseparated likelihood comparisons withdraw certification.
areas: [actuarial, economics and income]
classification: specialist
classification_reason: A two-sided power-law model for positive quantities.
summary: Double Pareto has power-law behavior on either side of a kink.
existence:
  uniqueness: proved
  statement: >-
    A finite all-free fit exists for nonconstant data when an interior knot attains
    the best profile value including both endpoint limits. Interior knot ties
    give distinct maximizers. With one rate free the same comparison applies
    among finite knots and the relevant one-sided limit. A held kink gives a
    unique finite fit if each free rate has positive total deviation on its
    side. With both rates free, one zero deviation and one positive deviation
    give a finite unattained one-sided limit, while two zero deviations give
    unbounded likelihood. With one rate held, zero free-side deviation gives a
    finite unattained supremum, including on constant data. Holding both rates
    always gives a finite fit, with a possible kink interval.
  reason: >-
    Conditional rate optima are explicit. The profiled likelihood is strictly
    convex between distinct observation knots with at least one free rate.
    At fixed rates the kink criterion is weighted absolute deviation in log
    observations.
  equivariance:
    group: power-scale
    map: {scale: A*scale**B, left_rate: left_rate/B, right_rate: right_rate/B}
    held: {}
    evidence: On the log scale this is an affine change of the kink and both exponential rates.
  two_point:
    outcome: limit
    evidence: The two endpoint profiles tie, and the single interval has a strictly convex profile.
examples: |
  set.seed(1)
  x <- rml(500, mldblpareto())
  mldblpareto(x)
---

The logarithm of a double Pareto variable is asymmetric Laplace. Double Pareto is a limiting law of GB2 and of Feller–Pareto on excesses over its threshold.

## Details

The name `scale` matches the GB2 and Feller–Pareto scale and gives the kink in observation units. The names `left_rate` and `right_rate` identify the two exponential rates on the log scale and preserve the direction labels used in Feller–Pareto's limit payload. They become the shape of `mlpower` and `mlpareto`, respectively, on the one-sided limits.

Let $L$ and $R$ be the mean positive distances below and above the log kink. With both rates free the profiled negative log likelihood, up to an additive constant, is $2\log(\sqrt L+\sqrt R)$. The two means are affine between consecutive distinct observations and the profile is strictly convex there. With one rate held, maximization over the other rate also gives a strictly convex profile on each such interval. Consequently a free kink need only be compared at observations, including the one-sided suprema at the extremes. Outside the data range the profile strictly worsens as the kink moves away from the nearest extreme. At fixed rates the kink is a weighted quantile. When the weighted counts balance in a gap, every kink across that gap maximizes the likelihood.

A rate with no positive log distance on its side tends to infinity. If the other rate is held, keep it when evaluating the limiting law. When both distances vanish and both rates are free, the likelihood is unbounded. This also covers a single observation with a free kink.

The comparisons use positive accumulations of adjacent log gaps to avoid subtracting nearly equal absolute logs. A margin of $128\epsilon(1+|s_1|+|s_2|)$ in the compared profile coordinates gives a practical separation test in binary64. Unseparated comparisons retain a finite closed-form candidate when one is available and report undetermined uniqueness. Exact reciprocal reflection can establish a tied pair, with exact kinks and relative rate enclosures of width $2\times10^{-11}$. Held-rate flat intervals are recognized by comparing the exact products of the rates and the integer counts.

## Numerical inference

Fitted-Hessian validation is open. The sample-kink likelihood has one-sided derivatives, and a flat weighted-quantile interval gives no unique estimate.
