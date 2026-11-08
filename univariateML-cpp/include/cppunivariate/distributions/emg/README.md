---
id: emg
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu, sigma > 0 and lambda > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Exponentially Modified Gaussian
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: normal scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: lambda
  symbol: '\lambda'
  role: exponential rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: demg
  sampler: remg
  control:
    mu: Optional held normal-component mean; NULL estimates it.
    sigma: Optional held normal-component standard deviation; NULL estimates it.
    lambda: Optional held exponential rate; NULL estimates it.
    reltol: Relative accuracy of every free coefficient; location uses the normal scale as a floor.
    iterlim: "Work limits: min(iterlim, 100000) iterations per solve; 128 times that many global certificate evaluations."
log_density: log(lambda/2) + lambda*(mu-x) + lambda**2*sigma**2/2 + log(erfc((mu+lambda*sigma**2-x)/(sqrt(2)*sigma)))
limits:
  - model: norm
    firmness: proved
    when: 'The rate satisfies $\lambda\to\infty$ with $\mu$ and $\sigma>0$ fixed.'
    detect: 'Pointwise domination, or exhaustive exclusion of finite shape intervals and a signed normal-tail proof; otherwise an unexcluded normal face produces an unresolved stop.'
    status: NormalLimit
    numerical: false
  - name: translated exponential
    firmness: proved
    when: 'The normal scale satisfies $\sigma\to0$ with location and positive rate fixed.'
    detect: 'Constant-data domination, or exhaustive exclusion of finite shape intervals and both tails using the compatible physical holds and endpoint half-density.'
    status: TranslatedExponentialLimit
    numerical: false
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlemg())
  mlemg(x)

group: normal convolutions
construction:
- compounding
relations:
  convolution_of:
  - family: norm
    operation: sum
    components:
    - family: norm
      at:
        mean: mu
        sd: sigma
    - family: exp
      at:
        rate: lambda
fit:
  method: profile-search
  certified: 'yes'
  note: 'emg/README.md, Details: location-only has a signed root enclosure; the other six subsets use global shape bounds and validated coefficient enclosures; unexcluded normal faces and exhausted or inseparable searches stop with witnesses.'
areas:
- measurement and errors
classification: specialist
classification_reason: Used to model chromatographic peaks combining Gaussian broadening and exponential delay.
summary: Exponentially Modified Gaussian provides a model for chromatographic peaks combining Gaussian broadening
  and exponential delay.
aliases:
- "exponentially modified normal"
- "ex-Gaussian"
- "EMG"
references:
- Grushka, E. (1972). Characterization of exponentially modified Gaussian peaks in chromatography. Analytical Chemistry,
  44(11), 1733–1738. doi:10.1021/ac60319a011.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant samples have unbounded likelihood.
    For nonconstant samples a finite maximum need not exist, because a
    normal or translated-exponential limit can compete with interior fits.
    A complete existence criterion and joint uniqueness result are not
    established. Holding $\lambda$ removes the normal escape; holding
    $\sigma$ removes the translated-exponential escape. With both held,
    free location has a unique finite maximum. Other held subsets use
    conditional global bounds and their compatible limits; an unexcluded
    normal face stops unresolved.
  reason: >-
    The normal and exponential limits follow from the independent-sum
    representation. Fixed dimensionless shape $\lambda\sigma$ gives a
    strictly concave location and inverse-scale objective with a unique
    finite maximum for nonconstant samples. This does not prove uniqueness
    when shape is free or when the physical rate alone is held.
---

The exponentially modified Gaussian is the sum of independent normal and exponential variables, used for asymmetric response times and chromatographic peaks.

## Details

With positive normal scale and exponential rate held, the free location has a
unique finite global maximum, including for constant data. Its decreasing
score is enclosed with a signed bracket; acceptance requires the returned
location to be within `reltol * max(abs(mu_star), sigma)` of every point in
that bracket. Score signs use consecutive Mills continued-fraction convergents
in the positive tail and a practical margin of 128 double epsilons times the
sum of absolute score and Mills-evaluation operands, with an absolute
subnormal floor. An
unresolved sign or unrepresentable result
stops with numerical failure; exhausting the solver iterations stops without
an estimate. These certificates are in exact arithmetic conditional on the
computed comparisons and that margin, rather than outward-rounded intervals.

When location is held at or above the sample maximum and rate is free, each
normal kernel in the convolution is bounded by its zero-delay value. The
conditional normal face therefore dominates globally, also when scale is
free. Constant observations with held scale and free location and rate have
the same conclusion by the normal peak bound. If all observations equal the
held location and scale and rate are free, the likelihood is unbounded instead.
With constant observations, free scale and held rate, the exponential face
is globally optimal when location is free or held at the observation. Its
supremum density is the rate with free location, and half the rate with held
location.

For the other six subsets, the outer shape executor profiles the strictly
concave fixed-shape problem with public holds imposed as constraints. It
excludes both shape tails, then subdivides intervals using conditional
likelihood upper bounds. A surviving finite mode needs a Krawczyk score-root
enclosure accurate in every free physical coefficient, a negative-definite
Hessian box, and a uniform nuisance-root inclusion across its shape interval.
These establish a unique global inner optimizer and a strictly concave local
profile; every other interval must be excluded. Separated surviving modes
stop as a tie. A normal face without a strictly better finite witness stops
unresolved unless a global domination proof succeeds. Budget, numerical and
unresolved stops retain finite and compatible face witnesses.

The interval operations use a practical
32-double-epsilon operand margin, Mills evaluations use 64 epsilons, and
likelihood comparisons use 256 epsilons on separate log-density operands,
with standardized-coordinate error propagated through the score enclosure. The certificate is in exact arithmetic
conditional on the computed comparisons and these margins; it does not use
outward-rounded arithmetic. The [derivations and independent checks](../../../../../dev/research/evaluations/2026-11-07-emg-cert-report.md)
include the distinct curvature bound for held physical rate, both uniform
shape tails, endpoint multiplicities and a finite/normal-face tie counterexample.

Write $X=\mu+\sigma Z+E_\lambda$, with $Z$ standard normal and
$E_\lambda$ independent exponential. For fixed finite $\mu$ and $\sigma>0$,
$\lambda\to\infty$ makes $E_\lambda\to0$ in probability; the convolution
density tends pointwise to the normal density by bounded convergence. For
fixed finite $\mu$ and $\lambda>0$, $\sigma\to0$ gives the translated exponential:
the convolution density converges at every $x\ne\mu$ by the approximate
identity property. Both limits also hold in distribution. At $x=\mu$ the
latter density tends to half its one-sided value, a distinction relevant
when an observation lies exactly at the endpoint. The executor retains the endpoint multiplicity in its face bounds.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-skewreal.md) describes the limiting paths and the scope of the boundary comparisons.

For fixed dimensionless shape $k=\lambda\sigma$, the log-likelihood is strictly
concave in $(u,b)=(\mu/\sigma,1/\sigma)$. For nonconstant observations
it has a unique finite maximum in these coordinates; constant observations
collapse when location and scale are free. Holding the physical rate fixed is a different constraint and uses its own
shape curvature bound. See the [proof and scope](../../../../../dev/research/evaluations/2026-10-31-logconcave-report.md).

$\mu$ is the normal component mean, $\sigma$ its standard deviation and
$\lambda$ the exponential rate. The mean delay is $1/\lambda$.
Constant observations can have unbounded likelihood. A finite interior maximum
need not exist. Normal and translated exponential faces are compared with
interior candidates, respecting held parameters. The translated exponential face has endpoint $\min_i x_i$ and rate
$1/\overline{(x-\min_i x_i)}$ when both are free.
With a held endpoint exactly at the minimum the limiting density there is
half the exponential density. Repeated minima count separately.

## R notes

`mlnorm` fits an unrestricted normal and does not retain held EMG
coefficients automatically. There is no translated-exponential fitter.
For a free endpoint, `mlexp(x - min(x))` fits distances from the fitted
endpoint. Held endpoints or rates require their conditional comparison.
