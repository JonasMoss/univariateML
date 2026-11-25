# Likelihood transfers and fixed-shape concavity

TASK-249 · 2026-11-25

The 51-family ledger screen includes normal and gamma images whose proofs
already appear in their dossiers. Fixed-shape concavity establishes further
global and uniqueness claims, including exact maximizing intervals for
Huber, Laplace and some uniform fits. These results concern exact likelihoods
on the stated hold slices. The accuracy of a returned coefficient vector
requires its own check.

[Claims inventory](2026-11-24-claims-inventory-report.md) ·
[51-family triage with file and line references](2026-11-25-transfer-math-data/triage.csv) ·
[Earlier EMG and Bhattacharjee proof](2026-10-31-logconcave-report.md)

## 1. Transfers

**Proved.** For a fixed differentiable data bijection \(y=T(x)\) with finite
nonzero derivative at the observations, suppose
\(f_X(x;\theta)=f_Y(T(x);M(\theta))|T'(x)|\). Then
\[
\ell_X(\theta)=\ell_Y(M(\theta))+\sum_i\log|T'(x_i)|.
\]
On a hold slice \(H\) the image is \(M(H)\). If \(M\) is injective on \(H\),
the entire argmax sets correspond bijectively. Attainment, unboundedness
and strict likelihood comparisons, including comparisons with matched limit
paths, are preserved. A noninjective map instead lifts the source argmax
to its full inverse image. The same proof works at endpoints where the
density identity and the support conventions extend. An estimated threshold
can move both the transformed data and the Jacobian.

All samples below are finite and nonempty. Constant includes one observation.
Fully held feasible parameters require only evaluation. Gamma source
coordinates are shape \(a\) and rate \(r\), whose reciprocal is public scale.

| Target and fixed observation map | Source and hold correspondence | Exact conditions |
|---|---|---|
| invgamma, \(y=1/x\), \(x>0\) | Gamma \((a,r)=(\alpha,\beta)\), Jacobian \(x^{-2}\); each public hold fixes one source coefficient. | Both free uniquely attained exactly for nonconstant samples and unbounded for constants. Either single hold gives a unique fit even for constants. |
| lgamma, \(y=\log x\), \(x>1\) | Gamma \((a,r)=(\mathrm{shapelog},\mathrm{ratelog})\), Jacobian \(1/x\); each hold transfers. | Same gamma rules. |
| naka, \(y=x^2\), \(x>0\) | Gamma \(a=m,r=m/\Omega\), Jacobian \(2x\). Holding \(m\) fixes shape. Holding \(\Omega\) fixes the gamma mean, tracing \(r=m/\Omega\). | With both free the gamma rule applies; held \(m\) gives \(\widehat\Omega=\bar y\). With \(\Omega\) held the score per datum is \(\log m-\psi(m)-\overline{y/\Omega-1-\log(y/\Omega)}\). The digamma difference decreases from infinity to zero. The nonnegative sample term vanishes only if all \(y=\Omega\), when concentration is unbounded; otherwise the score has one root. |
| invweibull, \(y=1/x\), \(x>0\) | Weibull shape \(k\), scale \(1/s\), Jacobian \(x^{-2}\). Shape hold is identical and scale hold reciprocal. | Both free uniquely attained iff nonconstant and unbounded for constants. Held shape permits all samples. Held scale gives unique shape unless all \(x=s\), when unbounded. |
| frechet, \(y=1/(x-\mu)\) at held \(\mu\), \(x>\mu\) | Weibull shape \(\lambda\), scale \(1/s\), Jacobian \((x-\mu)^{-2}\). | Same conditions on positive excesses. If a closed floor \(\lambda\ge1\) is also imposed, clamp the unique finite shape root to 1 by monotonicity of the conditional or profiled score. Unbounded cases persist. With location free the transfer is unavailable; the prior fixed-shape theorem instead requires \(\lambda(n-k)>k\) for minimum multiplicity \(k\). |
| lnorm, \(y=\log x,\ x>0\); logitnorm, \(y=\log[x/(1-x)],\ 0<x<1\) | Normal with identity parameter map. Jacobians \(1/x\) and \(1/[x(1-x)]\). | Both free give the transformed mean and divisor-\(n\) standard deviation for nonconstant data, and unbounded collapse for constants. Held location gives the root mean square residual unless all residuals vanish, when unbounded. Held scale gives location \(\bar y\) even for constants. |
| llogis and pareto3 at held threshold \(\mu_0\), \(y=\log(x-\mu_0)\) for positive excesses; take \(\mu_0=0\) for llogis | Logistic with location \(\log s\) and scale \(1/k\), Jacobian \(1/(x-\mu_0)\). Public scale fixes latent location; public shape fixes latent scale. | Joint fit unique iff nonconstant, otherwise unbounded. Held shape permits constants. Held scale is unique unless all excesses equal \(s\), when unbounded. Free threshold can introduce a singularity. |
| weibull, \(y=-\log x\); invweibull and held-location frechet, \(y=\log(x-\mu_0)\) | Gumbel maxima with location \(-\log s\) for Weibull and \(\log s\) otherwise, and scale \(1/k\) throughout. Jacobian reciprocal of the positive observation or excess. | The same hold rules as the Weibull transfer. Public scale fixes latent location and shape fixes latent scale. |
| invparalogis, \(y=1/x\), \(x>0\) | Paralogis \((k,1/s)\), Jacobian \(x^{-2}\); all four masks. | Joint attainment iff nonconstant, constant collapse unbounded. Either single hold permits constants. Held shape gives a unique scale. Joint and held-scale uniqueness transfer where a strict parent certificate excludes competitors. |
| invburr, \(y=1/x\); pareto4, \(y=x-\mu_0\) at held threshold; positive transformed observations | Burr with identical shapes and reciprocal or identical scale, respectively; all eight masks transfer. Jacobians \(x^{-2}\) and 1. | Held first shape gives a unique joint power/scale maximum iff nonconstant. Both shapes held give a unique scale even for constants. Only power free is unique unless all observations equal held scale, when unbounded. Only first shape free is unique for all samples. With scale alone held, the parent finite-unique branch for nonconstant data needs some \(y<s_{\rm Burr}\), equivalent to some \(x>s\) for inverse Burr. A constant on that side or at held scale is unbounded; otherwise the remaining branch selects the endpoint face. The all-free two-face criterion and per-input certificates transfer, with minimum multiplicity becoming maximum multiplicity under reciprocation. |
| Burr with second shape \(c\) held, or corresponding invburr or pareto4 mask | Lomax on \(y=x^c,\ x^{-c}\), or \((x-\mu_0)^c\), with first shape \(k\), rate \(s^{-c},s^c,s^{-c}\), respectively. Absolute Jacobian \(cy/(x-\mu_0)\) with \(\mu_0=0\) where absent. | Both residual coordinates free inherit the finite-candidate versus Lomax exponential-face criterion. The face becomes Weibull or inverse Weibull. Holding scale gives explicit first shape; additionally holding first shape instead gives the unique single-rate solution. |
| fpareto, \(y=x-\mu_0>0\) at held threshold | GB2 with power \(\gamma\), scale \(s\), beta shapes \(p=\tau,q=\alpha\); unit Jacobian. All 16 residual masks transfer. | Includes unique nonconstant fits at fixed beta shapes, or at fixed power and scale, and every additional compatible hold. The free-threshold singularity is a separate case. |
| trgamma and invtrgamma, \(x>0\) | Holding power \(p\) gives gamma on \(x^p\) or \(x^{-p}\) with rate \(s^{-p}\) or \(s^p\), Jacobian \(p y/x\). Reciprocation swaps the two families with \((k,p,s)\mapsto(k,p,1/s)\), all eight masks. | Fixed power inherits the gamma rules. Fixed gamma shape uses the strict log-data result below. With only scale held, the two free shapes require a fixed-endpoint face comparison; a constant above ordinary scale or below inverse scale is unbounded. The joint lognormal and power/Pareto comparisons and any strict winner transfer. |
| betapr at held scale; gb2 and fpareto at held power and scale; genbeta at held endpoint and power; nsbeta at both endpoints held | Beta on \(y=t/(1+t)\), \(t=x/s\) or \(t=(x/s)^c\); for genbeta \(y=(x/s)^c\); for nsbeta \(y=(x-a)/(B-a)\). Use excesses for fpareto and interior transformed observations. Jacobians \(t'/(1+t)^2,\ cy/x,\ 1/(B-a)\), respectively. | Both beta shapes free give unique fits on nonconstant interior data, unbounded for constants. Either beta shape held permits constants. With GB2 power fixed and scale free, the power map gives the scaled beta-prime problem, including conditional ties. On the genbeta \(q=1\) slice with \(p,c\) both free and endpoint held, only their product is identified. |
| invexp, rayleigh, fixed-endpoint pareto and power | Exponential on \(1/x,\ x^2,\ \log(x/B),\ -\log(x/B)\), rates \(s,1/(2s^2),a,\beta\). Jacobians \(x^{-2},2x,1/x,1/x\). | Unique rate \(1/\bar y\) for positive transformed sum. All zero excesses on a closed endpoint give unbounded likelihood. Free endpoint has its own optimization. |
| invpareto; pareto2 at held threshold | Lomax on \(1/x\) with rate equal to inverse-Pareto scale, or on \(x-\mu_0\) with rate reciprocal to Pareto II scale. Shape holds pass directly. | Transfers conditional uniqueness and finite/face comparisons on positive transformed data. Pareto II allows zero excesses and uses its separate threshold-tie rule. Under inversion the exponential face becomes inverse exponential. |

For gamma these are the dossier's monotone digamma profile and conditional
scores. For beta the log normalizer has the positive-definite covariance of
\((\log Y,\log(1-Y))\) as Hessian. For nonconstant interior data the empirical
mean of those statistics lies in the interior of their convex support, giving
attainment. With either shape held, the other score decreases from positive
infinity to a strictly negative sample log moment. The endpoint cases for
beta images require their own density and support conventions. Positive-data
transformations preserve nonconstancy in exact arithmetic; rounding the
transformed observations can erase their spread.

[Gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md#L86) ·
[Weibull](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md#L99) ·
[Fréchet](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md#L121) ·
[Earlier reciprocal fits](2026-11-02-existence-7-report.md) ·
[Burr and its held masks](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md#L122) ·
[Lomax and practical margins](2026-11-24-claims-inventory-report.md#lomax) ·
[GB2](2026-10-28-gb2.md) ·
[Conditional beta-prime ties](2026-11-07-betapr-prof-report.md)

## 2. Location and scale at fixed shape

**Proved.** At fixed standardized shape, let \(b=1/s>0,\ u=\mu/s\),
and \(h=\log f\) extended by \(-\infty\) outside positive support. Then
\[
L(u,b)=n\log b+\sum_i h(bx_i-u).
\]
Affine composition gives concavity for concave \(h\). Physical location held
imposes \(u=b\mu\), while physical scale held fixes \(b\). Any two maximizing
points must have the same \(b\), by strictness of \(n\log b\). At that scale,
two distinct locations can maximize only when every residual traverses
an affine piece of \(h\) between them and the sum of slopes is zero.
Conversely, such a feasible interval is optimal at that scale. Strict
concavity of \(h\) rules out a nondegenerate interval. This is the exact-data
specialization of J. Burridge, “A Note on Maximum Likelihood Estimation for
Regression Models Using Grouped Data,” JRSS B 43 (1981), 41–45.

[Burridge (1981)](https://doi.org/10.1111/j.2517-6161.1981.tb01146.x) ·
[Earlier EMG and Bhattacharjee proof](2026-10-31-logconcave-report.md#theorem-and-proof)

### Attainment and ties

The standardized densities below are upper semicontinuous on their declared
closed support, with zero-density edges excluded from positive feasibility.
Their log-concavity and integrability give \(f(z)\le C e^{-c|z|}\)
for \(c,C>0\): extend secants on either tail, or use the finite support and
bounded density there. For nonconstant data \(\sum|bx_i-u|\ge bR\), where
\(R\) is the sample range. Thus \(n\log b+n\log C-cbR\) excludes arbitrarily
large \(b\). Bounded density excludes \(b\to0\) and the tail bound excludes
unbounded \(|u|\) at bounded positive \(b\). Positive likelihood level
sets are compact. A nonempty positive slice therefore attains a maximum.
A constant sample with both coefficients free permits collapse by taking
\(u=bx_0-z_0\) at any positive-density \(z_0\).

With location held, use \(\sum|x_i-\mu|\) in place of \(R\).
If it is zero, free-scale collapse is unbounded when \(f(0)>0\); otherwise
the held slice has no positive likelihood. With scale held, positive
likelihood level sets in location are compact, including for constants.
For standardized support \([l,r]\), the held-scale location interval is
\([\max x-r/b,\min x-l/b]\), with zero-density ends removed. At held
location, intersect the constraints \(b(x_i-\mu)\in\{z:f(z)>0\}\) for
all observations. These criteria include singleton feasible slices.
More generally a held physical anchor \(A\) at standardized coordinate
\(z_A\) imposes \(u=bA-z_A\). The same proof uses
\(b(x_i-A)+z_A\), gives strictness in \(b\), and permits collapse on
all \(x_i=A\) precisely when \(f(z_A)>0\). This covers the fixed
triangular mode and fixed upper endpoint even when their standardized
coordinates differ from zero.

- **Laplace, GED at one, two-piece GED at one.** Let the standardized
  kink be \(m\), with negative log density equal, up to a constant, to
  \(w_-(m-z)_++w_+(z-m)_+\). At fixed \(b\) the physical kink
  \(\mu+m/b\) is the empirical quantile set at fraction
  \(w_+/(w_-+w_+)\). In a gap with \(j\) lower observations the loss
  derivative is \(jw_--(n-j)w_+\). This gives exactly the maximizing
  interval between distinct bracketing order statistics when the slope
  is zero. With both coefficients free, \(b=n/D\) where \(D\) is the
  minimized weighted absolute deviation sum in data units. \(D=0\) iff
  the sample is constant. A held mean gives \(u=b\mu\), while a held
  physical kink \(c\) gives \(u=bc-m\). The same quantile argument
  applies to double Pareto on log data at fixed rates.
- **Huber.** A location plateau requires an even sample \(n=2j\),
  with the lower and upper \(j\) observations in opposite linear
  tails. At fixed \(b\) the full interval, when nondegenerate, is
  \([x_{(j)}+\epsilon/b,\ x_{(j+1)}-\epsilon/b]\). A residual in the
  quadratic core gives strict curvature, while any unequal tail counts
  give nonzero slope. If scale is free, a plateau also requires
  \(b_*=n/(\epsilon D)\) with
  \(D=\sum_{i>j}x_{(i)}-\sum_{i\le j}x_{(i)}>0\), by the scale score.
  Conversely if the interval at \(b_*\) has positive length, the two
  scores vanish there and concavity proves the entire set. At equality
  the interval is a point. For the exact sample \((0,1)\) and
  \(\epsilon=1/2\), the full set is \(s=1/4,\ \mu\in[1/8,7/8]\).
- **Uniform and affine one-sided cases.** Uniform at fixed scale
  maximizes at every feasible location. With both free the fitted interval
  has the distinct sample endpoints, and with location held the greatest
  feasible \(b\) wins. For affine log density with nonzero slope,
  location places the positive-density boundary at the extreme
  observation: sample minimum for shifted exponential, maximum for
  reversed exponential.

### Catalogue census in physical observations

Each listed strict slice uses the attainment and hold conditions above.
Scale families anchored at zero have only the held-location ray.
A standard density defined only at fixed dimensionless shape may
require holding a ratio of public coefficients.

| Family | Fixed shape and proof |
|---|---|
| norm, logis, gumbel | Strict by quadratic, negative softplus and negative exponential curvature, respectively. |
| sn, snorm | Strict for every finite allowed skewness. For sn use \(-z^2/2+\log\Phi(\alpha z)+C\), with log-concave \(\Phi\) by Prékopa marginalization. For snorm use the piecewise negative quadratic with matching slopes, followed by its fixed-shape affine moment standardization. |
| laplace, huber, ged, sged | Convex absolute or Huber loss. GED and skew GED use positive multiples of \(|z-m|^p\) on each side, strictly convex for \(p>1\), piecewise affine at 1, and concave on each open side for \(p<1\). The held public mean and standard deviation restrict the centered coordinates directly. |
| emg, bhatt | Strict by the prior Gaussian convolution proof at \(k=\lambda s\) or \(k=a/s\); Bhattacharjee at \(k=0\) is normal. A held physical \(\lambda\) or positive \(a\) with free scale keeps a different slice. Holding both that coefficient and a positive \(k\) fixes scale. |
| hnorm, foldnorm | Half-normal has negative quadratic log density on a closed half-line. A free half-normal threshold is the sample minimum since density decreases away from it. Folded normal has anchored support and at fixed \(r=\mu/s\ge0\) has \(h=C-z^2/2+\log\cosh(rz)\) on \(z\ge0\). Curvature is \(-1+r^2\operatorname{sech}^2(rz)\), hence strict concavity iff \(r\le1\), with equality of curvature only at \(r=1,z=0\). At fixed \(r\le1\), a positive data value gives a unique scale and all zeros collapse. Held physical scale has a separate unique-location proof. |
| gev, gpd | GEV strictly log-concave for \(-1<\xi\le0\), reversed exponential at \(-1\). GPD strict for \(-1<\xi<0\), exponential at 0 and uniform at \(-1\). Apply the moving-support feasibility and tie rules, including nonzero boundary densities. The GPD threshold is ordinarily held. |
| nsbeta, triang, unif | Nonstandard beta requires both shapes at least one, strict if either exceeds one. Triangular at fixed mode fraction \(m=(c-a)/(B-a)\in[0,1]\) has a piecewise-affine tent; its logarithm is strictly concave on positive support, including a one-sided tent at \(m=0,1\). Uniform is flat. Take lower endpoint as location and width as scale. Holding upper endpoint \(B\) imposes \(u=bB-1\), and holding physical mode \(c\) at fixed \(m\) imposes \(u=bc-m\). A freely estimated mode fraction changes the problem. |
| tlambda | The shape-free fitting branch includes strict log-concavity at \(0\le\lambda<1\) and uniform at 1. Shapes above 1 may be held. They give a second uniform at 2 and strict log-concavity for \(2<\lambda\le2+d_*\), proved below. Held-scale feasibility includes closed endpoints at the uniform shapes and the latter strict shapes; at \(0<\lambda<1\) both endpoints have zero density. |
| exp, rayleigh, gamma, weibull, naka, trgamma | Anchored half-line. Exponential is affine, Rayleigh strict, gamma and Weibull qualify at shape at least one, and Nakagami at \(m\ge1/2\) in units \(s=\sqrt{\Omega}\). Transformed gamma at fixed \(k,p\) has log density \(C+(kp-1)\log z-z^p\), concave exactly for \(p\ge1,\ kp\ge1\) by the two endpoint curvatures. Generalized gamma in its positive-power representation has the same test at fixed gamma shape and power. The affine exponential cases give strict inverse-scale likelihood nonetheless. Retain the catalogue's positive observation restrictions. |
| gompertz, sgomp | Anchored at zero. Gompertz at fixed \(a/b>0\) has \(h=C+z-(a/b)(e^z-1)\), \(z=bx\), strictly concave. Shifted Gompertz is log-concave at every fixed \(\eta\ge0\) by its dossier curvature, strict for \(\eta>0\). A positive sample sum gives a unique scale and all zeros collapse. Holding Gompertz \(a\) as \(b\) varies changes the standardized shape. |
| expweibull, pgw, genbeta, kumar, power | Anchored at zero with scale free only where the catalogue fits it. Exponentiated Weibull qualifies iff power \(c\ge1\) and \(cd\ge1\) for exponent \(d\). The same two inequalities characterize physical PGW. Generalized beta requires \(q\ge1,\ cp-1\ge(q-1)(1-c)_+^2/4\); Kumaraswamy is \(p=1\) and power is \(c=q=1\). Proofs below. |

For GEV put \(t=1+\xi z,\ q=-1/\xi\) for \(\xi\ne0\).
Then \(t^2h_{tt}=1-q-q(q-1)t^q\). It is negative for \(q>1\),
zero at \(q=1\), positive for \(0<q<1\) and positive at large \(t\)
for \(q<0\). For GPD the second derivative at nonzero shape has the
sign of \(1+1/\xi\). Positive \(\xi\) fails by the power tail. The
interval argument also explains the fixed-\(-1\) GPD scale fit at
held threshold, \(s=\max_i(x_i-\mu)\) for some positive excess.
At fixed shape 0 and free threshold the shifted exponential puts
the threshold at the minimum. With GEV shape \(-1\) and free location,
the upper endpoint is the maximum, and the scale is the mean distance
below it when nonconstant.

For exponentiated Weibull let \(t=z^c,\ H(t)=t/(e^t-1)\).
Then \(0<H<1,\ -1<H'<0\); the lower derivative bound follows from
\(1+H'=e^t(e^t-1-t)/(e^t-1)^2\). The curvature is
\[
z^2 h''=-(c-1)-c(c-1)t+(d-1)(c^2tH'-cH).
\]
The last brace is negative, so the largest curvature at \(d\ge1/c\)
is at \(1/c\), where it equals
\((c-1)[H-1-ct(1+H')]\le0\). The small-\(z\) asymptote requires
\(cd\ge1\); for \(c<1\) the dominating curvature at infinity comes
from \(-z^c\) and is positive. For PGW, differentiation gives
\[
z^2 h''=-(c-1)
+\frac{(d-1)ct(c-1-t)}{(1+t)^2}
-dct(1+t)^{d-2}\{c-1+(cd-1)t\}.
\]
Small and large \(z\) require \(c\ge1\) and \(cd\ge1\), respectively.
For \(d\ge1\) the last term dominates the middle term wherever the
latter is positive. For \(d<1\), the positive part of the middle
term is bounded by \(c(1-d)t^2/(1+t)^2<c-1\). This proves sufficiency
and strictness except at the exponential case. For generalized beta,
with \(t=z^c\),
\[
-z^2 h''=cp-1+(q-1)c\,t(c-1+t)/(1-t)^2.
\]
At \(t\uparrow1\) one needs \(q\ge1\). For \(c\ge1\) the infimum of
the last factor after \(q-1\) is zero. For \(c<1\) differentiation
gives its minimum \(-(1-c)^2/4\) at \(t=(1-c)/(1+c)\). The density is
strictly log-concave except in the uniform case, since any isolated
curvature zeros in its analytic interior preserve strictness.

Fréchet at fixed shape has eventually convex physical log density.
All finite-shape Student families (lst, std, st, sstd, ht), Cauchy and
half-Cauchy, and slash have polynomial tails, excluding log-concavity
by the exponential envelope. The same tail exclusion applies on
physical observations to Lomax and its Pareto II and inverse Pareto
forms, Pareto, Burr and inverse Burr, Pareto III and IV, beta prime,
GB2 and Feller–Pareto, both paralogistic families, log-logistic,
double Pareto, inverse transformed gamma, inverse gamma, inverse
exponential, inverse Weibull and actuarial log gamma. Lognormal and
negative-power generalized gamma also lack a physical exponential
envelope. Fatigue and inverse Gaussian have exponential tails but
eventually positive log-density curvature: the leading log-power
corrections are \(-\tfrac12\log x\) and \(-\tfrac32\log x\),
respectively. Logit-normal's location and scale are in the logit
coordinate. Discrete Laplace has convex absolute loss at fixed
decay, restricted to integer location, with the integer median set.
This lattice model uses its own normalizer when decay varies.

### Catalogue census on transformed or grouped observations

| Family or transform | Fixed-shape likelihood and holds |
|---|---|
| lnorm, logitnorm, llogis, pareto3, weibull, invweibull, frechet with threshold held | Strict log-location-scale or logit-location-scale by normal, logistic and Gumbel, with the hold correspondences in section 1. |
| gengamma at fixed \(Q\) | On log observations, \(h=C+(Qz-e^{Qz})/Q^2\) for \(Q\ne0\) has curvature \(-e^{Qz}\); \(Q=0\) is normal. All four location/scale masks use the strict full-line rule. |
| trgamma, invtrgamma at fixed gamma shape \(k\); gamma, invgamma, lgamma, naka with shape held | Log gamma has \(h=C+kz-e^z\), strictly concave. Transformed gamma uses \(z=\pm p(\log x-\log s)\): power holds latent scale and public scale holds latent location. With power and scale free there is a unique conditional maximum iff nonconstant. For gamma, Nakagami and their stated images, fixed shape moves only log-location on the corresponding transformed line. |
| burr, invburr, pareto4, gb2, fpareto, betapr, paralogis, invparalogis, lomax, invpareto, pareto2 | At fixed beta shapes, \(h=C+pz-(p+q)\log(1+e^z)\) is strict. Use \(z=\pm c(\log x-\log s)\) with fixed threshold removed before logs where needed. Burr fixes \(p=1,q=k\); beta prime fixes \(c=1\); paralogis has \(p=1,q=c\), so this proof applies with \(c\) held and scale free. Lomax and its two images use \(p=1,c=1\) and \(q\) equal to their shape on strictly positive transformed data. With both beta shapes held and free power and scale there is a unique conditional maximum iff nonconstant. Each added hold follows the strict full-line rule. |
| expweibull, pgw | Fix exponent \(d>0\) and use log-location \(\log s\) and log-scale \(1/c\). At \(t=e^z\) the curvatures are \(t[-1+(d-1)H']<0\) and \(t[(d-1)-d(1+t)^d(1+dt)]/(1+t)^2<0\), respectively. All four masks obey the strict full-line rule, including constant data with power held. PGW is a density-only catalogue entry. |
| genbeta, kumar, power | At fixed beta shapes on log observations, \(h=C+pz+(q-1)\log(1-e^z)\) for \(z\le0\): strict for \(q>1\), affine at 1, convex for \(q<1\). Held upper endpoint fixes latent location and held power fixes latent scale. At \(q>1\) positive likelihood requires strictly smaller observations than a held endpoint. At \(q=1\) endpoint equality is permitted, and free power is unique unless all observations equal the endpoint, when unbounded. Both power and endpoint free at \(q=1\) place endpoint at the sample maximum; nonconstant data give a unique power for fixed \(p\). |
| fatigue at fixed threshold and shape | On \(y=\log(x-\mu_0)\), location is \(\log\beta\); \(h=C+\log\cosh(z/2)-(\cosh z-1)/\alpha^2\). Curvature is \(\operatorname{sech}^2(z/2)/4-\cosh(z)/\alpha^2\), negative away from zero for \(\alpha\le2\) and positive at zero for \(\alpha>2\). At shape at most 2 there is a unique location for any positive excess sample. At shape above 2 constant excess gives the known reciprocal pair, with \(\cosh z=\alpha^2/2-1\) from the score equation. |
| invgauss at fixed ratio \(\kappa=\lambda/\mu\) | On \(\log x\), location \(\log\mu\) has standard log density \(C-z/2-\kappa(\cosh z-1)\), strictly concave. At fixed ratio, each public hold fixes the remaining coefficient. A single physical hold with ratio free defines a different problem. |
| dblpareto, held-endpoint pareto and power, invexp | Double Pareto at fixed rates is asymmetric Laplace on log data; the weighted quantile rule gives its kink set. The next two give left- or right-sided exponential on log data. Inverse exponential on log data has strict reflected log-exponential curvature. |
| dnorm, dweibull, dgamma | For floor-normal integrate standard normal over \(bx_i-u\le z\le b(x_i+1)-u\). This is a convex region, so Prékopa gives concavity. For floor-Weibull use log cell endpoints with a one-sided zero cell. For floor-gamma at held shape, concavity on the log-rate line follows by integrating log gamma over the corresponding log cells. The existing separate grouped-data arguments supply strictness and attainment. |

For grouped normal the joint fit is uniquely attained exactly when
the integer range is at least two. With scale held a unique location
always exists. With location held, a unique finite scale exists unless
all cell closures contain the held location. Floor-Weibull also has a
unique joint maximum exactly for count range at least two. With power
held, a unique \(q\) needs a positive count. With \(q\) held, a count
at least two gives a unique finite power, all zeros give a flat
power likelihood, and other samples on \(\{0,1\}\) require the
excluded face. For floor-gamma
either single hold gives a finite unique fit if a count is positive;
the positive adjacent-cell joint case has an unattained supremum.
The exact-data nonconstant rule cannot be used for these cells.

[Grouped-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md#L82) ·
[Grouped Weibull proof](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md#L142) ·
[Grouped gamma](2026-11-25-dgamma-math-report.md) ·
[Exponentiated Weibull conditional analysis](2026-11-21-ew-limits-report.md) ·
[Folded normal](2026-10-29-foldnorm-report.md) ·
[Generalized Pareto ties](2026-11-05-gpd-cert-report.md)

### Tukey lambda beyond the fitted branch

**Proved.** For \(D(u)=Q'(u)=u^a+(1-u)^a,\ a=\lambda-1\),
the log-density curvature at \(Q(u)\) is
\((2D'^2-DD'')/D^4\).
At \(a=-k,\ 0<k\le1\), Cauchy–Schwarz gives
\(DD''\ge(k+1)D'^2/k\), strictly at the center where \(D'=0\)
and also away from it because the two derivative terms have opposite
signs. A negative \(\lambda\) has polynomial tails. At \(a=0,1\),
\(D\) is constant. For \(0<a<1\), \(D''<0\) at the center.
For \(a\ge2\), \(DD''-2D'^2<0\) near the endpoint \(u=0\),
from the limiting values \(4-8\) at \(a=2\) and
\(a(a-1)-2a^2\) above 2.

Now take \(a=1+d,\ 0<d<1\) and write \((1-u)/u=e^{-2v}\) for
\(v\ge0\) by symmetry. Substituting into \(DD''-2D'^2\) and cancelling
positive powers gives the exact criterion
\[
\frac{\sinh(dv)^2}{\cosh(v)^2}\le \frac{d}{2+d}\quad(v\ge0).
\]
The ratio on the left has one positive maximum since half its log
derivative is \(d\coth(dv)-\tanh v\), strictly decreasing from infinity
to \(d-1\). The ratio normalized by \(d/(2+d)\) is strictly increasing
in \(d\) at each \(v>0\), since \(v\coth(dv)>1/d\). Its maximum is
continuous on \(0<d<1\) by uniform tail decay on compact \(d\)-ranges.
This maximum tends to zero as \(d\downarrow0\), using
\(\sinh(dv)\le dv\cosh(v/2)\) for \(d\le1/2\), and exceeds one for
\(d\) close to 1, by fixing a sufficiently large \(v\) and taking
\(d\uparrow1\). There is exactly one transition \(d_*\), defined by
equality at the positive maximum. The log density remains strictly
concave at the transition, since its curvature has only isolated
zeros. In fact \(d_*=2/3\) exactly. With \(v=3w\) and \(T=\sinh w\),
the unsquared ratio at \(d=2/3\) becomes
\(\sinh(2w)/\cosh(3w)=2T/(1+4T^2)\le1/2\). Equality holds at \(T=1/2\),
and \(d/(2+d)=1/4\). The cutoff is therefore \(\lambda=8/3\).
Endpoint density is the continuous positive limit for these shapes.

[Existing fitted-branch and shape-free analysis](2026-11-25-tl-math-report.md#uniqueness-and-maximizer-sets-on-the-fitted-branch)

## 3. Triage and reproducibility

The CSV covers each of the 51 screened families once. Its hold-pattern
column specifies the slices behind each statement. The census counts
closed-form, exact-comparison and practical-margin estimate routes with
a uniqueness basis, and an exact-comparison route that proves the whole
typed maximizing set. The last rule accounts for the difference between
51 and 52. Fatigue has a proved reciprocal pair, so it is outside that
screen when the complete set counts as backing. If the screen literally
requires a singleton on a qualifying ledger row, add fatigue; the
fixed-threshold analysis above gives both its singleton and pair cases.
The census describes the inventory's evidence fields. It does not
promote an inferred ledger entry to a proof of the current return path.

The workstation ran the Python checker through the one-slot lock.
It checks the 86-family ledger, all 51 CSV entries, and file and line
references. The independent numerical checks use 80 decimal digits
on 33 equally spaced log observations from \(-4\) to \(4\) and
four shapes \(0.2,1,1.5,4\). They check reciprocal Burr likelihood
identity, logit restoration, log-scale curvature signs for exponentiated
Weibull and PGW, physical PGW curvature in the eligible grid region,
fatigue curvature and the Huber interval witness. The checker also
checks that the report's local file links resolve.
The computed Tukey transition agrees with \(8/3\); the exact value
follows from the preceding argument. The grid and the numerical root
serve as cross-checks of the proofs.

[Ledger](2026-11-24-claims-inventory-data/ledger.csv) ·
[Triage CSV](2026-11-25-transfer-math-data/triage.csv) ·
[Python checker](2026-11-25-transfer-math-data/check.py)

## Appendix. Dossier-ready existence and uniqueness text

Each paragraph is scoped to the stated holds. Family-wide uniqueness
should remain unknown where other slices are open.

- **invgamma.** Reciprocation gives a gamma likelihood with unchanged
  shape and rate and a data-only Jacobian. With both coefficients free
  the maximum is unique for nonconstant positive data and unbounded
  for constants. Either single hold gives a unique finite fit,
  including for constant data.
- **lgamma.** Taking logarithms of observations strictly greater than
  one gives a gamma likelihood in the same shape and rate. A finite
  free maximum exists and is unique exactly for nonconstant data.
  Either coefficient held gives a unique finite maximum for constants
  as well.
- **naka.** Squared observations give a gamma likelihood with shape
  \(m\) and mean \(\Omega\). Free estimation is unique for nonconstant
  positive samples and unbounded for constants. Held shape gives the
  mean of the squares; held \(\Omega\) gives a unique shape unless
  all squares equal \(\Omega\), when the likelihood is unbounded.
- **weibull.** Negative log observations follow a Gumbel likelihood
  with location \(-\log s\) and scale \(1/k\). The free fit is unique
  for nonconstant data. Holding shape permits constants; holding scale
  permits all samples except those constant at the held scale.
- **invweibull.** Reciprocation retains shape and inverts scale in the
  Weibull fit. The free maximum is unique for nonconstant data and
  unbounded for constants. Held shape permits constants; held scale
  gives a unique shape unless all data equal that scale.
- **frechet.** At held location, reciprocal positive excesses give
  Weibull with reciprocal scale and unchanged shape. Both remaining
  coefficients free require nonconstant excesses. Holding shape permits
  constants, while holding scale gives a unique shape unless every
  excess equals that scale. With free location the minimum multiplicity
  condition is separate from this reduction.
- **lnorm and logitnorm.** The logarithm or logit, respectively, maps
  the observations to a normal likelihood in the fitted coefficients.
  A free-scale fit collapses when all transformed observations equal
  the fitted or held mean. Otherwise it is unique. Held scale permits
  constant data and gives the transformed sample mean.
- **llogis and pareto3.** Log excesses above the fixed anchor give a
  logistic likelihood. Holding shape fixes latent scale, while holding
  physical scale fixes latent location. Both coefficients free require
  nonconstant excesses. With physical scale held, unboundedness occurs
  exactly when every excess equals that scale; with shape held,
  constants give a unique fit.
- **paralogis and invparalogis.** At fixed shape the log-data criterion
  is strictly concave in log scale, including for constant samples.
  The reciprocal variant transfers both holds and the entire maximizing
  set, with scale inverted. Joint attainment holds for nonconstant
  samples, and uniqueness on the joint and held-scale routes requires
  exclusion of competing maxima.
- **burr, invburr and pareto4.** Holding the first shape makes log
  excesses a strictly log-concave location-scale problem, with
  reciprocal orientation for inverse Burr. With power and scale free
  it requires nonconstant excesses. Holding power instead gives
  Lomax on powered excesses. Reciprocation inverts the scale and
  reverses the order of tie conditions; a fixed threshold shift
  transports all compatible Burr holds to Pareto IV. The joint
  finite-versus-face comparison is sample-specific.
- **gb2, fpareto and betapr.** With both beta shapes held, log excesses
  give a strictly log-concave location-scale likelihood, requiring
  nonconstant data if power and scale both vary. With power and scale
  held, the transformed beta shapes have a unique fit for nonconstant
  interior data; a held shape permits constants. Beta prime uses fixed
  unit power. Feller–Pareto requires a held threshold for these
  conclusions.
- **trgamma and invtrgamma.** At held power the powered observations
  give gamma with a monotone scale-to-rate map. At held gamma shape
  the log-data likelihood is strictly concave in scaled log location
  and inverse log scale. For either held shape, fitting the remaining
  two coefficients is unique for nonconstant samples. The reciprocal
  variant preserves the hold pattern and inverts scale. Joint discovery
  of a finite winner requires the face comparison.
- **genbeta, nsbeta, kumar and power.** At fixed endpoint and power,
  generalized beta gives the ordinary beta shape problem; for
  nonstandard beta the corresponding hold fixes both endpoints.
  Two free beta shapes need nonconstant interior data and a single
  free shape permits constants. At fixed shapes the physical density
  is log-concave under the curvature inequalities in the fixed-shape
  analysis; the upper edge at \(q=1\) has positive density. In
  generalized beta at \(q=1\), a free shape and a free power whose
  product determines the density give a maximizing curve when the
  optimal product is finite. Kumaraswamy uses \(p=1\) and power
  uses \(c=q=1\).
- **invexp, rayleigh and pareto.** Reciprocals, squares or log excesses
  above a fixed positive endpoint, respectively, give an exponential
  likelihood. An estimated rate is unique when the transformed
  sum is positive. If the closed support allows all transformed
  observations to be zero, the likelihood is unbounded.
- **invpareto and pareto2.** Inverse Pareto transfers the Lomax fit on
  reciprocals with the same shape and a data-only Jacobian. Pareto II
  transfers it on positive excesses over a held threshold, with rate
  reciprocal to scale. Observations at the Pareto II threshold require
  the separate multiplicity comparison.
- **norm, logis and gumbel.** The standard log density is strictly
  concave. A nonconstant sample has a unique joint maximum. Held
  positive scale permits constants; held location with free scale
  requires at least one nonzero residual.
- **sn, snorm, ged and sged.** At fixed skewness the two skew-normal
  forms are strictly log-concave; for GED and skew GED the equivalent
  condition is power greater than one. At power one the maximizing
  kink is a weighted median set, and all maximizing locations share
  the same scale. At power below one the fit uses a separate
  likelihood comparison. For the log-concave cases, free scale
  collapses only on all-zero residuals at a possible location.
- **laplace and huber.** At fixed shape the criterion is concave in
  scaled location and inverse scale. An attained maximum has a
  unique scale. Laplace locations form the median set. For Huber,
  a location interval requires equal tail counts and a gap wider
  than two Huber transition widths at the optimum. Holding location
  gives a unique finite scale unless all residuals vanish.
- **emg and bhatt.** With the dimensionless rate or half-width
  fixed, Gaussian convolution yields a strictly log-concave standard
  density. The conditional joint fit is unique for nonconstant data,
  and holding scale permits constants. With only location held
  the conditional scale is unique unless all observations equal
  that location. A physical rate or half-width hold with changing
  scale requires its constrained shape comparison.
- **hnorm and foldnorm.** The half-normal location-scale likelihood is
  strictly concave on the admissible inverse-scale coordinates,
  with free threshold at the sample minimum. The folded-normal scale
  likelihood at fixed ratio \(\mu/s\le1\) is strictly concave in inverse
  scale, uniquely attained when any observation is positive. A held
  physical scale instead uses the separate unique-location result.
- **gev and gpd.** At fixed shape, GEV for \(-1<\xi\le0\) and GPD
  for \(-1<\xi<0\) have strictly log-concave standard densities.
  Maximizers are unique on positive feasible slices away from scale
  collapse. GEV at \(-1\) is reversed exponential; GPD is exponential
  at 0 and uniform at \(-1\). At held scale, the uniform slice returns
  every feasible threshold.
- **triang and unif.** At fixed mode fraction the triangular density
  is strictly log-concave on positive support and gives a unique
  location-scale maximum if a positive slice exists away from
  collapse. A physical endpoint hold is an affine constraint in
  inverse scale and scaled location. Uniform at held width instead
  gives the whole compatible location interval. Moving the triangular
  mode fraction can produce separated maximizers.
- **tlambda.** At fixed fitted shape below one the standard log density
  is strictly concave on positive support when the held shape is
  nonnegative. At shape one the likelihood is uniform and a held
  width permits a maximizing location interval. A held shape of two
  also gives a uniform, and \(2<\lambda\le8/3\) gives strict
  log-concavity on the closed standardized support.
- **exp and gamma.** Fixed positive gamma shape gives a strictly concave
  log-data location criterion. In physical units the standard gamma
  density is log-concave from shape one upward; the exponential
  member has affine log density and a strictly concave inverse-scale
  likelihood. Positive total exposure gives its unique rate.
- **gompertz and sgomp.** At fixed ratio \(a/b\) for Gompertz, or
  at held \(\eta\) for shifted Gompertz, the physical standard log
  density is concave. The anchored inverse-scale maximum is unique
  if the sample sum is positive and diverges for all zeros.
- **expweibull and pgw.** At held exponent the log observations form
  a strictly log-concave location-scale family. Free power and
  physical scale require nonconstant data. Held power permits
  constants; held physical scale permits all samples except a
  constant at that scale. This is a mathematical likelihood result
  for PGW, whose catalogue entry supplies density evaluation.
- **gengamma.** On log observations every finite fixed \(Q\) gives a
  strictly log-concave standard density, including the normal at
  zero. There is one maximum for nonconstant data, with constant
  data allowed at fixed scale and at fixed location away from
  the constant log observation.
- **fatigue and invgauss.** At fixed threshold and shape at most two,
  fatigue is strictly log-concave in the log scale on positive
  excesses; above two a constant excess has the reciprocal pair.
  Inverse Gaussian at fixed shape-to-mean ratio is strictly
  log-concave in log mean on the log-data line. Every positive
  sample has a unique location along that ratio slice.
- **dblpareto and dlaplace.** Double Pareto at fixed rates gives a
  weighted median set for the log kink. Discrete Laplace at fixed
  decay gives the integer median set for its lattice location.
- **dnorm, dweibull and dgamma.** Cell integration preserves
  concavity in the stated inverse-scale coordinates. Floor-normal
  needs an integer range of at least two for a finite joint fit.
  Floor-Weibull uses its positive-count and feasible endpoint
  rules. Floor-gamma at held shape has a unique finite scale when
  a count is positive, with a separate strict shape result at held
  rate. Adjacent occupied cells can have nonattained joint suprema.

## What this changes for the fitter

- Use the transformed-data conditions to justify exact constant-sample
  stops only on the matching hold slices. Test equality and one-sided
  order in original coordinates when possible. Preserve spread with
  log ratios or centered sufficient statistics, restore the public
  coordinates and evaluate the Jacobian in the reported likelihood.
  Do not infer public-coordinate relative accuracy from a small
  transformed score.
- On a strict fixed-shape support slice, maintained brackets or a
  checked score enclosure plus a curvature bound can certify a
  unique solution. Use support inequalities and the corresponding
  one-sided optimality conditions for an attained endpoint. A
  likelihood-gap bound by itself gives no uniqueness or coefficient
  tolerance bound.
- Return the full interval with a nonunique indication for the exact
  Huber and weighted-median cases. For Huber the test is equal tail
  counts and a strictly positive gap after subtracting two
  transition widths at the common scale. A zero-length interval
  gives a unique location. Use the fixed-width feasible interval
  for uniform plateaus.
- Outer shape remains a separate optimization. Keep a successful
  unrefuted search result as uncertified when an exact comparator
  or face cover cannot close. Classify true nonattainment and
  unboundedness only when proved, and propagate actual search
  failure. The \(8/3\) calculation allows a fixed-shape certificate
  for eligible held Tukey shapes and leaves the shape-free fitting
  domain unchanged.

## Open

- **Global ties and attainment with free shape.** The CSV identifies the
  affected slices. Fixed-shape concavity gives uniqueness of each
  nuisance solution on its strict slices, but comparing the resulting
  scalar or multivariate profiles requires tail exclusions and either
  uniqueness of the best root or exhaustive isolation and strict
  likelihood comparisons. For Burr, inverse Burr and shifted Burr this
  includes strict comparisons against both named faces. For GB2 and
  Feller–Pareto a joint closure or an exhaustion bound for the outer
  beta-shape profile is still needed.
- **Paralogis and inverse paralogis.** Universal joint and held-scale
  uniqueness need an inequality controlling the number or ordering of
  profile maxima, or a rigorously tied counterexample. Strict
  per-input parent certificates transfer and the existence conditions
  are already proved.
- **Folded normal and Fréchet.** At fixed physical folded-normal
  location the ratio \(\mu/s\) changes with scale. The argument here
  leaves that slice and the joint uniqueness question to the existing
  profile comparison. For Fréchet with location and shape free,
  a universal uniqueness result would need strict global exclusion
  between the finite location-shape solutions and the known collapse
  and Gumbel competitors.
- **Runtime promotion.** The mathematics leaves floating-point
  representation and coverage of the actual return routes to a
  fitter follow-up. Near transformed collapse, exact real-data
  injectivity alone supplies no numerical accuracy guarantee.
