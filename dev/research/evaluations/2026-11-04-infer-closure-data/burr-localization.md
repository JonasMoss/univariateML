# Global likelihood localization at Burr's Pareto face

This establishes a limit for the mathematical likelihood supremum, not for
the output of an uncertified fitter. Use the notation in the
[endpoint derivation](pareto-experiment.md). Observations are iid from a
nondegenerate Pareto law, equivalently a shifted exponential on the log scale.
Translate and rescale so its endpoint is zero and its log-scale rate is one.

## 1. Tightness and consistency before claiming a local experiment

The [Burr closure report](../2026-11-03-burr-closure-report.md), Section 2,
establishes, for the log-data density and its mode y0,

$$p(y)\le\lambda,\qquad
 p(y)\le e\lambda e^{-\lambda|y-y_0|/2},\quad
 \lambda=\min(c,\alpha),\quad\alpha=kc.$$

A likelihood-competitive candidate has log likelihood per observation at
least that of the true law minus o(1). The first bound keeps lambda away
from zero. The sample's minimum mean absolute deviation from a constant
converges to a strictly positive value (the population exponential median
minimizes it), so

$$n^{-1}\ell_n\le1+\log\lambda
 -\frac\lambda2\inf_a n^{-1}\sum_i|Y_i-a|$$

keeps lambda bounded above. The second bound and
$n^{-1}\sum|Y_i-y_0|\ge|y_0|-n^{-1}\sum|Y_i|$ then bound |y0|.
These bounds hold for all competitive candidates simultaneously with
probability tending to one, with constants that can be made deterministic.

The coordinates `(lambda,y0,k)` therefore lie in a compact set after adjoining
k=0 and k=infinity. For k<=1 use alpha=lambda,c=lambda/k; for k>=1 use
c=lambda,alpha=lambda*k. At zero the density limit is shifted exponential;
at infinity it is log-Weibull. The mode-to-endpoint difference at zero is
$-w\log k\to0$. The upper-semicontinuous density extension at the sharp
endpoint uses the inside value alpha. All pathwise density traces there
are <=alpha; away from that endpoint the density is continuous in these
compactified coordinates. The density is uniformly bounded above, so its
log is uniformly bounded above.

Here is a direct Wald consistency argument despite logs unbounded below.
For any compact parameter subset excluding the true law, the expectation of
each log density is strictly below the true expected log density, by the
information inequality; support failures give expectation -infinity. At a
point in that subset, take shrinking neighborhoods and replace the log
density by its local supremum, then truncate below at -M. Upper
semicontinuity, the uniform upper bound and reverse monotone convergence
let one choose a neighborhood and M whose expected envelope remains
strictly below the true expectation. A finite cover and the ordinary LLN
for these bounded envelopes give a uniform strict likelihood gap. Thus
every competitive candidate converges to the true law. Identifiability of
the compactified laws forces

$$k\to0,\qquad \alpha\to1,\qquad b\to0,\qquad w=k/\alpha\to0.$$

The argument uses the explicit envelope and both closure faces; it would
not follow just from a derivative at k=0. For rigor in the neighborhood
step, suprema over closed compact neighborhoods are upper semicontinuous
measurable envelopes, and truncation makes their LLNs immediate.

## 2. Exclude endpoint widths larger than 1/n

Put t=nb and v=nw. For arbitrary positive alpha the exact log likelihood
relative to the true law is

$$Q_n(\alpha)+\alpha t-(1+\alpha v/n)F_n(t,v),$$
$$Q_n(\alpha)=n\log\alpha-(\alpha-1)\sum_iY_i,\qquad
 F_n(t,v)=\sum_i\operatorname{sp}((t-nY_i)/v).$$

The unrestricted supremum of Qn is O_P(1). Along competitive sequences
alpha->1, t/n->0 and v/n->0 by consistency. For v->infinity and
t/v=a in a fixed compact interval, uniformly,

$$F_n(av,v)/v\longrightarrow
 I(a)=\int_0^\infty\operatorname{sp}(a-r)dr$$

in probability. More precisely, the error is uniformly small over
M<=v<=n epsilon_n as first n->infinity and then M->infinity, for any
deterministic epsilon_n->0. Consistency permits choosing such an epsilon_n
covering the competitive sequences with arbitrarily high probability.

One elementary justification uses exponential spacings: couple the lower
order statistics to a single iid unit-exponential sequence E_j, so

$$nY_{(j)}=\sum_{l=1}^j E_l\frac n{n-l+1}.$$

For j=o(n) the factors are uniformly 1+o(1). The strong law for partial
sums gives uniform relative accuracy of counts for lower windows with
size tending to infinity. Integrating these counts against the smooth,
exponentially decaying softplus kernel proves the assertion on compact
a intervals. For the far positive tail, split into dyadic intervals and
use the exponential-spacing law or binomial count bounds; the kernel
decays exponentially, so the weighted tail is arbitrarily small uniformly.
This also handles points outside the o(n) range.

As proved in the endpoint derivation,
$\sup_a\{a-I(a)\}<0$. Consequently the boundary part of the likelihood
is <=-delta*v on bounded a intervals when v is large. The alpha-1 error is
o(v), and Qn cannot compensate it. Large positive a is controlled by

$$F_n(t,v)\ge\frac{t}{2v}\#\{i:nY_i\le t/2\},\quad t>0.$$

The count is asymptotic to t/2 whenever t->infinity and t=o(n), uniformly
above large lower cutoffs by the same spacings argument. Hence the negative
term grows at least quadratically in a relative to v, whereas the positive
term is only alpha*t. Large negative a has boundary part <=alpha*t<0;
if t is not large in magnitude while v diverges, use the compact-a bound
near a=0 instead. These observations establish **v=O_P(1)** for every
competitive maximizer.

With v bounded, t->-infinity is immediately excluded by alpha*t and
Qn=O_P(1). For t->infinity the count inequality above again dominates t.
Therefore **t=O_P(1)**. These bounds control all escape regimes after
consistency, not just a predetermined compact rectangle.

## 3. Gaussian shape cancellation and the global LR

On bounded t,v sets with v bounded away from zero, the endpoint term's alpha
derivative is $t-(v/n)F_n(t,v)=O_P(1)$ uniformly. Widths arbitrarily close to
zero cannot spoil profiling: for alpha<=2 and v<=1/2,
$(1+\alpha v/n)\operatorname{sp}((t-Z_{(1)})/v)
\ge\alpha(t-Z_{(1)})$ when t>=Z_(1); when t<Z_(1), alpha*t is already
below alpha*Z_(1). Thus the endpoint term is never better than the sharp
endpoint at the same alpha in this range. The bulk profile Qn
has curvature of order n at alpha=1. Thus profiling alpha changes the
endpoint objective by o_P(1), and the fitted alpha differs from the
exponential-face estimate by O_P(1/n), on high-probability bounded sets.
The ordinary root-n shape fluctuation is independent of the endpoint
point process and cancels between the hypotheses.

The localized likelihood convergence in the endpoint derivation, combined
with the tightness of t,v just proved, now yields the **unrestricted closure
Burr-vs-fitted-Pareto LR limit**

$$2\{\sup_{\alpha,b,w\ge0}\ell_n-\sup_{\alpha,b}\ell_{P,n}\}
 \ \Rightarrow\ T_\Pi
 =2\left[\sup_{t\in\mathbb R,v\ge0}
 \left\{t-\sum_{z\in\Pi}\operatorname{sp}((t-z)/v)\right\}
 -Z_{(1)}\right].$$

In the first supremum alpha>0, b is unrestricted and w>=0; the display's
w>=0 includes the Pareto face. The notation at v=0 is the sharp-endpoint
likelihood defined in the endpoint derivation, not pointwise substitution
in a division by zero. Null fitting uses the upper-semicontinuous endpoint
density. The finite-sample supremum over ordinary Burr has the same value
after adjoining approachable Pareto/Weibull faces.

This establishes a stochastic-process distribution; it supplies **no
chi-square or chi-bar-square reduction**. It does not establish a numerical critical value,
an atom's exact probability, or a certified search for its supremum. The
retained grid diagnostics are feasible comparisons only. Confidence-set
inversion needs the full candidate-indexed PPP experiment, not just this
null distribution. Uniformity in bounded local t,v and compact positive
rates uses the same bounds; it does not give a uniform Gaussian interval
in an interior-to-face transition where nw is unbounded.
