# What a local experiment can classify

A useful invariant is attached to a pair `(model of laws, limit submodel)`, a
base law, the observation scheme and a hypothesis. At sample size n restrict
the alternatives by H(p,g)<=R/sqrt(n), allowing face nuisance to vary at its
own relevant speed. Compare experiments up to statistical equivalence (Markov
kernels), not the rank of a selected parameter chart. Taking increasing R
after the local limit gives a parametrization-free statistical object whenever
the limit exists. A fixed bijective change of data also preserves it.

This describes the examples more faithfully than a leakage fraction:

- A regular one-sided seam has a transverse Gaussian ray after projecting
  the regular nuisance tangent.
- The BB Poisson corner has a transverse Gaussian half-plane. Integer sheet
  spacing disappears at the cancellation scale, so a countable union of
  one-dimensional mean-fixed sheets has a two-dimensional local experiment.
- Burr's Pareto face has an independent Gaussian shape factor and a PPP
  endpoint/width factor with singular alternative mass. A strong L2 tangent
  cone misses the mass concentrating at the endpoint.

It does **not** give a unique geometric trichotomy or settle every escape
question. BB's half-plane experiment is statistically the same local
experiment as a smooth two-dimensional Gaussian model with one constrained
coordinate, although their local topology differs. Coefficient rates depend
on which functional of a law is being estimated; replacing a coordinate t
by t^3 changes its named rate without changing the experiment. A proper
smooth extension across a boundary is an additional embedding property.

There is a stronger obstruction to recovering **exact face-selection
probabilities** from the first local experiment. Use the counterexample in
[boundary-points.tex](../../boundary-points.tex), Example “Dominant leakage
can improve likelihood.” On three atoms let

$$g=(1/2,1/2,0),\qquad
 p_t=\big((1-t^2)(1+t^{3/2})/2,
          (1-t^2)(1-t^{3/2})/2,t^2\big),\quad t\ge0.$$

At the distinguishability scale t=h/sqrt(n), the third-atom count tends to
Poisson(h^2). The conditional probability perturbation on the first two
atoms is O(n^-3/4), and its product Hellinger effect tends to zero. Thus the
local experiment is identical to the pure leakage model
$\widetilde p_t=((1-t^2)/2,(1-t^2)/2,t^2)$, whose closure ML equals g
for every null sample.

Nevertheless let $Z_n=n_1-n_2$. Under g, the perturbed model's log likelihood
for null-supported data is

$$Z_n t^{3/2}-nt^2+O_P(nt^3+nt^4).$$

For Z_n>0 a small positive t improves the likelihood, while for Z_n<=0 the
exact logarithm inequalities make g optimal. Its face-selection probability
tends to 1/2, compared with 1 in the pure leakage model, despite identical
first local experiments. On positive Z_n, expansion and differentiation give

$$\widehat t=\left(\frac{3Z_n}{4n}\right)^2\{1+o_P(1)\},\qquad
 \sup_t\{\ell_n(t)-\ell_n(0)\}
 =\frac{27Z_n^4}{256n^3}\{1+o_P(1)\}=O_P(1/n).$$

These expansions hold on null-score sets bounded away from zero after
normalization; near zero the nonnegative maximum is still O_P(1/n).
The remainder and its derivative are smaller at t=O(1/n). Global
localization follows since log(1-t^2)<=-t^2 and the bounded inside
logarithm perturbation is controlled by |Zn|t^(3/2)+C n t^3; shrinking
the fixed parameter interval if necessary excludes remote t. The LR limit
is zero for both models. The distinction lives below first-order
detectability, where n H^2 tends to zero.

Consequently a local experiment is a defensible invariant **for rates and
first-order test/confidence behavior**, provided localization is proved.
It cannot replace a uniform likelihood expansion when the question is an
exact finite-sample face optimum or its probability. A higher-order
hierarchy may distinguish those questions, but no exhaustive invariant
taxonomy is proved here. “Seam”, “corner” and especially “cusp” should
remain qualified working labels rather than theorem-bearing definitions.
