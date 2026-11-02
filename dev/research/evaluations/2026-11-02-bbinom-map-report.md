# Free size and both shapes in the beta-binomial: a numerical map

Source revision: `493fa56a7a8c7bda0960c07ba234b116e7122615`. The library and bindings were not modified. This study does **not** provide a global size-search certificate. Its fixed-size results use the existing profile certificate, conditional on computed comparisons and its stated practical floating-point allowance; its comparisons over an unbounded range of sizes remain numerical.

## Findings

Dispersion alone does not classify the free-size beta-binomial likelihood. Even underdispersion and exact equidispersion can favor a finite-shape or endpoint beta-binomial over the binomial or Poisson. The simplest finite-shape counterexample in the tested `h >= 3` range is `(0, 1, 2)`: its mean is 1, variance 2/3, and `BB(2, 1, 1)` assigns probability 1/3 to each observed value. Its log likelihood is `-3 log(3) = -3.2958368660`; the best free-size binomial is `Bin(2, 1/2)`, at `-3.4657359028`. This beta-binomial is a **proved global maximum**, since it equals the empirical distribution and no probability mass function can give the three observations a larger product. For `M=1` the empirical Bernoulli distribution already attains that unrestricted bound, so `M=2` is minimal in this range.

For exact equality, `(0, 0, 2, 2)` has mean and variance 1. Its two-point endpoint face at `N=2` has log likelihood `-2.7725887222`, compared with Poisson `-5.3862943611`. The endpoint face equals the empirical distribution, proving this global supremum. Equality can also favor finite shapes: `(1, 1, 4)` has mean and variance 2 and a checked `BB(4, 1.12527, 0.98826)` likelihood `-4.81337237`, versus Poisson `-5.01917075`. The latter comparison proves Poisson is not the supremum, but does not prove the displayed fit is globally optimal.

The local negative-binomial criterion below is also not a global classifier. For `(1, 1, 5)`, the NB optimum has shape `4.51203`, log likelihood `-5.70476783`, and size-direction slope `-0.01539496`: moving slightly away from the NB face initially **decreases** the likelihood. Nevertheless the certified fixed-size profile at `N=5` is `-5.32419053`, with shapes `(0.723939, 0.653456)`, an improvement of `0.38057730`. The library profiles at `N=30, 60, 200` are `-5.70502559`, `-5.70497905`, and `-5.70484168`, approaching the NB face from below. Thus even the direction of the local comparison does not decide whether a better finite fit exists. This is the first such example in the targeted ordered enumeration of `h=3`, `M<=6`; the exhaustive `h=3`, `M<=4` study also found none. It is not a proof of minimality over all sample sizes.

## Numerical map and limitations

The [sample table](2026-11-02-bbinom-map-data/samples.csv) records the observations, sample size `h`, maximum `M`, mean `mu`, variance `v` with divisor `h`, `D=v/mu`, central third moment, factorial moments `F2` and `F3`, independent optimizer results, and initial size limit. [faces.csv](2026-11-02-bbinom-map-data/faces.csv) gives independently recomputed face values and local slopes; [certified.csv](2026-11-02-bbinom-map-data/certified.csv) summarizes the fixed-size library profiles; [extended.csv](2026-11-02-bbinom-map-data/extended.csv) records six longer searches. [classification.csv](2026-11-02-bbinom-map-data/classification.csv) joins these by zero-based `index`, records the effective limit and flags each tail and capped candidate. Scripts and supporting profile output are in the same data directory.

The 2,202 nonzero samples consist of all 437 sorted multisets of lengths 3–6 from `0,...,4`, 1,761 seeded random samples (464 binomial, 456 Poisson, 415 negative binomial, 426 beta-binomial), and four hand examples. Random sample sizes are selected from 3, 5, 10, 20, 30, and 50; generating binomial sizes from 5, 10, 20, and 40; means from 0.5, 1, 2, 4, and 8; NB shapes from 0.5, 1, 2, 5, and 15; and BB shapes from the grids in `map.py`. All-zero draws and random draws with maximum above 65 were omitted. Seed is 113. Repeated observations and repeated samples are retained. The design explores diverse cases; it is not a representative population or a random sample of all possible datasets.

For every sample, sizes `M,...,max(60,M+15)` were enumerated: 117,947 calls to the library's two-shape fixed-size profile. For six samples the local NB slope exposed a missed improvement, so sizes through 1,000 were also enumerated, adding 5,640 calls. All 123,587 profiles returned an accepted fixed-size status; no profile certificate failed. At each size the solver compares interior shapes with the binomial and applicable endpoint face, with its documented `1e-8*h` face-tie allowance. The separate multi-start SciPy screening agreed with the initial library grid's best likelihood within `3.30e-8` for every sample. It had 20,570 failed starts in 1,184 samples and 122,075 starts near parameter search bounds in 1,197 samples, mostly underdispersed cases; these counts describe attempts, not failed library profiles. The library results, not unchecked optimizer iterates, supply the table below. Face likelihoods were recomputed using stable finite-product formulas; overdispersed NB and underdispersed binomial face optimizations were interior to their numerical search ranges.

The NB face is optimized over its shape at mean `mu`; the Poisson is its large-shape limit. The free-size binomial face is optimized over integer `N` when `v<mu`; otherwise its supremum is Poisson. The standard finite-shape NB existence/uniqueness result for `v>mu` is established in, for example, [Brøns and Jensen's negative-binomial MLE preprint](https://www.math.ku.dk/bibliotek/arkivet/preprints-fra-ims/1988/preprint_1988_-_no._8_br_ns__hans_and_jensen__s_ren_tolver_-_maximum_likelihood_estimation_in_the---.pdf). The binomial face result and caveats are described in the [binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md). Constant samples are handled as point masses.

Entries below are **best checked candidates**, not proved global winners. A distinct improvement means more than `1e-5` in total log likelihood. “Interior” means a finite-size, finite-shape profile exceeded the separately optimized faces by that tolerance and was not at the size limit. “NB” means the NB face exceeded every checked finite-size candidate; it is not a proof that it wins beyond the grid. “Size unresolved” means an improving interior candidate was at the last enumerated size; no capped size is presented as an estimate. Endpoint means the two-point face; point means constant data.

| Sample dispersion | Samples | Finite interior | Binomial face | NB face | Two-point endpoint | Point mass | Size unresolved |
|---|---:|---:|---:|---:|---:|---:|---:|
| `v < mu` | 1,292 | 732 | 512 | 0 | 14 | 34 | 0 |
| `v = mu` | 16 | 8 | 0 | 0 | 8 | 0 | 0 |
| `v > mu` | 894 | 673 | 0 | 158 | 57 | 0 | 6 |

Among the 894 overdispersed samples, the NB slope was positive in 710: 647 had an interior checked candidate, 57 an endpoint, and six an improving candidate at the size limit. The slope was negative in 184: 26 nevertheless had a better finite interior candidate, while 158 had NB as the best checked candidate. No slope was within `1e-5` of zero in this subset. This comparison demonstrates the local/global distinction; the 158 are not certified global NB cases.

The initial limit was chosen as a computational screening budget, **not** as a proved tail cutoff. For each sample, the table flags the tail as settled only if both the final profile-to-NB/Poisson difference and its deviation from a `1/N` ratio between half and final size are below `0.001*h` in total log likelihood, both profiles returned accepted statuses, and the best interior candidate is not at the cap. This heuristic passes 549 samples and flags 1,653 as unsettled; even a pass cannot rule out a later bump. The unusually many flags and all six capped cases are material limitations. In one extended sample, the best checked candidate still occurs at `N=1000`, and no size estimate is reported. The other five extended searches have checked maxima at 110, 131, 297, 386, and 730, but none excludes a better size beyond 1,000. The full profiles and tails of seven selected examples, through 150 and at 250, 500 and 1,000, are in [profile-grid.csv](2026-11-02-bbinom-map-data/profile-grid.csv).

## Expansion at the negative-binomial face

Let `a>0`, `mu>0`, `c=a/mu`, and put `b=cN`. For fixed `x`, as `epsilon=1/N` tends to zero,

```text
log P_BB,N,a,cN(x) = log P_NB,a,mu(x) + epsilon t(x;a,mu) + O(epsilon^2),

t = -x(x-1)/2 + x(x+1)/(2(c+1)) + a(a-1)/(2c(c+1)).
```

To derive it, write the likelihood with `(N)_falling,x`, `(a)_rising,x`, `(b)_rising,N/(a+b)_rising,N`, and the reciprocal product `(b+N-x)_rising,x`. Taylor expansion of the two finite products yields the first two terms in `t`. Euler–Maclaurin applied to `sum_{j=0}^{N-1} log(1+a/(b+j))` yields the last term. The expansion holds for fixed positive `a,mu` and fixed finite observations; no uniformity in `a` is asserted.

At the finite NB fit `mu=xbar`, `a=ahat`, the mean and shape scores vanish. Although the path `b=cN` changes the finite-N mean slightly, the envelope theorem makes its first-order likelihood coefficient the profile slope. Summing and simplifying gives the particularly simple **derived local** statistic

```text
T_NB = sum_i t(x_i; ahat, mu)
     = h [mu^2 - ahat (v-mu)] / [2 (ahat+mu)].
```

For `v>mu`, `T_NB>0` exactly when the NB ML shape is smaller than its moment shape `mu^2/(v-mu)`. Positive `T_NB` proves that sufficiently large finite `N` has likelihood strictly above the NB face; negative means the face is locally better, not globally better. Equality needs higher-order terms. The ML shape depends on the full sample through its score, not merely the first two moments. The `(1,1,5)` counterexample rules out sufficiency of the negative sign.

[expansion.py](2026-11-02-bbinom-map-data/expansion.py) verifies the symbolic algebra with SymPy and uses 60-digit mpmath likelihoods. For `(0,1,2,4)`, `mu=1.75`, `a=5.203397940581812`, the coefficient is `0.2260803733`; `N*(ell_BB-ell_NB)` at `N=100, 1,000, 10,000, 100,000` is respectively `0.12749752, 0.21629896, 0.22510298, 0.22598264`. This is a numerical check of the expansion, not a bound on its remainder.

Near the Poisson face, write `F2=mean[x(x-1)]`, `F3=mean[x(x-1)(x-2)]`, and let `a` increase. A separate expansion gives

```text
(ell_NB(a,mu) - ell_Pois(mu))/h
  = (v-mu)/(2a) + B/a^2 + O(a^-3),
B = (mu^3-F3)/6 - F2/4.
```

When a sequence of samples has `v-mu -> 0+`, `B<0` stays away from zero, and the relevant optimum tends to infinity, its local approximation is `ahat ≈ -4B/(v-mu)`. Thus the third factorial moment affects the rate and the NB/BB local comparison near equality. This is a local approximation under the stated conditions, not a claim that the free-size BB optimum is near Poisson.

## Binomial-face slope and size instability

At a fixed integer `N`, let `p=mu/N`, `q=1-p`, and `rho=1/(a+b+1)`. At the binomial face, the derivative toward finite shapes, evaluated at the binomial fitted `p`, is

```text
T_Bin(N) = [sum_i (x_i-Np)^2 - h*N*p*q] / (2*p*q)
         = h [v - mu(1-mu/N)] / [2*(mu/N)*(1-mu/N)].
```

This follows by expanding the rising factorials to first order in `rho`; profiling `p` leaves the first derivative unchanged because its binomial score is zero. For underdispersion, the slope is positive exactly when `N < mu^2/(mu-v)`. It is therefore **not** always non-positive at the integer free-size binomial fit. For `(0,1,2)`, `N=2` and `T_Bin=1`; the 60-digit finite-difference check in `expansion.py` tends to 1. A negative binomial-face slope is not globally decisive either: `(3,4,7)` has best binomial `N=13`, slope `-0.66857143`, and log likelihood `-5.78418033`, whereas the certified BB profile at `N=7` is `-5.65458630`.

One count can also change the apparent size regime. On `(1,0,3,0,1,1,0,0)`, the checked interior maximum is `N=3`, log likelihood `-9.30459728`. Replacing the last zero by one gives `(1,0,3,0,1,1,0,1)`: the best profile through `N=150` and at 250, 500 and 1,000 is the binomial face at `N=54`, log likelihood `-9.72588989`. No claim is made that those checks exclude all later sizes. In the OPZ examples, the binomial face sizes are 99 and 191, respectively, when the last observation changes from 27 to 28; the best checked BB sizes through 150 are instead 27 and 28, with log likelihoods `-13.56393580` and `-14.11471322`. Thus the binomial instability does not by itself describe the BB size behavior.

## Escape paths, conjectures, and open work

For bounded `N`, a subsequence has constant integer size. Finite positive shapes give the ordinary BB; both shapes tending to infinity with a limiting ratio give the fixed-size binomial; both tending to zero with a limiting ratio give a mixture on `{0,N}`; other one-sided shape limits give point masses at 0 or `N`. At `N=1` the entire family is Bernoulli and concentration is unidentified. A nonconstant sample has positive endpoint-face likelihood only when its observations lie in `{0,M}` and `N=M`.

For `N -> infinity`, the identified positive-count limit paths are NB when `a -> a0>0` and `b/N -> a0/mu>0`, and Poisson when `a -> infinity` while the count mean stays finite and positive. The Poisson is also the limit of free-size binomials. If `a -> 0`, the probability of any fixed positive count tends to zero along the ordinary small-shape regimes; if the count scale escapes, a fixed positive count likewise loses mass. These observations motivate the following **conjecture**, not a complete closure proof: for any sample with `M>=1`, every escaping sequence with likelihood bounded away from zero has a subsequence represented by one of the fixed-size faces, an NB law, or a Poisson law just listed. A full proof must control all joint rates and rule out defective limits retaining positive mass at every observed count. Oscillating paths should be treated by subsequences.

Conditional on that closure conjecture and attainment within each finite-size profile, the global alternatives are finite-size BB, finite-size binomial, applicable endpoint/point faces, and the NB or Poisson limit. The two local slopes give **proved necessary tests** for a face to be globally optimal, not sufficient tests. In particular, positive `T_NB` excludes the NB face; positive `T_Bin` at a finite binomial maximizer excludes that binomial face. The counterexamples above show that neither converse holds. A plausible proof route for a particular sample is therefore to certify each fixed-size profile and then establish a uniform upper bound on all remaining sizes, rather than infer global behavior from dispersion or a local score.

A usable uniform bound must handle the shape regimes separately. For small `a`, use the positive observed counts to bound their probabilities uniformly and make the likelihood small. For `a` in a compact interval bounded away from zero, control `b/N` and compare with the NB likelihood with a uniform remainder. For large `a`, the held-alpha correction proportional to `a^2/N` is inadequate: one needs a joint Poisson or binomial comparison that remains valid as `a` and `N` grow together, including the mean and shape optimization and the near-equidispersed case. A finite-grid allowance, endpoint comparisons and floating-point margins must be propagated. No such bound, no proof of the exhaustive escape classification, and no global classification of the 158 checked NB cases are supplied here.

## Reproduction and validation

All computation used the local workstation, one thread per slot and `nice -n 10`; no simbox, package install, library change, or test-suite run was needed for this research-only task. Environment: Python 3.14.5, NumPy 2.4.6, SciPy 1.18.0, mpmath 1.3.0, SymPy 1.14.0, GCC 15.2.0. From the worktree root, with `/tmp/umvl-cpu/run` available:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/map.py
/tmp/umvl-cpu/run nice -n 10 sh -c 'boost=$(Rscript -e '\''cat(system.file("include", package="BH"))'\''); g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-02-bbinom-map-data/check.cpp -o build/scratch/bbinom-map-check'
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/faces.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/certify.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/summarize.py --base
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/extend.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/summarize.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/profile_grid.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/expansion.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/verify.py
```

The one-count search is reproduced with the same environment and `run nice -n 10 python3 dev/research/evaluations/2026-11-02-bbinom-map-data/perturb.py`; its seed is 3113. The final verification passed: 2,202 aligned samples, no failed fixed-size profiles in either grid, the six extensions, independent likelihood agreement, and the constructed examples. Symbolic and high-precision expansion checks also passed. These checks do not establish global tail bounds, uniqueness of free-size estimates, or a complete closure theorem.
