# TASK-116.25: transformed-gamma and reciprocal certificates

The free and held-scale profiles now have global exclusion certificates, including the refitted-normal seam at zero log skewness, and a separate coefficient-inclusion check. All eight combinations of held coefficients are covered by a scalar reduction, a profile certificate, or a statistical/numerical stop. Both dossiers now declare `fit.certified: 'yes'` in the package sense: exact-arithmetic bounds conditional on the computed comparisons, with practical floating-point margins. This is not an outward-rounded interval implementation.

The final C++, R, JavaScript and site checks passed. No golden, density/CDF/quantile/RNG implementation, special-function implementation, generated R source, shared core header, roadmap or status file was edited. No commits were made.

## Scope and source identity

Base revision: `3de415f0896527001caeb0d612bcb91b858d644d`, branch `codex/trgamma-seam`; changes remain uncommitted. Final fitter SHA-256 values:

| File | SHA-256 |
| --- | --- |
| `trgamma/mle.hpp` | `f5fad619d282f24de69d2829d23851bee9f1981b27541ae9d756b93cf9179d3e` |
| `invtrgamma/mle.hpp` | `f070ae25d85952907c9c607040ac62932dddd497e6bc812cbfa8bbc794e1f55f` |

The four fitter/binding hashes, commands, machines, exit statuses and log hashes are in [validation.json](2026-11-07-trgamma-seam-data/validation.json); retained [validation excerpts](2026-11-07-trgamma-seam-data/validation-excerpts.txt) contain the final pass summaries. The simbox run printed the four hashes before testing.

Implementation changes are confined to the two family fitters, dossiers, family tests and binding-neutral family adapters. A dedicated R certificate test and appended JavaScript tests exercise the binding contract. New C++ cases are appended. One existing inverse-family test now correctly calls an exhausted five-iteration coefficient solve a numerical stop rather than global-certificate-budget exhaustion. The full-tier stochastic density-oracle tests request `iterlim=1000` because coefficient certification can need more work near the normal seam; the public default remains 100, and oracle/golden tolerances were not relaxed.

GB2 and Feller–Pareto delegation and their witness contracts remain intact; the full suites include those families. No shared header required a change.

## Subsets and statistical endpoints

Write the transformed-gamma coefficients as gamma shape \(k\), power \(p\), scale \(a\). For inverse transformed gamma reverse all data logarithms and the held scale logarithm.

| Held coefficients | Reduction and outcome |
| --- | --- |
| None | One-dimensional nuisance-profile certificate, comparing finite points, lognormal and moving-endpoint power/Pareto limits. Constant positive data have unbounded likelihood. |
| \(k\) | Unique scalar maximum in log power after profiling scale, for nonconstant data. |
| \(p\) | Gamma shape equation and closed-form scale, unique for nonconstant data. |
| \(k,p\) | Unique closed-form scale, including constant data. |
| \(a\) | Global held-scale profile certificate described below. Every nonconstant sample has a finite maximum. Constant transformed-gamma data below scale also have a finite maximum; constant data at or above scale have unbounded likelihood. The inverse inequalities reverse. |
| \(a,k\) | Strictly concave power likelihood. A unique finite maximum unless every observation equals scale, when likelihood is unbounded. |
| \(a,p\) | Strictly monotone digamma equation for gamma shape, with one finite root. |
| \(a,k,p\) | Evaluate the represented coefficients; stop numerically if that evaluation cannot be certified in the supported arithmetic. |

These statements concern positive finite data and positive finite held coefficients. Invalid data or controls remain validation errors. Unrepresentable scales, lost logarithmic distinctions, unresolved comparisons, failed coefficient inclusion and exhausted work do not return a successful fit.

## Free-profile proof

Use divisor-\(n\) standardization \(u_i=(\log x_i-\bar y)/s\), \(t=ps\), and
\[
 K(t)=\log\overline{e^{tu}},\qquad
 f(k)=\log k-\psi(k)=K(t).
\]
Here \(f\) is strictly decreasing from infinity to zero, so the inner gamma shape is unique. Put
\[
 R(k)=\log\Gamma(k)-(k-\tfrac12)\log k+k-\tfrac12\log(2\pi).
\]
The improvement over the fitted lognormal, per observation, is
\[
 \Delta(t)=\log t+\tfrac12\log k-R(k)-kK(t)+\tfrac12,
 \quad \Delta'(t)=1/t-kK'(t).
\]
The profile and closure derivation is retained in [the earlier derivation](2026-10-29-trgamma-derivation.md). Its historical small-power limitation and tolerance-based face selection are superseded here; its gamma reduction and large-power inequality are retained.

### Uniform normal seam, including zero skewness

Let \(B=\max|u_i|\), \(\kappa_3=\overline{u^3}\), \(\mu_4=\overline{u^4}\), and consider \(0\le t\le T\). Set
\[
 h=TB,\quad c=\mu_4e^{-h}/12-e^{2h}/4,\quad
 d_0=1-|\kappa_3|T/3-|c|T^2>0.
\]
Integral Taylor remainders for the exponential and \(\log(1+v)\ge v-v^2/2\) give
\[
 D(t)=2K(t)/t^2\ge1+\kappa_3t/3+ct^2,
 \qquad K(t)\le t^2e^h/2.
\]
For \(K_{\max}=T^2e^h/2\le1/2\), the bounds
\[
 \frac1{2k}<f(k)<\frac1{2k}+\frac1{12k^2},\qquad
 R(k)\ge\frac1{12k}-\frac1{360k^3}
\]
imply \(k>1/(2K)\) and
\(k<(1+\sqrt{1+4K/3})/(4K)\). Maximizing
\(\frac12\log k-kK+\frac12\) over \(k\), then bounding \(-\frac12\log D\) by its quadratic Taylor bound with denominator \(d_0^2\), gives
\[
 \Delta(t)\le At+Ct^2,\qquad A=-\kappa_3/6,
\]
\[
 C=-\frac c2+
 \frac{(|\kappa_3|/3+|c|T)^2}{4d_0^2}
 -\frac{d_0}{6(1+\sqrt{1+4K_{\max}/3})}
 +\frac{T^4e^{3h}}{360}.
\]
The maximum of this signed quadratic on \([0,T]\), including its interior vertex when applicable, bounds the whole seam after both normal nuisance coordinates have been refitted. There is no division by skewness or assumption that it is nonzero. The implementation halves \(T\) until the bound excludes the seam, or reports work exhaustion/numerical failure. A directional derivative alone is no longer treated as an exclusion proof.

### Remaining cells and the large-power tail

The function
\(H(K)=\sup_k[k\log k-k-\log\Gamma(k)-kK]\)
is convex, with derivative \(-k\). Its secant bounds it above on every evaluated cell. Substitution produces a concave upper envelope \(\log t-k_{\rm sec}K(t)+C_0\); its endpoint tangents bound its maximum. Unexcluded cells are subdivided, irrespective of grid score sign changes.

The retained analytic cutoff is \(t\max u\ge4(n+\log n+10)\). Since \(K(t)\ge t\max u-\log n\), the maximizing shape in this upper bound is below \(1/n\). From \(\log\Gamma(1+k)\ge-\gamma_Ek\), its improvement over the power face is bounded by
\[
 \log z+1-z+k[\log(kn)+\gamma_E-1]\le0,
 \qquad z=kt\max u.
\]
Thus the entire unsearched large-power tail cannot beat that face.

For an interval \([l,r]\), tilted variance satisfies
\(K''(t)\ge K''(l)e^{-(r-l)(u_{\max}-u_{\min})}\).
An upper bound on profile curvature is
\[
 U=-1/r^2+
 \frac{K'(r)^2}{-f'(k(l))}
 -k(r)K''(l)e^{-(r-l)(u_{\max}-u_{\min})}.
\]
Subdivision proves strict concavity in the winning root neighborhood. Integrating this curvature upper bound can also prove monotonicity in cells outside that neighborhood. All other cells must be excluded by monotonicity or likelihood bounds. A proposal grid is an initializer, never the certificate.

### Coefficients and numerical conditioning

The winning root has opposite endpoint scores and relative power-bracket width at most \(10^{-14}\). Monotonicity of the gamma equation gives a shape enclosure of relative width at most \(2\times10^{-11}\). Propagating it through
\(\log a=\bar y+s(K-\log k)/t\)
requires width at most \(10^{-10}\) when scale is representable. A Newton proposal a few ulps outside its independently established box is clipped into that box and its equation residual rechecked. Previously this caused two avoidable no-long-double stops; [the retained diagnostic](2026-11-07-trgamma-seam-data/pre-correction-stops.json) records the narrow boxes that exposed it. Widening the coefficient threshold or accepting an objective gap alone was rejected.

For large shape and small \(tB\), the score uses
\[
 \Delta'(t)=\frac{-[2kf(k)-1]-k[tK'(t)-2K(t)]}{t},
\]
with exponential and Stirling remainders, avoiding subtraction of two terms of order \(1/t\). The root neighborhood shrinks with standardized power, with a floor for score rounding. The former fixed relative neighborhood required increasingly many curvature cells near the seam. Fitter log-sum-exp truncation now occurs at exponent \(-745\), rather than \(-50\), avoiding an observation-count-dependent omitted-mass error; omitted relative mass is at most \(ne^{-745}\).

## Held-scale proof and endpoint trace

For oriented logarithms \(z_i=\log(x_i/a)\), normalize \(w_i=z_i/b\), \(b=\max|z_i|\), \(q=pb\), and \(m=\bar w\). With the additive density-coordinate constant denoted \(J\),
\[
 G(v)=\sup_{k>0}[kv-\log\Gamma(k)],\quad \psi(k)=v,
 \qquad F(q)=\log q+G(qm)-\overline{e^{qw}}+J.
\]
\(G\) is increasing and convex. Its secants, combined with the concave remainder, give the same cell-exclusion construction. Root inclusion uses monotonicity of digamma. A local curvature upper bound is
\[
 -1/r^2+m^2/\psi_1(k_{\max})
 -\overline{w^2\exp(\min(lw,rw))}.
\]
A successful result requires a strictly concave winning neighborhood, a narrow included root and exclusion of every competing cell.

The small-\(q\) tail is bounded by
\(\log q+G(\max(0,qm))+J\), which tends to minus infinity. For \(m<0\), the log-gamma tangent inequality above gives the whole upper-tail bound
\[
 F(q)\le \log(-1/m)-1+J
 -\log\!\left(1-\frac{\gamma_E}{-qm}\right)
 -\begin{cases}
 n_0/n,&\max w\le0,\\
 e^{q\max w}/n,&\max w>0.
 \end{cases}
\]
It applies when \(-qm>\gamma_E\) and decreases thereafter. Here \(n_0\) is the number exactly at the held endpoint.

For \(m\ge0\), nonconstant data have \(M=\max w>m\). The inverse-digamma and Stirling bounds give
\[
 G(qm)\le e^{qm}+1+\tfrac12(qm+\log2)-\tfrac12\log(2\pi).
\]
Subtracting \(e^{qM}/n\) bounds the upper tail by a quantity tending to minus infinity. The checks \(e^{q(M-m)}\ge2n\) and \(Me^{qM}/(2n)>m/2+1/q\) establish that the bound is decreasing throughout the remaining tail. These analytic tails replace the fixed 720-point scan.

When \(M\le0,m<0\), the finite profile instead satisfies
\[
 F(q)=F_\infty+\frac{\gamma_E}{-qm}+O(q^{-2})
       -\overline{\mathbf1_{w<0}e^{qw}},\quad
 F_\infty=\log(-1/m)-1+J-n_0/n.
\]
It approaches the limit **from above**. Together with the small-power tail this proves finite attainment for every nonconstant sample, and for constant samples strictly on the nonconcentrating side. At or beyond the held endpoint, a constant sample can concentrate with unbounded likelihood when both shapes are free.

Each observation exactly at the held endpoint contributes an extra \(-1\) to the limiting log likelihood: its latent exponential factor is \(e^{-1}\). The `Power`/`Pareto` tag identifies the limiting law, but this diagnostic likelihood is the held-family path trace, not the ordinary endpoint-density convention of `mlpower` or `mlpareto`. A free endpoint can move slightly past the extreme observation and remove this factor. The raw-data/log-collision checks prevent rounding from inventing an endpoint multiplicity.

## Stops, margins and reciprocal transport

A finite improvement within \(10^{-8}\) per observation of a face is an unresolved comparison, with finite/face witnesses and a global upper bound, rather than a non-existence claim. For the free profile, face selection uses only the practical floating-point face margin. For held scale, tolerance-level comparisons always stop unresolved because of the approach-from-above result.

Practical free finite-profile comparisons use \(64\epsilon(1+|\Delta|)\), adding \(|\Delta_{\rm face}|\) for face comparisons; exclusion of the normal seam against a finite incumbent requires a separation of \(2\times10^{-13}(1+|\Delta|)\). Held-scale comparisons use \(2\times10^{-12}(1+|F(1)|)\), with a separate cancellation guard. These are stated working margins, not rigorous outward-rounding error bounds.

All returned finite coefficients and finite diagnostic candidates are re-evaluated in physical coordinates using
\[
 \log f(x)=\log p+\tfrac12\log k-\tfrac12\log(2\pi)
 -R(k)-\log x-k(e^v-1-v),\quad
 v=p\log(x/a)-\log k.
\]
The witness allowance includes component rounding and the first/second-order sensitivity to error in \(v\). A successful fit stops numerically if reconstruction discrepancy plus that allowance exceeds \(10^{-10}(1+|\ell|)\). Distinct raw values whose logarithms collide also stop numerically. Held coefficients and free support endpoints are restored exactly from the raw inputs. A scale outside the normal-double representation range now carries numerical stop details in both bindings, retaining the explanatory log-scale message.

For inverse transformed gamma, fitting uses \(-\log x\) and \(-\log a\), never reciprocals of observations or a held scale. The identity
\(\ell_{\rm inv}(x)=\ell_{\rm tr}(1/x)-2\sum\log x\)
shifts **every** likelihood field: global lower/upper bounds, segment upper bound, limit likelihood and every candidate interval. Flags are copied; lognormal location changes sign, power becomes Pareto, and finite scales reciprocate. Physical candidate likelihoods are then checked again. The raw sample minimum supplies Pareto endpoints even when its reciprocal overflows. A synthetic transport test checks all fields independently.

Certificate loops share \(128\min(\mathrm{iterlim},100000)\) units per certificate; scalar solves use \(\min(\mathrm{iterlim},100000)\). Grid, seam, tail and subdivision work are metered. A solver/root-width failure is numerical; exhausting the certificate allowance is a budget stop. Neither returns a capped estimate as a success. Available finite witnesses and face information survive; unresolved numerical/global searches have infinite global upper bounds. A completed face comparison has a finite comparison upper bound.

## Validation and timings

Final commands ran in the foreground to completion. Local work used the shared slot wrappers, one BLAS/OpenMP thread and nice scheduling. Long suites ran on simbox. An earlier concurrent same-worktree mirror race returned 75; its required local fallback passed. Final mirror/suite runs were serial and all exited zero.

| Final check | Machine | Result |
| --- | --- | --- |
| `UMVL_TEST_TIER=dev just cpp-test` | simbox | 106/106 shards |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 shards |
| `just cpp-test-no-long-double` | simbox | 129/129 shards |
| `UMVL_TEST_TIER=dev just test` | simbox | PASS 21048, FAIL 0, WARN 0, SKIP 154 |
| `just test-full` | simbox | PASS 43868, FAIL 0, WARN 0, SKIP 0 |
| `just js-build && just js-test` | workstation | 444/444 tests |
| `just site-check` | workstation | Green, including 99 compiled C++ snippets, browser checks and build-time MathML |

The C++ simbox toolchain is GNU 13.3; R 4.6.1 uses g++ 15.2 there. Workstation checks used GCC 15.2, Python 3.14.5 and Node 22.22.1. Existing compiler warnings in unrelated headers are distinct from the zero R test-warning counts. No package installs were needed. The site check's historical screenshot side effect was restored byte-for-byte after its last run.

The independent [checker](2026-11-07-trgamma-seam-data/check.py) uses 70-digit mpmath likelihoods, scores and root solves. [Normal-width results](2026-11-07-trgamma-seam-data/results.json) and [double-width results](2026-11-07-trgamma-seam-data/results-no-long-double.json) each contain 45 checks: both goldens and all eight held masks, four held-endpoint fixtures, 24 seeded held-scale samples (seed 11625), and a small-power represented normal-seam fit. They also check 36 seam inequalities over negative/zero/positive skewness, support endpoints, thin-cloud reconstruction and the multiplicity example below.

Maximum golden relative drift is \(5.56\times10^{-15}\) for transformed gamma and \(1.95\times10^{-11}\) for its inverse in ordinary width; both are within the existing \(2\times10^{-11}\) and \(2\times10^{-10}\) check tolerances. Double-width drift is at most \(2.08\times10^{-14}\) and \(1.95\times10^{-11}\), respectively. The represented small-power fixture agrees with the independent root within \(8.86\times10^{-14}\) (ordinary width) and \(3.69\times10^{-13}\) (double width); held-endpoint root drift is at most \(1.34\times10^{-15}\). Maximum absolute independent log-likelihood errors are \(1.65\times10^{-12}\) and \(2.33\times10^{-12}\). Score checks require \(10^{-7}\); observed maxima are \(3.13\times10^{-10}\) and \(7.53\times10^{-12}\).

Three clouds with physical spread \(10^{-14},2\times10^{-14},5\times10^{-14}\) around one lose approximately 0.001447, 0.0001834 and 0.0001371 log-likelihood units when scale is rounded. All correctly stop numerically. Their retained point likelihoods agree with the independent evaluation. These checks demonstrate reconstruction failure handling; they are not successful-fit coverage.

[Paired timings](2026-11-07-trgamma-seam-data/timings.json), reproduced by [benchmark.py](2026-11-07-trgamma-seam-data/benchmark.py), use the base headers reconstructed read-only into ignored scratch and the final headers, C++17 `-O2`, 20 fits each, one workstation CPU slot, and 500 observations from the transformed-gamma golden (reciprocated for inverse timing):

| Fit | Base ms/fit | Final ms/fit |
| --- | ---: | ---: |
| transformed gamma, free | 3.579 | 4.386 |
| transformed gamma, scale held at 2 | 11.602 | 0.568 |
| inverse transformed gamma, free | 3.146 | 3.937 |
| inverse transformed gamma, scale held at 0.5 | 11.357 | 0.537 |

This records the modest additional free-certificate work and removal of the held-scale fixed scan. Shared-workstation wall times are noisy; these are workload-specific measurements, not universal speed ratios. Baseline/current coefficients and likelihoods agree within the unchanged tolerances.

Reproduction from the worktree, with the prescribed temporary environment and slot reservation:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-trgamma-seam-data/check.py
/tmp/umvl-cpu/run env UMVL_CHECK_NO_LD=1 python3 dev/research/evaluations/2026-11-07-trgamma-seam-data/check.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-trgamma-seam-data/benchmark.py
```

## Multiplicity and precise open items

The numerical [multiplicity scan](2026-11-07-trgamma-seam-data/multiplicity.py), seed 11625, inspected 1200 free and 1200 held-scale mixture samples and retained 17 samples with competing stationary maxima. The first sample's logarithms are saved in [multiplicity.json](2026-11-07-trgamma-seam-data/multiplicity.json). Independent 70-digit root solves find two free-profile local maxima:

| Standardized power | Gamma shape | Improvement per observation | Curvature |
| ---: | ---: | ---: | ---: |
| 0.6657702070546474 | 2.6445607976909002 | 0.016987581503436665 | −0.04537370898787368 |
| 12.098420026967762 | 0.059313832906772716 | 0.030522632158395457 | −0.00010690573305524802 |

Both have positive left and negative right scores. This is numerical evidence against unimodality/single-root fitting, with a reproducible counterexample to a unique local maximum. Their heights differ: **it is not a counterexample to uniqueness of the global MLE**.

1. **Open: general global uniqueness and exact finite global ties.** The scan and high-precision local solves do not settle whether two distinct finite global maximizers can tie. An analytic theorem or a globally verified equal-height construction is missing. The dossiers say uniqueness is unknown. The algorithm does not require such a theorem: it accepts only an isolated winning basin with all competitors excluded; unresolved competitors cause a stop.
2. **Open, protected d/p/q/r scope: tiny-shape density overflow.** At `x=1, shape1=1e-320, shape2=1, scale=1`, existing `dtrgamma(..., log=TRUE)` returns `-Inf`, while the 70-digit value is approximately `-737.8272408909739`. The latent log argument is about 736, so forming `expm1` before multiplication by the tiny shape overflows. The fitter now stops numerically for this represented evaluation; this does not repair frozen-parameter density evaluation. A separately authorized `dpqr.hpp` change must evaluate the shape-times-exponential term in log scale and add independent extreme-shape density checks, including the reciprocal route. The checker deliberately records this confirmed failure separately from its passing coefficient/likelihood cases; no density truth file or protected implementation was changed.

Mathematical bounds establish the profile exclusions in exact arithmetic; golden regression, 70-digit comparisons and stochastic optimizer comparisons supply distinct numerical evidence. None of these finite validation sets proves uniform floating-point correctness or a global uniqueness theorem.
