# SGED held-moment certificates — TASK-116.53

Fixed positive shape with **both public moments held and skew free** now uses a global certificate. It returns an enclosed finite maximizer, a certified strict-support half-GED supremum, a comparison tie, or a structured numerical/budget stop. The shape-two delegation is retained. Free-shape held-moment fits, joint shape/skew fits, and fixed-shape fits with exactly one moment held remain open as specified below. The family flag remains `partial`.

The default all-free output is bit-identical to the baseline, including its work count. The frozen golden bytes are unchanged. This is compatibility evidence, not certification of the default fit.

Base: `ddb399172bd99091c8d1b04dbf7fe329c8adc43e`, branch `codex/sged-held`. Final workstation validation finished on 2026-11-09. No commits, board edits, installs, or changes to another worktree were made.

The implementation is [held_moment_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/held_moment_certificate.hpp), dispatched by the [public fitter](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp). The existing [common binding](../../../bindings/common/distributions/sged/fit.hpp) calls that fitter, so R and JS share the route. No common header, binding message, GED file, special-function file, d/p/q/r implementation, truth fixture or code generator changed. The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) scopes the new certificate and retains the open default.

**Constrained objective and global cover.** Put $p=\nu$, $k=\Gamma(2/p)/\Gamma(1/p)$, $v=\Gamma(3/p)/\Gamma(1/p)$ and $D=3v-4k^2$. For kernel side scales $a,b$, write $s=a+b$ and $z=(b-a)/s$. The held moments force

$$
 W(z)=\sqrt{v+Dz^2},\qquad s=\frac{2\sigma}{W(z)},\qquad
 c=\mu-kzs,\qquad \xi=\exp(\operatorname{atanh}z).
$$

Thus the whole skew domain is $-1<z<1$, with no finite skew cutoff. For $d_i=(x_i-\mu)/\sigma$,

$$
 Q_i=\tfrac12d_iW+kz,\quad e_i=\operatorname{sign}Q_i,\quad
 R_i=\frac{2|Q_i|}{1+e_i z},\quad
 \ell=C+n\log W-\sum_iR_i^p,
$$

where $C=n[\log p-\log\Gamma(1/p)-\log(2\sigma)]$. These are the actual held-mean/sd constraints; neither moment is replaced by a held mode or a profiled unconstrained scale.

On a cell $[L,H]$, interval arithmetic encloses $W,Q_i$. A residual lower bound is $2Q_{i,\mathrm{lo}}/(1+H)$ when $Q_{i,\mathrm{lo}}>0$, $-2Q_{i,\mathrm{hi}}/(1-L)$ when $Q_{i,\mathrm{hi}}<0$, and zero otherwise. Substitution gives a likelihood upper bound on the **whole cell**, including cells touching an infinite-skew endpoint.

On smooth cells the score is enclosed using

$$
 Q_i'=k+\frac{d_iDz}{2W},\qquad
 R_i'=\frac{2e_i[Q_i'(1+e_i z)-e_iQ_i]}{(1+e_i z)^2},\qquad
 \ell'=\frac{nDz}{v+Dz^2}-p\sum_iR_i^{p-1}R_i'.
$$

A definite score excludes an interior maximum. Cells crossing a join retain both signs and receive no score exclusion. Remaining cells are bisected. Every possible finite winner must lie in a union of log-skew width at most $\min(\mathtt{tol},2\times10^{-7})$, including the stored representative. A connected union that is still too wide is refined again; individual cell widths alone never authorize success. This corrected the initial held-golden diagnostic, whose first union was about $3.96\times10^{-7}$ wide at a $2\times10^{-7}$ target.

**Both skew faces.** At $z=\mathrm{side}\in\{-1,1\}$, the moments determine the half-GED kernel scale and endpoint uniquely:

$$
 \beta=\frac{\sigma}{\sqrt{v-k^2}},\qquad
 c=\mu-\mathrm{side}\,k\beta.
$$

If every observation is strictly on the supported side, the occupied-side denominator stays positive and the profile and score extend to the endpoint. An outward definite score can then exclude its endpoint cell in favor of the recovered face. Both faces and all surviving interior regions are compared only after the complete cover closes. A selected face carries its original-coordinate endpoint, kernel scale, shape, likelihood and finite global bounds. Failed coordinate recovery never becomes a limit claim.

For $p=20$, held mean zero and sd one, data consisting of two copies of $0.2$ select the reflected face; reflecting the data selects the right face. Independent 70-digit values are $|c|=1.7197401288804266705$, $\beta=3.5195732303004236503$ and total log likelihood $-2.4629734382661482971$. Two zeros give a tie between the two reflected faces, retained as distinct model witnesses.

**Finite uniqueness fails.** For $p=1$, held mean zero and sd one, and observations $(-d,d)$ with $1<d<5/(3\sqrt2)$, put $t=z^2$. Then

$$
 \ell(t)=\log[2(1+t)]-2\log2-
 \frac{2[d\sqrt{2(1+t)}-2t]}{1-t}.
$$

Its stationary equation is

$$
 d=h(t)=\frac{t^2+2t+5}{(t+3)\sqrt{2(1+t)}}.
$$

The sign of $h'$ is the sign of $(t-1)(t^2+10t+13)$, so $h$ decreases strictly from $5/(3\sqrt2)$ to 1. There is exactly one root in $0<t<1$, hence exactly two reciprocal maximizing skews. The symmetric skew is a strict local minimum, and both skew tails have likelihood tending to minus infinity. For the binary64 inputs $(-1.1,1.1)$ the independent skews are $0.625297508725291642$ and $1.599238740033624936$. The fitter returns a tie with both finite witnesses and a finite global upper bound.

**Arithmetic and work.** These are exact-arithmetic certificates conditional on computed comparisons, with practical floating-point margins, not outward-rounded intervals. Score/likelihood padding uses $4096\epsilon(1+\text{magnitude})(1+n\epsilon)(1+p+1/p)$; likelihood magnitudes include the normalizer and original scale. Residuals use $64\epsilon$ times the sum of their absolute component magnitudes plus one, with the same shape factor. Strict-support and held-mean recovery checks use $128\epsilon$ component bounds; relative scale recovery uses $64\epsilon$, again conditioned by $1+p+1/p$.

The returned public likelihood must agree with the constrained profile. Nonfinite bounds, unrepresentable moments, cancellation or failed reconstruction stop numerically. Point, face and cell evaluations, including adaptive refinement, share $128\min(\mathtt{iterlim},100000)$ units. Budget stops retain witnesses and the budget flag. Numerical/budget stops use a conservative infinite global upper bound; closed-cover ties retain their finite bound. No certificate failure falls back to the legacy scan or an optimizer score gate.

**Independent accuracy evidence.** [check_math.py](2026-11-08-sged-held-data/check_math.py) evaluates a separate kernel-side-scale density and differentiates it at 70 digits. Its inputs are the exact binary64 values. It passed 75 sampled value/score enclosure checks; those checks validate the implementation, while the interval argument above supplies the global proof. [math.json](2026-11-08-sged-held-data/math.json) retains the full reference values.

| Case | Reference skew | Measured relative error |
|---|---:|---:|
| Seven-point sample, held mean .1, sd 1.4, shape 1 | 0.903132767535269140 | $1.64\times10^{-12}$ |
| Same sample, shape 1.7 | 0.910718373012193953 | $1.96\times10^{-12}$ |
| Two finite shape-one maxima | 0.625297508725291642; 1.599238740033624936 | $1.06\times10^{-11}$ |
| Golden sample, mean/sd/shape held at frozen values | 2.589067503283481802 | $5.62\times10^{-10}$ |

The last reference is conditional on the three held coefficients. Its total log likelihood is $-639.4828052408410213$. Its skew differs from the stored all-free skew by $4.12\times10^{-8}$ relatively, below the frozen $3\times10^{-5}$ tolerance. It does not certify the other three coefficients or the joint default. The golden was not edited.

**Timings.** These are single workstation fits compiled with `-O2`, one computational thread, under the slot wrapper. They exclude compilation and lock waiting. Shared-host load affects the unchanged rows; small differences are not speedup estimates. Controls are `tol=1e-10`, `maxit=200`.

The seven-point sample is $(-3,-1,-.2,0,.3,.4,2)$, with mean .1 and sd 1.4 held. [Before/after data](2026-11-08-sged-held-data/small-after.tsv) and the [baseline projection script](2026-11-08-sged-held-data/validate-local.sh) reproduce the comparison.

| Held shape | Before ms | After ms | After outcome |
|---:|---:|---:|---|
| .5 | .241 | .430 | Numerical reconstruction stop; no estimate |
| 1 | .142 | .330 | Certified finite maximum |
| 1.7 | .194 | 1.340 | Certified finite maximum |
| 4 | .363 | .216 | Certified reflected half-GED supremum |
| 10 | .387 | .121 | Certified reflected half-GED supremum |

The following covers every original shape/skew scope and moment mask on the frozen 500-point sample. A held mean is 2 and held sd is 3. `nu` rows hold skew 1.4; `xi` rows hold shape 4. [probe.cpp](2026-11-08-sged-held-data/probe.cpp), [before.tsv](2026-11-08-sged-held-data/before.tsv) and [after-workstation.tsv](2026-11-08-sged-held-data/after-workstation.tsv) retain arguments, coefficients, likelihoods and work counts.

| Free shapes | Held moments | Before ms | After ms | Certificate scope after |
|---|---|---:|---:|---|
| nu | none | 1219.10 | 1064.01 | Existing weighted certificate |
| nu | mean | 14.64 | 12.53 | Open, unchanged |
| nu | sd | 34.22 | 29.34 | Open, unchanged |
| nu | mean, sd | 2.86 | 2.45 | Open, unchanged |
| xi | none | 161.35 | 137.61 | Existing mode certificate |
| xi | mean | 277.92 | 252.14 | Open, unchanged |
| xi | sd | 36.12 | 32.00 | Legacy boundary comparison, unverified |
| xi | mean, sd | 7.51 | 1.03 | New certified right half-GED supremum |
| nu, xi | none | 1161.22 | 1068.38 | Default unchanged, unverified |
| nu, xi | mean | 485.86 | 347.25 | Open, unchanged |
| nu, xi | sd | 136.28 | 61.68 | Open, unchanged |
| nu, xi | mean, sd | 42.97 | 31.65 | Open, unchanged |

**Final validation.** All workstation checks below finished green on the same recorded source hashes: GCC 15.2.0, R 4.6.1, Python 3.14.5; at most four reserved slots, BLAS/OpenMP pinned to one thread, lowered scheduling priority. Wall times include builds but exclude slot waiting.

| Run | Result | Seconds |
|---|---|---:|
| Targeted C++, R full-file, independent mathematics | 64 C++ cases / 10,794 assertions; R 956 passes; independent checks passed | 583.00 |
| `just cpp-test` (dev) | 106/106 shards | 318.92 |
| `just test` (dev) | FAIL 0, WARN 0, SKIP 154, PASS 22,185 | 209.11 |
| C++ full tier | 129/129 shards | 487.47 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 45,020 | 608.97 |
| `just cpp-test-no-long-double` | 129/129 shards, workstation | 471.47 |
| `just site-check` | 99 family pages with MathML; 99 C++ snippets; browser and link checks passed; WASM rebuilt | 123.63 |
| Focused public WASM API | 8/8 finite, tie, face, budget and contact cases | .44 |

The full JS suite was not required by the changed paths and was not run. The eight [public API checks](2026-11-08-sged-held-data/targeted-js.mjs) directly verify the new R/JS routing and structured stops. The source audit confirms append-only C++ tests, unchanged protected sources and golden bytes, and a bit-identical default. `site-check` refreshed an existing browser screenshot; its original bytes were restored. The reproducer preserves that screenshot automatically.

[validation.json](2026-11-08-sged-held-data/validation.json) records results, source hashes, comparisons and timings. Reproduce the workstation sequence with `/tmp/umvl-cpu/run4 bash dev/research/evaluations/2026-11-08-sged-held-data/targeted.sh`. It sets temporary directories inside the worktree and runs the tiers sequentially. No fitting source changed after the recorded checks began.

**Open O2 — asymmetric held skew, free shape, three held-moment masks.** The new code fixes shape and cannot certify its variation. With sd held, scale depends on nonmonotone gamma ratios; with mean held, mode and scale are coupled. Missing are constrained shape-score/critical-region enclosures and quantitative tails that preserve each hold, distinguish infeasible/interior/contact uniform limits, and enclose the returned coefficients. Unit held skew remains covered by GED delegation.

There is a useful proved tail fact for the both-held mask. For fixed finite skew, the log normalizing height satisfies

$$
 \log p-\log\Gamma(1/p)-\log(2\sigma)+\tfrac12\log(v+Dz^2)
 =-\log(2\sqrt3\sigma)+\frac{3\zeta(2)(1+z^2)}{2p^2}+O(p^{-3}).
$$

This follows from the convergent log-gamma expansion at $1/p=0$. If every observation is strictly inside $(\mu-\sqrt3\sigma,\mu+\sqrt3\sigma)$, all residual penalties decay exponentially in $p$. Finite sufficiently large shapes therefore exceed the uniform height, so the uniform cannot be the global supremum in this case. The [70-digit tail check](2026-11-08-sged-held-data/normalizer-tail.json) supports the expansion numerically. A computable finite tail threshold and a complete finite-shape root cover are still missing; this existence argument is not an implemented shape certificate.

**Open O4 — exactly one moment held, fixed nonnormal shape, free skew.** The remaining masks are held mean/free sd and held sd/free mean, for both subunit and smooth shapes. They need global constrained skew-profile enclosures, inner-solution/public-coordinate recovery bounds, and reachable half-GED comparisons; free sd additionally needs scale-tail control. The new both-held route cannot substitute its forced mode/scale for these free coordinates. Shape two retains its existing certified delegate.

**Open O3 — both shapes free, all four moment masks.** Still missing are a joint finite-shape critical-region cover and global half-GED shape profilers. With free moments, the earlier uniform-in-skew large-shape bound is retained, but pointwise fixed-shape mode certificates do not cover a shape interval. With held moments, joint shape/skew corner bounds must preserve the moment equations. As the [earlier free-shape derivation](2026-11-08-sged-free-report.md) shows, independent face scans are insufficient: at exact held mean zero and sd one, endpoint observations $\pm\sqrt3$, and $z=1-\lambda/p$,

$$
 p\log\frac{c+\sqrt3}{a}\longrightarrow-\gamma+\frac{2\zeta(2)}{\lambda},\qquad
 p\log\frac{\sqrt3-c}{b}\longrightarrow-\gamma.
$$

The [corner calculation](2026-11-08-sged-held-data/corners.json) independently checks these path-dependent limits. The default is explicitly among the unchanged uncertified joint searches; balanced two-point free-moment data retain their earlier analytic uniform result.

**Open C1 — contact and representation stops in the new route.** Exact half-GED contacts require one-sided penalty envelopes, not a closed-support density substitution. For shape one, held mean zero and sd one, the right endpoint tends to $-1$. An observation at $-1$ remains on the disappearing side and its kernel penalty tends to 1, whereas the closed half-GED density assigns penalty zero. The new contact regression `(-1,.2)` therefore stops numerically with an unclosed upper bound. Subunit cusps can also fail the public-likelihood reconstruction check (the shape-.5 timing row), and unrepresentable kernel moments or severe cancellation remain numerical stops. No such stop asserts non-existence or returns its witness as an MLE.

**Open V1 — final GCC 13 double-precision validation.** Development probes used simbox, but the final suite could not be started there: the machine was unreachable and the last `simbox-run ... just cpp-test-no-long-double` attempt returned 75. The prescribed workstation fallback completed green. The final GCC 13 double-precision run remains unverified; it can be repeated with `/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just cpp-test-no-long-double` when the machine is available.

**Open V2 — artifact-pane display.** The required `show` command was attempted after validation, but the viewer returned HTTP 403 because this worktree is outside its registered readable roots. Its documented remedies are to open a Nook agent thread in this worktree or add `/home/jonas/Files/programming/univariateML.wt/sged-held` to the viewer's `CODEX_MATH_WEB_VIEW_ROOTS`. No viewer configuration or file outside this worktree was changed. The report is saved at the requested path; pane display remains blocked by that registration.
