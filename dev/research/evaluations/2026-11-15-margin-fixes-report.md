# Margin and unpadded-comparison fixes (TASK-214.27)

Source analysis: [margins report](2026-11-15-margins-report.md). Branch `claude/margin-fixes`, from master `6db72aaf`; not committed. Families are under `univariateML-cpp/include/cppunivariate/distributions/<id>/`.

## Per family

All four counterexamples were added to `test.cpp` before any code change and failed natively in both `univariateML-cpp/build` (long double) and `build/cpp-no-long-double`; after the fixes all pass in both. No `core/` change: no double-double helper exists in `core/`, so the TwoSum/TwoProd helpers are local to `gompertz/mle.hpp`.

### pareto2

- Native confirmation: `pareto2_mle({1, 28.809206572777494}, min = 0)` returned `ExponentialLimit` with `limit(certified = true)`. Failed in both builds.
- Fix: `pareto2/mle.hpp:128` now `FitOutcome::limit(false)`, as lomax and invpareto.
- Test: `pareto2: a margin-decided exponential stop is not certified`.
- The dossier front matter already carries `numerical: true` for this limit; unchanged.

### slash

- Native confirmation: `(0, 0, -1, 1.0001)` returned `CollapseLimit` mapped to certified `nonexistence(Unattained)`. Failed in both builds.
- Fix: `SlashResult` gained `margin_decided` (`slash/mle.hpp:14`); the free-location margin branch (line 406) sets it; `outcome` (line 445) returns `failure(Unresolved)` when it is set. The held-location half-mass `CollapseLimit` (line 287, proved in the dossier) stays `nonexistence(Unattained)`. The status remains `CollapseLimit` so the existing test on `(0,0,-1,1)` is unchanged.
- Rule on the "strict cover": no finite cover can prove $\sup L < B_v$. The interior supremum equals $B_v$ at the corner $m\to v$, $s\to0$ (gap $10^{-25}$ at $t=1$). So the free-location collapse is never certified; it is `Unresolved` in every case, including cases where the interior best is far below the collapse. This is my reading of the report's "keep `Unattained` only when a strict cover proves it"; no strict cover was implemented.
- Cost: samples such as `(0,0,-1,1)` that return nonexistence before now return an unresolved failure.
- Test: `slash: a margin-decided half-mass collapse is not a certified nonexistence`.
- Not changed (outside the allowed files): `bindings/common/distributions/slash/fit.hpp:28` still builds the stop message "Slash likelihood selects scale collapse; no finite fit is returned." with a Nonexistence payload tag for `CollapseLimit`. The binding header says payload tags never decide a rendered stop, and the outcome now says failure/unresolved, but the message text is stale for the margin case.

### binom

- Native confirmation: `(68,56,51,51,44,43,49,59,53)` returned status `Ok` (a finite fit) instead of `Underdispersed`. Failed in both builds.
- Fix: `binom/mle.hpp:56-83` adds `binom_detail::poisson_face`, used at line 253. It tests $nS+S^2\le nQ$ in `unsigned __int128`. Counts are `int`, so they are integers by type; for $0\le x_i<2^{31}$ and $n<2^{32}$ all terms are below $2^{127}$ (stated in a code comment). The binary64 ratio is kept only when `__int128` is unavailable or $n\ge2^{32}$.
- Tests: the tie, a shift series ($t = 0, 1, 1000, 10^8$; $\delta = 81t$; the limit only at $t=0$) and $(0,2)$.
- Dossier: `binom/README.md` states the exact test and the fallback.

### gompertz

- Native confirmation: `(415.8753190533696, 2688.353999767103, 240.47409880682352)` returned a certified exponential limit. Failed in both builds.
- Fix: `gompertz_detail::exponential_face` (`gompertz/mle.hpp:43-130`) decides once, on the original data, at the top of `gompertz_mle_impl` when `!standardized`. Result $+1$: certified limit; $0$: limit with `margin_decided = true`, so `outcome` gives `limit(false)`; $-1$: continue into the standardized fit. The old binary64 comparison inside the standardized recursion is removed (the recursion is called only from that function). The overflow-safe mean for the standardization is untouched; the limit result's rate in `gompertz_mle` is computed as before.
- Result of the example: the decision is "finite", the fit then ends `IterationFailure` (the maximum is at $b\sim10^{-17}$ of the data scale), a reported failure rather than a wrong certified limit. The test asserts it is not the limit.
- Pad derivation (comment in `mle.hpp`; also in the dossier). $u=2^{-53}$, $\gamma_k=ku/(1-ku)$, $n\le2^{30}$, data scaled by an exact power of two to $[0,1)$.
  1. Sum2/Dot2 (Ogita, Rump, Oishi 2005): the unevaluated pairs satisfy $|\hat\ell_S-\ell_S|\le\gamma_n^2S$ and $|\hat\ell_Q-\ell_Q|\le\gamma_{2n}^2Q$.
  2. With $\tilde T=n\hat Q-2\hat S^2$: $|T-\tilde T|\le n\gamma_{2n}^2Q+2\gamma_n^2S(2S+\gamma_n^2S)\le5\gamma_{2n}^2\,nQ$, using $S^2\le nQ$.
  3. $nq_h-2s_h^2=d+f+e_1-2e_2$ exactly (two TwoProd, one TwoSum). The remaining terms take at most eight roundings: error $\le\gamma_8W$, $W=|f|+|e_1|+2|e_2|+n|q_l|+4|s_hs_l|+2s_l^2$; the sign of the final sum is preserved.
  4. Underflow: at most $16n^22^{-1074}$.
  - Pad $P=(6\gamma_{2n}^2N+16uW+16n^22^{-1074})(1+10^{-6})$, with $N\ge nQ$ computed upward. Steps 1 and 3 use the cited bounds; the constants in steps 2-4 are my derivation, not independently proved or machine-checked, and the $10^{-6}$ factor is a safety slack, not derived. Validity: correctly rounded binary64 operations with no extended-precision intermediates, exact `std::fma`.
  - Size: $P/nQ\approx5\times10^{-30}$ for $n=3$ and $\approx3\times10^{-25}$ for $n=1000$ (from the formula, not measured).
- Tests: the counterexample; 2000 integer triples $(a,b,c)$ with $a,b\in[2^{24},2^{26})$ and $c$ within $\pm2$ of the root, exact sign of $T$ in `__int128` (relative gaps below binary64 spacing; $T>0$ must be a certified limit, $T<0$ not a limit, $T=0$ never certified); exact tie $(0,3)$ uncertified, `(0,0,1)` certified limit, `(1,2,3)` finite.
- Not covered: the held-$a$ comparison (`gompertz_mle(x, tol, maxit, a, b)`), which has the same shape; the margins report did not analyse it.

## Snapshot diff

Snapshots: `build/snapshots/{native,double}` for pareto2, slash, binom, gompertz; `build/snapshots/routes-{native,double}` for tbinom, zib, zmbinom, ztbinom (the modifier routes that call `binom_mle`; `--modifier-routes` takes route ids, `binom` is not one). Diffed against [2026-11-15-fit-snapshot-data](2026-11-15-fit-snapshot-data/); the diff tool lists every other family as "only in old", which is not a difference.

- pareto2: 21 cases per mode differ (native and double identical), all stored exponential-limit cases. Fields: `certified` and `outcome.certified` true to false, plus the `message`, which gains the adapter's sentence "No improvement above the likelihood margin was found." for an uncertified limit. No coefficient or likelihood differs.
- slash, binom, gompertz: no case differs, in either mode.
- binom routes (tbinom, zib, zmbinom, ztbinom), both modes: no field differs.

## Strict layer

On `build/snapshots/{native,routes-native}` and `{double,routes-double}`, with `--prune` only:

- fit-judge: 1060 cases per mode; failures by check `nesting` 2, both on the known list; 0 new.
- fit-consistency: `phenomenon:near-constant` 2, both known; 0 new.
- fit-density: no failure; largest relative difference for the four families $9.5\times10^{-12}$ (slash).
- cert-replay (`--family pareto2,slash,binom,gompertz`): 486 cases, no failure, 0 new, 0 listed but passing. The pareto2 `face` predicate holds on all 42 cases and does not test the certified flag, so it does not disagree with the changed flag. No replay module was edited. `strict-known.json` is unchanged.

The report's proposed stricter pareto2 replay predicate (certified limit but only the margin decides is a failure) is not in `cert-replay.py`; with the code now uncertified it would not fire.

## Suites

Each through `/tmp/umvl-cpu/run4`, one at a time.

- `just cpp-test` (dev tier): 110 of 110 passed.
- `just cpp-test-no-long-double`: 154 of 154 passed.
- `just test-changed`: first run failed in R package assembly (a duplicate `numerical` key I had added to the pareto2 front matter, which already had it); reverted. Rerun: 10 of 10 ctest shards (bbinom, binom, cmb, geom, gompertz, lgser, nbinom, pareto2, pois, slash), R tier `FAIL 0 | WARN 0 | SKIP 19 | PASS 2996`, JS 267 of 267.
- `python3 dev/tools/check_links.py`: no new failure (only the "inherited" notes it always lists). The C++ suites above ran before the one-line README revert; it changed no code.

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/pareto2/{mle.hpp,test.cpp}`
- `univariateML-cpp/include/cppunivariate/distributions/slash/{mle.hpp,test.cpp,README.md}`
- `univariateML-cpp/include/cppunivariate/distributions/binom/{mle.hpp,test.cpp,README.md}`
- `univariateML-cpp/include/cppunivariate/distributions/gompertz/{mle.hpp,test.cpp,README.md}`
- this report

Untracked build outputs (ignored): `univariateML-cpp/build`, `build/cpp-no-long-double`, `build/snapshots/`.
