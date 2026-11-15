# Unique flags (TASK-214.36, TASK-214.37, TASK-214.40)

Sources: [replay-integer](2026-11-15-replay-integer-report.md) (214.36), [face-fixes](2026-11-15-face-fixes-report.md)
(214.37), [replay-covers](2026-11-15-replay-covers-report.md) (214.40). Branch `claude/unique-flags` (worktree
`univariateML.wt/unique-flags`), uncommitted. Coefficients are unchanged in every case; only `unique` and warnings change.

## TASK-214.36: hyper, nhyper, triang

### Rules and proofs

Paths below are under `univariateML-cpp/include/cppunivariate/distributions/`.

1. **hyper, $(m,k)$ swap** (`hyper/mle.hpp:318-345` `hyper_detail::mark_exact_ties`, called at `:740`). The mass
   $P(X=x)=\binom{m}{x}\binom{N-m}{k-x}/\binom{N}{k}$ equals $\binom{k}{x}\binom{N-k}{m-x}/\binom{N}{m}$, so $L(N,m,k)=L(N,k,m)$ exactly.
   With $m$ and $k$ both free (N held or free) a returned $m\neq k$ ties with $(k,m)$: `unique = false`. Holds also for the
   uncertified fallback points, since the symmetry does not depend on the point being a maximum.
2. **hyper and nhyper, constant sample.** All free, constant sample $c$: $(N,N,c)$ has likelihood one for every $N\ge c$
   (hyper: $m=N$ makes $X=k$ certain, with $k=c$ and the roles of $m,k$ exchanged as needed; nhyper: $m=N$, $r=c$ makes the first $c$ draws
   white). So $(c,c,c)$ ties with $(c+1,c+1,c)$. hyper `mark_exact_ties` also covers $N$ held with $m,k$ free and $c<N$ (the pair
   $(N,c)$ gives probability one; $c=N$ stays unique), and $N$ free with $k$ (or $m$) held at $s$ when $c=0$ ($m=0$) or $c=s$
   ($m=N$). nhyper: `nhyper/mle.hpp:68-70` (all free, $r$ free or $r=c$).
3. **hyper, size one** (`mark_exact_ties`, reason 3). $N$ free with $k$ (or $m$) held at 1: the mass is Bernoulli$(m/N)$, so
   $(2N,2m)$ ties with $(N,m)$.
4. **triang, reflection** (`triang/mle.hpp:16-19` field, `:329-347` rule, `:355` outcome). With $a,b,c$ all free and the sample symmetric
   about its midrange ($x_i+x_{n+1-i}=s=\min+\max$, tested exactly with error-free sums), the map
   $(a,b,c)\mapsto(s-b,s-a,s-c)$ is a bijection of the parameter space with $\mathrm{dtriang}(s-x;s-b,s-a,s-c)=\mathrm{dtriang}(x;a,b,c)$,
   hence maps maximizers to maximizers with equal likelihood. It is a different point unless $c=s/2$ (the code
   requires $\lvert 2c-s\rvert>10^{-8}(\max-\min)$). For $n=2$ every sample is symmetric (the dossier's example). The existing
   numerical `not_unique` (`1e-10 n` margin) is not used for the outcome. Not covered: held coefficients (no rule),
   asymmetric samples with a numerical tie (those still warn only).

### Fix
- `hyper/mle.hpp`: `HyperResult::unique`, `non_unique_reason`; `outcome()` Ok returns `estimate(certified, unique)` (`:757`).
- `nhyper/mle.hpp`: `NhyperResult::unique`; `outcome()` at `:352`.
- `triang/mle.hpp`: `TriangResult::reflection_tie`; `outcome()` returns `estimate(true, !reflection_tie)`.
- Bindings (warning text only, no payload field exists for a non-comparison estimate): `bindings/common/distributions/{hyper,nhyper,triang}/fit.hpp`.
  The protocol attaches candidates only to comparison outcomes; none of these results has a comparison payload, so the tied point is
  stated in the warning (hyper: both orderings of $(m,k)$).
- READMEs of hyper, nhyper, triang updated (silent-tie sentences).

### Tests
`hyper: grid winners with exact ties are not unique` (`hyper/test.cpp`, samples of archive 003482, 003459, 003455; also the rules for
$N$ free with $k$ held and for size one, and a non-tied control stays unique), `nhyper: constant sample ...` (`nhyper/test.cpp`, archive 005151),
`triang: symmetric samples ...` (`triang/test.cpp`, archive 008159 and class `two`, with the reflected likelihood compared; asymmetric
control stays unique). All fail before the fix, pass after.

## TASK-214.37: pig shape held at 0, cmp lambda held at 0

- pig (`pig/mle.hpp:366-372`): with shape held at 0 the data are all zero (otherwise invalid) and every $\mu$ has likelihood one:
  `unique = fm` (false when the mean is free). Previously only the free-shape branch (face fix) was flagged.
- cmp (`special/cmp.hpp:463-469`): at $\lambda=0$ every $\nu$ has the same atom at zero. The rule `unique = fn` (nu held) now covers
  $\lambda$ held at 0 as well as free; the face fix had `!(!fl && !fn)`, which flagged only the free case.
- Tests: `pig: shape held at 0 with a free mean ...` (`pig/test.cpp`), `CMP lambda held at 0 with a free nu ...` (`cmp/test.cpp`), each with a
  held-other-coefficient control that stays unique. Both fail before the fix. The archive snapshots hold no such case (no hold of 0), so the snapshot
  diff is empty for pig and cmp.

## TASK-214.40: fatigue, alpha held above 2

- **The premise differs from the current records.** The stored outcome of the five inputs is not a certified unique estimate: it is an uncertified
  estimate (`certified = false`, core status 3, a comparison tie), with both roots already listed in `candidates` (for 001458 the returned
  $\beta=0.1507$ and the twin $5.4309$; log likelihoods differ in the last bit). The same record existed in the baselines 24dd714f and 318b2fd3. The
  certificate does isolate the second root (`certificate.hpp:77-95` finds both, `:111` raises the tie); the replay's hypothesis that the segment tests miss it
  is not what happens. What was wrong is the flag: `comparison_outcome_of` returns `estimate(false)` with `unique = true`.
- Fix (`fatigue/mle.hpp:53-61`): a comparison tie whose every listed root is an accurate family point is returned by `nonunique_outcome_of(r, false)`:
  uncertified estimate, `unique = false`; otherwise the old comparison rule. `comparison_payload` already takes the first listed (best) maximizer
  for a non-unique estimate and keeps both candidates. No coefficient changes.
- **Caveat for review.** For an exactly constant sample the twin tie is exact (closed-form reciprocal pair). For the near-constant inputs the roots tie
  only within the certificate's margin $10^{-10}n$; the true difference is $10^{-24}$ to $10^{-26}$ (replay report), not an exact tie, and binary64 cannot
  order them. `unique = false` there states "indistinguishable at the accuracy contract", not "proved equal". If a proved-tie-only rule is wanted, split on
  an exact-constant flag as ged does (`proved_tie`); then the five inputs would stay `unique = true` and uncertified.
- Ten cases flip, not five: the same comparison tie occurs in 001457, 001460, 001469, 001471 and 009357 (all alpha held 2.858, mask 5); each lists two roots
  with log likelihoods within one ulp.
- Test: `fatigue: alpha held above 2 with a near-constant sample lists both reciprocal roots` (`fatigue/test.cpp`, the five stored samples as exact hex
  literals): kind estimate, not certified, not unique, two candidates, $\lvert\log(\beta_0/\beta_1)\rvert>1$, first candidate not worse, tie within $10^{-10}n$.
  Fails before (`unique` true), passes after.

## Snapshot diff

Snapshots `build/snapshots/native` and `build/snapshots/double` (families hyper, nhyper, triang, pig, cmp, fatigue; 756 cases each) against
`2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`. Same case set. Paths changed: `result.unique`, `result.outcome.unique`, and `result.warnings` (hyper
only). Native and double changed the same 43 cases each; no coefficient, loglik, status or certification changed. (`data[0]` NaN of the class `nan`
cases compares unequal as NaN; not a change.)

- hyper (28), `unique` true to false: mask 2 ($N=50$ held): 003482-003492, 003494, 010973 (13, all in the finding); mask 0: 003455, 003459-003464 (7, in the finding),
  and 003454, 003456, 003457, 003458, 003465, 003466, 003467, 010971 (8 further, the uncertified fallback points with $m\ne k$, covered by the swap rule).
- nhyper (3): 005151, 005152, 005154.
- triang (2): archive 008159, class `two`.
- fatigue (10): archive 001457, 001458, 001459, 001460, 001461, 001462, 001469, 001471, 009357, 009817.
- pig, cmp: none.

## Strict layer

On the two snapshot dirs: `fit-judge.py`: 4 known (nesting), 0 new. `fit-consistency.py`: 6 known, 0 new. `fit-density.py`: no failures, 0 new. `cert-replay.py --family hyper,nhyper,triang,pig,cmp,fatigue`
(`CERT_REPLAY_COVERS_CACHE=build/covers-cache.json`): 958 cases judged; failures by check `cert-replay:cover` 10, 0 new.

**Removed from `strict-known.json` (`--prune`): 50 entries, all `cert-replay:tie` (TASK-214.36), native and double:** hyper 40 (003455, 003459-003464, 003482-003492,
003494, 010973: 20 per mode), nhyper 6 (005151, 005152, 005154 per mode), triang 4 (archive 008159, class `two` per mode). 243 entries before, 193 after; none added.

**The 10 `cert-replay:cover` fatigue entries (TASK-214.40) did not leave.** `replay_fatigue` (`dev/validation/cert_replay_covers.py:1056`) judges every `estimate` outcome and does not read
`unique`; the competing twin is still above the returned point by $10^{-24}$, so the cover still fails. The cauchy replay handles `unique = false` by also
excluding the mirror window (`:395`); fatigue needs the same: exclude the second listed root's window and check it is listed. That file is outside this job's change list, so it is not changed.

## Tests

- `just cpp-test`: 110/110 passed (before the last hyper additions; see below).
- `just cpp-test-no-long-double`: 154/154 passed (same).
- `just test-changed`: cpp-test, test, js-build, js-test all exit 0 (85 families selected; 586 JS tests passed).
- After the last additions to `hyper/test.cpp` (size-one and constant controls) only a stand-alone build of the six families' `test.cpp` (-O1) was run: 47 hyper cases and
  all new cases of the six families pass. The full suites were not rerun after that addition.
- Before the fix: 6 of the 8 new or extended cases failed in the stand-alone build (the pig and cmp cases of the face fix pass already; the new held-0 cases fail).

## Files changed

- `univariateML-cpp/include/cppunivariate/distributions/{hyper,nhyper,triang,pig,fatigue}/mle.hpp`, `.../special/cmp.hpp` (the cmp core lives there)
- `.../distributions/{hyper,nhyper,triang,pig,cmp,fatigue}/test.cpp` and `README.md`
- `bindings/common/distributions/{hyper,nhyper,triang}/fit.hpp`
- `dev/validation/strict-known.json` (pruned), this report.

## Replay of uncertified fatigue fits (added by the coordinating session)

The ten fatigue entries stayed because `replay_fatigue` judged the cover
on outcomes that are uncertified (they were uncertified before this job too;
the covers report called them certified). The design leaves uncertified
outcomes listed and not judged, so `replay_fatigue` now reports such a cover
as `open` with a note; the fatigue cache version is bumped to 4. Rerun on
this branch's snapshots: 10 open, no failure; `--prune` removed the 10
`cert-replay:cover` entries (TASK-214.40).
