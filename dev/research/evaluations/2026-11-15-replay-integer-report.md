# Certificate replay, mechanism G (integer searches)

Job `replay-integer` (TASK-214.35, step 8e). Code: `dev/validation/cert_replay_integer.py`, registered in
`dev/validation/cert-replay.py` the same way as the faces module (import, then the `combined()` loop; no other change to
the driver). Families that another module also replays (binom, bbinom, cmb) get both modules' comparisons concatenated.
Design: [certificate-replay](../../design/certificate-replay.md), item 4 of
[the inventory report](2026-11-14-cert-replay-report.md). Templates: [replay-roots](2026-11-14-replay-roots-report.md),
[replay-faces](2026-11-15-replay-faces-report.md). Code read at master b80a1831; replayed on the post-5b baselines of master
a1166cbf (`2026-11-15-fit-snapshot-data`: `native`, `routes-native`, `double`, `routes-double`). Data and scripts:
`2026-11-15-replay-integer-data/`.

## Summary

- Families judged: binom (size free; prob held and size free), hyper (all seven non-evaluation hold subsets), nhyper (same),
  bbinom (size free with alpha and beta held, beta held, alpha held), cmb (size free, four hold subsets), triang (free mode,
  endpoints free or held). 441 cases per mode, 1145 comparisons per mode (2290 in total, native and double).
- **Everything the certificates claim about the integers holds**: the returned integer(s) maximize the likelihood over the
  enumerated range and every tail bound closes, at the stated evidence level, in every judged case. No `search` or `tail`
  comparison fails.
- **25 cases per mode fail the `tie` comparison** (50 records): an exact tie (hyper, nhyper) or a numerical tie (triang) with
  the returned point exists while the outcome says `unique = true`. All are real findings, none is a replay bug (details below).
- One `open` case per mode (cmb `archive/000838`), and 23 `info` entries per mode for cmb tails that the replay does not
  reproduce.
- 39 negative controls, all as intended. Run: 187 CPU s on one slot, both modes, all six families.

## Method and evidence levels

Each replay reads the claim from the record (outcome kind, `certified`, `unique`, the returned coefficients, the mask of held
coefficients) and the family `README.md` and `mle.hpp`, states what the certified outcome rests on, and evaluates it at high
precision. Nothing is read from the core beyond the record. Levels as in the module docstring:

- `exact`: Python integers and `Fraction`. Count likelihood ratios of the hypergeometric families are rational functions of
  integers (each distinct count contributes a power of an integer ratio), so adjacent-integer comparisons and ties are exact.
- `interval`: mpmath `iv`, outward rounded, at 50 digits and retried at 100, 200, 400 while an enclosure contains the
  threshold; logs of exact integer binomials or exact rationals, no gamma function.
- `numerical`: ordinary mpmath at 50 digits, for profiles over a continuous coefficient (bbinom masks 4 and 2, cmb masks 0, 2, 4,
  triang masks 0 to 2): evidence, not proof.
- Grids above 4000 points are screened in binary64 (margin $10^{-6}(1+\lvert l\rvert)$) before the survivors are judged at the
  interval level, with exact fractions deciding overlaps; 9 of 161 grid comparisons per mode were screened (7 hyper, 2 nhyper).
  The screen is a binary64 judgement for the points it discards.

Comparison names: `search` (the returned integers have the largest likelihood over the range the certificate covers), `tail`
(the bound that closes the search beyond it), `tie` (no other point attains the same likelihood, the outcome is flagged
`unique`), `coef` (binom: the returned probability is the profile maximizer at the returned size). The names do not collide
with `root`, `restore`, `moment`, `support`, `unique`, `face`, `gain`, `global`, `concave`, `dispersion`, `size2`.

## Claims and code sources

| family, subset (mask) | claim replayed | source |
|---|---|---|
| binom, size free (0) | smallest $k\ge\max x$ with $l(k+1)\le l(k)$ for the profile at $p=\bar x/k$; unimodal (DeRiggi 1983) | binom/mle.hpp:225-304 |
| binom, prob held (2) | smallest $k$ with $L(k+1)/L(k)=(1-p)^n\prod((k+1)/(k+1-x_i))\le1$; the ratio decreases in $k$ | binom/mle.hpp:104-121 |
| hyper, $m$ or $k$ free (6, 3) | last $t$ with $L(t)/L(t-1)>1$ on $[\max x,\,N-k+\min x]$ | hyper/mle.hpp:196-211, 243-283, 608-614 |
| hyper, $N$ free (5) | same on $N\ge m+k-\min x$; $D(u)<0$ for $u>k(m+k)/\bar x$ | hyper/mle.hpp:213-228, 653-681 |
| hyper, $N$ held (2) | maximum over the grid $m,k\ge\max x$, $m+k\le N+\min x$, lexicographically smallest | hyper/mle.hpp:562-605 |
| hyper, $m$ or $k$ held (1, 4) | every $N$ below the cutoff of $B_s+h\sum_{j<s}-\log(1-j/N)<l^*$, profile over the free count | hyper/mle.hpp:464-561 |
| hyper, all free (0) | nonconstant: Poisson envelope for sizes $\ge S$ and $N\ge N_0$, binomial face per smaller size; constant: likelihood one | hyper/mle.hpp:319-460, 322-326 |
| nhyper, $m$ or $r$ free (6, 3) | adjacent ratio, as for hyper | nhyper/mle.hpp:269-306, 235-267 |
| nhyper, $N$ free, $m$ held (1, 5) | lines $r$ of $N\in[m+\max x-r,\max(\cdot,\lfloor m\max x/r\rfloor)]$; every adjacent factor $\le1$ beyond ($x\,m\le N\,r$) | nhyper/mle.hpp:150-234 |
| nhyper, $N$ held (2) | finite grid $r\in[1,\min x]$, $m\in[r,N-\max x+r]$ | nhyper/mle.hpp:150-234 |
| nhyper, $r$ held or all free (4, 0) | every $N$ below the cutoff of $\max_rB_r-\sum_ic_N(x_i)<l^*$ (shifted negative-binomial face) | nhyper/mle.hpp:53-149 |
| bbinom, both shapes held (6) | sizes $[\max x,N_{\mathrm{dec}}+1]$, $\alpha(N+1)\ge x(\alpha+\beta-1)$ makes every pmf decrease | bbinom/mle.hpp:514-528 |
| bbinom, beta held (4) | sizes up to the first with $m(N+1-M)\ge hM(\beta-1)H_N$, profile over $\alpha$ | bbinom/mle.hpp:465-512 |
| bbinom, alpha held (2) | sizes below the first where the signed bounds $B(N,c)$ and $F+\ldots$ fall below $l^*$, profile over $\beta$ | bbinom/mle.hpp:402-463 |
| cmb, prob and nu held (6) | sizes from $\max x$ to the first below the decreasing bound (nu > 0: normalizer term at $\max x+1$; nu < 0: endpoint terms) | cmb/mle.hpp:1406-1467 |
| cmb, prob and/or nu free (0, 2, 4) | profile over the free coefficients at every size (concave exponential family) | cmb/mle.hpp:920-1401, README Free-size profile bounds |
| triang, mode free (0-3) | candidate modes are the distinct observations within the held endpoints; each profiled over the free endpoints; best returned | triang/mle.hpp:193-201, 207-280, 324 |

The binom prob-held, hyper/nhyper adjacent-ratio and bbinom mask 6 comparisons use no core quantity at all: the claim is an
integer, and the replay computes the same ratios the claim implies. The bounds of the dossiers (DeRiggi unimodality,
monotone ratios, the Poisson envelope, $B(N,c)$, the cmb bounds) are taken as the mathematics; the replay evaluates them, it
does not check them. The replay's cutoffs are its own (strict, without the core's $10^{-9}$ margins), so they cover at least the
range the core enumerates.

## Per-family verdicts

Counts are per mode (native and double are identical); `tail` and `tie` per case, `search` per case.

| family | cases | `search` | `tail` | `tie` | evidence |
|---|---|---|---|---|---|
| binom (0: 31, 2: 17) | 48 | 48 hold | 48 hold | 48 hold | search exact 44, interval 4 (sizes with $n(k+1)>3000$); tail exact 17 (prob held), interval 31 (sampled, free size); coef 31 exact |
| hyper (0: 7, 1-6: 13, 13, 13, 13, 13, 30) | 102 | 102 hold | 45 hold | 82 hold, **20 fail** | one-coordinate searches exact (57); grids interval (45, 7 screened); tail exact 13, interval 32 |
| nhyper (0: 13, 1: 14, 2: 13, 3: 13, 4: 14, 5: 14, 6: 30) | 111 | 111 hold | 52 hold | 108 hold, **3 fail** | one-coordinate exact (46); grids and lines interval (65, 2 screened); tail exact 28, interval 24 |
| bbinom (2: 15, 4: 17, 6: 18) | 50 | 50 hold (18 interval, 32 numerical) | 50 hold (35 exact, 15 interval) | 50 hold | masks 4, 2: numerical profiles |
| cmb (0: 5, 2: 4, 4: 14, 6: 14) | 37 | 36 hold (14 interval, 22 numerical), **1 open** | 14 hold (interval), 23 `info` | 37 hold | masks 0, 2, 4: numerical profiles, tail not replayed |
| triang (0: 35, 1: 20, 2: 19, 3: 19) | 93 | 93 hold (19 interval, 74 numerical) | not applicable | 91 hold, **2 fail** | mask 3: exact rational ties; masks 0 to 2: numerical |

`tail` counts: hyper 45 = mask 5 (13) + masks 1, 4 (26) + all free nonconstant (6); nhyper 52 = masks 1, 5 (28) + masks 4, 0 (24).
The closest tail margin among the finite-cutoff bounds is $1.4\times10^{-4}$ in log likelihood (nhyper `archive/005206`,
mask 4, bound $-26.2722$ against $l^*=-26.2721$), far above the interval widths.

Not replayed (listed by the driver, counts per mode pair in `run.log`): uncertified outcomes (hyper 46, nhyper 42, cmb 30,
bbinom 4 and 2, binom 4, triang 38), evaluation-only masks (hyper 26, nhyper 30), mask subsets with the size held (not integer
searches), triang with the mode held (one mode, no enumeration: 168), hyper with a zero count (unidentified, 2).

## Failures

All are real findings: the replay disagrees with a flag a certified outcome carries, and the replay is right. Keys are
`cert-replay:tie|mode|family|source|case`, native and double. No entry was added to `strict-known.json`.

1. **hyper, $N$ held, $m$ and $k$ free (mask 2): 13 cases per mode** (archive 003482-003492, 003494, 010973). The likelihood
   is symmetric in $(m,k)$, so every returned $(m,k)$ with $m\neq k$ ties exactly with $(k,m)$ (found by the exact comparison
   of the two likelihoods, e.g. $(50,38,6)$ against $(50,6,38)$). The outcome is a certified estimate with `unique = true`.
   The README says joint grids "resolve ties silently"; AGENTS.md principle 1 and the contract say a proved non-unique maximum is
   returned with the flag. The replay evidence is exact. Cause (read from `mle.hpp:595-600`, `outcome()`: hypothesis about
   intent): `HyperStatus::Ok` carries no tie information for grid winners; the tie status exists only for one-coordinate fits.
2. **hyper, all counts free (mask 0): 7 cases per mode.** Six nonconstant cases (archive 003459-003464, $(N,m,k)=(13,5,12)$)
   tie exactly with $(13,12,5)$, the same symmetry. One constant sample (003455, $c=5$): $(5,5,5)$ has likelihood one, and so has
   $(6,6,5)$ (exact), and every $(N,N,c)$. Same outcome flag as above. The README says constant samples "need not identify a
   unique count vector".
3. **nhyper, all free, constant sample (mask 0): 3 cases per mode** (archive 005151, 005152, 005154). $(c,c,c)$ and
   $(c+1,c+1,c)$ both have likelihood one (exact; with $m=N$ and $r=c$ the first $c$ draws are white with probability one).
   Outcome: certified, `unique = true`.
4. **triang, mode free, all endpoints free (mask 0): 2 cases per mode** (archive 008159, class `two`). Two modes have the same
   profile at the 20-digit tolerance (numerical; for two observations the reflection $x\mapsto\min+\max-x$ maps one maximizer to
   the other, as the README shows for $(0,1)$). `TriangResult` has `not_unique` set (`mle.hpp:324`, checked in `test.cpp:21`) and the
   binding warns (`bindings/common/distributions/triang/fit.hpp:60`), but `outcome()` returns `FitOutcome::estimate(true)` with
   no uniqueness argument, so the shared outcome says `unique = true`.

The one-coordinate hyper and nhyper searches (masks 6, 3, 5) and the other masks have no exact tie, so their `unique = true`
holds, with two exceptions that agree with their flags: hyper `archive/003525` (mask 5, $N$ free: $L(40)=L(39)$ exactly, outcome
`unique = false`) and cmb `archive/000838` (size one, $\nu$ unidentified, `unique = false`) are exact ties and are flagged.

## Negative controls

`2026-11-15-replay-integer-data/negative-controls.py`, output `negative-controls.out`: 39 controls, all as intended. Each
takes a stored record (and its data) and changes a returned integer, a continuous coefficient, the data or the flag, or builds
an input with an exact tie, and replays it:

- binom: free size $\pm1$ (`search`), size $-3$ (`tail`, $D>0$ at the sampled point), prob $\times(1+10^{-6})$ (`coef`), prob held
  size $\pm1$, counts all one with $p=1/2$ (sizes 1 and 2 tie) claimed unique (`tie`), data with the smallest count set to 0;
- hyper: $m+1$, $k+1$ (one-coordinate), $N+1$ and $N$ beyond $k(m+k)/\bar x$ (`tail`), grid points moved, $m$ or $k$ held with
  $N+1$, a point below the binomial face (`tail`), constant sample with $N+1$, a constructed exact tie
  ($N=7$, $k=2$, counts $0,1,2$; $m=3$ and $4$) claimed unique, and a flagged tie without an exact tie (expected `open`);
- nhyper: $m+1$, $r+1$, $N+2$ on the lines, grid and face searches, constant sample with $N+1$;
- bbinom: size $\pm1$ and $+1$ or $+2$ for masks 6, 4, 2; cmb: size $\pm2$ or $+3$ for masks 6, 4, 2, 0;
- triang: another observation as the mode, masks 3 and 0.

A returned point with zero likelihood (for example $N+1$ outside the support) is judged `fails` (`zero_likelihood`). This was a
replay bug found by the controls (an `AttributeError` on a missing likelihood) and fixed; the baseline run was repeated after
the fix.

## Open entries

- cmb `archive/000838` (native and double, mask 0, data four zeros and one 1, returned size 1, `unique = false`): `search` is
  `open`. At sizes 2 to 41 the damped Newton iteration finds no finite maximum of the profile over $(\eta,\nu)$ (the supremum
  is not attained: an escape), so the replay cannot value those sizes against the size-one value $-2.50201\ldots$. The
  tie comparison holds (exact: at size one $\nu$ does not enter the likelihood, so the flagged non-uniqueness is right).

## Families and parts not done, and what each needs

- **cmb tails for masks 0, 2, 4** (`info`, 23 per mode): the K-truncated entropy-dual bound, the blocks and the
  candidate-dependent dispersion bounds need an optimization over the entropy dual in high precision; only the enumerated window
  of sizes ($\max x$ to $\max(N^*+8,\max x+40)$) is replayed, as a witness. Needs an interval or certified dual solver, or a
  trace.
- **bbinom masks 4 and 2 `search`**: profiles over $\alpha$ or $\beta$ are numerical (log grid, golden section, root of the
  score at 50 digits), not enclosures; the global maximum of a profile over a continuous shape is not proved by an enclosure
  (the likelihood is not concave in the shape). Needs an interval cover of the shape (as lomax) per size.
- **binom free-size `tail`** is sampled ($k+2^j$, $j\le30$); DeRiggi's theorem proves it. A rigorous tail would be the theorem
  itself, not a replay.
- **hyper and nhyper partial routes** (`certified = false`: growing-urn face comparisons, unfinished grids) are not judged; they
  claim no global maximum. **triang** with the mode held, and masks 0 to 2 at the numerical level (an interval version of the
  endpoint profile needs an enclosure of a two-dimensional concave maximum).
- **Routes with integer searches not in item 4**: binom `+truncate=tbinom`, `+zero=inflated|modified|truncated` (free-size face
  certificates, mechanisms E and G), nbinom zero routes, bnbinom, zinb. Each needs its own claims table.
- bbinom with all three coefficients free is unsupported in the core (5b) and has no record; cmb `nu = 0` (smallest feasible
  size) is not replayed.

## Files changed

- `dev/validation/cert_replay_integer.py` (new).
- `dev/validation/cert-replay.py`: registration of the module and the `combined()` loop (4 lines); nothing else changed in the
  driver, no driver bug found.
- `dev/research/evaluations/2026-11-15-replay-integer-report.md` (this file).
- `dev/research/evaluations/2026-11-15-replay-integer-data/`: `records.jsonl.gz` (all comparisons, both modes, for the six
  families, including the other modules' comparisons for binom, bbinom, cmb), `run.log`, `summary.out` (from `summarize.py`),
  `negative-controls.py`, `negative-controls.out`, `summarize.py`.

Command (one slot, 50 digits, empty known list, 187 CPU s, exit status 1: the 50 `tie` failures are new against an empty list):

    cd dev/validation && echo '{"known": {}}' > /tmp/integer-none.json
    D=../research/evaluations/2026-11-15-fit-snapshot-data
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 cert-replay.py \
      $D/native.jsonl.gz,$D/routes-native.jsonl.gz $D/double.jsonl.gz,$D/routes-double.jsonl.gz \
      --known /tmp/integer-none.json --family binom,hyper,nhyper,bbinom,cmb,triang \
      --records ../research/evaluations/2026-11-15-replay-integer-data/records.jsonl
