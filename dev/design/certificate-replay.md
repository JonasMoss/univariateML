# Certificate replay (consolidation step 8)

Design, 2026-11-14 (job `cert-replay`, TASK-214.26), against master
`7f12c88d`. Prototype: [`cert-replay.py`](../validation/cert-replay.py)
for nbinom and lomax. Inventory of the comparisons every certificate makes,
prototype results and the proposed jobs:
[report](../research/evaluations/2026-11-14-cert-replay-report.md).

## What a replay checks

The package standard for a certificate is: *certified in exact arithmetic,
conditional on the computed comparisons, with a stated practical
floating-point margin*. The dossier proves the mathematics (bounds,
monotonicity, concavity, tail inequalities). The fitter evaluates a finite
list of comparisons in binary64, each widened by a pad that is a practical
allowance, not an outward-rounded error bound. A certificate can therefore be
wrong for an input only if some comparison it relied on would come out
differently in exact arithmetic.

A replay evaluates those comparisons again, in high precision, for the inputs
the strict layer stores (fit-audit archive, goldens, magnitude classes and
resamples of `fit-snapshot.py`, both precision modes). Each comparison ends in
one of three states:

- **holds**: the high-precision enclosure lies strictly on the side the outcome
  claims;
- **fails**: it lies strictly on the other side. The condition of the
  certificate is false for this input and the outcome is unsupported. This is
  a strict-layer failure;
- **open**: the enclosure contains the threshold, a work cap was reached, or no
  rigorous evaluation exists. Listed, not judged.

A replay does not check the mathematics of the certificate: a wrong bound
formula is reproduced by a replay that uses it. Independent checks of the
mathematics stay with the dossier proofs, `fit-refine.py` (a 50-digit local
refinement), `fit-consistency.py` and the multistart oracle of
`just quality`. A replay also says nothing about inputs outside the stored
set.

## Evidence levels

| level | how | what it supports |
|---|---|---|
| exact | rational arithmetic on the binary64 inputs (`fractions.Fraction`, Python integers) | proof for that input |
| interval | mpmath `iv` at 50 digits (outward rounded), with the dossier's analytic bounds | proof for that input, conditional on the mpmath interval library and the replay code, neither independently verified |
| numerical | ordinary mpmath at 50 digits, for functions without an interval version (digamma, incomplete gamma, Bessel) | evidence, not proof |
| mirror | the core's double evaluation re-implemented in Python (binary64, the same compensated sums and pads) | context for the margins only; it can drift from the core |

Each replayed comparison records its level. When every comparison a claim
needs is replayed at the exact or interval level (for lomax all free: gain,
root, concave and global), the replay supports the statement "this certified
outcome holds in exact arithmetic for this input", without the conditional,
given the dossier's mathematics.

## Re-derivation or trace

There are two ways to know which comparisons to replay.

**Re-derivation.** The replay reads the claim from the outcome and the
returned coefficients and recomputes the comparisons the claim implies, from
the dossier mathematics. Example (nbinom): the certificate bisects the
profile score until the signed bracket has relative width at most
$5\times10^{-7}$ and returns a size inside it, so the claim implies
$g(\hat r(1-w))>0>g(\hat r(1+w))$ with $w=5\times10^{-7}$; if both signs hold
in exact arithmetic, the claim holds for this input whatever the core
computed on the way.

- Strengths: independent of the core's internals; no core change; runs on
  every stored baseline, including old ones; also catches wrong claims that
  arise outside the certificate (a final polish that leaves the certified
  enclosure, a binding that relabels an outcome).
- Limits: it checks the claim, not the certificate's actual comparisons, so a
  pad that was too small but happened to give a true answer goes unseen; the
  claimed widths and margins are read from the code and must follow it when
  it changes; comparisons at points that are not in the output (cells of a
  cover, runner-up candidates, rejected brackets) can be checked only by
  redoing the search in high precision, cheap for a one-dimensional profile and
  costly in two or more dimensions.

**Trace.** The core records each deciding comparison in an object the caller
passes in; the replay evaluates exactly those comparisons in high precision
and checks two things: the decision, and that the binary64 value lies within
its stated pad of the high-precision value. The second check, pad adequacy,
is what re-derivation cannot do.

The hook (a design; nothing is implemented):

```cpp
// core/comparison_trace.hpp
namespace cppunivariate {
struct TraceRecord {
  std::uint16_t quantity;          // family-local id, documented next to the certificate
  std::array<double,4> at;         // the point, in certificate coordinates; unused entries NaN
  double value, pad, threshold;    // binary64 value, its pad, what it was compared with
  std::int8_t decision;            // +1 above, -1 below, 0 unresolved
};
struct ComparisonTrace {
  bool enabled = false;
  std::size_t limit = 0;           // records kept; further records are only counted
  std::size_t dropped = 0;
  std::vector<TraceRecord> records;
  void add(const TraceRecord& r) {
    if (!enabled) return;
    if (records.size() < limit) records.push_back(r); else ++dropped;
  }
};
}
```

- Passing: each certified fitter gets an overload taking `ComparisonTrace&`
  (AGENTS.md forbids raw pointers); the existing signature forwards a disabled
  local trace. When disabled the cost is one predictable branch per
  comparison: no allocation, printing or exceptions.
- Only the snapshot probe enables it; R, JS and the C++ public API never see
  it. The probe writes the records next to the result.
- Brackets record two entries (the final signed ends). Covers record one entry
  per closed cell (cell, bound, target); the certificate budget
  ($128\min(\mathrm{iterlim},10^5)$ evaluations) bounds the count, and
  `limit` caps memory.
- The replay needs a high-precision evaluator for every `quantity` id: the
  same per-family code that re-derivation needs.
- Example, nbinom: one record for the dispersion decision (path, value, pad)
  and two for the final signed bracket ($r$, score, error); about three
  records per fit. lomax: one record per closed cell (endpoints, chord bound
  or score enclosure, target), the two ends and the curvature bound of each
  isolated root, and the gain comparison; a few hundred records per fit,
  at most the certificate budget.

Cost: an edit in every certificate header (about 70 files, several per
family), which must follow step 5b and touches the files every family job
edits. Recommendation: **re-derivation first**, for every mechanism whose claim
determines its comparisons; a trace only for multi-dimensional covers, where
an independent high-precision cover is too expensive, and as the instrument of
the outward-rounding pilot.

## Mechanisms and replay route

The report's inventory assigns every family to these mechanisms (a family
usually has several, by hold subset).

| mechanism | what decides the outcome | replay | level | cost per case |
|---|---|---|---|---|
| A closed form | exact predicates on the data (constant sample, zero sum, median interval) | none needed; the predicates are exact in binary64 | n/a | 0 |
| B represented likelihood | `restored()`: ideal supremum minus the likelihood at the rounded public coefficients, against $\mathrm{tol}\max(1,\lvert\ell\rvert)+128\epsilon(1+\text{magnitudes})$ | re-derive: high-precision supremum (closed form or scalar root) and likelihood at the returned coefficients | interval or numerical | ms |
| C scalar root under a uniqueness theorem | signs of a monotone score at the ends of the final bracket (unpadded or padded) | re-derive: signs at the returned coefficient times $1\mp w$, $w$ the claimed width | interval (elementary scores) or numerical (digamma) | ms |
| D inequality that selects a face | a moment or multiplicity inequality (variance against mean, $\overline{x^2}$ against $2\bar x^2$, half-mass) | re-derive exactly | exact | µs |
| E face against interior with a likelihood margin | best interior value minus the face supremum, against a margin ($10^{-8}n$, $10^{-9}n$, `compare_face`) | re-derive both sides; whether the interior value is the interior supremum needs F | interval or numerical | ms |
| F global cover | every cell of a compact domain closed by a chord, Lipschitz or score-sign bound, plus analytic tails | one-dimensional profile: an independent interval cover (prototype: lomax); two or more dimensions: trace, or an independent cover where affordable | interval or numerical | family and sample dependent |
| G exhaustive integer search | profile values over an integer range and a tail bound against the incumbent | re-derive: high-precision profile at each integer, exact or interval tail bound | interval or exact | proportional to the range |
| H ties and non-uniqueness | two candidates' values equal (proved by symmetry or exact data) or separated by a margin | re-derive both values | exact or interval | ms |

Step 8d uses re-derived comparisons for the Lomax transfers (pareto2,
invpareto, beta-prime held unit shape and Burr held power). The scalar
profiles include GPD, half Student t, Skellam, slash, shifted Gompertz,
discretized gamma, paralogis and its reciprocal, folded normal,
Birnbaum–Saunders, held-scale Cauchy, transformed gamma and its reciprocal,
and Huber's epsilon profile.

- Shifted Gompertz uses interval endpoint shape roots, monotone shape
  enclosures over rate cells, a curvature upper bound and analytic rate
  tails. The likelihood allowance and the coefficient windows are separate
  comparisons.
- Transformed gamma uses numerical inverse digamma, the conjugate secant,
  signed root neighborhoods and analytic seam and tail bounds. The reciprocal
  uses the same profile with negated logs and reciprocal scale.
- Huber evaluates endpoint nuisance supporting planes at high precision and
  adds the normalizer chord error on each cell. It recomputes the Gaussian
  and Laplace faces subject to the same holds. Numerical planes give
  approximate evidence for the inequalities.
- Discretized gamma uses numerical incomplete-gamma evaluations. With
  joint estimation it bounds the inner log-rate optimum by score monotonicity
  and uses conditional log-cell secants on the outer shape cells.

For comparisons re-derived from a rounded public fit, an upper bound that
overlaps the target remains open. A definite failure requires a witness on the
wrong side. Reconstructing a scalar cover checks the outcome independently
of the order in which the fitter split its cells. It does not recover those
cells or test their individual floating-point pads.

The replay must use the coordinates of the certificate, not the public ones:
the claim is an enclosure in the certificate's coordinate ($\log\lambda$,
$\log(\lambda\bar x)$, the size $r$), and converting it to public coefficients
is part of what the replay checks.

## Strict-layer integration

- Script `dev/validation/cert-replay.py`, check names
  `cert-replay:<comparison>` (prototype: `dispersion`, `root`, `gain`,
  `concave`, `global`, `face`; later `restore`, `moment`, `tie`, `cover`,
  `enum`), keys `cert-replay:<comparison>|mode|family|source|case`, the
  ratchet of `strict_common.py` with `owns = startswith('cert-replay:')`.
- A failure enters `strict-known.json` only through
  `--add TASK-<card> --match <regex>` after review; `--prune` removes entries
  that pass. Open comparisons are printed with their reason and not judged,
  as `fit-refine.py` does with open refinements.
- Wiring (proposed, not done): in the `strict` recipe after `fit-refine.py`,
  `python3 $v/cert-replay.py "{{native}}" "${double[@]}" || status=1`; add the
  script to the `NOTE` of `strict_common.py` and to the validation guide's
  table.
- Cache: once covers in two or more dimensions are replayed, cache results by
  (family, input digest, returned coefficients in hex, outcome, script
  version) in `dev/validation/strict-cert-replay.jsonl.gz`, with
  `--cache-only` for `just strict`, as `fit-refine.py` does. The prototype
  needs no cache (seconds per mode).
- Versioning: a re-derived replay encodes widths and margins read from the
  core at a commit (the script's docstring names it). A change to a
  certificate updates its replay in the same commit; otherwise a tightened
  claim shows up as false failures and a loosened one goes unchecked. A trace
  removes this coupling for the comparisons it records.

## Cost

Measured (prototype, one slot, per precision mode): nbinom 1.5 CPU s for 86
cases, lomax about 10 CPU s for 104 cases (the lomax golden, $n=500$, 7 s;
every other case below 0.5 s); 23 CPU s for both families and both modes.

Projected for the full program, per mode: mechanisms A, B, C, D and H well
under a CPU minute in total; G proportional to the enumerated ranges (small on
the archive); one-dimensional covers about a CPU minute per family; two- and
three-dimensional covers seconds to minutes per case in mpmath, an hour or
more per family over the archive, hence the cache and, where the domain
allows, the trace.

## Outward-rounding pilot

Proposal: **nbinom** (scalar bracket; its score uses only $+,\times,\div$ and
one logarithm per point; the face decision is already exact integer
arithmetic) and **lomax** (one-dimensional cover with tails and a
margin-decided face; exp, log1p and log per observation and point). Both have
a replay, so the pilot can be validated against it.

What is rounded outward, in C++17 with Boost.Math only:

- A small interval type in `core/` (`struct Interval { double lo, hi; }`).
  Basic operations are computed in round-to-nearest and widened by one ulp in
  each direction (`std::nextafter`, or Boost.Math `float_next`/`float_prior`
  through `special.hpp`). IEEE 754 rounds $+,-,\times,\div,\sqrt{}$
  correctly, so the exact result lies inside. No rounding-mode switching:
  GCC needs `-frounding-math` for it, libm is not specified under directed
  modes, and WebAssembly has round-to-nearest only.
- Elementary functions: libm is not correctly rounded, and its error bound
  differs between glibc, musl (Emscripten), macOS and Windows. Two options:
  widen by a stated number of ulps (an explicit assumption, much weaker than
  today's pads but not a proof), or compute log, log1p and exp in two-word
  arithmetic with an explicit truncation bound; the core already has a
  two-word logarithm with a stated remainder (`transformed_detail::log_pair`,
  remainder below $2\times10^{-50}$). The pilot should use the second option
  for log and log1p and add a two-word exp with argument reduction by a
  two-word $\ln 2$.
- Sums of $n$ terms accumulate one ulp per addition; compensated sums are not
  needed inside an interval.
- nbinom: interval score $g(r)$ at the bracket points and the bisection
  decisions on interval signs (positive lower end, negative upper end). The
  dispersion decision stays exact; its padded fallback for products above
  $2^{52}$ becomes an interval comparison.
- lomax: interval $A,B,C$ and $h$ at every point, the chord bound and the
  score-sign exclusion on interval values, the tail bounds and the comparison
  with the margin $10^{-8}$.

What the pilot would establish:

1. On every snapshot case of the two families, whether the outward-rounded
   certificate decides the same outcome as the padded one. Any difference is
   either a pad that was too small (a certification that was not supported)
   or a pad that was larger than needed.
2. Runtime: nbinom is negligible (about 60 bracket evaluations of
   $O(\sum_v v)$ interval operations). lomax evaluates exp, log1p and log at
   every distinct observation for every point, a few hundred points per fit;
   two-word elementary functions cost perhaps ten to twenty times libm, so the
   golden ($n=500$) may go from a few milliseconds to some tens of
   milliseconds. The pilot measures this on the archive and the goldens.
3. Agreement with the replay: every C++ interval contains the mpmath value at
   the same point (a test).
4. Whether these two dossiers can drop "conditional on the computed
   comparisons", and the effort per family for the others.

Pilot structure: new headers next to the existing certificates, selected by a
compile-time switch for the pilot, so default fits do not change until the
snapshot comparison is reviewed. It edits `core/` and two families' headers,
so it runs after step 5b merges.

## What is proved and what is not

- Proved per input (conditional on mpmath and the replay code): the replayed
  comparisons at the exact or interval level. The prototype's nbinom
  dispersion is exact; its bracket signs are interval-level except for counts
  above 20,000 (numerical); all lomax comparisons are interval-level.
- Only numerical: digamma-based scores, the mirrors, and the "local maximum"
  values reported as information.
- Taken from the code, not proved here: the claimed widths (nbinom
  $5\times10^{-7}$; lomax $8\times10^{-7}$ in $z=\log(\lambda\bar x)$ for the
  all-free fit and $0.75\times10^{-10}$ in $\log\lambda$ with $\kappa$ held).
  If the code's claim is narrower than read, the replay is weaker than it
  could be; if wider, it reports false failures.
- Sketch (my argument, unchecked by a second reader): the lomax exponential
  supremum is unattained, so no finite maximizer exists, when $h<0$ on the
  compact domain and $f'(q_{lo})<0$ for
  $f(q)=-\log(1-qM_2/2)-q+q^2M_2/2$, where $M_2=\overline{y^2}$. Reason:
  $A\ge q-q^2M_2/2$ gives $h\le f(q)$ on the left tail, $f(0)=0$ and $f$ is
  convex, so $f<0$ on $(0,q_{lo}]$ when $f'(q_{lo})<0$ (this needs $M_2<2$);
  the right tail has $h<0$ as in the dossier. The prototype reports this
  classification as information only.
