# Typed maximizer sets

TASK-234 · Design for TASK-229 · Source inventory at `e74a8c2f`

A successful fit with proved non-uniqueness carries the union of all its maximizing pieces after applying holds and fitted-domain restrictions. The selected coefficients remain the first representative. `unique = FALSE` continues to suppress inference and requires no warning. The set belongs to the fitted law, including its modifier route. A limiting model, a likelihood witness and an unresolved comparison have their existing comparison payloads. [Decision](../roadmap/decisions.md) · [Outcome model](consolidation.md)

## Inventory

In the tables, $x$ is the common observation, $B=2^{53}-1$, and count intervals contain integers. Each recipe starts with the fitted domain and slices by the supplied holds. Discard empty pieces, collapse a one-vector union to a singleton and check uniqueness after slicing. A singleton expectation in strict uses the same recipe even though its successful public fit omits the set. Modifier window bounds and ZIPF's window size are design inputs at the current snapshot seam. They constrain the recipe and are omitted from the set's coefficient vectors there.

| Flat fixture family or route | Pieces before slicing by each fixture's holds |
|---|---|
| nhyper | For zeros, use a Box $r=0, 0\le m\le N\le B$. For a positive constant, use a Box $r=x, x\le N\le B, m=N$. Adjacent composition, population and stop ties give Points (m=2,3), (N=3,4), (r=1,2) respectively, with other coordinates held. |
| hyper | For zeros, use a union of Boxes $m=0$ and $k=0$ with $0\le m,k\le N\le B$. For a positive constant atom, use a union of Boxes $m=N,k=x$ and $k=N,m=x$, $x\le N\le B$. Adjacent composition and draw ties give Points (2,3). The adjacent population tie gives Points (3,4). |
| binom | For zeros, use a union of Boxes `size=0` and `prob=0`. The positive atom is a singleton `size=x, prob=1`. Adjacent sizes give exact Points (1,2). |
| bbinom | For zeros, use a Box `size=0`, positive shapes. Adjacent sizes give exact Points (1,2). |
| cmb | For zeros, use a union of Boxes `size=0` and `prob=0`, all finite dispersions where free and $0\le\text{size}\le10^7$. For a positive atom, use a Box `size=x, prob=1`, all finite dispersions where free. For Bernoulli fixtures, use a Box with size 1 and held probability, free dispersion. Adjacent sizes give exact Points (1,2). |
| cmp, nbinom, pig | CMP Box `lambda=0, nu≥0`. NB union of Boxes `size=0` and `prob=1` within real nonnegative size and $0<p\le1$. PIG Box `shape=0, mean>0`. |
| dweibull, skellam | Discrete Weibull Box with held `shape1=.5`, `shape2>0`. Skellam singleton controls (0,0) and (1,0). |
| zib, zinb, zip | ZIB and ZINB add the Box `pi=1` to the respective base zero-atom union with $0\le\pi\le1$. ZIP Box `pi=1, lambda>0`. |
| zmbinom, zmgeom, zmlogarithmic, zmnbinom, zmpois | For zeros a Box with `p0` held or 1 and the full admissible positive-component domain. The domains in order are `size≥1` integer and `prob∈[0,1]`, `prob∈(0,1]`, `prob∈[0,1)`; real `size≥0` and `prob∈(0,1]`, `lambda≥0`. |
| ztbinom, zmbinom | At one, union of Boxes `size=1` and `prob=0`, within positive integer size and `prob∈[0,1]`. Add `p0=0` for zmbinom. |
| ztnbinom, zmnbinom | At one, Box `prob=1, size≥0` (real size). Add `p0=0` for zmnbinom. |
| tbinom | For a singleton window at $x>0$, use a Box `size≥x, prob∈(0,1)` plus singleton `size=x, prob=1`. At zero, use a Box `size≥0, prob∈[0,1)` plus singleton `size=0, prob=1`. |
| tpois, zipf | For a singleton window, use a Box of rates $>0$ for positive Poisson observation, $\ge0$ at zero. ZIPF at $N=1$: all finite exponents. |
| gompertz, gev | Box of $b>0$ at held $a$. Use a Box of $\xi\in[-1,1]$ at held location and scale. |
| gpd | With scale held: union of the Box $\mu=x,\xi\ge-1$ and the Box $\xi=-1,\mu\in[x-\sigma,x]$. Threshold/shape holds select the corresponding slices. |
| sn, st | Box with all finite `alpha`, held location and scale, and held degrees of freedom for st. |
| genbeta | Product Relation `shape1 * shape3 = 1/log(2)`, positive factors, `shape2=1, scale=1`. |
| tlambda | For the uniform member, use a Box `mu∈[x-sigma/lambda,x+sigma/lambda]`, fixed scale and shape. Scale-only fixture selects `lambda=1`. Held shape $3/2$ fixture has two support contacts `mu=-2,2`. Exact for those inputs, enclosed when the general contact calculation rounds. |
| sged | Two enclosed Points, reciprocal skews around 1 at fixed mean, SD and shape. |
| triang | Exact Points for both fixtures (0,3/2,0) and (-1/2,1,1) for the free fit and modes (1/4,3/4) with endpoints held. |

The [inventory script](../research/evaluations/2026-11-22-maxset-design-data/inventory.py) groups all 165 case keys by recipe and checks the old predicates on a hold-sliced grid. It restores the nhyper domain condition $r\le m$ before comparing the predicates. It uses the existing numerical SGED reference for branch membership. Source proofs and the recorded binding behavior are in the [hold-aware inventory](../research/evaluations/2026-11-22-zero-unique-report.md), [urn flags](../research/evaluations/2026-11-22-hyper-flags-report.md), [nhyper zeros](../research/evaluations/2026-11-22-nhyper-zero-report.md), [shared rendering](../research/evaluations/2026-11-22-render-fu-report.md), [continuous cases](../research/evaluations/2026-11-22-flat-cont-report.md) and [count cases](../research/evaluations/2026-11-22-flat-count-report.md).

| Additional fitter path from the flag and outcome audit | Mapping and migration requirement |
|---|---|
| laplace, dlaplace | Closed median interval with scale fixed at the fitted or held scale. Integer interval for dlaplace. Capture both central order statistics in core. |
| cauchy | The balanced two-point free fit returns `maximizer_support=(a,b)`. Its complete set is the upper semicircle `sigma²=(mu-a)(b-mu)` with `a<mu<b`. Use a Relation with exact observed anchors and a positive square-root branch. Held-scale fits below half the gap use Points for the two proved reflected locations. Use both accurate root brackets or exact coordinates when representable. Close likelihood comparisons on general samples remain comparison candidates. |
| hyper | The scalar `Unidentified` arms are integer Boxes (admissible composition, draw or population interval). For nonconstant swap symmetry, include the reflected Points only once global completeness is established. The held-population grid currently returns the first row within its comparison margin on pass two. Retain and adjudicate all surviving orbits before claiming coverage. A rounded-adjacent tie or a swap identity alone cannot establish exhaustiveness. For a one-draw Bernoulli fit, use the reduced empirical successes/trials as the level of a count ratio, with $1\le N\le B$. The solutions are multiples of the primitive pair. At most one multiplier means a singleton. Check the count-bound branch if the empirical fraction is infeasible. Pairs from constant data use the atom Boxes above. |
| nhyper | `Tie` uses two exact integer Points after proving equality. `Unidentified` uses the appropriate zero slice. When scalar comparisons overlap, retain an uncertified representative until the equality is proved. |
| bbinom | Size-one mixed Bernoulli ray is the Ratio Relation `alpha/(alpha+beta)=successes/trials`, positive shapes. A held shape pins the other coordinate. Profile and face ties require their existing comparison path. |
| zib | On mixed binary observations at size 1, `prob * (1-pi)=successes/trials`. Both operands free form a Product Relation with a complemented factor, `prob∈[level,1]`, `pi∈[0,1-level]`. If size is free, mixed binary data force size 1 at the attained maximum because larger sizes with positive probability have positive mass above one. A held zero-inflation or probability slices the curve. Check all-one data separately for the unique atom. |
| cmb | For any size-one Bernoulli fit the dispersion can be free even with a mixed sample. Endpoints likewise admit every allowed free dispersion when the data match the atom. Held dispersion collapses that axis. A face-comparison candidate can coexist with the finite set only as separate comparison evidence. |
| Shared count truncation routes | Geometric and held-size NB in the shared modifier seam use a Box of parent probabilities with positive mass at the singleton window count. Include atom endpoints only at feasible counts. For NB, a zero-size parent permits every admitted probability at zero. Intersect with holds and verify the current route-specific domain. |
| tbinom, tpois, zipf | Use the singleton-window constructions for each admitted design, including separately held coefficients and degenerate parent support. Derive the tbinom union from the conditional positive-event condition and any parent atom. |
| ztbinom, zmbinom, ztnbinom, zmnbinom | Constant-one atoms and absent positive components use the Boxes above. Check propagation of holds, size/probability endpoints, and the modifier's extra coordinate in every delegation path. |
| triang | Proved reflection or exact held-endpoint tie uses Points, enclosed if the profile endpoints round. The current overlapping-comparison path also sets `unique=FALSE, certified=FALSE`. Keep the selected estimate and both comparison candidates, and represent uniqueness as undetermined. |
| bnbinom | The certified size/beta reflection uses enclosed Points if the certificate covers the global set with just these two branches. Transport the full coordinate enclosure including alpha, and reflect its bounds. If the certificate establishes only a reflected pair among possible winners, retain the existing evidence until coverage is proved. |
| std, lst | Symmetry excludes a unique maximum in the named branches. Current witnesses have no coefficient accuracy or exhaustive isolation, and the branches stop unresolved. The prospective representation is Points if all roots are isolated. Preserve the stop until then. |
| Other flag carriers | `binom`, `gpd`, `gev`, `gompertz`, `tlambda`, `sged`, `sn`, `st`, `cmp`, `dweibull`, `nbinom`, `pig`, `zip`, `zinb`, `zmpois` and the other hurdle routes use the corresponding rows above, with the fitted or held constants substituted. The current exp, lomax, pareto, rayleigh and weibull censoring accessors carry a uniqueness field whose successful paths leave it true. The geom and logarithmic hurdle adapters still warn for their all-zero branches and need the shared flag-only rendering. Comparison statuses in EMG, Bhattacharjee and similar fitters are audited as candidates, including shifted limit models. |

The three piece kinds suffice. The balanced free Cauchy fit requires one additional relation on public coefficients. Product and ratio cover the scalar ridges. Use one closed `semicircle` form with the two observed support anchors and a positive square root. Restrict Box constraints in v1 to integer coordinate order $a\le b$, with equality represented by both directions. Cross-coordinate constraints in the urn examples reduce to this form once holds are applied. Support rational integer ratios using a dedicated evaluation rule within Ratio, and allow $1-z$ as a Product operand for ZIB. This keeps count multiplication out of binary64 and avoids a fourth piece kind or a family-specific expression language. The hyper ratio could also be written as an integer linear equality, but the reduced fraction makes solving for the admissible multiples and serialization simpler. A proof of multiple maxima without an exhaustive description is a certificate gap, regardless of piece vocabulary.

## Core value

The [C++17 layout prototype](../research/evaluations/2026-11-22-maxset-design-data/value.cpp) checks fixed storage, value copying and exact ratio membership near the urn count cap.

Put the value in `core/maximizer_set.hpp` with fixed storage and checked builders. Use positional coordinates in each fitter's declared order. The binding associates canonical public names from the same metadata/order used for `estimates`. Normalize modifier aliases at that mapping, including the logarithmic hurdle probe's `theta`/fixture `prob` alias. A C++ caller can supply its own static names for display. The following is the proposed v1 storage for the currently enumerable sets. Confirm the capacity bounds from complementary branches before adopting them.

```cpp
constexpr std::size_t maxset_dim = 5;
struct Interval { double lower, upper; bool lower_closed, upper_closed; };
struct Axis { Interval range; bool integer; };
struct Coordinates { std::array<double, maxset_dim> values; };
struct Enclosure { Coordinates lower, upper; };
struct Point { Coordinates estimate; std::optional<Enclosure> enclosure; };
struct Points { std::array<Point, 4> values; std::uint8_t size; };
struct CountOrder { std::uint8_t lhs, rhs; };  // lhs <= rhs, by coordinate index
struct Box {
  std::array<Axis, maxset_dim> axes;
  std::array<CountOrder, 4> order;
  std::uint8_t order_size;
};
enum class RelationForm { Product, PartOverSum, CountRatio, Semicircle };
struct Level {
  double value;
  std::uint64_t numerator, denominator;  // denominator == 0 selects value
};
struct Relation {
  std::array<Axis, maxset_dim> axes;
  RelationForm form;
  Level level;
  std::array<double, 2> anchors;  // Semicircle only.
  std::uint8_t driver, dependent;
  bool complement_driver, complement_dependent;
};
struct Piece { std::variant<Points, Box, Relation> value; };
struct MaximizerSet {
  std::array<Piece, 3> pieces;
  std::uint8_t dimension, size;
};
```

An absent set has size zero. Store the value only in affected result types, and expose it through a const result accessor. Keep `FitOutcome` a small value returned by `outcome()`. An estimate's uniqueness decision comes from the core's validation and cardinality check. A proved multi-point estimate has `unique=false` and a set. A proved singleton can keep the current boolean and omit the payload. Add an explicit `uniqueness_known` bit for comparisons whose cardinality is undetermined, and gate inference on both bits. This also lets older uncertified estimates use their existing inference policy until callers choose to propagate the new state. For the triangular ambiguity, keep the refusal of inference and the two comparison rows. The compatibility boolean needs an explicit transition, discussed under Open.

Build with `fixed(value, integer)`, `interval(...)`, `add_points(span<const Point>)`, `add_box(...)`, `add_relation(...)` and a final hold slice and seal. Builders return a small status distinguishing invalid input, an empty slice and storage overflow. No heap allocation, exception or returned view into a temporary. Initialize all arrays on construction, read only active entries, and use checked variant access. Check the per-family maximum dimension, piece and branch counts statically where possible. A capacity failure blocks publication of a supposedly complete set. If a newly proved family has unbounded finite multiplicity, choose explicitly provisioned storage and a work budget for that fitter before adopting this API.

Validate interval orientation, finite fixed coordinates, infinities of the proper sign with open ends, exact safe integer bounds on the urn paths, lattice nonemptiness, and coordinate order relations on integer axes only. Propagate bounds along the order graph and collapse equality cycles. For relations check distinct valid indices, admissible signs and the range where solving for the dependent coordinate stays within its axis, including open endpoints. Reduce rational levels and check nonzero denominators. The semicircle has location as driver, scale as dependent, ordered finite anchors, and strictly positive scale. Solve a scale hold by intersecting both branches, producing zero, one or two Points. A location hold yields at most one point. Use centered or scaled arithmetic for the square root. Validate the representative and the exact holds, and reject duplicate/overlapping root boxes unless the certificate identifies them as the same root. At the named boundary reject empty or duplicate names and schema/order mismatches. Fitter proofs establish fitted-domain inclusion and completeness. Schema validation checks encodings and feasibility.

A point enclosure denotes one isolated maximizer. Its width and the point's position within it must satisfy the fitter's coefficient accuracy rule. An interior vector is a numerical representative of that root. An exact point is a finite coefficient vector with no enclosure. Compare exact integers using the fitted count domain. For computed analytic endpoints or relation levels, the binary64 channel preserves the stored constants exactly. Where a mathematical constant rounds, label the encoding as a rounded analytic bound or level and carry its derivation or a certified scalar enclosure in a narrowly typed extension if clients require exact real evaluation. For example, genbeta computes `1/log(b/x)`, and GPD's endpoint `x-sigma` may round. An enclosure of a boundary or level by itself describes uncertainty about the scalar, so treating it as a wider maximizing piece would be incorrect. This is a wire-semantics decision before implementation.

Keep `StopDetail::candidates` and its likelihood bounds for comparisons, budgets and escapes. In particular `Shifted*` model identities carry their own coordinates and can never enter an own-family set by a name match. During migration copy an accurate candidate into Points only under an explicit family proof of exhaustiveness. Put the typed value in the shared binding `FitResult` alongside a separate comparison candidate list. A temporary legacy branch can carry old maximizer rows and prose until the last route migrates. Remove that branch before enforcing the new invariant. Treat an invalid typed payload as an implementation defect. When the certificate cannot resolve an ordinary comparison, apply the existing search/score gate and return the successful search uncertified. A proved refutation or failed search retains the existing stop semantics. [Core outcomes](../../univariateML-cpp/include/cppunivariate/core/outcome.hpp) · [Comparison detail](../../univariateML-cpp/include/cppunivariate/core/stop_detail.hpp) · [Failure handling](../validation/diagnostics.md)

## Representatives and message

Start with the returned estimate verbatim. Use canonical coordinate and piece order, deduplicate on exact canonical binary64 tuples and preserve every isolated point (one row per certified branch). For each other piece emit one witness and, when available, a second distinct witness. For real intervals use the finite closed endpoints when present, an overflow-safe interior midpoint for a bounded open interval, and 0 or a deterministic inward step for unbounded intervals. Use adjacent representable values when necessary and validate the result. For integer Boxes use the least and greatest feasible bound vectors for bounded coordinates after propagation. For an unbounded axis use a finite neighbor of the first vector with the order constraint propagated. A bounded integer interval is discrete even when it has many points. Keep the Box, sampling its extreme vectors rather than expanding it into Points. For Relations choose driver witnesses by the same rules and evaluate the dependent coordinate with stable arithmetic. For example, the Cauchy sample (1, 1, 5, 5) has semicircle witnesses (3, 2) and (2, √3). On the semicircle, use interior driver values at the midpoint and one quarter of the available location span, with an overflow-safe affine evaluation. Count Ratio witnesses use the smallest and largest admissible multipliers, guarded by exact integer division before multiplication. Use the next feasible vector in a piece if the preferred vector equals the selected estimate. A real interval can have distinct mathematical points but only one admissible binary64 witness. Preserve the piece and report the available representative count. Representative rows retain the owning model and likelihood columns for compatibility. Use a computed point likelihood or validated point likelihood bounds for enclosed or rounded representatives. A strict reviewer can evaluate the rows independently.

Generate `outcome_message` once in the common binding, using canonical public names, coordinate order and one clause per piece joined by “or”. Use “Maximum is not unique.” followed by the set description. Print every fixed coordinate and interval restriction once per piece, followed by any integer order constraints. Render endpoints with brackets and parentheses, `-Inf`/`Inf` for unbounded sides, integers in exact decimal, and finite real values with locale-independent shortest round-trip formatting or `max_digits10`. Print “approximately” for enclosed points and rounded levels, and include the closed coefficient enclosures where applicable. A table heading identifies representatives when any piece has a free coordinate. Examples of the generated message follow.

- “Maximum is not unique. size = 0 and prob in [0, 1], or prob = 0 and size in [0, Inf) (integer).”
- “Maximum is not unique. r = 0, m and N are integers, and 0 ≤ m ≤ N ≤ 9007199254740991.”
- “Maximum is not unique. alpha/(alpha + beta) = 2/3 with alpha > 0 and beta > 0, at size = 1.”

## R, JS and snapshots

Use `attr(fit, "maximizer_set")` in R and `fit.maximizer_set` in JS. Existing `maximizers` remains a data frame in R and an array of row objects in JS. Use `candidates` for comparison evidence even when a proved finite set also exists. Route the output of the shared renderer to both converters, including a separate message field so `render_fit` can clear the success error. Set `outcome_message` independently of whether there are two representable rows. Extend `ml_print_outcome`, called by both print and summary, to show the canonical description once. Keep the existing uncertified message and the old object fallback when a serialized fit predates this attribute. Extend the R test helper `fit_payload()` to expose `maximizer_set`, `maximizers`, `outcome_message` and the real `candidates`, along with estimates and flags. During migration retain its candidates alias for legacy tests via an explicit compatibility path. The catalogue's dossier-based existence panel describes family-wide results. Any interactive fit display can use the returned diagnostic and rows, without learning the fitter's private reason codes. [R converter](../../bindings/r/scripts/common_fit.h) · [JS converter](../../bindings/js/bindings.cpp) · [R fit printing](../../bindings/r/R/code_generics.R) · [JS layout](../../bindings/js/index.mjs)

Use a versioned value with ordered coordinates and tagged pieces. Arrays in the wire value follow coordinate order. Fixed axes encode as closed equal endpoints, e.g. the negative-hypergeometric zero plateau.

```json
{
  "version": 1,
  "coordinates": ["m", "N", "r"],
  "pieces": [{
    "kind": "box",
    "axes": [
      {"lower": 0, "upper": 9007199254740991, "closed": [true, true], "integer": true},
      {"lower": 0, "upper": 9007199254740991, "closed": [true, true], "integer": true},
      {"lower": 0, "upper": 0, "closed": [true, true], "integer": true}
    ],
    "order": [[0, 1]]
  }]
}
```

Points use `{"kind":"points","points":[{"estimate":[1,0.5]},{"estimate":[2,0.5]}]}` for the adjacent binomial tie. An enclosed entry adds `"enclosure":{"lower":[...],"upper":[...]}`. Relations use the same axes as Boxes, `kind="relation"`, a `form` of `product`, `part_over_sum`, `count_ratio` or `semicircle`, `driver` and `dependent` indices, two complement bits for product, and `level` as a finite number or `{"numerator":"2","denominator":"3"}`. The semicircle supplies `anchors:[a,b]` in place of a level and omits complement bits. R translates wire indices to integer or numeric values with the same zero-based convention inside the list. Signed infinities use explicit strings `"-inf"` and `"inf"` in JSON; R can use native infinities internally with an explicit encoding step. Use round-trip binary64 text for finite numbers, never the print-method digits. Canonicalize negative zero to positive zero for set comparison and wire output. Reject NaN, invalid infinity, unknown tags/versions and malformed arrays. Include tests through R serialization and JSON encode/decode in JS and Python with subnormals, one-ulp neighbors and $B$. Use the same schema in public and route snapshots, with old snapshots still readable for baseline comparison.

## Strict checks

Replace the predicates in `flat_maxima.py` with expected sets constructed independently from the cases' data, holds and analytic proofs. The inventory script is a mapping aid. For strict, generate or store independently enclosed roots with branch identity and bounds, and preserve each expected likelihood and certification requirement. Verify decoded structure, both uniqueness fields, completeness of the union, membership of the returned estimate and every representative, and first-row identity. A singleton fixture checks the expected point even though its public fit has no set. Keep the existing likelihood tolerance for likelihood comparisons.

Canonicalize by slicing, dropping empties, replacing closed singleton axes with fixed values, normalizing integer endpoints to closed lattice bounds, propagating integer orders, identifying equality classes, reducing rational levels and collapsing duplicate pieces. Compare exact Points independently of row order. For union equality, use mutual coverage, including shared edges and missing endpoints. For the v1 Boxes partition at the combined finite endpoints and split the integer cells by the weak order of constrained coordinates. Test feasibility in each cell via interval bounds and integer difference constraints, where a strict integer inequality costs one lattice step. This finds a missing face or an extra lattice slice even when two representations decompose the union differently. Merge equivalent relation forms and compare normalized levels and exact driver coverage. Test any isolated or box intersection with a nonconstant relation before discarding it as redundant. Each relation has a monotone dependent coordinate over its driver interval, except the semicircle, which splits at the anchor midpoint into two monotone arcs for coverage checks. Cross-form cases that normalization cannot settle fail comparison with a diagnostic. A sparse witness grid cannot decide equality.

Match enclosed Points one-to-one to the independent reference branches. Require finite, disjoint fitted enclosures, branch containment with a documented floating-point allowance, and the representative-to-farthest-corner error permitted by the fitting accuracy contract coordinate by coordinate. For strict's numerical reference, refine until its enclosure is negligible against that allowance. Use a small stated ulp allowance for transporting bounds across precision modes, with $10^{-9}$ relative allowed for initial fixture reference comparison only if supported by its independent reference error. Use exact held values and exact integers. Near zero use the fitter's documented absolute/centered coordinate check, with no blanket absolute tolerance for counts or endpoints. Overlapping wide boxes, two representatives on one branch and a missing boundary fail.

Have `fit-snapshot.py` emit the field and retain comparison candidates independently. Extend `fit-density.py` to evaluate selected and representative rows on the own-law path, with route density dispatch for modifiers and likelihood bounds for enclosed roots. A finite replay verifies row likelihoods and cannot prove the set. `fit-judge.py` retains coefficient, hold, nesting and outcome checks. On nested cases with expected sets on each side and equal maximal likelihood, the more-held set should equal the less-held set intersected with those holds. Put that check in consistency using the same set comparison machinery. Native versus double comparisons can report shape/coverage disagreements with the enclosed-point rules, while allowing different representatives. Keep limit, refinement and certificate replays consuming their original candidate fields. [Existing flat rule](../validation/fit-consistency.py) · [Density replay](../validation/fit-density.py) · [Judge](../validation/fit-judge.py)

## Migration

1. Establish the core type, normalization, shared payload and renderer, and snapshot schema. Keep a migration read path for old snapshots and R objects. Pilot exact Points in binom, constrained Box in nhyper, open endpoints and a union in GPD, and enclosed Points in Cauchy. Add reader round trips and negative coverage tests for a missing ray, endpoint and branch.
2. Migrate the remaining analytic Boxes and exact Points, followed by modifier routes and the closed list of relation forms. Reuse the base set only when the modifier likelihood differs by a coefficient-independent term on that set. Validate the complete slice in core, including the hyper count cap and zero-component hold logic. Check comparison branches last, including bnbinom enclosure coverage and triangular ambiguity. Keep the std/lst witness stops until complete accurate isolation is available.
3. Switch strict fixtures to structural equality and add cases absent from the original fixtures, especially the free Cauchy semicircle and its held-scale slices, ZIB's complemented product, bbinom's ray, the hyper primitive count ratio at its cap, both Laplace variants and rounded comparisons. Keep existing accuracy and likelihood tests. Update field/wording assertions only where the contract changes, preserving branch and failure coverage. Inspect all 114 TASK-229 baseline entries, 57 per mode,, replay each key, and prune with the strict driver only after a passing check. Any new mismatch requires diagnosis. Do not add a replacement known entry to compensate for a missing set. Compare four snapshots, allowing only explained payload/flag changes and explicitly justified fixes to former false claims. Keep default representative coefficients and goldens wherever they meet the contract.
4. Close the migration in the same job. Run targeted native/double/R tests, full shared-code suites, JS build and tests, snapshot comparisons and strict. Check the site if dossier text or fitter interfaces move. Include new proof or replay evidence where a prior flag was heuristic.

Estimate 4–6 engineering days plus suite time, spanning the shared value and three host/validation paths, about 30 family/route entries, and the independent coverage checker. Use one TASK-229 job with internal checkpoints. Split a follow-up proof job only if an audit finds a branch whose present result claims completeness but whose certificate cannot establish it, and record that branch's exact gap. Such a branch should retain a score-gated uncertified estimate or an existing justified stop. Representation and validation still ship together.

## Alternatives considered

| Alternative | Tradeoff |
|---|---|
| Membership-predicate callback | Compact in C++, but carries captures and lifetime requirements, cannot round-trip to R/JS, and gives strict no finite procedure to check coverage. |
| Generic polytope or mixed-integer solver | Handles urn diagonals and linear slicing, but requires arithmetic and equality machinery unrelated to the observed order constraints. Product and Bernoulli rays would still need separate nonlinear machinery. |
| SymPy-style expressions | Can express complemented products and computed levels. General expression evaluation introduces domain, singularity, simplification and host-consistency obligations. The observed relations need product, ratio and one anchored semicircle, with exact integer arithmetic for the count ratio. |
| Free text or representative rows alone | Easy to display. A half-open interval and its closed counterpart, or one arm of a union and the entire union, can share the same displayed rows. |
| Reuse StopDetail or add a new outcome kind | Comparison and limit candidates mix laws and carry a different likelihood contract. Cardinality is a property of a successful estimate, so existing outcome dispatch can continue to handle it. |

## Decisions (2026-11-22)

- **Relations (Jonas):** v1 supports the closed list of four forms: product (with complement bits), part over sum,
  count ratio and the anchored semicircle. A new form needs explicit review.
- **Undetermined uniqueness (Jonas):** R and JS expose `unique = NA` (JS `null`) when the comparison cannot settle
  cardinality: no maximizer set, comparison candidates kept, inference refused. `TRUE` is proved unique, `FALSE` proved
  non-unique with a set. The core may keep a `uniqueness_known` bit internally; the public flag is three-valued.
- **Exactness (Jonas):** binary64 values at launch. Rounded analytic endpoints and levels are labelled
  "approximately" in the message and flagged in the wire value; the dossier records the derivation. Exact recipes only
  if a consumer needs them.
- **Jobs (Jonas):** two implementation jobs. A: core type, normalisation, renderer, R/JS wire and printing, snapshot
  schema, strict set equality, pilots binom (exact Points), nhyper (ordered integer Box), gpd (open ends, union),
  cauchy (enclosed Points and the semicircle). B, after review of A: every remaining family and route and the 114
  TASK-229 strict entries.
- **Witness policy (Claude default):** only the first `maximizers` row (the returned estimate) is part of the public
  contract; further rows are illustrative and may change.
- **Coverage boundary (Claude default):** where a certificate cannot establish that the listed pieces are the whole set
  (hyper swap symmetry, near-rounded adjacent comparisons, bnbinom's reflection, std/lst), the fit keeps its current
  behaviour (uncertified estimate or stop) and claims no set until a proof exists.

## Open (as proposed; see Decisions above)

- Approve the anchored semicircle as the third named relation. It covers the free balanced Cauchy fit already reporting non-uniqueness.
- Approve the proposed distinction for unresolved comparisons. Should the compatibility `unique` boolean mean “no proved non-uniqueness” when `uniqueness_known=FALSE`, or should R/JS expose an explicit unknown value in the next API version? Preserve current inference refusal for triangular ambiguity in either case.
- I favor narrow evaluable recipes for exact-real levels and endpoints at launch, with binary64 approximations for display. Should the first release adopt that guarantee? Genbeta's log level and the GPD and Tukey translated endpoints motivate the latter. Candidate recipes are reciprocal log-ratio and location plus or minus scale divided by shape. Their rounded values alone cannot define the exact mathematical set. Fitted fixed coordinates such as the Laplace scale also need a documented representation of numerical uncertainty.
- Confirm the witness policy and whether separately stored legacy row ordering beyond the first row is part of the public contract. Current tests also match some family-specific diagnostic wording.
- Confirm the implementation boundary for hyper swap and near-rounded adjacent comparisons and bnbinom's reflected global set if the current certificates cannot establish exhaustiveness. The design supports each proved pair, while the archived witnesses and symmetry identities alone cannot settle coverage.
