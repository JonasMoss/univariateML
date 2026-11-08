# Generalized beta free power with free shape1 — TASK-116.47

Implemented a global certificate for finite winners with `shape2` held and
`shape1, shape3` free. It returns an enclosed global maximizer or a terminal
numerical, budget or comparison-tie stop with diagnostic witnesses. The
all-free three-shape certificate remains **open GENBETA-PQA**, below; its
default numerical output and the frozen goldens are preserved. The dossier's
`fit.certified` remains `partial`, and `fit.note` now states the new scope.

Here p=shape1, q=shape2, a=shape3, and b=scale is held strictly above max(x).
Work was confined to `codex/genbeta-rest`, starting at
`9fee8c34b838db7b11b7518a071f61495d23f282`. No commits were made.

**Implemented scope.**

| Free coefficients, with b held | Result |
|---|---|
| p,a; q=1 held | Existing exact attained ridge, with the canonical a=1 representation, preserved. |
| p,a; q positive and unequal to 1 held | New conditional-shape inclusion, uniform tail exclusion, complete compact interval coverage, joint coefficient inclusion and global candidate comparison. Unclosed face neighborhoods stop. |
| a; or q,a with p held | Previous scalar and held-p certificates preserved. Their compensated reducer is reused by the new header. |
| p,q,a | Existing numerical mesh and its face decisions preserved; global certification remains open. |

The implementation is
[free_p_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/free_p_certificate.hpp),
routed in the C++ fitter before its legacy mesh. The
[binding-neutral handler](../../../bindings/common/distributions/genbeta/fit.hpp)
transports the new gamma-neighborhood and fixed-endpoint-power-neighborhood
failures to both R and JS. No R-only routing was added.

**Certificate and evidence.**

The [held-q proof](2026-11-08-genbeta-rest-data/held-q-proof.md) uses
Y=log(b/x), v=Y/mean(Y), h=a*mean(Y), and
r(h)=-mean(log(1-exp(-h*v))). At each h, the conditional p score
`digamma(p+q)-digamma(p)-h` decreases strictly and has exactly one root.
Bisection proposes it; independently padded opposite signs validate its
coefficient enclosure. With J=trigamma(p)-trigamma(p+q), implicit
differentiation gives p'=-1/J. Monotone bounds on p, J, r' and r'' give
whole-cell profile score and curvature enclosures.

The previous job's uniform small-power gamma bound is now wired into this
route, retaining the held q. The large-power bound retains the held endpoint
b, rather than substituting the moving endpoint available when q is free.
A finite lower witness must strictly dominate both tail upper bounds.
On the remaining compact interval, score signs exclude monotone cells,
positive curvature excludes minima, and negative curvature isolates maxima.
Other cells subdivide. Integrated score bounds also exclude cells below the
best finite witness. Every retained maximum is compared through likelihood
enclosures; overlapping comparisons stop as ties. No unimodality assumption
is used.

Acceptance requires relative widths of both the h and p enclosures below
`min(gradtol,1e-9)/2`. Evaluated likelihoods are lower witnesses; concave
tangents supply conditional and isolated-root upper bounds. Every profile
point and popped cell consumes the shared `128*min(iterlim,100000)` ledger.
Inner solves, proposal refinement and tail bracketing use
`min(iterlim,100000)` iteration caps. Exhaustion never returns a proposal as
a successful estimate. Stopped finite witnesses use (p,q,a,b) order and an
infinite global likelihood upper bound.

These are certificates in the package's conditional exact-arithmetic sense,
with practical floating-point margins. Score allowances use 64 epsilon,
conditional trigamma differences 128 epsilon, profile scores 256 epsilon,
objective/cell comparisons 512 epsilon, and curvature comparisons 1024
epsilon, scaled by absolute operands. This is not outward-rounded arithmetic
or a universal libm error bound. Cancellation or ill-conditioning can force
a terminal stop.

Independent numerical evidence is separate from that proof:

- The 70-digit stationary oracle checks q=0.01, 0.1 and 0.3 on the frozen
  n=500 sample. Maximum measured relative coefficient error is `1.15e-10`;
  maximum absolute likelihood error is `2.28e-12`. The
  [workstation](2026-11-08-genbeta-rest-data/oracle-local.json),
  [workstation double-width](2026-11-08-genbeta-rest-data/oracle-double-local.json)
  and [simbox](2026-11-08-genbeta-rest-data/oracle-simbox.json) results agree.
  The [emitter](2026-11-08-genbeta-rest-data/oracle.cpp) and
  [checker](2026-11-08-genbeta-rest-data/check-oracle.py) are retained.
- The [60-digit bound checker](2026-11-08-genbeta-rest-data/check-cell-bounds.py)
  passed 200 derivative comparisons over 40 cells, two samples and five held
  q values, including values below and above one.
  [Results](2026-11-08-genbeta-rest-data/cell-bounds.json) are numerical
  consistency evidence, rather than a substitute for the interval proof.
- The default coefficients and likelihood agree exactly at printed 17-digit
  precision before and after. The timing script also checks the frozen
  coefficient tolerances and absolute likelihood tolerance `1e-8`.
  No golden file was changed.

**New mathematical findings.**

For every held q>1, sufficiently large finite power improves on the
fixed-endpoint power face. Set p=1/h. The normalized log likelihood has
correction `(digamma(q)-digamma(1))/h + O(h^-2)` plus exponentially small
data terms; its leading coefficient is positive. Consequently that power
face cannot be a global winner. For q=2, v=(0.5,1.5), h=100, p=0.01, the
normalized likelihood exceeds the power face by more than 0.009. The old
q=2 frozen-sample power selection is therefore mathematically invalid.
The new route returns a budget stop on that difficult sample and retains a
finite witness.

The [all-free seam analysis](2026-11-08-genbeta-rest-data/all-free-seam.md)
gives an exact zero-quadratic example with a positive quartic gain. Let
`t=sqrt(1-exp(-2*EulerGamma))` and v=(1-t,1+t). The fitted gamma shape is
exactly one. Although its quadratic profile coefficient vanishes, the
finite conditional profile improves by
`exp(-4*EulerGamma)*h^4 / (1152*(pi^2/6-1)) + o(h^4)`.
The [80-digit exploration](2026-11-08-genbeta-rest-data/closure-audit.json)
checks this coefficient and additional positive/negative quadratic examples;
its [script](2026-11-08-genbeta-rest-data/closure-audit.py) is reproducible.
The example concerns the gamma neighborhood; the moving-power face must
still be compared separately.

General uniqueness fails on the attained q=1 ridge. For Y=(1/6,1/2), every
p*a=3 is a maximizer, including (p,a)=(1,3), (3,1) and (6,1/2). No
non-ridge uniqueness theorem is claimed; the new enumeration permits
multiple maxima and unresolved comparisons.

**Timings.** Mean milliseconds per fit, ten repetitions, frozen n=500 sample,
C++17 `-O2`, simbox GCC 13.3, nice 10, one computation thread:

| Route | Before | After | After outcome |
|---|---:|---:|---|
| Default p,q,a free | 3.277 | 3.363 | Same numerical fit |
| Held q=0.01 | 3.122 | 2.928 | Certified finite fit |
| Held q=0.1 | 3.225 | 2.668 | Certified finite fit |
| Held q=0.3 | 3.028 | 4.025 | Certified finite fit |
| Held q=0.7 | 2.882 | 4.041 | Unresolved gamma neighborhood |
| Held q=2 | 2.913 | 225.031 | Certificate budget stop, 38,400 work units |

The previous q=0.7 and q=2 outcomes were uncertified selected faces.
These timings compare different correctness guarantees and include shared-machine
variation. The
[results](2026-11-08-genbeta-rest-data/timings-simbox.json),
[script](2026-11-08-genbeta-rest-data/timings.py) and
[frozen starting fitter](2026-11-08-genbeta-rest-data/mle-baseline.hpp.txt)
are retained. Run `timings.py` from the repository root through a CPU wrapper.
The expensive q=2 stop remains a practical limitation.

**Final validation.** Every command finished in the foreground through the
prescribed workstation or simbox wrappers. Workers had BLAS/OpenMP pinned to
one thread; long runs used nice 10. Final full tiers were rerun after replacing
the duplicated reducer with the existing power-certificate reducer.

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13.3 | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21959 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44782 |
| `just js-build`, `just js-test` | workstation, WebAssembly | Build passed; 541/541 tests |
| `just site-check` | workstation | 99 family pages; 99 C++ examples compile and link; site checks green |

The repository-wide Markdown checker reports four pre-existing destinations
outside this change: the missing `boundary-points.pdf`, the nbinom formula
`1+m/r` parsed as a link, and two site-route `/inference/` links. Those source
files match HEAD. All local destinations in the new report, proof notes and
changed genbeta dossier resolve. No unrelated source was changed to suppress
these baseline findings.

The [validation record](2026-11-08-genbeta-rest-data/validation.json) preserves
machines and numerical checks. The site check's regenerated historical
screenshot was restored. No shared core header, settled d/p/q/r, special
function, truth fixture, code generator, NEWS, status file, known-issues file
or roadmap file was changed. C++ and shared JS tests were appended. No board
edits, package installs, git writes, pushes or publishing occurred.

**Open GENBETA-PA-FACES — winning-face decisions with q held.** Finite
winners now have complete coverage and coefficient inclusion. A winning gamma
face still requires a signed uniform small-power remainder retaining q,
including zero quadratic coefficients and higher-order degeneracies. The
current unsigned gamma bound cannot establish that decision and returns a
specific unresolved-neighborhood stop. A signed large-power bound for held
q<1 was already proved in the previous job, but is not integrated into a
face-candidate comparison ledger here; this route selects no limit face.
Ill-conditioned shallow maxima can also exhaust the ledger or fail inclusion,
as the q=2 frozen-sample stop demonstrates. These are terminal diagnostics,
not unchecked estimates. The q=1 ridge remains exact.

**Open GENBETA-PQA — all three shapes free.** The unsigned small-power and
signed moving-endpoint bounds from the preceding job remain available, but
the fitter still lacks a validated conditional two-shape beta solve with
conjugate upper enclosures, complete compact coverage and joint coefficient
inclusion. It also needs a signed uniform gamma-face neighborhood, with an
explicit outcome at degenerate seams, and global comparison of all finite
candidates and both surviving faces with floating-point margins. The exact
quartic example above shows that a zero quadratic coefficient is not a face
certificate. The retained fixed 240-point mesh is a proposal mechanism whose
coverage does not increase with iterlim. Its successful default estimates and
selected gamma/moving-power faces remain numerical. Preserving that default
output was an explicit constraint; the family cannot be marked certified yes.

**Artifact display.** The requested `show` command was attempted after the
report was written. The viewer returned HTTP 403 because this worktree lies
outside its configured readable roots. The report and all evidence remain
in this worktree; no copy was written to another checkout or directory.
