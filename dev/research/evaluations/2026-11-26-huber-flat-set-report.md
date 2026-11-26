# TASK-364 · Huber flat location maximizer sets

For `mlhuber(c(-10,-9,9,10), sigma=1, epsilon=1.345)`, the fit now returns
`mu=0`, `certified=TRUE`, `unique=FALSE`, and a closed Box with location
bounds about `[-7.655, 7.655]` and fixed scale and shape. At the exact cutoff
`epsilon=9, sigma=1` on the same sample, it returns the singleton `mu=0` and
`unique=TRUE`.

## Reproduction and reference

The historical revision `19b2fb7b` reproduced the reported `unique=TRUE`
on the strict interval, including the two other hold masks with finite flat
fits. At the job baseline `2d89fd4a117a7ee7850558e6a04b4c4d74b87589`, these
cases already had unknown cardinality. They still omitted the maximizer set.
The held-scale equality case on the baseline also had unknown cardinality.
The same probe recorded status, certification, known cardinality, uniqueness,
coefficients and likelihood for both revisions before checking the fix.
[Historical probe](2026-11-26-huber-flat-set-data/19b2fb7b.txt) ·
[Job baseline](2026-11-26-huber-flat-set-data/2d89fd4a.txt) ·
[Fixed probe](2026-11-26-huber-flat-set-data/fixed.txt)

The independent mpmath reference evaluated the piecewise log density at
100 decimal digits, with `1.345` interpreted as the exact binary64 input. The
three strict rows have the listed likelihood at every one of `mu=-5, 0, 5`.
For free shape the reference independently solved the scalar shape score at
the symmetric midpoint and scanned the profile for other sign changes. That
scan is numerical evidence. The fitter uses the separate exclusion bound
below for the global claim.
[Reference script](2026-11-26-huber-flat-set-data/reference.py) ·
[Reference values to 60 digits](2026-11-26-huber-flat-set-data/reference.txt) ·
[Reproduction command](2026-11-26-huber-flat-set-data/replay.sh)

| Held coordinates | Scale | Shape | Upper location bound | Log likelihood |
| --- | ---: | ---: | ---: | ---: |
| Scale and shape | 1 | 1.34499999999999997335464741 | 7.65500000000000002664535259 | −51.40634277828734697004501652 |
| Scale and shape, equality | 1 | 9 | 0 | −183.6757541328186909671322059 |
| Shape | 4.75 | 0.5 | 6.625 | −15.73984499887975643758677333 |
| Scale | 1 | 0.10527171946471847640885387 | 8.89472828053528152359114613 | −15.77767441526718239258782387 |

## Maximizer sets and numerical decisions

Write $a,b$ for the two central order statistics, $g=(b-a)/2$,
and $c=\epsilon$. In an even sample with $g\ge c\sigma$, the clipped
scores balance on $[a+c\sigma,b-c\sigma]$. Each half contributes the same
number of saturated scores of opposite sign. Convexity of the location loss
and equality in the supporting-plane inequality give the converse. Two
distinct maximizing locations force every observation's clipped score to
stay constant between them. A strictly quadratic segment would violate that
condition, so the two locations are inside the eroded interval. Equality of
the cutoff gives the unique midpoint. An odd sample cannot balance saturated
scores alone. The held-shape joint proof also shows that strict concavity in
inverse scale forces all joint maximizers to share one scale.
[Lean proof report](2026-11-25-lean-huber-report.md) ·
[Lean theorems](../../lean/UnivariateML/Family/Huber.lean)

- **Held scale and shape.** `huber_gap_sign` compares the original binary64
  order statistics against the exact product, independently of the private
  normalized location search. Power-of-two scaling puts the larger
  endpoint's magnitude in $[1/2,1)$, so a nonzero gap is at least
  $2^{-54}$ in those units. The exponent test separates products outside
  the comparison range. Inside it, FastTwoSum and an FMA residual recover the
  exact difference and product as rounded high parts plus exact corrections.
  The compared product's least possible bit is $2^{-160}$. If an endpoint
  is dropped below $2^{-200}$, its sign matters only when the retained
  terms tie exactly. This gives an exact sign under binary64 round to nearest,
  including in the double-only build. An exact-rational oracle agreed on
  67,104 comparisons, with subnormal and extreme inputs, rounded product
  ties and seeded cases around the cutoff. The midpoint is rounded from the
  original central observations and FMA rounds the two physical bounds once.
  [Comparator](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L943) ·
  [Box construction](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L977) ·
  [Held route](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L1095) ·
  [Rational check](2026-11-26-huber-flat-set-data/exact-gap.txt)
- **Held shape, free scale.** Let $D$ be the mean absolute deviation from
  any median. On an all-tail location interval the scale score vanishes
  exactly at $\sigma=cD$, independently of location. The joint likelihood
  is concave in inverse scale and scaled location, so if $g>c^2D$, this
  stationary slice is the full joint maximizer set. Since $D\ge g$, a strict
  interval requires $c<1$. The numerical check accumulates nonnegative
  pair differences after exact power-of-two scaling, except for possible
  subnormal rounding. It requires a normal scaled gap and separation by
  $32\,\mathrm{eps}(1+n/2)$, covering the summation and product rounding
  in this comparison. The existing coefficient polish supplies the reported
  scale. [Free-scale comparison](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L1067)
- **Held scale, free shape.** Put $T=D/\sigma$ and
  $q(c)=2e^{-c^2/2}/(cZ(c))$. For $c<g/\sigma$ the profile per observation
  is $-\log(\sigma Z(c))-cT+c^2/2$, with derivative $c+q(c)/c-T$.
  Its curvature is $1-(\log Z)''$, where
  $(\log Z)''=q+(2q-q^2)/c^2$ decreases with (c). The fitter chooses a
  boundary $B$ strictly inside the flat region above the polished shape. It
  requires negative curvature up to $B$ and a negative profile derivative
  at $B$, with floating-point guards. The derivative tends to positive
  infinity at zero, so this region has one maximizing shape. The existing
  nuisance upper bounds and the shape chord bound exclude $[B,U]$, where
  $U$ is beyond the constrained normal core threshold. The normal face
  bounds the rest. This check requires separation below the fitted
  likelihood by $10^{-8}n$ plus arithmetic padding. The proof is
  exact-arithmetic conditional on those numerical comparisons. If the
  separation does not close, the existing polished estimate is returned
  uncertified with unknown cardinality.
  [Shape isolation](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L995) ·
  [Set publication and fallback](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L1169)
- **Both shape and scale free.** A flat finite joint maximizer would require
  $\sigma=cD$. Along that curve the derivative per observation is
  $c-(1-q(c))/c>0$. For $0<c<1$, set
  $I=\int_0^c e^{-t^2/2}\,dt$. Integration by parts gives
  $I=ce^{-c^2/2}+\int_0^c t^2e^{-t^2/2}\,dt$, so
  $(1-c^2)I<ce^{-c^2/2}$. Using
  $q=(1+ce^{c^2/2}I)^{-1}$ gives $1-q<c^2$. For $c\ge1$,
  $1-q<1\le c^2$. The same derivative holds at the kink by differentiability
  of the Huber loss. Thus a finite optimum with a flat location interval
  cannot occur on this route.
- **Location held.** Each of the four location-held masks removes the
  location direction. None can have a location interval. Any question of
  multiplicity across free shapes on those slices is separate.

The binding uses the existing typed maximizer-set serializer, with
coordinates ordered `mu, sigma, epsilon`. Estimated fixed coordinates of a
Box and its computed location bounds are marked rounded. The existing
singleton publication site remains in the proof inventory, with the held
equality proof documented in the dossier. [Binding](../../../bindings/common/distributions/huber/fit.hpp#L30) ·
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md)

## Route search and checks

The targeted search covered all eight hold masks in 100 fits, varying the
central pair on either side of the binary64 cutoff as well as around the
reported sample. Of the 20 fits with scale and shape held and location free,
14 gained a strict Box and two gained proved singleton cardinality at equality.
Two of 20 fits with shape alone held and all five with scale alone held gained
a strict Box. All five all-free fits went to the normal limit. The four
location-held masks gave identical probe results. No reported coefficient or
log likelihood changed in the targeted probe. The C++ and R regressions pin
the historical sample, the exact equality and both other finite flat routes.
C++ also pins opposite exact signs for two products which round to 1.
[C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/huber/test.cpp#L553) ·
[R tests](../../../bindings/r/distributions/huber/test.R#L158)

| Workstation check | Result |
| --- | --- |
| `just test-changed --no-long-double` through `run4` | Native and double-only Huber CTest passed, 1/1 each. R dev selection had 445 passes, 7 skips, zero failures and warnings. WASM build succeeded and 238/238 selected JS tests passed. |
| `just js-build`, `just js-test` | Additional serial run on the final source passed 601/601. |
| `just site-check` | Passed, including browser checks and 86 generated C++ snippets, with `BROWSER_EXECUTABLE` set to the workstation's Quarto Chromium. The default Playwright cache was absent. |
| Native and double-only fit snapshots | Both matched all 173 stored Huber records. No changes, additions or removals. |
| Native and double-only modifier-route snapshots | Huber is absent from the registered modifier route inputs and both stored modifier baselines, giving zero applicable records. |
| `fit-density.py`, `fit-consistency.py`, `strict-uniqueness.py` | Zero new findings. Density discrepancies were at most $3.1\times10^{-16}$ native and $5.4\times10^{-16}$ double-only. The singleton inventory covered 218 sites. |

The late change to the uncertified fallback guard was covered by the final
snapshots and targeted suite. The dossier and site inputs were already in
place for the site run. All runs used the workstation.
[Snapshot and strict-check output](2026-11-26-huber-flat-set-data/snapshot-checks.txt)

## Open

- The free-scale comparison uses a practical roundoff margin. If the
  cutoff falls within that margin, the floating-point check does not decide
  an exact equality or the width of a very narrow interval. Resolving those
  cases would require an exact or interval-enclosed calculation of
  $g-c^2D$.
- At held scale and free shape, a tight competition with other shapes or a
  very narrow interval can defeat the exclusion bound or its work budget.
  Establishing the complete set there would require sharper bounds for the
  competing shape regions. The fallback retains the polished fit with
  unknown cardinality.
