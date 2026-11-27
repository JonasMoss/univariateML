# TASK-421 · Lean results for two-piece likelihoods

**SGED source discrepancy.** The atlas claims a unique scale with mean, shape and skew held at all valid positive held shapes. The statement is false. At shape one half, held mean zero and one datum at minus one, Lean proves that a valid positive finite skew gives two distinct globally maximizing scales. The construction takes the positive maximum of `log (1+t²)/t` and chooses the narrow-side width fraction so that a smooth branch ties the observation join. The source dossier and the earlier subunit analysis already allow such ties. A separate theorem proves uniqueness for every held shape at least one whenever the fit exists.

[SGED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) · [Subunit tie analysis](2026-11-07-sged-subunit-report.md)

The workspace uses mode and the positive total width with a left width fraction `w`. The side scales are `w*s` and `(1-w)*s`, so the dossier skew is `sqrt ((1-w)/w)`. With fixed shape and skew, width and public sd differ by a fixed positive factor. With held mean, the offset in standardized residual coordinates is the half-kernel mean times `1-2*w`. The Student theorems use denominator widths and the existing `TDist.exponent` convention. The exponent is positive when collapse loses.

The new [atlas fragment](../existence-atlas/lean-map.d/lean-twopiece.csv) lists all 47 public theorems. Coverage cells are empty for independent review.

## Main-question targets

Checked means the corresponding claim follows from the listed results, using existing Lean modules where indicated. The compact GED held-mean comparison is expressed using the maximum over the explicit retained shape interval, including shape one. It does not enumerate stationary candidates.

| Family | Held | Checked | Open proof obligation |
|---|---|---|---|
| snorm | all free | criterion, failure, uniqueness | — |
| snorm | mean | — | criterion, failure, uniqueness. The fixed-skew scale coercivity and collapse ray are checked. Establish uniform coercivity in skew, the support-clipped quadratic half-scale solve, strict half-face dominance on constants away from the mean, and completeness of finite stationary pieces. |
| snorm | mean+sd | — | criterion, failure, uniqueness. The exact penalty for a right endpoint observation is checked. Combine it with reflected and unsupported faces, the full compactification, and the algebraic stationary pieces and joins. |
| snorm | mean+xi | criterion, failure, uniqueness | — |
| snorm | sd | — | criterion, failure, uniqueness. The constant-data two-face tie is checked. On nonconstant data, derive the weighted location profile with sd fixed, exhaust the face traces, and retain all maximizing finite stationary pieces. |
| snorm | sd+xi | criterion, failure, uniqueness | — |
| snorm | xi | criterion, failure, uniqueness | — |
| ged | all free | criterion, failure | uniqueness. Fixed-shape location classification is available. Exact joint maximizing ties across shape, including the stated finite-uniform equality examples, still require their global comparisons. |
| ged | mean | criterion, failure | uniqueness. The compact-profile comparison is checked. The specified separated-shape tie still needs the two branch envelopes, strict exclusion of intermediate shapes, and the crossing for the stated large multiplicities. |
| ged | mean+sd | — | criterion, failure, uniqueness. Derive the signed log-Gamma width expansion and exact contact penalty at fixed mean, then prove the compact comparison, off-contact strict dominance, and the two-band global crossing on the stated sample. |
| ged | nu | uniqueness | — |
| ged | nu+sd | criterion, failure, uniqueness | — |
| ged | sd | — | criterion, failure, uniqueness. Binary64 contact exclusion is checked through irrationality. Real width contact needs the optimized two-extreme penalty and tail bounds. The balanced-pair Laplace interval also needs domination over every other shape. |
| sged | all free | — | criterion, failure, uniqueness. Combine the optimized finite-shape half-GED envelopes with the range uniform and prove joint compactness. The eight-observation reflected tie requires strict separation from these faces and from unit skew. |
| sged | mean | — | criterion, failure, uniqueness. The fixed-shape collapse is checked on rays. Establish the two optimized half-GED envelopes with mean held, unpenalized contact paths and the centered uniform. Then transfer the exact reflected witness. |
| sged | mean+nu | — | criterion, failure, uniqueness. Exclude all two-coordinate escapes apart from collapse and the two mean-constrained half ends, optimizing the supporting endpoint and width. Prove the shape-one reflected witness globally. |
| sged | mean+nu+sd | — | criterion, failure, uniqueness. Derive the forced contact ratio in SGED moment coordinates and compare the feasible skew interval against both faces. The exact two-maximizer example also needs the monotone stationary equation in squared asymmetry. |
| sged | mean+nu+xi | criterion, failure, uniqueness refuted | — |
| sged | mean+sd | — | criterion, failure, uniqueness. Control coupled shape and skew near finite-shape half contact and exact uniform width, including the zero half-contact penalty. Compare the exact reflected witness with both faces and unit skew. |
| sged | mean+sd+xi | — | criterion, failure, uniqueness. Derive the held-weight uniform trace and endpoint penalty, plus the strict off-contact comparisons. The unit-skew crossing requires the held-sd GED two-band proof. |
| sged | mean+xi | — | criterion, failure, uniqueness. Extend fixed-shape collapse and tail bounds uniformly in fitted shape, and prove the centered uniform comparison. The exact unit-skew tie needs the specified GED crossing. |
| sged | nu | — | criterion, failure, uniqueness. The fixed-skew mode and width solves are checked. With skew free the joint mode/side-scale profile and positive-width half faces remain, along with global comparison for the reflected shape-one sample. |
| sged | nu+sd | — | criterion, failure, uniqueness. Show that only the two held-sd half shapes can win, including equal bounds on constants, and compare the specified finite reflected shape-one witness. |
| sged | nu+sd+xi | criterion, failure, uniqueness | — |
| sged | nu+xi | criterion, failure, uniqueness | — |
| sged | sd | — | criterion, failure, uniqueness. Prove the sharp contact penalty using the smaller extreme count at a vanishing side, localize the finite-shape half faces, and compare the stated finite witness strictly with unit skew. |
| sged | sd+xi | — | criterion, failure, uniqueness. Prove the optimized held-weight uniform contact penalty and off-contact dominance. The unit-skew median interval requires the analogous global GED shape comparison. |
| sged | xi | — | criterion, failure, uniqueness. Obtain a signed high-shape cutoff for the weighted range-uniform profile. The exact eight-observation Laplace interval also requires the comparison with every higher shape. |
| sstd | all free | — | criterion, failure, uniqueness. Prove simultaneous shape, width, skew and mode compactification, including the normal and t2 corners and both half-profile shape corners. The six-level sample needs the global boundary exclusions and exact center separation. |
| sstd | mean | — | criterion, failure, uniqueness. Combine the held-mean normal and t2 profiles with the half profiles, concentration threshold and sharp variable-skew envelope. Prove the exact reflected six-level witness under the mean hold. |
| sstd | mean+nu | — | criterion, failure, uniqueness. The fixed-shape moving-join collapse exponent bound and sharp fixed-skew paths are checked. The optimized critical envelope over skew, both half-shape profiles, and the shape-31 reflected example remain. |
| sstd | mean+nu+sd | — | criterion, failure, uniqueness. Derive the forced thin-side contact penalty for the held Student moments and compare both half profiles with finite stationary values. The shape-31, sd-9/8 example needs exact global separation. |
| sstd | mean+nu+xi | criterion, failure, uniqueness | — |
| sstd | mean+sd | — | criterion, failure, uniqueness. Maintain both moments in the normal and half profiles and prove full contact height for the joint finite-shape half envelope. Control the lower shape endpoint and compare the exact reflected witness. |
| sstd | mean+sd+xi | — | criterion, failure. Derive the normal endpoint under both held moments and skew, and the sharp lower-shape collapse limit along the variance equation. Show that these exhaust the excluded competitors. |
| sstd | mean+xi | — | criterion, failure. Prove the normal, t2 and collapse comparisons on the held-mean, held-skew surface, including paths with simultaneous shape and width degeneration. |
| sstd | nu | — | criterion, failure, uniqueness. The fixed-shape collapse power bound allows moving skew and mode. The optimized sharp critical envelope, the two supporting-extreme half profiles and the shape-31 reflected witness remain. |
| sstd | nu+sd | — | criterion, failure, uniqueness. Exhaust both fixed-shape half ends at held sd and prove their optimized values with joins at the sample extremes. Compare the exact reflected six-level witness. |
| sstd | nu+xi | — | criterion, failure, uniqueness. Exact-join collapse and the moving-path power bound are checked. Prove the sharp critical envelope for all moving joins and the two-dimensional semialgebraic stationary components, including equal-value components. |
| sstd | sd | — | criterion, failure, uniqueness. Control the normal, half-profile and corner traces with held sd and prove the exact reflected six-level comparison. |
| sstd | sd+xi | — | criterion, failure, uniqueness. Prove the normal profile and lower-shape sharp atom envelope with held sd and skew. The symmetric pair at ±100 requires global boundary exclusion and strict preference over the centered location. |
| sstd | xi | — | criterion, failure. Establish jointly the normal and t2 fixed-skew profiles and the critical atom envelope through the lower-shape corner. |

Of the 121 main-question claims, 31 are marked checked here, 1 is refuted, and 89 retain open obligations. The checked GED fixed-shape classification uses `Ged.joint_existsUnique`, `Ged.joint_one`, `Ged.heldScale_existsUnique` and `Ged.heldScale_one` for the shape-one and superunit cases.

## Limit paths and face comparisons

The keys below follow the requested coverage report. The SGED collapse detector keys in that report are shifted relative to the current atlas. The corresponding current indices, in the order mean, mean+nu, mean+nu+xi, mean+xi, nu, nu+xi and xi, are 4, 16, 24, 18, 9, 21 and 11.

[Coverage inventory](2026-11-27-atlas-coverage3-report.md) · [Current atlas](../existence-atlas/atlas.csv)

| Family | Held | Key | Result or open obligation |
|---|---|---|---|
| snorm | all free | `path:1` | Checked in the corresponding theorem below. |
| snorm | all free | `path:2` | Both sample traces converge in the specified orientations. Distributional convergence to the translated half-normal remains. |
| snorm | all free | `path:3` | Both sample traces converge in the specified orientations. Distributional convergence to the translated half-normal remains. |
| snorm | all free | `limit:1` | Checked in the corresponding theorem below. |
| snorm | all free | `limit:2` | Checked in the corresponding theorem below. |
| snorm | all free | `limit:3` | Checked in the corresponding theorem below. |
| snorm | mean | `path:1` | Checked in the corresponding theorem below. |
| snorm | mean | `limit:1` | A fixed-skew collapse path is checked. The necessity test must exclude collapse uniformly over skew with mean held. |
| snorm | mean+xi | `path:1` | Checked in the corresponding theorem below. |
| snorm | mean+xi | `limit:1` | Checked in the corresponding theorem below. |
| snorm | xi | `path:1` | Checked in the corresponding theorem below. |
| snorm | xi | `limit:1` | Checked in the corresponding theorem below. |
| ged | all free | `path:2` | The positive-width, vanishing-widening likelihood path is checked. Distributional convergence and the full optimal-parameter limit are still open. |
| ged | mean | `path:2` | The positive-width, vanishing-widening likelihood path is checked. Distributional convergence and the full optimal-parameter limit are still open. |
| ged | mean+sd | `path:2` | Prove the held-sd uniform limit, including the exact contact trace and moving optimal location when location is free. |
| ged | sd | `path:2` | Prove the held-sd uniform limit, including the exact contact trace and moving optimal location when location is free. |
| sged | mean | `limit:6` | Collapse on fixed-shape and fixed-skew rays is checked. The exclusion of simultaneous collapse off the all-tied branch requires uniform bounds over the remaining free coefficients. |
| sged | mean+nu | `limit:20` | Collapse on fixed-shape and fixed-skew rays is checked. The exclusion of simultaneous collapse off the all-tied branch requires uniform bounds over the remaining free coefficients. |
| sged | mean+nu+xi | `limit:30` | Checked in the corresponding theorem below. |
| sged | mean+xi | `limit:22` | Collapse on fixed-shape and fixed-skew rays is checked. The exclusion of simultaneous collapse off the all-tied branch requires uniform bounds over the remaining free coefficients. |
| sged | nu | `limit:12` | Collapse on fixed-shape and fixed-skew rays is checked. The exclusion of simultaneous collapse off the all-tied branch requires uniform bounds over the remaining free coefficients. |
| sged | nu+xi | `limit:26` | Checked in the corresponding theorem below. |
| sged | xi | `limit:14` | Collapse on fixed-shape and fixed-skew rays is checked. The exclusion of simultaneous collapse off the all-tied branch requires uniform bounds over the remaining free coefficients. |
| sged | sd | `case:two-observations` | Prove the stated two-observation held-sd comparison, including the exact-width contact branch. |
| sstd | mean | `limit:7` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | mean+nu | `limit:31` | Checked in the corresponding theorem below. |
| sstd | mean+nu+xi | `limit:51` | Checked in the corresponding theorem below. |
| sstd | mean+nu+xi | `limit:52` | Checked in the corresponding theorem below. |
| sstd | mean+sd | `limit:26` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | mean+sd+xi | `limit:48` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | mean+xi | `limit:35` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | nu | `limit:18` | Checked in the corresponding theorem below. |
| sstd | nu+xi | `limit:44` | Checked in the corresponding theorem below. |
| sstd | sd | `limit:13` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | sd+xi | `limit:41` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |
| sstd | xi | `limit:22` | The fixed-shape exponent is checked. The threshold at free shape requires the coupled lower-shape limit and the scale/variance relation when sd is held. |

## Effort and checked statements

### snorm

About 28 minutes through successive file checks. The fixed holds used strict concavity and compactness, followed by the cubic-root split, half-face traces, exact sample comparisons and the contact expansion.

#### `heldMeanSkew_eq`

Inverse width recovers the actual two-piece likelihood at the held mean. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L63)

```lean
theorem heldMeanSkew_eq (x : Fin n → ℝ) (μ : ℝ) {w u : ℝ} (hu : 0 < u) :
    heldMeanSkew x μ w u =
      logLik x (μ - halfMean * ((1 - w) / u - w / u)) (w / u) ((1 - w) / u)
```

#### `heldMeanSkew_collapse`

With all data at the held mean, the Gaussian exponent is constant throughout the positive inverse-width ray and log likelihood is unbounded. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L94)

```lean
theorem heldMeanSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (heldMeanSkew x μ w) atTop atTop
```

#### `heldMeanSkew_existsUnique`

A nonzero residual gives exactly one finite maximizing scale when mean and skew are held. The scale is represented by inverse total width. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L204)

```lean
theorem heldMeanSkew_existsUnique (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    ∃! u, u ∈ Ioi (0 : ℝ) ∧ IsMaxOn (heldMeanSkew x μ w) (Ioi 0) u
```

#### `heldMeanSkew_exists_iff`

The held-mean and held-skew likelihood attains its maximum exactly when the sample contains a nonzero residual from the mean. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L268)

```lean
theorem heldMeanSkew_exists_iff (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (heldMeanSkew x μ w) (Ioi 0) u) ↔
      ∃ i, x i ≠ μ
```

#### `heldScaleSkew`

For positive fixed scale and skew, the log likelihood tends to minus infinity at both location ends and admits exactly one maximizing mean. The shape of the sample is unrestricted. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L324)

```lean
theorem heldScaleSkew (hn : 0 < n) (x : Fin n → ℝ) {w u : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hu : 0 < u) :
    Tendsto (fun μ ↦ heldMeanSkew x μ w u) (cocompact ℝ) atBot ∧
      ∃! μ, IsMaxOn (fun m ↦ heldMeanSkew x m w u) univ μ
```

#### `heldSkew_collapse`

If the sample is constant, the fixed-skew model has a collapsing path at its common observation, expressed in inverse total width. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L429)

```lean
theorem heldSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {c : ℝ} (w : ℝ)
    (hx : ∀ i, x i = c) :
    Tendsto (fun u ↦ logLik x c (w / u) ((1 - w) / u)) atTop atTop
```

#### `heldSkew_existsUnique_iff`

With skew held, nonconstant data give exactly one joint maximizer and constant data have no finite maximizer. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L444)

```lean
theorem heldSkew_existsUnique_iff (hn : 0 < n) (x : Fin n → ℝ) {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    ((∃ p ∈ LocationScale.domain, IsMaxOn (heldSkew x w) LocationScale.domain p) ↔
      LocationScale.Nonconstant x) ∧
      (LocationScale.Nonconstant x →
        ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (heldSkew x w) LocationScale.domain p)
```

#### `allFree_graph`

The all-free finite maximizing graph over mode, left width fraction and total width. This describes every tie for a nonconstant sample. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L776)

Nonconstant branch. Reconstructed side scales are w*s and (1-w)*s.

```lean
theorem allFree_graph (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {c w s : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    (∀ (d v t : ℝ), v ∈ Ioo (0 : ℝ) 1 → 0 < t →
      heldSkew x v (d, t) ≤ heldSkew x w (c, s)) ↔
      IsMinOn (modeProfile x) univ c ∧
        w = modeFraction x c ∧ s = sqrt (modeProfile x c / n)
```

#### `allFree_exists_iff`

A bracketed mode is a mode strictly between the sample extremes. The all-free fit exists exactly when there is a bracketed minimizing mode. This condition also excludes every constant sample. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L893)

```lean
theorem allFree_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ c w s : ℝ, w ∈ Ioo 0 1 ∧ 0 < s ∧
      ∀ (d v t : ℝ), v ∈ Ioo (0 : ℝ) 1 → 0 < t →
        heldSkew x v (d, t) ≤ heldSkew x w (c, s)) ↔
    ∃ c : ℝ, (∃ i, x i < c) ∧ (∃ j, c < x j) ∧ IsMinOn (modeProfile x) univ c
```

#### `allFree_halfFace`

A half-normal path at any supporting endpoint has its unpenalized trace. Its value is the all-free supremum exactly when that endpoint minimizes the mode profile. The endpoint RMS fixes its positive scale. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L987)

Joint log-likelihood trace and exact face comparison. Distributional convergence is a separate claim.

```lean
theorem allFree_halfFace (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) (c : ℝ) (left : Bool)
    (hsupp : if left then ∀ j, x j ≤ c else ∀ j, c ≤ x j) :
    let b := sqrt (endpointSquares x c / n)
    Tendsto (fun w ↦ heldSkew x w (c,b)) (𝓝[Ioo 0 1] (if left then 1 else 0))
      (𝓝 (halfLogLik x c b)) ∧
    (IsLUB (allFreeValues x) (halfLogLik x c b) ↔ IsMinOn (modeProfile x) univ c)
```

#### `allFree_endpoint_only`

Without an interior minimizing mode, the all-free supremum is a half-normal trace at a sample extreme. Both orientations are allowed. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L1023)

```lean
theorem allFree_endpoint_only (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x)
    (hmiss : ¬ ∃ c : ℝ, (∃ i, x i < c) ∧ (∃ j, c < x j) ∧ IsMinOn (modeProfile x) univ c) :
    ∃ (i : Fin n) (left : Bool),
      (if left then ∀ j, x j ≤ x i else ∀ j, x i ≤ x j) ∧
      IsLUB (allFreeValues x)
        (halfLogLik x (x i) (sqrt (endpointSquares x (x i) / n)))
```

#### `allFree_reflected_tie`

The exact symmetric sample has two distinct finite all-free maximizers at reflected modes. The maximizing side scales are supplied by `allFree_graph`. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L1069)

The rational mode 1/2 beats the center and both ends. The exact sample is the one in the written proof.

```lean
theorem allFree_reflected_tie :
    ∃ c : ℝ, c ≠ 0 ∧
      modeFraction reflectedSample c ∈ Ioo 0 1 ∧
      modeFraction reflectedSample (-c) = 1 - modeFraction reflectedSample c ∧
      IsMinOn (modeProfile reflectedSample) univ c ∧
      IsMinOn (modeProfile reflectedSample) univ (-c)
```

#### `heldSd_constant`

With sd held, constant data give two equal half-normal traces and every finite fit has strictly lower likelihood. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L1175)

Constant-data branch with sd held.

```lean
theorem heldSd_constant (hn : 0 < n) {x : Fin n → ℝ} {c σ : ℝ}
    (hσ : 0 < σ) (hx : ∀ i, x i = c) :
    let L := (n : ℝ) * log (halfMean * sqrt (1 - halfMean^2) / σ)
    (∀ d w : ℝ, w ∈ Ioo 0 1 →
      heldSkew x w (d, σ / sqrt (varianceFraction w)) < L) ∧
    ∀ left : Bool,
      Tendsto (fun w ↦ heldSkew x w (c, σ / sqrt (varianceFraction w)))
        (𝓝[Ioo 0 1] (if left then 1 else 0)) (𝓝 L)
```

#### `heldMeanSd_rightContact`

With both moments held, a datum at the limiting right-half endpoint retains the exact Gaussian contact penalty as its standardized residual approaches the endpoint from the narrow side. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceNormal.lean#L1229)

One right contact at both fixed moments. Remaining face branches require further results.

```lean
theorem heldMeanSd_rightContact (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let H := 1 - halfMean^2
    let z := μ - halfMean * (σ / sqrt H)
    Tendsto (fun w ↦
      let R := residual μ w (sqrt (varianceFraction w) / σ) z
      R^2 / (2 * (if R < 0 then w else 1 - w)^2))
      (𝓝[Ioo 0 1] 0) (𝓝 (halfMean^2 / (8 * H^2)))
```

### ged

About 15 minutes through successive file checks. The knot gap argument used strict power concavity, unrestricted collapse used integer Stirling bounds, and the compact profile used the log-Gamma tangent and the explicit widening path.

#### `minimizingKnots`

At a positive shape below one, the location minimizers are exactly the knots whose loss is smallest among the knots. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L126)

```lean
theorem minimizingKnots {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    (x : Fin n → ℝ) (m : ℝ) :
    IsMinOn (Ged.residualSum ν x) univ m ↔
      ∃ i, m = x i ∧ ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)
```

#### `heldSd_belowOne`

At positive fixed sd and shape below one, the knots of least power loss are precisely all the location maximizers. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L146)

Subunit branch. The previously checked GED module handles shape one and superunit shape.

```lean
theorem heldSd_belowOne {ν σ : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hσ : 0 < σ)
    (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    IsMaxOn (fun t ↦ Ged.logLik ν x (t, σ)) univ m ↔
      ∃ i, m = x i ∧ ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)
```

#### `heldShape_belowOne`

With shape below one and free scale, every maximizing knot shares the same profiled scale. The sample is nonconstant so that scale is positive. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L154)

Subunit branch. The previously checked GED module handles shape one and superunit shape.

```lean
theorem heldShape_belowOne {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    Ged.jointMaximizers ν x =
      {p | (∃ i, p.1 = x i ∧
        ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)) ∧
        p.2 = Ged.bestScale ν x p.1}
```

#### `heldSd_attains`

With positive sd and shape held, a maximizing location exists for every finite nonempty sample. This includes constant data and all subunit shapes. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L166)

```lean
theorem heldSd_attains {ν σ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) (hn : 0 < n)
    (x : Fin n → ℝ) : ∃ m, IsMaxOn (fun t ↦ Ged.logLik ν x (t, σ)) univ m
```

#### `heldSd_tail`

At positive fixed sd and shape the log likelihood vanishes to minus infinity at either location end. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L192)

```lean
theorem heldSd_tail {ν σ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) (hn : 0 < n)
    (x : Fin n → ℝ) :
    Tendsto (fun μ ↦ Ged.logLik ν x (μ, σ)) (cocompact ℝ) atBot
```

#### `rational_noContact`

A rational radius and a positive rational sd cannot meet the exact real uniform contact condition. Binary64 inputs are a special case. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L230)

Arithmetic branch of the held-sd failure claim. Any finite binary64 radius or sd is rational in exact arithmetic.

```lean
theorem rational_noContact (r σ : ℚ) (hσ : σ ≠ 0) :
    (r : ℝ) ≠ sqrt 3 * (σ : ℝ)
```

#### `kernelLogLik_eq`

The positive kernel-scale parametrization is exactly the GED model. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L246)

```lean
theorem kernelLogLik_eq {ν b : ℝ} (hν : 0 < ν) (hb : 0 < b)
    (x : Fin n → ℝ) (m : ℝ) :
    kernelLogLik ν x m b = Ged.logLik ν x (m, b * Ged.sdFactor ν)
```

#### `uniform_path_freeSd`

A vanishing relative widening approaches the unpenalized uniform value, even with observations exactly at either endpoint of the limiting interval. The fitted sd tends to the sd of that interval. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L302)

The sample likelihood path widens the kernel range by a vanishing amount. The argument uses free sd.

```lean
theorem uniform_path_freeSd {x : Fin n → ℝ} {μ ρ : ℝ}
    (hρ : 0 < ρ) (h : ∀ i, |x i-μ| ≤ ρ) :
    let scale := fun p : ℝ ↦ (ρ * p^(1/p)) * Ged.sdFactor p
    Tendsto scale atTop (𝓝 (ρ / sqrt 3)) ∧
      Tendsto (fun p ↦ Ged.logLik p x (μ, scale p)) atTop (𝓝 (-(n : ℝ) * log (2*ρ)))
```

#### `unrestricted_unbounded`

If shape and both location and sd are free, allowing positive shapes below one makes the log likelihood unbounded on every nonempty sample. The construction uses reciprocal integer shapes at a sample atom. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L348)

```lean
theorem unrestricted_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ BddAbove {L : ℝ | ∃ ν μ σ : ℝ, 0 < ν ∧ 0 < σ ∧ L = Ged.logLik ν x (μ, σ)}
```

#### `uniform_tail_excluded`

Beyond the compact cutoff, a positive unavoidable radius forces every finite likelihood below the unpenalized uniform. This bound is uniform over the positive kernel scale and the location. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L461)

```lean
theorem uniform_tail_excluded (hn : 0 < n) {x : Fin n → ℝ} {μ ρ p b : ℝ}
    (hρ : 0 < ρ) (hb : 0 < b) (hp : uniformCutoff n < p)
    (hx : ∃ i, ρ ≤ |x i-μ|) :
    kernelLogLik p x μ b < -(n : ℝ) * log (2*ρ)
```

#### `freeSd_compactCriterion`

The compact finite-shape profile decides attainment by comparison with the range uniform. The hypotheses express that the positive radius is unavoidable for the location set and encloses the data at some location. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L600)

```lean
theorem freeSd_compactCriterion (hn : 0 < n) {x : Fin n → ℝ} {S : Set ℝ}
    (hS : IsCompact S) {ρ : ℝ} (hρ : 0 < ρ)
    (hlower : ∀ μ ∈ S, ∃ i, ρ ≤ |x i-μ|)
    (hupper : ∃ μ ∈ S, ∀ i, |x i-μ| ≤ ρ) :
    let K := Icc 1 (uniformCutoff n) ×ˢ S
    let F := -(n : ℝ)*log (2*ρ)
    ∃ y ∈ K, IsMaxOn (freeSdProfile x) K y ∧
      ((∃ L, IsGreatest (freeSdValues x S) L) ↔ F ≤ freeSdProfile x y) ∧
      (freeSdProfile x y < F → IsLUB (freeSdValues x S) F)
```

#### `freeSd_heldMean`

When the held location has a nonzero residual, the compact shape profile decides the comparison against the centered uniform. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L659)

Signs and attainment are decided by the compact profile. Constant data at the held mean use fitted_constant_collapse.

```lean
theorem freeSd_heldMean (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    ∃ i : Fin n, ∀ (ρ : ℝ), ρ = |x i-μ| →
      0 < ρ ∧ (∀ j, |x j-μ| ≤ ρ) ∧
      ∃ y ∈ Icc 1 (uniformCutoff n) ×ˢ ({μ} : Set ℝ),
        IsMaxOn (freeSdProfile x) (Icc 1 (uniformCutoff n) ×ˢ {μ}) y ∧
        ((∃ L, IsGreatest (freeSdValues x {μ}) L) ↔ -(n : ℝ)*log (2*ρ) ≤ freeSdProfile x y) ∧
        (freeSdProfile x y < -(n : ℝ)*log (2*ρ) →
          IsLUB (freeSdValues x {μ}) (-(n : ℝ)*log (2*ρ)))
```

#### `freeSd_allFree`

Nonconstant data with all coefficients free are decided by the profile on the sample range and the stated compact shape interval. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L711)

```lean
theorem freeSd_allFree (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    ∃ l r : Fin n, (∀ i, x l ≤ x i ∧ x i ≤ x r) ∧
      let ρ := (x r-x l)/2
      let K := Icc 1 (uniformCutoff n) ×ˢ Icc (x l) (x r)
      let F := -(n : ℝ)*log (2*ρ)
      ∃ y ∈ K, IsMaxOn (freeSdProfile x) K y ∧
        ((∃ L, IsGreatest (freeSdValues x univ) L) ↔ F ≤ freeSdProfile x y) ∧
        (freeSdProfile x y < F → IsLUB (freeSdValues x univ) F)
```

#### `fitted_constant_collapse`

Constant data at an admissible location have unbounded fitted-domain likelihood even if the shape restriction includes only shapes at least one. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceGED.lean#L771)

Applies with free mean or with mean held at the common observation.

```lean
theorem fitted_constant_collapse (hn : 0 < n) {x : Fin n → ℝ} {S : Set ℝ} {μ : ℝ}
    (hμ : μ ∈ S) (hx : ∀ i, x i = μ) :
    ¬ BddAbove (freeSdValues x S)
```

### sged

About 11 minutes through successive file checks. The subunit tie required a compact maximization of the slope ratio, followed by power-loss coercivity, strict convexity and fixed-width and fixed-skew profile transfers.

#### `heldMeanSkew_subunitTie`

At shape one half and held mean zero there is a valid held skew for which the sample at minus one has distinct global scale maximizers. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L120)

Counterexample to the atlas uniqueness statement at all positive held shapes.

```lean
theorem heldMeanSkew_subunitTie :
    ∃ w ∈ Ioo (0 : ℝ) 1, ∃ u₁ u₂ : ℝ, 0 < u₁ ∧ u₁ < u₂ ∧
      IsMaxOn (heldMeanSkew (fun _ : Fin 1 ↦ -1) 0 (1/2) w) (Ioi 0) u₁ ∧
      IsMaxOn (heldMeanSkew (fun _ : Fin 1 ↦ -1) 0 (1/2) w) (Ioi 0) u₂
```

#### `heldMeanSkew_collapse`

With all observations at the held mean the held-shape, held-skew likelihood diverges as width vanishes. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L215)

```lean
theorem heldMeanSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hx : ∀ i, x i = μ) : Tendsto (heldMeanSkew x μ p w) atTop atTop
```

#### `heldMeanSkew_lossAtEnds`

A nonzero residual excludes both width ends under held mean, shape and skew, at every positive held shape. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L254)

```lean
theorem heldMeanSkew_lossAtEnds (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    Tendsto (heldMeanSkew x μ p w) (𝓝[>] 0) atBot ∧
      Tendsto (heldMeanSkew x μ p w) atTop atBot
```

#### `heldMeanSkew_attains_iff`

At every fixed positive shape and positive finite skew, the held-mean fit attains exactly when some residual from the held mean is nonzero. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L310)

```lean
theorem heldMeanSkew_attains_iff (hn : 0 < n) {x : Fin n → ℝ} {μ p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (heldMeanSkew x μ p w) (Ioi 0) u) ↔
      ∃ i, x i ≠ μ
```

#### `heldMeanSkew_unique`

At shapes at least one the held-mean and held-skew maximum, when it exists, is unique. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L420)

Shapes at least one. Subunit asymmetric holds can give ties.

```lean
theorem heldMeanSkew_unique {x : Fin n → ℝ} (hn : 0 < n) {μ p w : ℝ}
    (hp : 1 ≤ p) (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    ∃! u, u ∈ Ioi (0 : ℝ) ∧ IsMaxOn (heldMeanSkew x μ p w) (Ioi 0) u
```

#### `heldWidth`

At fixed positive shape, skew and sd, likelihood tends to zero at both location ends, a maximizer always exists, and above shape one it is unique. Fixing sd fixes total width in these coordinates. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L465)

Fixed positive skew and shape. Holding sd fixes total kernel width and the mode is a translate of mean.

```lean
theorem heldWidth (hn : 0 < n) (x : Fin n → ℝ) {p w s : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    Tendsto (fun c ↦ fixedSkewLogLik x p w (c,s)) (cocompact ℝ) atBot ∧
      (∃ c, IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c) ∧
      (1 < p → ∃! c, IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c)
```

#### `heldSkew_collapse`

Constant samples collapse at the fixed skew, with the mode set at the common observation and total kernel width tending to zero. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L608)

```lean
theorem heldSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {p w c : ℝ}
    (hp : 0 < p) (hx : ∀ i, x i = c) :
    Tendsto (fun u ↦ fixedSkewLogLik x p w (c,1/u)) atTop atTop
```

#### `heldSkew_criterion`

Fixed positive shape and skew leave a finite joint mode and scale fit exactly on nonconstant samples. At shapes above one it is unique. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L621)

```lean
theorem heldSkew_criterion (hn : 0 < n) (x : Fin n → ℝ) {p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    ((∃ cs ∈ LocationScale.domain, IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain cs) ↔
      LocationScale.Nonconstant x) ∧
    (LocationScale.Nonconstant x → 1 < p →
      ∃! cs, cs ∈ LocationScale.domain ∧
        IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain cs)
```

#### `heldSkew_lowShapeTie`

At every shape between zero and one inclusive, this sample with unit skew gives two maximizing locations at a common positive width. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceSGED.lean#L670)

```lean
theorem heldSkew_lowShapeTie {p : ℝ} (hp : 0 < p) (hp' : p ≤ 1) :
    ∃ s > 0, IsMaxOn (fixedSkewLogLik symmetricPair p (1/2)) LocationScale.domain (-1,s) ∧
      IsMaxOn (fixedSkewLogLik symmetricPair p (1/2)) LocationScale.domain (1,s)
```

### sstd

About 15 minutes of proof work through successive file checks, plus shared-slot waits. The collapse expansion was followed by pasting derivatives at observation joins, compact comparison and polynomial clearing of stationary denominators.

#### `collapse_expansion`

The log likelihood after removal of the width exponent tends to the sharp constant along the specified fixed-shape, fixed-skew path. This includes no observations at the atom, any side count zero, and all ties. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L117)

Take e=halfMoment ν*(1-2*w) for held mean or e=0 for an exact join.

```lean
theorem collapse_expansion (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    Tendsto (fun u ↦ logAlongAtom x y ν w e u + TDist.exponent x y ν * log u)
      atTop (𝓝 (collapseLogHeight x y ν w e))
```

#### `collapse_cases`

The sign of the collapse exponent classifies the fixed-width-fraction path at either specified offset. At equality the product likelihood has the stated strictly positive limit. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L160)

Sharp fixed-offset path values. Other moving paths require separate bounds.

```lean
theorem collapse_cases (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    let E := TDist.exponent x y ν
    let f := logAlongAtom x y ν w e
    (E < 0 → Tendsto f atTop atTop) ∧
    (0 < E → Tendsto f atTop atBot) ∧
    (E = 0 → Tendsto f atTop (𝓝 (collapseLogHeight x y ν w e)) ∧
      Tendsto (fun u ↦ exp (f u)) atTop (𝓝 (exp (collapseLogHeight x y ν w e))))
```

#### `infiniteWidth_loses`

Infinite total width sends the log likelihood to minus infinity for every nonempty sample under fixed shape and skew. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L187)

```lean
theorem infiniteWidth_loses (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) : Tendsto (logAlongAtom x y ν w e) (𝓝[>] 0) atBot
```

#### `hasDerivAt_scale`

The score remains valid at observations crossing the moving mode. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L250)

```lean
theorem hasDerivAt_scale (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (logAlongAtom x y ν w e) (scaleScore x y ν w e u) u
```

#### `fixedOffset_criterion`

The held-mean, held-shape, held-skew fit exists exactly when a finite score zero reaches the collapse envelope. For a general fixed offset this is the same criterion on the resulting inverse-width ray. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L343)

The held-mean result specializes the offset to halfMoment ν*(1-2*w).

```lean
theorem fixedOffset_criterion (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u) ↔
      ∃ u > 0, scaleScore x y ν w e u = 0 ∧ meetsCollapse x y ν w e u
```

#### `fixedOffset_maximizers`

All maximizing scales are the stationary points that meet the collapse envelope and tie for the greatest stationary value. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L355)

Use with scaleScore_roots_finite for the finite candidate set.

```lean
theorem fixedOffset_maximizers (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) {u : ℝ} (hu : 0 < u) :
    IsMaxOn (logAlongAtom x y ν w e) (Ioi 0) u ↔
      scaleScore x y ν w e u = 0 ∧ meetsCollapse x y ν w e u ∧
      ∀ v > 0, scaleScore x y ν w e v = 0 →
        logAlongAtom x y ν w e v ≤ logAlongAtom x y ν w e u
```

#### `scaleScore_roots_finite`

The inverse-width score has only finitely many positive roots. Clearing denominators on every sign assignment gives a polynomial with nonzero constant term, including assignments with zero side counts. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L371)

```lean
theorem scaleScore_roots_finite (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    {u : ℝ | 0 < u ∧ scaleScore x y ν w e u = 0}.Finite
```

#### `exactJoin_critical`

At critical multiplicity on the exact-join path, the likelihood strictly increases as width shrinks and its finite supremum is unattained. Unit skew makes this the held-mean path as well. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L437)

At unit skew and fixed mean the offset is zero. At arbitrary skew this theorem uses an exact join.

```lean
theorem exactJoin_critical (hn : 0 < n) (x : Fin n → ℝ) (y ν w : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) (hE : TDist.exponent x y ν = 0) :
    let f := logAlongAtom x y ν w 0
    let C := collapseLogHeight x y ν w 0
    StrictMonoOn f (Ioi 0) ∧
      IsLUB (f '' Ioi 0) C ∧ ∀ u > 0, f u < C
```

#### `movingCollapse_upper`

A moving join and even a vanishing side cannot increase the power of fixed-shape collapse. The width fractions may vary arbitrarily while positive, and the concentration point may be any finite real number. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L509)

Upper bound for fixed shape, allowing moving join and simultaneous skew degeneration. Uses fixed-shape, fixed-skew paths for sharp strict divergence.

```lean
theorem movingCollapse_upper {ι : Type*} {l : Filter ι} (x : Fin n → ℝ) (y ν : ℝ)
    (hν : 2 < ν) {u w e : ι → ℝ}
    (hu : Tendsto u l atTop) (hw : ∀ᶠ t in l, w t ∈ Ioo (0 : ℝ) 1)
    (he : Tendsto (fun t ↦ e t/u t) l (𝓝 (0 : ℝ))) :
    ∃ C : ℝ, ∀ᶠ t in l, logAlongAtom x y ν (w t) (e t) (u t) ≤
      -TDist.exponent x y ν * log (u t) + C
```

#### `fixedOffset_unbounded_iff`

The held-mean, held-shape, held-skew likelihood is unbounded precisely at strict collapse multiplicity. The conclusion also holds on the more general fixed-offset ray. [Lean source](../../../dev/lean/UnivariateML/Family/TwoPieceStudent.lean#L573)

Specialize to the held-mean offset for the strict collapse detector.

```lean
theorem fixedOffset_unbounded_iff (hn : 0 < n) (x : Fin n → ℝ) (y ν w e : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) :
    ¬ BddAbove (logAlongAtom x y ν w e '' Ioi 0) ↔ TDist.exponent x y ν < 0
```

## Verification

All four standalone `lake env lean` checks passed. The final `lake build` completed successfully with 7,986 jobs and exit code zero. Lake emitted style warnings, including whitespace and long-line warnings in the new modules.

The workstation runs used the shared one-slot lock. The Mathlib preflight checked cached build targets before the aggregate build. The temporary aggregate imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed. The new modules contain no proof placeholders.
