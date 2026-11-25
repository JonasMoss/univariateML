# TASK-321 · Exact fit outcomes in Lean

[Outcome.lean](../../lean/UnivariateML/Outcome.lean) formalizes layer A for a real log-likelihood `L : Θ → ℝ` and a feasible set `D`. Three family modules supply exact instances. All five targets are closed.

## Claims and source fidelity

An estimate carries a feasible global maximizer. A singleton claim identifies the full maximizer set. A multiplicity claim supplies the full set and proves that it contains two distinct points. Unknown cardinality carries the global-maximizer claim without a cardinality assertion. Requiring feasibility explicitly matters because Mathlib's `IsMaxOn L D θ` alone does not require `θ ∈ D`.

Finite nonattainment uses a real least upper bound and strict inequality at every feasible point. This formulation avoids the default value of real `sSup` on an empty or unbounded set. The limit claim additionally requires eventual feasibility and convergence along a nontrivial filter. The label identifies the model in the payload. Its interpretation as a statistical model comes from the family theorem. A named limit and finite nonattainment can both describe the same likelihood. Whether the public interface has a recognized model to name depends on its model catalogue. Unboundedness means the likelihood image has no real upper bound.

The transfer takes a parameter equivalence from the new parameter space to the old one. It only requires the likelihood identity on the feasible preimage. This admits exact transformations whose formulas agree only on the model domains. It pulls back the representative, full set and path, retains the path label and filter, and adds the constant to finite supremum values.

The family results concern exact real arithmetic on the all-free, uncensored routes. Weibull inherits its positive, nonconstant sample hypotheses. Laplace uses strict ordering of the two central values, which is possible only at even sample size. This also implies positive minimum deviation, so the outcome theorem discharges that hypothesis of `Laplace.joint`. The returned representative uses the upper median as in the implementation. Lomax inherits the no-witness hypothesis for the exact exponential escape and supplies the explicit fixed-product path. No errors were found in the source mathematical statements used for these instances.

## C++ and R mapping

| C++ and R evidence | Exact Lean claim |
| --- | --- |
| `Estimate`, certified, `uniqueness_known = true`, `unique = true` (R `TRUE`) | `.estimate θ .singleton` requires a singleton proof for the fitted route. |
| `Estimate`, certified, `uniqueness_known = true`, `unique = false` (R `FALSE`) | `.estimate θ (.multiple S)` requires the entire set `S` and two distinct members. |
| `Estimate`, certified, `uniqueness_known = false` (R `NA`) | `.estimate θ .unknown` retains global optimality. |
| `Limit`, certified, with the model and value in the payload | `.limit p v` with a family path along which the log-likelihood tends to the finite unattained supremum `v`. |
| `Nonexistence`, `Unattained` or `Unbounded` (R `nomle`) | `.unattained v` or `.unbounded`, respectively. `v` is a mathematical witness and need not be returned by the stop. |
| `Estimate` or `Limit` with certification cleared | An estimate may still pass the numerical accuracy gate. A margin-decided limit may still bound the improvement at the tested margin. Neither field combination supplies the corresponding exact global claim. |
| `Budget` or `Failure` | Search and request diagnostics. A cap, unresolved comparison, invalid or unsupported request, convergence failure, evaluation failure, tolerance failure or conditioning failure carries no general statement about attainment. |

The typed `MaximizerSet` encodes a union of points, boxes and named relations. Lean uses the exact set those pieces are meant to describe. Rounded endpoints, enclosed points, normalization, dimensional and capacity limits, and fit tolerance concern the representation. R's `maximizers` rows illustrate the set and start with the representative. `candidates` supplies comparison evidence. Global likelihood bounds and segment bounds in `StopDetail` have different scopes, and neither is an outcome proof in isolation. The R inference gate, availability of a public model name, and `attainable_tol` lie outside the exact proposition. An invalid sample whose likelihood vanishes throughout would require an extended-real log-likelihood to represent directly.

**Implementation discrepancy.** The current header has both `unique` and `uniqueness_known`, while the historical type sketch in the consolidation plan lists only `unique`. The fields and R renderer already support certified optimality with `unique = NA`. The zero-argument `FitOutcome::undetermined()` still clears `certified`. For example, the hypergeometric mapping selects it for an `Ok` result with unknown cardinality, discarding the incoming certification bit if set. Beta-negative-binomial constructs the flags independently on its `Ok` route. The Lean weakening theorem follows the 2026-11-25 decision independently of that helper. See [the header](../../../univariateML-cpp/include/cppunivariate/core/outcome.hpp), [hypergeometric mapping](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/mle.hpp), [beta-negative-binomial mapping](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp), [R rendering](../../../bindings/r/scripts/common_fit.h), and [the decision](../../roadmap/decisions.md#2026-11-25).

**Lomax scope.** The dossier documents a nonconstant-sample margin of `10⁻⁸ n` and explicitly allows the margin stop when a finite point ties or slightly beats the limit. The exact instance requires strict nonattainment. The constant-sample analytic route sets the certified limit bit when it can export the limit rate. The two routes therefore have different evidentiary requirements. See [the dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) and [certificate](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/certificate.hpp).

## Lean theorem statements

The consistency and transfer statements use the surrounding implicit variables `{Θ Φ : Type*} {L : Θ → ℝ} {D : Set Θ}`. The family statements use `{n : ℕ}`.

A sound estimate can forget any cardinality information while retaining the same exact maximizer.

```lean
theorem estimate_unknown {θ : Θ} {u : Uniqueness Θ}
    (h : Sound L D (.estimate θ u)) : Sound L D (.estimate θ .unknown)
```

The unique estimate determines the full maximizer set.

```lean
theorem maximizers_eq_singleton {θ : Θ} (h : Sound L D (.estimate θ .singleton)) :
    maximizers L D = {θ}
```

A sound named limit gives the real supremum and rules out every feasible maximizer.

```lean
theorem limit_unattained {p : LimitPath Θ} {v : ℝ} (h : Sound L D (.limit p v)) :
    sSup (L '' D) = v ∧ ¬ (maximizers L D).Nonempty
```

A sound estimate rules out a named limit, a finite unattained supremum and unboundedness.

```lean
theorem estimate_incompatible {θ : Θ} {u : Uniqueness Θ} (h : Sound L D (.estimate θ u)) :
    (∀ p v, ¬ Sound L D (.limit p v)) ∧
    (∀ v, ¬ Sound L D (.unattained v)) ∧ ¬ Sound L D .unbounded
```

A finite unattained supremum provides an upper bound and rules out unboundedness.

```lean
theorem finite_incompatible_unbounded {v : ℝ} (h : Sound L D (.unattained v)) :
    ¬ Sound L D .unbounded
```

The same bound exclusion applies to a named limit.

```lean
theorem limit_incompatible_unbounded {p : LimitPath Θ} {v : ℝ}
    (h : Sound L D (.limit p v)) : ¬ Sound L D .unbounded
```

There can be no sound multiplicity claim when the maximizer set is a singleton.

```lean
theorem singleton_incompatible_multiple {θ η : Θ} {S : Set Θ}
    (h : Sound L D (.estimate θ .singleton)) : ¬ Sound L D (.estimate η (.multiple S))
```

Pulling back the parameters and shifting the likelihood preserves every outcome claim.

```lean
theorem sound_transfer {L₂ : Φ → ℝ} (φ : Φ ≃ Θ) (c : ℝ)
    (he : ∀ q ∈ φ ⁻¹' D, L₂ q = L (φ q) + c)
    {o : Outcome Θ} (h : Sound L D o) :
    Sound L₂ (φ ⁻¹' D) (transfer φ c o)
```

Nonempty, positive and nonconstant data give a unique joint Weibull estimate via `Weibull.jointMLE`.

```lean
theorem weibull_sound (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ p, Sound (fun q : ℝ × ℝ ↦ Weibull.logLik x q.1 q.2) Weibull.domain
      (.estimate p .singleton)
```

The two unequal central values are the endpoints of the whole maximizing location interval, each with the same positive scale. `Laplace.joint` identifies the full set.

```lean
theorem laplace_sound_multiple (hn : 0 < n) {x : Fin n → ℝ}
    (h : Laplace.lowerMedian x hn < Laplace.upperMedian x hn) :
    let d := Laplace.meanAbsDev x (Laplace.lowerMedian x hn)
    Sound (fun q : ℝ × ℝ ↦ Laplace.logLik x q.1 q.2) Laplace.domain
      (.estimate (Laplace.upperMedian x hn, d) (.multiple (Laplace.medianSet x hn ×ˢ {d})))
```

A zero minimum deviation gives arbitrarily large joint log-likelihood by `Laplace.joint_unbounded`.

```lean
theorem laplace_sound_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hd : Laplace.meanAbsDev x (Laplace.lowerMedian x hn) = 0) :
    Sound (fun q : ℝ × ℝ ↦ Laplace.logLik x q.1 q.2) Laplace.domain .unbounded
```

Without a sign witness, every feasible likelihood is strictly below the best exponential value by `Lomax.exponential_sup_of_no_witness`. The shape tends to infinity with shape times rate fixed at the reciprocal sample mean, and `Lomax.logLik_tendsto` gives convergence. `Generic.limit_le_upperBound` makes that limit a least upper bound.

```lean
theorem lomax_sound_limit (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar x), Lomax.psi x u ≤ 0) :
    Sound (fun q : ℝ × ℝ ↦ Lomax.logLik x q.1 q.2) Lomax.domain
      (.limit ⟨"exp", ℝ, atTop, fun κ ↦ (κ, (Lomax.avg x)⁻¹ / κ)⟩
        (Lomax.bestExp x))
```

## Verification and effort

Checks ran sequentially in the workstation worktree through the single-slot wrapper with `nice -n 10` and one Lean thread.

| Command from `dev/lean` | Result |
| --- | --- |
| `lake env lean UnivariateML/Outcome.lean` | Exit 0 with no diagnostics. |
| `lake build +UnivariateML.Outcome UnivariateML` | Exit 0, 7,364 jobs. Explicitly built `UnivariateML.Outcome` in 2.2 seconds. |
| `lake build` | Exit 0, 7,362 jobs. Replayed existing family diagnostics in Weibull, GPD, FoldedNormal, Frechet and LomaxTwoPoint. |

The root module import belongs to the coordinator. The explicit module target checks the new module before that import lands. All proofs are closed without placeholders or added axioms. Mathlib was reused from the existing cache.

| Target | Effort |
| --- | --- |
| 1. Semantics | Defined four outcome constructors, three cardinality claims, and the common finite nonattainment predicate. |
| 2. Consistency | Proved seven public consequences using feasibility, upper bounds and the two distinct points in a multiplicity claim. |
| 3. Transfer | Largest proof and most of the elaboration work. Checked each outcome branch via order translation, set preimages and eventual likelihood equality. |
| 4. Families | Built four instances from three imported families. Proved positivity and distinctness for Laplace directly from the ordered central values and established eventual feasibility for the Lomax path. |
| 5. Mapping | Compared the design and decision against the C++ constructors, inspected the core and R payloads, and traced certified unknown cardinality through representative route mappings. |

Layer D remains future work. A per-input theorem would need to connect computed bounds and coefficient enclosures to the exact claims and to the numerical accuracy contract. The public choice between named and unnamed nonattainment additionally depends on a family model catalogue.
