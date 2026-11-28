# Almost-everywhere uniqueness on fixed-count strata

TASK-504 · 2026-11-28 · baseline `b1f6ccc6`

Lean proves that finite-score identification at a fixed number of distinct levels gives a Lebesgue-null tie set in the free level coordinates. A second theorem derives the threshold from parameter dimension when identification is by analytic score germs. The pilot applies the distinct-level theorem to Lomax, inverse Pareto and Pareto II with the minimum held. It joins the null result to the exact attainment test for each physical likelihood.

## Atlas inventory

The atlas contains **115 written claims** matching “almost every” case-insensitively. A case-sensitive search gives 102 because 13 rows capitalize the phrase. The three pilot rows already have status `lean` through the Base7 declarations and therefore sit outside this inventory. The new pilot joins the exact existence criterion to the a.e. claim directly in the physical likelihood. [Atlas](../existence-atlas/atlas.csv) · [Regime-level inventory](2026-11-28-lean-ae5-data/claims.csv)

| Proof route | Rows | Inputs still needed |
| --- | ---: | --- |
| Direct finite-level score identification | 28 | Beta prime needs the compact scalar criterion and end comparisons. Burr, inverse Burr, Pareto IV, Feller–Pareto, GB2 and fixed-endpoint generalized beta need the physical-chart and exact face or ridge decisions appropriate to each hold. Generalized gamma needs the signed and normal stationary comparisons, including low-count attainment where claimed. Transformed gamma and its inverse need the small-stratum and power-face arguments when all free, and the centered profile decrease at held scale. Held fatigue, EMG and Bhattacharjee need the joins to their attainment or exceptional-stratum results. |
| Analytic incidence already applied | 18 | Match the strict charts for exponentiated Weibull, Fréchet, fatigue, free-endpoint generalized beta, PGW, EMG and skew normal to the full attainment and escape criteria. For moving-minimum Feller–Pareto and Pareto IV, compare the strict chart against the included unit-exponent contact. PGW still needs the finite stationary inventory and, at held shape, comparison to the optimized Gompertz height with equality retained. |
| Rational Student comparisons | 13 | Weight the derivatives in the physical levels, use the three-level quadratic at fixed df and the four-level cubic for moving df and center, or use the two unequal nonzero radii at a fixed center. Add the normal and collapse comparisons where applicable and join the pointwise location polynomial, low-count optimizer sets or critical atom decisions. For half-Student at held scale, profile the threshold at the minimum and apply the one-magnitude decision separately. |
| Analytic germ identification available | 11 | Skew-t needs joint analyticity in the free coefficients and observation, including df when free, and the fixed-contact offsets in the held-location charts. Slash needs the joint Gaussian-integral score chart. Both need weighted incidence and the relevant collapse, normal or half-Student comparisons in the same height problem. |
| Piecewise score charts | 32 | GED and SGED need the missing unit/subunit and superunit comparisons, including the median set and moving cusp where applicable. Skew Student needs the stationary-component and compatible-end decisions and the equally replicated three-level and two-level branches in the all-free case. Huber needs the strict core/tail configurations for all-free and held-location winners and the full per-cutoff location interval at held scale. |
| Moving or included branch | 9 | GEV needs a common strict/reverse-exponential winner comparison and the indexed-maximum incidence with slack at held location. Tukey lambda needs joint inverse regularity, moving support charts and uniform/nonuniform height comparison. Shifted Gompertz needs the exponential seam, the positive-level score comparison and the stationary heights including zeros. |
| Score input still missing | 4 | For Bhattacharjee, identify moving center or scale together with width, including the normal seam. For folded normal, prove the distinct-parameter score comparison across the half-normal member and the zero-plus-one-positive-level singleton. |

The inventory records the threshold, existing score and contact results, the status of the weighted chart and the remaining input for each family and hold. The direct proofs use continuity of the spatial score. Their classification does not credit joint analyticity merely because the corresponding densities have closed forms. [Earlier scope review](2026-11-27-atlas-coverage6b-report.md#what-the-statements-leave-open) · [Student and slash scope](2026-11-27-atlas-coverage6a-report.md#almost-everywhere-uniqueness)

## Shared theorem

Copied from [`Generic/AE5Weighted.lean`](../../lean/UnivariateML/Generic/AE5Weighted.lean).

```lean
theorem measure_ties_eq_zero [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P]
    {U : Set (P × ℝ)} (hU : IsOpen U) {f d : P × ℝ → ℝ} {c : P → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (k : ℕ) (hident : ∀ p q, ∀ s : Finset ℝ, k ≤ s.card →
      (∀ t ∈ s, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) = d (q, t)) → p = q)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    volume {x : Fin n → ℝ | k ≤ (Finset.univ.image x).card ∧
      ∃ p q, p ≠ q ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x p ∧
        UniqueAE.Winner (AE3WeightedChart.domain U)
          (AE3WeightedChart.likelihood f c w) x q} = 0
```

Fix the positive counts and take Lebesgue measure in the space of level coordinates. If equal observation scores at any `k` distinct common levels identify the parameter, the samples with at least `k` distinct levels and two different feasible global winners have measure zero. The conclusion restricts to an ordered chamber of levels in the same measure, including a chamber representing a repeated-observation stratum. Fixed observations can contribute to the continuous parameter term `c`.

The weighted envelope theorem derives equality of sample gradients at finite winners from Rademacher differentiation on a countable cover of compact comparison pieces. A gradient component is the spatial score times the positive count. Cancelling the counts and collecting the distinct observed values gives the pointwise identification hypothesis. The explicit bound on the number of common score zeros makes connectedness unnecessary for this route. The theorem uses analyticity to get score continuity, which would also suffice for this direct proof.

The germ version, `ae_unique_of_identifiable_germs`, instead takes an analytic kernel and connected common support for each pair. The analytic identity theorem gives finite order for every zero of the score difference of unequal parameters. The first derivative before the first nonzero one gives an incidence equation at each level. The sample Jacobian is diagonal with nonzero entries. Countably many order strata and local implicit graphs yield a null projection when the number of levels exceeds twice the parameter chart dimension. The proof uses the existing AE3 incidence construction and supplies its regularity and identification inputs from the analytic kernel and the pairwise score hypothesis. [Weighted envelope](../../lean/UnivariateML/Generic/AE3WeightedChart.lean) · [Analytic incidence](../../lean/UnivariateML/Generic/AE3AnalyticIncidence.lean)

`ae_unique_with_competitors` combines distinct-level uniqueness with finitely many analytic face or contact heights on the same chart. Given a null tie set for each height against an attained finite height, it proves that any finite winner that reaches or exceeds all competing heights strictly exceeds them almost everywhere. The finite union preserves nullity. **Open for moving contacts in general.** The caller must first express each height on a chart with its contact motion included and prove the off-null difference there. Analyticity of a face height alone cannot supply it, since a maximized finite height can itself be analytic and identical to the proposed face height. The theorem does not require throwing away a finite maximizer at an exact equality with an excluded limit.

## Pilot theorems

| Public theorem in `UnivariateML.AE5Lomax` | Atlas claim and attainment |
| --- | --- |
| `lomax` | All free, positive data. Almost everywhere on each fixed-count stratum with two or more distinct levels, the finite maximizing set is a singleton exactly when the sign test on the expanded observations succeeds. The same test characterizes existence. The theorem includes pointwise uniqueness at attained two-level samples and the exact three-level tie above the exponential face. |
| `invpareto` | All free, positive data. The sign test uses the expanded reciprocal observations and the maximizers are stated for the physical inverse Pareto log likelihood. It includes the pointwise two-level branch and the exact reciprocal three-level tie above the inverse-exponential face. |
| `pareto2` | Minimum held, strictly positive excesses. The sign test uses the expanded excesses and the parameters are physical shape and scale. It includes pointwise two-level uniqueness and the atlas’s exact exception at held minimum zero. |

The common pilot proof establishes joint analyticity of the Lomax log density with the physical level transformed by identity, reciprocal or translation. The derivative of the transform is nonzero on the respective support. Cancelling it and clearing the positive denominators makes score equality an affine equation in the transformed level. Two distinct levels identify the shape and rate. A finite sum over the expanded sample is exactly the weighted likelihood, so the existing unweighted sign criterion applies at every multiplicity. The inverse Pareto Jacobian adds a parameter-independent term and Pareto II exchanges positive rate and scale by inversion. The pilot uses the weak sign comparison `≤ 0` from the attainment theorem, including finite equality with an excluded limit. [Pilot declarations](../../lean/UnivariateML/Family/AE5Lomax.lean) · [Atlas map](../existence-atlas/lean-map.d/lean-ae5.csv)

## Effort and verification

- **Classification.** Reconciled all 115 row keys against the atlas and the two coverage reviews. The inventory carries the mapped uniqueness and attainment references and a remaining proof input for each row.
- **Shared theorem.** Added the explicit measure-zero result, its analytic-kernel formulation, the germ-incidence formulation and the analytic-height extension in one module. Reused the compact-envelope and implicit-incidence proofs and checked the module directly.
- **Pilot.** Proved one analytic kernel and two-level separation result, transported it through three physical likelihoods, and joined each to its attainment criterion and existing exact special cases. Checked the pilot module directly.
- **Final checks on the workstation.** Both `lake env lean` checks and `python3 dev/tools/existence_atlas.py --check` exited successfully. `lake build` with the temporary imports completed successfully with **8656 jobs**. The build emitted existing linter warnings in other modules. The temporary imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed.
