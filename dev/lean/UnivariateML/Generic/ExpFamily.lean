module

public import UnivariateML.Generic.Escape
public import Mathlib.Analysis.Convex.Intrinsic
public import Mathlib.Analysis.InnerProductSpace.Dual
public import Mathlib.Analysis.LocallyConvex.Separation
public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Analysis.SpecialFunctions.Exp
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Maximum likelihood for finite exponential families

The carrier weight is strictly positive on a nonempty finite support. Zero-weight
points can be removed from the index type. The parameter space is the whole
finite-dimensional real inner product space.

## Deviations

The criterion uses relative interior. Uniqueness of the natural parameter
requires affine spanning. In a lower-dimensional family, orthogonal projection
onto the space of statistic differences preserves the likelihood.
-/

@[expose] public section

open Set Filter Real
open scoped RealInnerProductSpace

namespace UnivariateML.ExpFamily

noncomputable section

variable {ι E : Type*} [Fintype ι] [Nonempty ι]
  [NormedAddCommGroup E] [InnerProductSpace ℝ E]

/-- Finite log-partition function. -/
def logPartition (T : ι → E) (h : ι → ℝ) (θ : E) : ℝ :=
  log (∑ i, h i * exp ⟪θ, T i⟫)

/-- Log-likelihood per observation, up to a parameter-independent term. -/
def logLik (T : ι → E) (h : ι → ℝ) (m θ : E) : ℝ :=
  ⟪θ, m⟫ - logPartition T h θ

private theorem partition_pos (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (θ : E) :
    0 < ∑ i, h i * exp ⟪θ, T i⟫ :=
  Finset.sum_pos (fun i _ ↦ mul_pos (hh i) (exp_pos _)) Finset.univ_nonempty

private theorem logLik_center (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (m θ : E) :
    logLik T h m θ = -logPartition (fun i ↦ T i - m) h θ := by
  unfold logLik logPartition
  have he : (∑ i, h i * exp ⟪θ, T i - m⟫) =
      (∑ i, h i * exp ⟪θ, T i⟫) * exp (-⟪θ, m⟫) := by
    rw [Finset.sum_mul]
    refine Finset.sum_congr rfl (fun i _ ↦ ?_)
    rw [inner_sub_right, sub_eq_add_neg, exp_add, mul_assoc]
  rw [he, log_mul (partition_pos T hh θ).ne' (exp_pos _).ne', log_exp]
  ring

private theorem normalized_sum (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (θ : E) :
    (∑ i, h i * exp (⟪θ, T i⟫ - logPartition T h θ)) = 1 := by
  unfold logPartition
  simp_rw [exp_sub, exp_log (partition_pos T hh θ), ← mul_div_assoc,
    ← Finset.sum_div, div_self (partition_pos T hh θ).ne']

private theorem combo_sum (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (x y : E) {a b : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b) (hab : a + b = 1) :
    (∑ i, h i * exp (⟪a • x + b • y, T i⟫ -
      (a * logPartition T h x + b * logPartition T h y))) ≤ 1 ∧
    ((∃ i, ⟪x, T i⟫ - logPartition T h x ≠ ⟪y, T i⟫ - logPartition T h y) →
      0 < a → 0 < b →
      (∑ i, h i * exp (⟪a • x + b • y, T i⟫ -
        (a * logPartition T h x + b * logPartition T h y))) < 1) := by
  let p : ι → ℝ := fun i ↦ ⟪x, T i⟫ - logPartition T h x
  let q : ι → ℝ := fun i ↦ ⟪y, T i⟫ - logPartition T h y
  have he (i : ι) : ⟪a • x + b • y, T i⟫ -
      (a * logPartition T h x + b * logPartition T h y) = a * p i + b * q i := by
    simp [p, q, inner_add_left, real_inner_smul_left]
    ring
  have hs : (∑ i, h i * (a * exp (p i) + b * exp (q i))) = 1 := by
    simp_rw [mul_add, mul_left_comm (h _) a, mul_left_comm (h _) b,
      Finset.sum_add_distrib, ← Finset.mul_sum, p, q, normalized_sum T hh, mul_one]
    exact hab
  have hw (i : ι) : h i * exp (a * p i + b * q i) ≤
      h i * (a * exp (p i) + b * exp (q i)) :=
    mul_le_mul_of_nonneg_left
      (strictConvexOn_exp.convexOn.2 (mem_univ _) (mem_univ _) ha hb hab) (hh i).le
  simp_rw [he, ← hs]
  refine ⟨Finset.sum_le_sum (fun i _ ↦ hw i), ?_⟩
  rintro ⟨i, hi⟩ ha' hb'
  exact Finset.sum_lt_sum (fun j _ ↦ hw j) ⟨i, Finset.mem_univ _, mul_lt_mul_of_pos_left
    (strictConvexOn_exp.2 (mem_univ _) (mem_univ _) hi ha' hb' hab) (hh i)⟩

private theorem convex_logPartition (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) :
    ConvexOn ℝ univ (logPartition T h) := by
  refine ⟨convex_univ, fun x _ y _ a b ha hb hab ↦ ?_⟩
  have hw := (combo_sum T hh x y ha hb hab).1
  simp_rw [exp_sub, ← mul_div_assoc, ← Finset.sum_div, div_le_one (exp_pos _)] at hw
  exact (log_le_iff_le_exp (partition_pos T hh _)).2 hw

theorem concave_logLik (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (m : E) :
    ConcaveOn ℝ univ (logLik T h m) := by
  rw [show logLik T h m = fun θ ↦ -logPartition (fun i ↦ T i - m) h θ from
    funext (logLik_center T hh m)]
  exact (convex_logPartition _ hh).neg

theorem strictConcave_logLik (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) (m : E) :
    StrictConcaveOn ℝ univ (logLik T h m) := by
  rw [show logLik T h m = fun θ ↦ -logPartition (fun i ↦ T i - m) h θ from
    funext (logLik_center T hh m)]
  apply StrictConvexOn.neg
  let S : ι → E := fun i ↦ T i - m
  change StrictConvexOn ℝ univ (logPartition S h)
  refine ⟨convex_univ, fun x _ y _ hxy a b ha hb hab ↦ ?_⟩
  have hd : ∃ i, ⟪x, S i⟫ - logPartition S h x ≠ ⟪y, S i⟫ - logPartition S h y := by
    by_contra! hc
    let f : E →ᵃ[ℝ] ℝ := (innerSL ℝ (x - y)).toLinearMap.toAffineMap
    let g : E →ᵃ[ℝ] ℝ := AffineMap.const ℝ E
      (logPartition S h x - logPartition S h y + ⟪x - y, m⟫)
    have hfg : f = g := AffineMap.ext_on hspan (by
      rintro _ ⟨i, rfl⟩
      change ⟪x - y, T i⟫ = logPartition S h x - logPartition S h y + ⟪x - y, m⟫
      have hi := hc i
      simp only [S, inner_sub_right, inner_sub_left] at hi ⊢
      linarith)
    have hzero : ⟪x - y, (0 : E)⟫ = g 0 := congrArg (fun h : E →ᵃ[ℝ] ℝ ↦ h 0) hfg
    have hself : ⟪x - y, x - y⟫ = g 0 := congrArg (fun h : E →ᵃ[ℝ] ℝ ↦ h (x - y)) hfg
    exact hxy (sub_eq_zero.mp (inner_self_eq_zero.mp (hself.trans (by simpa using hzero.symm))))
  have hw := (combo_sum S hh x y ha.le hb.le hab).2 hd ha hb
  simp_rw [exp_sub, ← mul_div_assoc, ← Finset.sum_div, div_lt_one (exp_pos _)] at hw
  exact (log_lt_iff_lt_exp (partition_pos S hh _)).2 hw

theorem unique_mle (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) {m θ φ : E}
    (hθ : IsMaxOn (logLik T h m) univ θ) (hφ : IsMaxOn (logLik T h m) univ φ) :
    θ = φ :=
  (strictConcave_logLik T hh hspan m).eq_of_isMaxOn hθ hφ (mem_univ _) (mem_univ _)

private theorem partition_decreases (S : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (θ d : E) (hle : ∀ i, ⟪d, S i⟫ ≤ 0) (hlt : ∃ i, ⟪d, S i⟫ < 0) :
    logPartition S h (θ + d) < logPartition S h θ := by
  apply log_lt_log (partition_pos S hh _)
  have hw (i : ι) : h i * exp ⟪θ + d, S i⟫ ≤ h i * exp ⟪θ, S i⟫ := by
    rw [inner_add_left]
    exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr (by linarith [hle i])) (hh i).le
  obtain ⟨i, hi⟩ := hlt
  refine Finset.sum_lt_sum (fun j _ ↦ hw j) ⟨i, Finset.mem_univ _, ?_⟩
  rw [inner_add_left]
  exact mul_lt_mul_of_pos_left (exp_lt_exp.mpr (by linarith)) (hh i)

/-- A direction exposing a proper face through the observed statistic improves every fit. -/
theorem improves_in_direction (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (m θ d : E) (hle : ∀ i, ⟪d, T i - m⟫ ≤ 0) (hlt : ∃ i, ⟪d, T i - m⟫ < 0) :
    logLik T h m θ < logLik T h m (θ + d) := by
  simp_rw [logLik_center T hh]
  exact neg_lt_neg (partition_decreases _ hh θ d hle hlt)

variable [FiniteDimensional ℝ E]

omit [Fintype ι] [Nonempty ι] [FiniteDimensional ℝ E] in
private theorem zero_intrinsicInterior_iff (S : ι → E)
    (hzero : (0 : E) ∈ convexHull ℝ (range S)) :
    (0 : E) ∈ intrinsicInterior ℝ (convexHull ℝ (range S)) ↔
      (0 : Submodule.span ℝ (range S)) ∈
        interior ((↑) ⁻¹' convexHull ℝ (range S)) := by
  unfold intrinsicInterior
  rw [affineSpan_convexHull, affineSpan_eq_span_of_zero_mem
    (affineSpan_convexHull (𝕜 := ℝ) (range S) ▸ subset_affineSpan _ _ hzero)]
  constructor
  · rintro ⟨x, hx, he⟩
    have : x = ⟨0, Submodule.zero_mem _⟩ := Subtype.ext he
    rwa [this] at hx
  · exact fun hx ↦ ⟨⟨0, Submodule.zero_mem _⟩, hx, rfl⟩

private theorem partition_min_iff (S : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) :
    (∃ θ, IsMinOn (logPartition S h) univ θ) ↔
      (0 : E) ∈ intrinsicInterior ℝ (convexHull ℝ (range S)) := by
  let C := convexHull ℝ (range S)
  let V := Submodule.span ℝ (range S)
  let K : Set V := (↑) ⁻¹' C
  have hS (i : ι) : S i ∈ C := subset_convexHull ℝ _ (mem_range_self i)
  have hSV (i : ι) : S i ∈ V := Submodule.subset_span (mem_range_self i)
  have hC : Convex ℝ C := convex_convexHull _ _
  let Z : E → ℝ := logPartition S h
  change (∃ θ, IsMinOn Z univ θ) ↔ (0 : E) ∈ intrinsicInterior ℝ C
  constructor
  · rintro ⟨θ, hθ⟩
    by_cases hzero : (0 : E) ∈ C
    · rw [zero_intrinsicInterior_iff S hzero]
      by_contra hn
      have hspan : affineSpan ℝ C = V.toAffineSubspace := by
        rw [show C = convexHull ℝ (range S) from rfl, affineSpan_convexHull]
        exact affineSpan_eq_span_of_zero_mem
          (affineSpan_convexHull (𝕜 := ℝ) (range S) ▸ subset_affineSpan _ _ hzero)
      have hKi : (interior K).Nonempty := by
        have hcne : C.Nonempty := ⟨0, hzero⟩
        have hi := hcne.intrinsicInterior hC
        unfold intrinsicInterior at hi
        rw [hspan, image_nonempty] at hi
        exact hi
      obtain ⟨f, hf, hle⟩ := geometric_hahn_banach_of_nonempty_interior_point
        (hC.linear_preimage V.subtype) hn hKi
      obtain ⟨d, rfl⟩ := (InnerProductSpace.toDual ℝ V).surjective f
      have hd (i : ι) : ⟪(d : E), S i⟫ ≤ 0 := by
        simpa using hle ⟨S i, hSV i⟩ (show (⟨S i, hSV i⟩ : V) ∈ K from hS i)
      have hs : ∃ i, ⟪(d : E), S i⟫ < 0 := by
        by_contra! ha
        have hz : V ≤ (innerSL ℝ (d : E)).ker := Submodule.span_le.mpr (by
          rintro _ ⟨i, rfl⟩
          exact le_antisymm (hd i) (ha i))
        have hd0 : (d : E) = 0 := inner_self_eq_zero.mp (hz d.property)
        exact hf (by simp [show d = 0 from Subtype.ext hd0])
      exact (not_lt_of_ge (hθ (mem_univ (θ + (d : E)))))
        (partition_decreases S hh θ d hd hs)
    · exfalso
      obtain ⟨f, u, hu, hlt⟩ := geometric_hahn_banach_closed_point hC
        ((finite_range S).isClosed_convexHull ℝ) hzero
      obtain ⟨d, rfl⟩ := (InnerProductSpace.toDual ℝ E).surjective f
      have hd (i : ι) : ⟪d, S i⟫ < 0 :=
        (hu _ (hS i)).trans (by simpa using hlt)
      exact (not_lt_of_ge (hθ (mem_univ (θ + d)))) (partition_decreases S hh θ d
        (fun i ↦ (hd i).le) ⟨Classical.choice inferInstance, hd _⟩)
  · intro hm
    have hzero := intrinsicInterior_subset hm
    have hKi : (0 : V) ∈ interior K := (zero_intrinsicInterior_iff S hzero).mp hm
    obtain ⟨ε, hε, hball⟩ := Metric.mem_nhds_iff.mp (mem_interior_iff_mem_nhds.mp hKi)
    classical
    let b : ℝ := Finset.univ.inf' Finset.univ_nonempty (fun i ↦ log (h i))
    have hbound (θ : V) : b + ε / 2 * ‖θ‖ ≤ Z θ := by
      have hCpos : ∃ i, ε / 2 * ‖θ‖ ≤ ⟪(θ : E), S i⟫ := by
        by_cases hz : θ = 0
        · exact ⟨Classical.choice inferInstance, by simp [hz]⟩
        · let q : V := (ε / 2 / ‖θ‖) • θ
          have hn : 0 < ‖θ‖ := norm_pos_iff.mpr hz
          have hq : q ∈ Metric.ball (0 : V) ε := by
            rw [Metric.mem_ball, dist_zero_right, show ‖q‖ = ε / 2 by
              rw [norm_smul, Real.norm_eq_abs, abs_of_pos (by positivity),
                div_mul_cancel₀ _ hn.ne']]
            linarith
          by_contra! hf
          have hsub : C ⊆ {x : E | ⟪(θ : E), x⟫ < ε / 2 * ‖θ‖} :=
            convexHull_min (by rintro _ ⟨i, rfl⟩; exact hf i)
              (convex_halfSpace_lt (innerSL ℝ (θ : E)).toLinearMap.isLinear _)
          have hq' : ⟪(θ : E), (q : E)⟫ < ε / 2 * ‖θ‖ :=
            hsub (hball hq)
          have he : ⟪(θ : E), (q : E)⟫ = ε / 2 * ‖θ‖ := by
            change ⟪θ, q⟫ = _
            rw [real_inner_smul_right, real_inner_self_eq_norm_sq]
            field_simp
          exact (not_lt_of_ge he.ge) hq'
      obtain ⟨i, hi⟩ := hCpos
      have hb : b ≤ log (h i) := Finset.inf'_le _ (Finset.mem_univ i)
      calc
        b + ε / 2 * ‖θ‖ ≤ log (h i) + ⟪(θ : E), S i⟫ := add_le_add hb hi
        _ = log (h i * exp ⟪(θ : E), S i⟫) := by
          rw [log_mul (hh i).ne' (exp_pos _).ne', log_exp]
        _ ≤ Z θ := log_le_log (mul_pos (hh i) (exp_pos _))
          (Finset.single_le_sum (f := fun j ↦ h j * exp ⟪(θ : E), S j⟫)
            (fun j _ ↦ (mul_pos (hh j) (exp_pos _)).le) (Finset.mem_univ i))
    have hcont : Continuous (fun θ : V ↦ -Z θ) := by
      apply Continuous.neg
      apply Continuous.log
      · exact continuous_finsetSum _ (fun i _ ↦ continuous_const.mul
          (continuous_exp.comp (continuous_subtype_val.inner continuous_const)))
      · exact fun θ ↦ (partition_pos S hh θ).ne'
    have hescape : Tendsto (fun θ : V ↦ -Z θ) (cocompact V) atBot := by
      refine tendsto_atBot.2 (fun r ↦ ?_)
      filter_upwards [tendsto_atTop.1 (tendsto_norm_cocompact_atTop (E := V))
        ((-r - b) / (ε / 2))] with θ hθ
      have hr := (div_le_iff₀ (show 0 < ε / 2 by positivity)).mp hθ
      linarith [hbound θ]
    have hescape' : Tendsto (fun θ : (univ : Set V) ↦ -Z (θ : V))
        (cocompact (univ : Set V)) atBot :=
      hescape.comp (Homeomorph.Set.univ V).map_cocompact.le
    obtain ⟨θ, -, hθ⟩ := Generic.exists_isMaxOn_of_tendsto_cocompact
      univ_nonempty hcont.continuousOn hescape'
    refine ⟨θ, fun φ _ ↦ ?_⟩
    have hp : Z (V.orthogonalProjectionOnto φ) = Z φ := by
      unfold Z logPartition
      congr 1
      apply Finset.sum_congr rfl
      intro i _
      have he : ⟪(V.orthogonalProjectionOnto φ : E), S i⟫ = ⟪φ, S i⟫ :=
        V.inner_orthogonalProjectionOnto_eq_of_mem_right ⟨S i, hSV i⟩ φ
      rw [he]
    change Z θ ≤ Z φ
    have hproj : -Z (V.orthogonalProjectionOnto φ) ≤ -Z θ :=
      hθ (mem_univ (V.orthogonalProjectionOnto φ))
    simpa only [hp, neg_le_neg_iff] using hproj

theorem mle_exists_iff (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (m : E) :
    (∃ θ, IsMaxOn (logLik T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (range T)) := by
  let e := AffineIsometryEquiv.constVAdd ℝ E (-m)
  have he (x : E) : e x = x - m := by simp [e, sub_eq_add_neg, add_comm]
  have himage : e '' range T = range (fun i ↦ T i - m) := by
    rw [← range_comp]
    simp [Function.comp_def, he]
  have hr : e '' intrinsicInterior ℝ (convexHull ℝ (range T)) =
      intrinsicInterior ℝ (convexHull ℝ (range (fun i ↦ T i - m))) := by
    have hhull : e '' convexHull ℝ (range T) = convexHull ℝ (e '' range T) :=
      e.toAffineEquiv.toAffineMap.image_convexHull (range T)
    calc
      _ = intrinsicInterior ℝ (e '' convexHull ℝ (range T)) :=
        (e.toAffineIsometry.intrinsicInterior_image _).symm
      _ = _ := by rw [hhull, himage]
  have hobj : (∃ θ, IsMaxOn (logLik T h m) univ θ) ↔
      ∃ θ, IsMinOn (logPartition (fun i ↦ T i - m) h) univ θ := by
    constructor <;> rintro ⟨θ, hθ⟩ <;> refine ⟨θ, fun φ hφ ↦ ?_⟩
    · have hi : logLik T h m φ ≤ logLik T h m θ := hθ hφ
      change logPartition (fun i ↦ T i - m) h θ ≤ logPartition (fun i ↦ T i - m) h φ
      simpa only [logLik_center T hh, neg_le_neg_iff] using hi
    · change logLik T h m φ ≤ logLik T h m θ
      have hi : logPartition (fun i ↦ T i - m) h θ ≤ logPartition (fun i ↦ T i - m) h φ := hθ hφ
      simpa only [logLik_center T hh, neg_le_neg_iff] using hi
  rw [hobj, partition_min_iff _ hh, ← hr]
  constructor
  · rintro ⟨x, hx, hx0⟩
    rw [he, sub_eq_zero] at hx0
    simpa [hx0] using hx
  · exact fun hx ↦ ⟨m, hx, by simp [he]⟩

/-- With full affine span, the existence criterion gives a unique natural parameter. -/
theorem mle_existsUnique_iff (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) (m : E) :
    (∃! θ, IsMaxOn (logLik T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (range T)) := by
  rw [← mle_exists_iff T hh m]
  exact ⟨ExistsUnique.exists, fun ⟨θ, hθ⟩ ↦ ⟨θ, hθ, fun _ hφ ↦ unique_mle T hh hspan hφ hθ⟩⟩

end

end UnivariateML.ExpFamily
