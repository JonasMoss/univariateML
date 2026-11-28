module

public import UnivariateML.Generic.ExpFamily
public import UnivariateML.Family.Discrete
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Calculus.LocalExtr.Basic

/-! Fixed-window exponential families and zero modifiers. The finite window is
represented by its subtype. Likelihoods in natural coordinates omit carrier terms. -/

@[expose] public section

open Set Real
open scoped RealInnerProductSpace

namespace UnivariateML.Truncation

noncomputable section

variable {X E : Type*} [NormedAddCommGroup E] [InnerProductSpace ℝ E]
variable (A : Finset X) (T : X → E) (h : X → ℝ)

/-- The log normalizer of the conditional law. -/
def partition (θ : E) : ℝ := log (∑ x ∈ A, h x * exp ⟪θ, T x⟫)

/-- Parameter-dependent part of the log likelihood per observation. -/
def logLik (m θ : E) : ℝ := ⟪θ, m⟫ - partition A T h θ

private theorem partition_subtype (θ : E) :
    partition A T h θ =
      ExpFamily.logPartition (fun x : A ↦ T x) (fun x : A ↦ h x) θ := by
  unfold partition ExpFamily.logPartition
  rw [← Finset.sum_coe_sort]

theorem finite_exists_iff [FiniteDimensional ℝ E] (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) (m : E) :
    (∃ θ, IsMaxOn (logLik A T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (T '' (A : Set X))) := by
  let : Nonempty A := hA.to_subtype
  have hr : range (fun x : A ↦ T x) = T '' (A : Set X) := by
    ext y
    exact ⟨fun ⟨x, hx⟩ ↦ ⟨x, x.property, hx⟩, fun ⟨x, hx, hy⟩ ↦ ⟨⟨x, hx⟩, hy⟩⟩
  have he : logLik A T h m =
      ExpFamily.logLik (fun x : A ↦ T x) (fun x : A ↦ h x) m :=
    funext fun θ ↦ by rw [logLik, ExpFamily.logLik, partition_subtype]
  rw [he, ← hr]
  exact ExpFamily.mle_exists_iff (fun x : A ↦ T x) (fun x ↦ hh x x.property) m

private theorem partition_sum_pos (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) (θ : E) :
    0 < ∑ x ∈ A, h x * exp ⟪θ, T x⟫ :=
  Finset.sum_pos (fun x hx ↦ mul_pos (hh x hx) (exp_pos _)) hA

private theorem normalized (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) (θ : E) :
    (∑ x ∈ A, h x * exp (⟪θ, T x⟫ - partition A T h θ)) = 1 := by
  simp_rw [partition, exp_sub, exp_log (partition_sum_pos A T h hA hh θ),
    ← mul_div_assoc, ← Finset.sum_div]
  exact div_self (partition_sum_pos A T h hA hh θ).ne'

private theorem partition_jensen (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x)
    (θ φ : E) {a b : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b) (hab : a + b = 1) :
    partition A T h (a • θ + b • φ) ≤ a * partition A T h θ + b * partition A T h φ ∧
    ((∃ x ∈ A, ∃ y ∈ A, ⟪θ - φ, T x⟫ ≠ ⟪θ - φ, T y⟫) → 0 < a → 0 < b →
      partition A T h (a • θ + b • φ) < a * partition A T h θ + b * partition A T h φ) := by
  let p (x : X) := ⟪θ, T x⟫ - partition A T h θ
  let q (x : X) := ⟪φ, T x⟫ - partition A T h φ
  let C := a * partition A T h θ + b * partition A T h φ
  have he (x : X) : ⟪a • θ + b • φ, T x⟫ - C = a * p x + b * q x := by
    simp [p, q, C, inner_add_left, real_inner_smul_left]
    ring
  have hs : (∑ x ∈ A, h x * (a * exp (p x) + b * exp (q x))) = 1 := by
    simp_rw [mul_add, mul_left_comm (h _) a, mul_left_comm (h _) b,
      Finset.sum_add_distrib, ← Finset.mul_sum, p, q, normalized A T h hA hh, mul_one]
    exact hab
  have hw (x : X) (hx : x ∈ A) : h x * exp (⟪a • θ + b • φ, T x⟫ - C) ≤
      h x * (a * exp (p x) + b * exp (q x)) := by
    rw [he]
    exact mul_le_mul_of_nonneg_left
      (strictConvexOn_exp.convexOn.2 (mem_univ _) (mem_univ _) ha hb hab) (hh x hx).le
  constructor
  · have H := (Finset.sum_le_sum hw).trans_eq hs
    simp_rw [exp_sub, ← mul_div_assoc, ← Finset.sum_div, div_le_one (exp_pos _)] at H
    exact (log_le_iff_le_exp (partition_sum_pos A T h hA hh _)).2 H
  · rintro ⟨x, hx, y, hy, hxy⟩ ha' hb'
    have hd : ∃ z ∈ A, p z ≠ q z := by
      by_contra! H
      apply hxy
      simp only [inner_sub_left]
      linarith [show p x = q x from H x hx, show p y = q y from H y hy]
    obtain ⟨z, hz, hdiff⟩ := hd
    have H := (Finset.sum_lt_sum hw ⟨z, hz, by
      rw [he]
      exact mul_lt_mul_of_pos_left
        (strictConvexOn_exp.2 (mem_univ _) (mem_univ _) hdiff ha' hb' hab) (hh z hz)⟩).trans_eq hs
    simp_rw [exp_sub, ← mul_div_assoc, ← Finset.sum_div, div_lt_one (exp_pos _)] at H
    exact (log_lt_iff_lt_exp (partition_sum_pos A T h hA hh _)).2 H

theorem convex_partition (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) :
    ConvexOn ℝ univ (partition A T h) :=
  ⟨convex_univ, fun θ _ φ _ _ _ ha hb hab ↦ (partition_jensen A T h hA hh θ φ ha hb hab).1⟩

theorem strict_partition_segment (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x)
    {θ φ : E} (hd : ∃ x ∈ A, ∃ y ∈ A, ⟪θ - φ, T x⟫ ≠ ⟪θ - φ, T y⟫)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1) :
    partition A T h (a • θ + b • φ) < a * partition A T h θ + b * partition A T h φ :=
  (partition_jensen A T h hA hh θ φ ha.le hb.le hab).2 hd ha hb

theorem maximizers_constant_direction (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x)
    {m θ φ : E} (hθ : IsMaxOn (logLik A T h m) univ θ)
    (hφ : IsMaxOn (logLik A T h m) univ φ) :
    ∀ x ∈ A, ∀ y ∈ A, ⟪θ - φ, T x⟫ = ⟪θ - φ, T y⟫ := by
  by_contra! hd
  have H := strict_partition_segment A T h hA hh hd
    (show (0 : ℝ) < 1/2 by norm_num) (show (0 : ℝ) < 1/2 by norm_num)
    (show (1/2 : ℝ) + 1/2 = 1 by norm_num)
  have hm : logLik A T h m ((1/2 : ℝ) • θ + (1/2 : ℝ) • φ) ≤
      logLik A T h m θ := hθ (mem_univ _)
  have he : logLik A T h m θ ≤ logLik A T h m φ := hφ (mem_univ _)
  simp only [logLik, inner_add_left, real_inner_smul_left] at hm he
  linarith

theorem partition_shift (d θ : E) {c : ℝ} (hA : A.Nonempty)
    (hh : ∀ x ∈ A, 0 < h x) (hd : ∀ x ∈ A, ⟪d, T x⟫ = c) :
    partition A T h (θ + d) = partition A T h θ + c := by
  unfold partition
  have he : (∑ x ∈ A, h x * exp ⟪θ + d, T x⟫) =
      (∑ x ∈ A, h x * exp ⟪θ, T x⟫) * exp c := by
    rw [Finset.sum_mul]
    exact Finset.sum_congr rfl fun x hx ↦ by rw [inner_add_left, hd x hx, exp_add, mul_assoc]
  rw [he, log_mul (partition_sum_pos A T h hA hh θ).ne' (exp_pos c).ne', log_exp]

theorem logLik_shift (d θ m : E) (hA : A.Nonempty)
    (hh : ∀ x ∈ A, 0 < h x) (hd : ∀ x ∈ A, ⟪d, T x⟫ = ⟪d, m⟫) :
    logLik A T h m (θ + d) = logLik A T h m θ := by
  unfold logLik
  rw [partition_shift A T h d θ hA hh hd, inner_add_left]
  ring

/-- On a real-valued window with distinct extremes, the mean must lie
strictly between them. -/
theorem scalar_existsUnique_iff {A : Finset X} {T : X → ℝ} {h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) (hb : ∀ x ∈ A, T x ∈ Icc (T l) (T u)) (m : ℝ) :
    (∃! θ, IsMaxOn (logLik A T h m) univ θ) ↔ m ∈ Ioo (T l) (T u) := by
  let : Nonempty A := ⟨⟨l, hl⟩⟩
  have he : logLik A T h m =
      ExpFamily.logLik (fun x : A ↦ T x) (fun x : A ↦ h x) m :=
    funext fun θ ↦ by rw [logLik, ExpFamily.logLik, partition_subtype]
  have hExist := finite_exists_iff A T h ⟨l, hl⟩ hh m
  have hHull : convexHull ℝ (T '' (A : Set X)) = Icc (T l) (T u) := by
    apply Subset.antisymm
    · exact convexHull_min (by rintro _ ⟨x, hx, rfl⟩; exact hb x hx) (convex_Icc _ _)
    · rw [← segment_eq_Icc hlu.le]
      exact (convex_convexHull ..).segment_subset (subset_convexHull ℝ _ ⟨l, hl, rfl⟩)
        (subset_convexHull ℝ _ ⟨u, hu, rfl⟩)
  constructor
  · rintro ⟨θ, hθ, _⟩
    have hm := intrinsicInterior_subset (hExist.mp ⟨θ, hθ⟩)
    rw [hHull] at hm
    have improve := ExpFamily.improves_in_direction (fun x : A ↦ T x)
      (fun x ↦ hh x x.property) m θ
    have noImprove (d : ℝ) : ¬ logLik A T h m θ < logLik A T h m (θ + d) :=
      not_lt_of_ge (hθ (mem_univ (θ + d)))
    refine ⟨lt_of_le_of_ne hm.1 ?_, lt_of_le_of_ne hm.2 ?_⟩
    · intro hz
      apply noImprove (-1)
      rw [he]
      apply improve
      · intro x
        change (T x - m) * (-1) ≤ 0
        linarith [(hb x x.property).1]
      · refine ⟨⟨u, hu⟩, ?_⟩
        change (T u - m) * (-1) < 0
        linarith
    · intro hz
      apply noImprove 1
      rw [he]
      apply improve
      · intro x
        change (T x - m) * 1 ≤ 0
        linarith [(hb x x.property).2]
      · refine ⟨⟨l, hl⟩, ?_⟩
        change (T l - m) * 1 < 0
        linarith
  · intro hm
    obtain ⟨θ, hθ⟩ := hExist.mpr (interior_subset_intrinsicInterior (𝕜 := ℝ) (by
      rw [hHull, interior_Icc]
      exact hm))
    refine ⟨θ, hθ, fun φ hφ ↦ ?_⟩
    have H := maximizers_constant_direction A T h ⟨l, hl⟩ hh hφ hθ l hl u hu
    change T l * (φ - θ) = T u * (φ - θ) at H
    have H' : (T u - T l) * (φ - θ) = 0 := by linarith
    exact sub_eq_zero.mp ((mul_eq_zero.mp H').resolve_left (sub_pos.mpr hlu).ne')

/-- Mean at zero natural tilt (the carrier normalized on the window). -/
def carrierMean (A : Finset X) (T h : X → ℝ) : ℝ :=
  (∑ x ∈ A, h x * T x) / (∑ x ∈ A, h x)

private theorem scalar_hasDerivAt {A : Finset X} {T h : X → ℝ} (hA : A.Nonempty)
    (hh : ∀ x ∈ A, 0 < h x) (m t : ℝ) :
    HasDerivAt (logLik A T h m)
      (m - (∑ x ∈ A, h x * (exp (t * T x) * T x)) /
        (∑ x ∈ A, h x * exp (t * T x))) t := by
  have hSum : HasDerivAt (fun u : ℝ ↦ ∑ x ∈ A, h x * exp (u * T x)) _ t :=
    HasDerivAt.fun_sum fun x _ ↦ (((hasDerivAt_id t).mul_const (T x)).exp).const_mul (h x)
  have hPos : 0 < ∑ x ∈ A, h x * exp (t * T x) :=
    Finset.sum_pos (fun x hx ↦ mul_pos (hh x hx) (exp_pos _)) hA
  change HasDerivAt (fun u ↦ m * u - log (∑ x ∈ A, h x * exp (T x * u))) _ t
  simpa only [mul_comm m, mul_comm (T _), id_eq, one_mul] using
    ((hasDerivAt_id t).mul_const m).fun_sub (hSum.log hPos.ne')

private theorem scalar_strictConcave {A : Finset X} {T h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) (m : ℝ) :
    StrictConcaveOn ℝ univ (logLik A T h m) := by
  refine ⟨convex_univ, fun x _ y _ hxy a b ha hb hab ↦ ?_⟩
  have hd : ⟪x - y, T l⟫ ≠ ⟪x - y, T u⟫ := by
    change T l * (x - y) ≠ T u * (x - y)
    exact fun H ↦ hlu.ne ((mul_left_inj' (sub_ne_zero.mpr hxy)).mp H)
  have H := strict_partition_segment A T h ⟨l, hl⟩ hh ⟨l, hl, u, hu, hd⟩ ha hb hab
  simp only [logLik, smul_eq_mul, RCLike.inner_apply, conj_trivial] at *
  nlinarith

/-- For a window restricted to negative natural tilts, the excluded
zero tilt replaces the upper endpoint in the scalar criterion. -/
theorem scalar_negative_existsUnique_iff {A : Finset X} {T h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) (hb : ∀ x ∈ A, T x ∈ Icc (T l) (T u)) {m : ℝ}
    (hm : m ∈ Ioo (T l) (T u)) :
    (∃! θ, θ < 0 ∧ IsMaxOn (logLik A T h m) (Iio 0) θ) ↔ m < carrierMean A T h := by
  obtain ⟨r, hr, _⟩ := (scalar_existsUnique_iff hh hl hu hlu hb m).mpr hm
  have hD := scalar_strictConcave hh hl hu hlu m
  have hanti := hD.strictAntiOn_deriv (fun t _ ↦
    (scalar_hasDerivAt ⟨l, hl⟩ hh m t).differentiableAt)
  have hz : deriv (logLik A T h m) r = 0 :=
    (hr.isLocalMax (by simp)).deriv_eq_zero
  have h0 : deriv (logLik A T h m) 0 = m - carrierMean A T h := by
    rw [(scalar_hasDerivAt ⟨l, hl⟩ hh m 0).deriv]
    simp [carrierMean]
  constructor
  · rintro ⟨t, ⟨ht, htmax⟩, _⟩
    have ht0 := (htmax.isLocalMax (Iio_mem_nhds ht)).deriv_eq_zero
    have H := hanti (mem_univ t) (mem_univ 0) ht
    linarith
  · intro h
    have hlt : r < 0 := by
      by_contra! hge
      have H := hanti.antitoneOn (mem_univ 0) (mem_univ r) hge
      linarith
    refine ⟨r, ⟨hlt, fun t _ ↦ hr (mem_univ t)⟩, fun t ⟨ht, htmax⟩ ↦ ?_⟩
    exact hanti.injOn (mem_univ t) (mem_univ r)
      ((htmax.isLocalMax (Iio_mem_nhds ht)).deriv_eq_zero.trans hz.symm)

/-- When the empirical mean reaches or exceeds the carrier mean,
the excluded zero tilt is strictly better than all feasible negative tilts. -/
theorem scalar_negative_upper_failure {A : Finset X} {T h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) {m : ℝ} (hm : carrierMean A T h ≤ m) :
    StrictMonoOn (logLik A T h m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (logLik A T h m) (Iio 0) t := by
  have hD := scalar_strictConcave hh hl hu hlu m
  have hDer (t : ℝ) := scalar_hasDerivAt (T := T) ⟨l, hl⟩ hh m t
  have hanti := hD.strictAntiOn_deriv (fun t _ ↦ (hDer t).differentiableAt)
  have h0 : deriv (logLik A T h m) 0 = m - carrierMean A T h := by
    rw [(hDer 0).deriv]
    simp [carrierMean]
  have hDiff : Differentiable ℝ (logLik A T h m) := fun t ↦ (hDer t).differentiableAt
  have hmono : StrictMonoOn (logLik A T h m) (Iic 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Iic 0) hDiff.continuous.continuousOn
    intro t ht
    have ht' : t < 0 := by simpa only [interior_Iic, mem_Iio] using ht
    have H := hanti (mem_univ t) (mem_univ 0) ht'
    linarith
  refine ⟨hmono, fun t ht hmax ↦ ?_⟩
  have he : t < t/2 := by linarith
  have hh : t/2 < 0 := by linarith
  exact (hmono ht.le hh.le he).not_ge (hmax hh)

/-- A singleton window has constant parameter-dependent log likelihood at its statistic. -/
theorem singleton_logLik (x : X) (hx : 0 < h x) (θ : E) :
    logLik {x} T h (T x) θ = -log (h x) := by
  simp only [logLik, partition, Finset.sum_singleton, log_mul hx.ne' (exp_pos _).ne', log_exp]
  ring

/-- Zero modification with the positive observations supplied as a separate sample. -/
def modifiedLikelihood {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (g : P → ℕ → ℝ) (p : P × ℝ) : ℝ :=
  p.2 ^ z * ∏ i, (1 - p.2) * g p.1 (x i)

theorem modified_factorization {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (g : P → ℕ → ℝ) (p : P × ℝ) :
    modifiedLikelihood z x g p =
      (p.2 ^ z * (1 - p.2) ^ k) * ∏ i, g p.1 (x i) := by
  simp [modifiedLikelihood, Finset.prod_mul_distrib, mul_assoc]

/-- Bernoulli likelihood with the endpoint conventions of the library. -/
def bernoulli (z k : ℕ) (w : ℝ) : ℝ := w ^ z * (1 - w) ^ k

private theorem bernoulli_eq (z k : ℕ) {w : ℝ} (hw : w ∈ Icc (0 : ℝ) 1) :
    bernoulli z k w = Discrete.Binomial.likelihood z k w := by
  rcases eq_or_lt_of_le hw.1 with h | h
  · rw [← h]
    by_cases hz : z = 0 <;> simp [bernoulli, Discrete.Binomial.likelihood, hz, zero_pow]
  rcases eq_or_lt_of_le hw.2 with h' | h'
  · rw [h']
    by_cases hk : k = 0 <;> simp [bernoulli, Discrete.Binomial.likelihood, hk, zero_pow]
  · unfold bernoulli Discrete.Binomial.likelihood
    rw [ite_eq_right (by simp [h.ne', h'.ne]), exp_add, exp_nat_mul, exp_nat_mul,
      exp_log h, exp_log (sub_pos.mpr h')]

theorem bernoulli_max_iff {z k : ℕ} (hn : 0 < z + k) {w : ℝ}
    (hw : w ∈ Icc (0 : ℝ) 1) :
    IsMaxOn (bernoulli z k) (Icc 0 1) w ↔ w = (z : ℝ) / (z + (k : ℝ)) := by
  have he : IsMaxOn (bernoulli z k) (Icc 0 1) w ↔
      IsMaxOn (Discrete.Binomial.likelihood z k) (Icc 0 1) w := by
    constructor <;> intro H u hu
    · change Discrete.Binomial.likelihood z k u ≤ Discrete.Binomial.likelihood z k w
      simpa only [bernoulli_eq z k hw, bernoulli_eq z k hu] using
        (show bernoulli z k u ≤ bernoulli z k w from H hu)
    · change bernoulli z k u ≤ bernoulli z k w
      rw [bernoulli_eq z k hu, bernoulli_eq z k hw]
      exact H hu
  rw [he]
  exact Discrete.Binomial.fixed_size_mle (Nat.cast_nonneg z) (Nat.cast_nonneg k)
    (by exact_mod_cast hn) w hw

/-- Compatible held zero weight contributes a positive constant. -/
theorem held_modified_max_iff {P : Type*} {D : Set P} {L : P → ℝ}
    {z k : ℕ} {w : ℝ} (hw : 0 < bernoulli z k w) {p : P} :
    IsMaxOn (fun t ↦ bernoulli z k w * L t) D p ↔ IsMaxOn L D p := by
  constructor
  · intro H q hq
    exact (mul_le_mul_iff_right₀ hw).mp (H hq)
  · intro H q hq
    exact (mul_le_mul_iff_right₀ hw).mpr (H hq)

/-- The empirical zero fraction. -/
def zeroFraction (z k : ℕ) : ℝ := (z : ℝ) / (z + (k : ℝ))

private theorem zeroFraction_mem {z k : ℕ} (hn : 0 < z+k) :
    zeroFraction z k ∈ Icc (0 : ℝ) 1 := by
  have h : 0 < (z : ℝ) + (k : ℝ) := by exact_mod_cast hn
  exact ⟨div_nonneg (Nat.cast_nonneg _) h.le, (div_le_one h).mpr (by simp)⟩

private theorem bernoulli_deriv {z k : ℕ} {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (bernoulli z k)
      (bernoulli z k w * (((z : ℝ) - (z + (k : ℝ)) * w) / (w * (1-w)))) w := by
  have hp := hw.1
  have hq := sub_pos.mpr hw.2
  have h := (((hasDerivAt_log hp.ne').const_mul (z : ℝ)).fun_add
    ((((hasDerivAt_id w).const_sub (1 : ℝ)).log hq.ne').const_mul (k : ℝ))).exp
  simp only [id_eq] at h
  have he : Filter.EventuallyEq (nhds w)
      (fun u ↦ exp ((z : ℝ) * log u + (k : ℝ) * log (1-u))) (bernoulli z k) := by
    filter_upwards [isOpen_Ioo.mem_nhds hw] with u hu
    rw [exp_add, exp_nat_mul, exp_nat_mul, exp_log hu.1, exp_log (sub_pos.mpr hu.2)]
    rfl
  have hs := h.congr_of_eventuallyEq he.symm
  convert hs using 1
  rw [he.eq_of_nhds]
  field_simp [hp.ne', hq.ne']
  ring

theorem bernoulli_strictMono {z k : ℕ} (hn : 0 < z+k) :
    StrictMonoOn (bernoulli z k) (Icc 0 (zeroFraction z k)) := by
  have hd : (0 : ℝ) < (z : ℝ) + (k : ℝ) := by exact_mod_cast hn
  apply strictMonoOn_of_deriv_pos (convex_Icc _ _) (by unfold bernoulli; fun_prop)
  intro w hw
  rw [interior_Icc] at hw
  have hw' : w ∈ Ioo (0 : ℝ) 1 := ⟨hw.1, hw.2.trans_le (zeroFraction_mem hn).2⟩
  rw [(bernoulli_deriv hw').deriv]
  have hnum : 0 < (z : ℝ) - (z + (k : ℝ)) * w := by
    have H := (lt_div_iff₀ hd).mp hw.2
    linarith
  have hB : 0 < bernoulli z k w :=
    mul_pos (pow_pos hw'.1 _) (pow_pos (sub_pos.mpr hw'.2) _)
  exact mul_pos hB (div_pos hnum (mul_pos hw'.1 (sub_pos.mpr hw'.2)))

theorem bernoulli_strictAnti {z k : ℕ} (hn : 0 < z+k) :
    StrictAntiOn (bernoulli z k) (Icc (zeroFraction z k) 1) := by
  have hd : (0 : ℝ) < (z : ℝ) + (k : ℝ) := by exact_mod_cast hn
  apply strictAntiOn_of_deriv_neg (convex_Icc _ _) (by unfold bernoulli; fun_prop)
  intro w hw
  rw [interior_Icc] at hw
  have hw' : w ∈ Ioo (0 : ℝ) 1 := ⟨(zeroFraction_mem hn).1.trans_lt hw.1, hw.2⟩
  rw [(bernoulli_deriv hw').deriv]
  have hnum : (z : ℝ) - (z + (k : ℝ)) * w < 0 := by
    have H := (div_lt_iff₀ hd).mp hw.1
    linarith
  have hB : 0 < bernoulli z k w :=
    mul_pos (pow_pos hw'.1 _) (pow_pos (sub_pos.mpr hw'.2) _)
  exact mul_neg_of_pos_of_neg hB (div_neg_of_neg_of_pos hnum (mul_pos hw'.1 (sub_pos.mpr hw'.2)))

/-- At a fixed parent zero mass, the unique fitted inflated zero weight
is the clipped empirical zero fraction. -/
theorem bernoulli_clip {z k : ℕ} (hn : 0 < z+k) {f w : ℝ} (hf : f ∈ Icc (0 : ℝ) 1)
    (hw : w ∈ Icc f 1) :
    IsMaxOn (bernoulli z k) (Icc f 1) w ↔ w = max f (zeroFraction z k) := by
  rcases le_or_gt f (zeroFraction z k) with h | h
  · rw [max_eq_right h]
    have hz : zeroFraction z k ∈ Icc f 1 := ⟨h, (zeroFraction_mem hn).2⟩
    have hopt := (bernoulli_max_iff hn (zeroFraction_mem hn)).mpr rfl
    constructor
    · intro H
      apply (bernoulli_max_iff hn ⟨hf.1.trans hw.1, hw.2⟩).mp
      intro v hv
      exact le_trans (hopt hv) (H hz)
    · rintro rfl v hv
      exact hopt ⟨hf.1.trans hv.1, hv.2⟩
  · rw [max_eq_left h.le]
    have H := bernoulli_strictAnti hn
    constructor
    · intro hmax
      by_contra hne
      exact (H ⟨h.le, hf.2⟩ ⟨h.le.trans hw.1, hw.2⟩
        (lt_of_le_of_ne hw.1 (Ne.symm hne))).not_ge (hmax ⟨le_rfl,hf.2⟩)
    · rintro rfl v hv
      exact H.antitoneOn ⟨h.le, hf.2⟩ ⟨h.le.trans hv.1, hv.2⟩ hv.1

theorem factorized_max_iff {P : Type*} (D : Set P) (W : Set ℝ) (L : P → ℝ)
    (B : ℝ → ℝ) {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ W)
    (hL : ∀ q ∈ D, 0 ≤ L q) (hB : ∀ v ∈ W, 0 ≤ B v)
    (hpos : ∃ q ∈ D, ∃ v ∈ W, 0 < B v * L q) :
    IsMaxOn (fun t : P × ℝ ↦ B t.2 * L t.1) (D ×ˢ W) (p, w) ↔
      IsMaxOn L D p ∧ IsMaxOn B W w := by
  constructor
  · intro H
    obtain ⟨q, hq, v, hv, hqv⟩ := hpos
    have h : 0 < B w * L p := hqv.trans_le (H (a := (q, v)) ⟨hq, hv⟩)
    have hb : 0 < B w := lt_of_le_of_ne (hB w hw) (by
      intro he
      simp [← he] at h)
    have hl : 0 < L p := lt_of_le_of_ne (hL p hp) (by
      intro he
      simp [← he] at h)
    exact ⟨fun q hq ↦ (mul_le_mul_iff_right₀ hb).mp (H (a := (q, w)) ⟨hq, hw⟩),
      fun v hv ↦ (mul_le_mul_iff_left₀ hl).mp (H (a := (p, v)) ⟨hp, hv⟩)⟩
  · rintro ⟨h₁, h₂⟩ ⟨q, v⟩ ⟨hq, hv⟩
    exact mul_le_mul (h₂ hv) (h₁ hq) (hL q hq) (hB w hw)

/-- The maximizing set of a zero-modified fit is the product of the
conditional maximizing set and the singleton empirical zero weight. -/
theorem modified_max_iff {P : Type*} {z k : ℕ} (hn : 0 < z + k)
    {D : Set P} {L : P → ℝ} (hL : ∀ p ∈ D, 0 ≤ L p)
    (hpos : ∃ p ∈ D, 0 < L p) {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1) (D ×ˢ Icc 0 1) (p, w) ↔
      IsMaxOn L D p ∧ w = (z : ℝ) / (z + (k : ℝ)) := by
  obtain ⟨q, hq, hqpos⟩ := hpos
  rw [factorized_max_iff D (Icc 0 1) L (bernoulli z k) hp hw hL]
  · rw [bernoulli_max_iff hn hw]
  · intro v hv
    exact mul_nonneg (pow_nonneg hv.1 _) (pow_nonneg (sub_nonneg.mpr hv.2) _)
  · refine ⟨q, hq, 1/2, ⟨by norm_num, by norm_num⟩, ?_⟩
    exact mul_pos (mul_pos (pow_pos (by norm_num) _) (pow_pos (by norm_num) _)) hqpos

private theorem power_max_iff {P : Type*} {D : Set P} {L : P → ℝ} {p : P} (hp : p ∈ D)
    (hL : ∀ q ∈ D, 0 ≤ L q) {k : ℕ} (hk : 0 < k) :
    IsMaxOn (fun q ↦ L q ^ k) D p ↔ IsMaxOn L D p := by
  constructor
  · intro H q hq
    change L q ≤ L p
    by_contra! h
    exact (pow_lt_pow_left₀ h (hL p hp) hk.ne').not_ge (H hq)
  · intro H q hq
    exact pow_le_pow_left₀ (hL q hq) (H hq) _

/-- Raising a positive conditional likelihood to the positive sample size
and adjoining the optimized Bernoulli factor preserves uniqueness. -/
theorem modified_unique {P : Type*} {D : Set P} {L : P → ℝ} {z k : ℕ} (hk : 0 < k)
    (hL : ∀ p ∈ D, 0 ≤ L p) (hwit : ∃ p ∈ D, 0 < L p)
    (hmax : ∃! p, p ∈ D ∧ IsMaxOn L D p) :
    ∃! t : P × ℝ, t ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1 ^ k) (D ×ˢ Icc 0 1) t := by
  obtain ⟨p, ⟨hp, hpmax⟩, huniq⟩ := hmax
  have hn : 0 < z+k := by omega
  let w := (z : ℝ) / (z + (k : ℝ))
  have hw : w ∈ Icc 0 1 := ⟨by positivity, (div_le_one (by positivity)).mpr (by simp)⟩
  have wt : ∃ q ∈ D, 0 < L q ^ k := by
    obtain ⟨q, hq, hqp⟩ := hwit
    exact ⟨q, hq, pow_pos hqp _⟩
  have he {q : P} {v : ℝ} (hq : q ∈ D) (hv : v ∈ Icc 0 1) :=
    modified_max_iff hn (fun t ht ↦ pow_nonneg (hL t ht) k) wt hq hv
  refine ⟨(p,w), ⟨⟨hp,hw⟩, (he hp hw).mpr ⟨(power_max_iff hp hL hk).mpr hpmax, rfl⟩⟩, ?_⟩
  rintro ⟨q,v⟩ ⟨⟨hq,hv⟩, hqmax⟩
  obtain ⟨hf, hvw⟩ := (he hq hv).mp hqmax
  exact Prod.ext (huniq q ⟨hq, (power_max_iff hq hL hk).mp hf⟩) hvw

theorem inflation_weight_iff {f w : ℝ} (_hf : 0 ≤ f) (hf' : f < 1) :
    (∃ rho ∈ Icc (0 : ℝ) 1, w = f + (1 - f) * rho) ↔ w ∈ Icc f 1 := by
  constructor
  · rintro ⟨rho, ⟨h₁, h₂⟩, rfl⟩
    constructor <;> nlinarith [mul_nonneg (sub_nonneg.mpr hf'.le) h₁,
      mul_nonneg (sub_nonneg.mpr hf'.le) (sub_nonneg.mpr h₂)]
  · rintro ⟨h₁, h₂⟩
    refine ⟨(w - f) / (1 - f), ⟨div_nonneg (sub_nonneg.mpr h₁) (by linarith),
      (div_le_one (by linarith)).mpr (by linarith)⟩, ?_⟩
    field_simp [show 1 - f ≠ 0 by linarith]
    ring

/-- A nonnegative log tilt of the zero carrier has exactly the
inflation weights below one that are at least the parent zero mass. -/
theorem inflation_tilt_iff {f w : ℝ} (hf : 0 < f) (hf' : f < 1) :
    (∃ d ≥ 0, w = f * exp d / ((1-f) + f * exp d)) ↔ w ∈ Ico f 1 := by
  constructor
  · rintro ⟨d, hd, rfl⟩
    have hpos : 0 < (1-f) + f * exp d := by positivity
    refine ⟨(le_div_iff₀ hpos).mpr ?_, (div_lt_one hpos).mpr (by linarith)⟩
    have H := mul_nonneg (mul_nonneg hf.le (sub_nonneg.mpr hf'.le))
      (sub_nonneg.mpr (one_le_exp_iff.mpr hd))
    nlinarith
  · rintro ⟨hfw, hw⟩
    let e := w*(1-f)/(f*(1-w))
    have hden : 0 < f*(1-w) := mul_pos hf (sub_pos.mpr hw)
    have hnum : 0 < w*(1-f) := mul_pos (hf.trans_le hfw) (sub_pos.mpr hf')
    have hE : 1 ≤ e := (le_div_iff₀ hden).mpr (by linarith)
    refine ⟨log e, log_nonneg hE, ?_⟩
    rw [exp_log (div_pos hnum hden)]
    apply (eq_div_iff (show (1-f) + f*(w*(1-f)/(f*(1-w))) ≠ 0 by positivity)).mpr
    field_simp [hf.ne', (sub_pos.mpr hw).ne']
    ring

/-- The ordinary mean fit is a global zero-inflation boundary optimum
if its parent zero mass is at least the empirical zero fraction.
The comparison is in the common count and zero log-tilt coordinates. -/
theorem fixed_carrier_inflation_boundary {P : ℕ → ℝ} {m z : ℝ}
    (hP : ∀ k, 0 ≤ P k) (h0 : 0 < P 0)
    (hMass : HasSum P 1) (hMean : HasSum (fun k : ℕ ↦ P k * k) m)
    (hz : z ≤ P 0) (a d : ℝ) (hd : 0 ≤ d)
    (hs : Summable (fun k : ℕ ↦ P k * exp (a * k + if k = 0 then d else 0))) :
    a*m + z*d ≤ log (∑' k : ℕ, P k * exp (a * k + if k = 0 then d else 0)) := by
  let v (k : ℕ) := a * (k : ℝ) + if k = 0 then d else 0
  let C := a*m + P 0*d
  have hz' : a*m + z*d ≤ C := add_le_add_right (mul_le_mul_of_nonneg_right hz hd) _
  suffices hC : C ≤ log (∑' k : ℕ, P k * exp (v k)) from hz'.trans hC
  have hZ : 0 < ∑' k : ℕ, P k * exp (v k) :=
    (mul_pos h0 (exp_pos _)).trans_le (hs.le_tsum 0 fun k _ ↦ mul_nonneg (hP k) (exp_pos _).le)
  have hlin : HasSum (fun k ↦ P k * (1 + (v k - C))) 1 := by
    have ha : HasSum (fun k : ℕ ↦ a * (P k * k)) (a*m) := hMean.mul_left a
    have hd' : HasSum (fun k : ℕ ↦ if k = 0 then P 0*d else 0) (P 0*d) := by
      simpa using hasSum_ite_eq 0 (P 0*d)
    have H := (((hMass.add ha).add hd').sub (hMass.mul_right C))
    have hC : 1+a*m+P 0*d-1*C = 1 := by dsimp [C]; ring
    rw [hC] at H
    convert H using 1
    ext k
    by_cases hk : k = 0 <;> simp [v, hk] <;> ring
  have hExp : Summable (fun k ↦ P k * exp (v k - C)) := by
    simpa only [exp_sub, ← mul_div_assoc] using hs.div_const (exp C)
  have hcomp := hlin.summable.tsum_le_tsum
    (fun k ↦ mul_le_mul_of_nonneg_left (show 1 + (v k - C) ≤ exp (v k - C) by
      simpa only [add_comm (1 : ℝ)] using add_one_le_exp (v k - C)) (hP k)) hExp
  rw [hlin.tsum_eq] at hcomp
  simp_rw [exp_sub, ← mul_div_assoc, tsum_div_const, one_le_div (exp_pos _)] at hcomp
  exact (le_log_iff_exp_le hZ).mpr hcomp

/-- A nonpositive inward first variation in every feasible direction suffices
for a global boundary maximum of a concave objective. -/
theorem concave_boundary_max {V : Type*} [AddCommGroup V] [Module ℝ V]
    {D : Set V} {L : V → ℝ} (hL : ConcaveOn ℝ D L) {p : V} (hp : p ∈ D)
    (hd : ∀ q ∈ D, ∃ s ≤ 0,
      HasDerivAt (fun t : ℝ ↦ L ((1-t) • p + t • q)) s 0) :
    IsMaxOn L D p := by
  intro q hq
  obtain ⟨s, hs, hder⟩ := hd q hq
  have H : ∀ᶠ t : ℝ in nhdsWithin 0 (Ioi 0), L q - L p ≤
      (L ((1-t) • p + t • q) - L p) / t := by
    have h : Ioo (0 : ℝ) 1 ∈ nhdsWithin 0 (Ioi 0) :=
      inter_mem_nhdsWithin _ (Iio_mem_nhds zero_lt_one)
    filter_upwards [h] with t ht
    have hJ := hL.2 hp hq (show 0 ≤ 1-t by linarith [ht.2]) ht.1.le (by ring)
    rw [le_div_iff₀ ht.1]
    simp only [smul_eq_mul] at hJ
    nlinarith
  have ht : Filter.Tendsto (fun t : ℝ ↦
      (L ((1-t) • p + t • q) - L p) / t) (nhdsWithin 0 (Ioi 0)) (nhds s) := by
    simpa [div_eq_inv_mul] using hder.tendsto_slope_zero_right
  exact sub_nonpos.mp ((ge_of_tendsto ht H).trans hs)

end

end UnivariateML.Truncation

end
