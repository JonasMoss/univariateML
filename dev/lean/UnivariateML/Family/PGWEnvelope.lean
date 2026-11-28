module

public import UnivariateML.Family.Base9PGW
public import UnivariateML.Generic.Compactification
public import Mathlib.Analysis.Calculus.DSlope
public import Mathlib.Analysis.SpecialFunctions.Complex.Analytic

/-! Joint superlevel bounds in log-observation coordinates for PGW. -/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.PGWEnvelope

noncomputable section

variable {n : ℕ}

def softplus (z : ℝ) : ℝ := log (1 + exp z)

def logistic (z : ℝ) : ℝ := exp z / (1 + exp z)

def loss (u : ℝ) : ℝ := u + 1 - exp u

def logProduct (y : Fin n → ℝ) (b c μ : ℝ) : ℝ :=
  ∑ i, (log (b * c * logistic (b * (y i - μ))) +
    loss (c * softplus (b * (y i - μ))))

private theorem softplus_pos (z : ℝ) : 0 < softplus z :=
  log_pos (by linarith [exp_pos z])

private theorem logistic_pos (z : ℝ) : 0 < logistic z := by
  unfold logistic
  positivity

private theorem logistic_le_one (z : ℝ) : logistic z ≤ 1 := by
  apply (div_le_one (by positivity)).mpr
  linarith

private theorem logistic_le_softplus (z : ℝ) : logistic z ≤ softplus z := by
  have hp : 0 < 1 + exp z := by positivity
  have h := log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [log_inv] at h
  unfold logistic softplus
  have he : exp z / (1 + exp z) = 1 - (1 + exp z)⁻¹ := by field_simp; ring
  linarith

private theorem loss_bound {u : ℝ} (hu : 0 ≤ u) : loss u ≤ -(u ^ 2 / 2) := by
  unfold loss
  linarith [quadratic_le_exp_of_nonneg hu]

private theorem point_bounds {b c z : ℝ} (hb : 0 < b) (hc : 0 < c) :
    let u := c * softplus z
    let f := log (b * c * logistic z) + loss u
    f ≤ log b ∧ f ≤ log b + log u ∧ f ≤ log b - log u ∧ f ≤ log (b * c) := by
  dsimp only
  let u := c * softplus z
  have hu : 0 < u := mul_pos hc (softplus_pos z)
  have hlog := log_le_log (mul_pos (mul_pos hb hc) (logistic_pos z))
    (show b * c * logistic z ≤ b * u by
      rw [mul_assoc]
      exact mul_le_mul_of_nonneg_left
        (mul_le_mul_of_nonneg_left (logistic_le_softplus z) hc.le) hb.le)
  rw [log_mul hb.ne' hu.ne'] at hlog
  have hk := log_le_log (mul_pos (mul_pos hb hc) (logistic_pos z))
    (mul_le_of_le_one_right (mul_pos hb hc).le (logistic_le_one z))
  have hl := log_le_sub_one_of_pos (show 0 < u ^ 2 / 2 by positivity)
  rw [log_div (pow_pos hu 2).ne' two_ne_zero, log_pow] at hl
  have h₂ : log 2 ≤ 1 := by
    linarith [log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 2)]
  have he := loss_bound hu.le
  exact ⟨by nlinarith only [hlog, he, hl, h₂, sq_nonneg u],
    by nlinarith only [hlog, he, sq_nonneg u],
    by linarith only [hlog, he, hl, h₂],
    by nlinarith only [hk, he, sq_nonneg u]⟩

/-- The log-observation objective differs from the physical one by the log Jacobian. -/
theorem physical_eq {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b c a : ℝ} (hb : 0 < b) (hc : 0 < c) (ha : 0 < a) :
    logProduct (fun i ↦ log (x i)) b c (log a) =
      ShapeFacesPGW.logLik x b a c + ∑ i, log (x i) := by
  unfold logProduct ShapeFacesPGW.logLik
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  let z := b * (log (x i) - log a)
  have hz : (x i / a) ^ b = exp z := by
    rw [rpow_def_of_pos (div_pos (hx i) ha), log_div (hx i).ne' ha.ne', mul_comm]
  have he : 0 < 1 + exp z := by positivity
  unfold ShapeFacesPGW.logDensity loss logistic softplus
  rw [hz, log_div (hx i).ne' ha.ne']
  change log (b * c * (exp z / (1 + exp z))) + _ =
    log b + log c - log (x i) + z + (c - 1) * log (1 + exp z) -
      ((1 + exp z) ^ c - 1) + log (x i)
  rw [log_mul (mul_pos hb hc).ne' (div_pos (exp_pos z) he).ne',
    log_mul hb.ne' hc.ne', log_div (exp_pos z).ne' he.ne', log_exp,
    rpow_def_of_pos he, mul_comm (log (1 + exp z)) c]
  ring

private theorem single_bound {f : Fin n → ℝ} {C : ℝ} (h : ∀ i, f i ≤ C) (j : Fin n) :
    (∑ i, f i) ≤ (n : ℝ) * C - C + f j := by
  have he := Finset.single_le_sum (f := fun i ↦ C - f i)
    (fun i _ ↦ sub_nonneg.mpr (h i)) (Finset.mem_univ j)
  rw [Finset.sum_sub_distrib] at he
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at he
  linarith

theorem logProduct_le {y : Fin n → ℝ} {b c μ : ℝ}
    (hb : 0 < b) (hc : 0 < c) :
    logProduct y b c μ ≤ (n : ℝ) * log b ∧
      logProduct y b c μ ≤ (n : ℝ) * log (b * c) := by
  constructor
  · simpa [logProduct] using Finset.sum_le_sum
      (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦
        (point_bounds (z := b * (y i - μ)) hb hc).1)
  · simpa [logProduct] using Finset.sum_le_sum
      (fun i (_ : i ∈ (Finset.univ : Finset (Fin n))) ↦
        (point_bounds (z := b * (y i - μ)) hb hc).2.2.2)

/-- A log-product superlevel with bounded shape has a bounded nuisance chart.
The two rates stay positive and the inverse exponent may reach zero. The
bounds are uniform in all three physical parameters. -/
theorem bounded_shape_envelope (hn : 0 < n) {y : Fin n → ℝ} (j : Fin n)
    {b c μ B M : ℝ} (hb : 0 < b) (hc : 0 < c) (hB : b ≤ B)
    (hM : M ≤ logProduct y b c μ) :
    let m := exp (M / n)
    let C := (n : ℝ) * log B - M
    b ∈ Icc m B ∧ m ≤ b * c ∧ c⁻¹ ∈ Icc 0 (B / m) ∧
      c * exp (b * (y j - μ)) ∈ Icc (exp (-C)) (exp C * exp (exp (2 * C))) := by
  dsimp only
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let C := (n : ℝ) * log B - M
  let m := exp (M / n)
  have hm : 0 < m := exp_pos _
  have hBR : 0 < B := hb.trans_le hB
  obtain ⟨h₁, h₂⟩ := logProduct_le (y := y) (μ := μ) hb hc
  have hl (v : ℝ) (hv : 0 < v) (h : M ≤ (n : ℝ) * log v) : m ≤ v := by
    rw [← exp_log hv]
    exact exp_le_exp.mpr ((div_le_iff₀ hnR).mpr (by linarith))
  have hmb := hl b hb (hM.trans h₁)
  have hmk := hl (b * c) (mul_pos hb hc) (hM.trans h₂)
  have hbc : c⁻¹ ≤ B / m := by
    apply (le_div_iff₀ hm).mpr
    have h := mul_le_mul_of_nonneg_left hmk (inv_nonneg.mpr hc.le)
    rw [show c⁻¹ * (b * c) = b by field_simp] at h
    exact h.trans hB
  refine ⟨⟨hmb, hB⟩, hmk, ⟨inv_nonneg.mpr hc.le, hbc⟩, ?_⟩
  let z := b * (y j - μ)
  let t := exp z
  let u := c * softplus z
  have ht : 0 < t := exp_pos _
  have hu : 0 < u := mul_pos hc (softplus_pos z)
  have hC : 0 ≤ C := by
    dsimp [C]
    exact sub_nonneg.mpr ((hM.trans h₁).trans
      (mul_le_mul_of_nonneg_left (log_le_log hb hB) hnR.le))
  let f := fun i ↦ log (b * c * logistic (b * (y i - μ))) +
    loss (c * softplus (b * (y i - μ)))
  have hsingle := single_bound (f := f)
    (fun i ↦ (point_bounds (z := b * (y i - μ)) hb hc).1) j
  have hbM : M ≤ (n : ℝ) * log b - log b + f j := hM.trans hsingle
  obtain ⟨_, hfl, hfu, hfc⟩ := point_bounds (z := z) hb hc
  have hlB := mul_le_mul_of_nonneg_left (log_le_log hb hB) hnR.le
  have hlu : exp (-C) ≤ u := by
    rw [← exp_log hu, exp_le_exp]
    dsimp [C]
    change f j ≤ log b + log u at hfl
    linarith
  have huu : u ≤ exp C := by
    rw [← exp_log hu, exp_le_exp]
    dsimp [C]
    change f j ≤ log b - log u at hfu
    linarith
  have hi : c⁻¹ ≤ exp C := by
    rw [← exp_log (inv_pos.mpr hc), exp_le_exp, log_inv]
    rw [log_mul hb.ne' hc.ne'] at hfc
    change f j ≤ log b + log c at hfc
    dsimp [C]
    linarith
  have hut : u ≤ c * t := by
    apply mul_le_mul_of_nonneg_left _ hc.le
    have h := log_le_sub_one_of_pos (show 0 < 1 + t by positivity)
    change log (1 + t) ≤ t
    linarith
  have hE : exp (c⁻¹ * u) = 1 + t := by
    dsimp [u, softplus]
    rw [← mul_assoc, inv_mul_cancel₀ hc.ne', one_mul, exp_log (by positivity)]
  have htu : c * t ≤ u * exp (c⁻¹ * u) := by
    have h := mul_le_mul_of_nonneg_left (add_one_le_exp (-(c⁻¹ * u)))
      (exp_pos (c⁻¹ * u)).le
    rw [← exp_add, add_neg_cancel, exp_zero] at h
    have he : c * (c⁻¹ * u) = u := by rw [← mul_assoc, mul_inv_cancel₀ hc.ne', one_mul]
    have hh := mul_le_mul_of_nonneg_left h hc.le
    rw [show c * (exp (c⁻¹ * u) * (-(c⁻¹ * u) + 1)) =
      (c - u) * exp (c⁻¹ * u) by
        calc
          _ = (c - c * (c⁻¹ * u)) * exp (c⁻¹ * u) := by ring
          _ = _ := by rw [he]] at hh
    linarith only [hh, congrArg (c * ·) hE]
  have htu' : u * exp (c⁻¹ * u) ≤ exp C * exp (exp (2 * C)) := by
    apply mul_le_mul huu _ (exp_pos _).le (exp_pos _).le
    apply exp_le_exp.mpr
    rw [show 2 * C = C + C by ring, exp_add]
    exact mul_le_mul hi huu hu.le (exp_pos _).le
  exact ⟨hlu.trans hut, htu.trans htu'⟩

private theorem softplus_upper (z : ℝ) : softplus z ≤ exp z := by
  unfold softplus
  linarith [log_le_sub_one_of_pos (show 0 < 1 + exp z by positivity)]

private theorem softplus_lower (z : ℝ) : z ≤ softplus z := by
  conv_lhs => rw [← log_exp z]
  exact log_le_log (exp_pos z) (by linarith)

private theorem logistic_le_exp (z : ℝ) : logistic z ≤ exp z := by
  unfold logistic
  apply div_le_self (exp_pos z).le
  linarith [exp_pos z]

private theorem softplus_ratio {s t d : ℝ} (hd : 0 < d) (hst : t - s = d)
    (hs : s ≤ -d / 2) : log (softplus s) - log (softplus t) ≤ log 2 - d / 2 := by
  have h₁ : log (softplus s) ≤ s := by
    simpa using log_le_log (softplus_pos s) (softplus_upper s)
  have hl (z : ℝ) (hz : z ≤ 0) : z - log 2 ≤ log (softplus z) := by
    have he : exp z ≤ 1 := by simpa using exp_le_exp.mpr hz
    have h : exp z / 2 ≤ softplus z :=
      (div_le_div_of_nonneg_left (exp_pos z).le (by positivity) (by linarith)).trans
        (logistic_le_softplus z)
    simpa [log_div (exp_pos z).ne' two_ne_zero] using
      log_le_log (show 0 < exp z / 2 by positivity) h
  by_cases ht : t ≤ 0
  · linarith [hl t ht]
  · have h : softplus 0 ≤ softplus t := by
      apply log_le_log (by positivity)
      linarith [exp_le_exp.mpr (le_of_not_ge ht)]
    linarith [hl 0 le_rfl, log_le_log (softplus_pos 0) h]

private theorem pair_bound {f : Fin n → ℝ} {C : ℝ} (h : ∀ i, f i ≤ C)
    {i j : Fin n} (hij : i ≠ j) :
    (∑ l, f l) ≤ (n : ℝ) * C - 2 * C + f i + f j := by
  have hs := Finset.sum_le_sum_of_subset_of_nonneg
    (f := fun l ↦ C - f l)
    (show ({i, j} : Finset (Fin n)) ⊆ Finset.univ from Finset.subset_univ _)
    (fun l _ _ ↦ sub_nonneg.mpr (h l))
  simp [Finset.sum_insert, hij, Finset.sum_sub_distrib] at hs
  linarith

/-- For nonconstant data at a positive likelihood superlevel, large shape
forces the product of shape and exponent into a compact positive interval,
keeps the minimum logistic factor away from zero, and bounds the largest
powered coordinate. -/
theorem large_shape_envelope {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    (M : ℝ) :
    ∃ B K U : ℝ, 0 < B ∧ 0 < K ∧ 0 < U ∧
      ∀ {b c μ : ℝ}, B < b → 0 < c → M ≤ logProduct y b c μ →
        exp (M / n) ≤ b * c ∧ b * c ≤ K ∧
        M - (n : ℝ) * log K ≤ log (logistic (b * (y i - μ))) ∧
        c * softplus (b * (y j - μ)) ≤ U := by
  have hn : 0 < n := Fin.pos i
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let D := y j - y i
  have hD : 0 < D := sub_pos.mpr hij
  let A := D / (4 * n)
  have hA : 0 < A := by positivity
  let B := max 1 (4 * (log 2 - (n : ℝ) * (1 + log A) - M) / D)
  let K := max 1 (max (16 * n / D ^ 2) (8 * (|M| + 1) / D ^ 2))
  have hB : 0 < B := zero_lt_one.trans_le (le_max_left ..)
  have hK : 0 < K := zero_lt_one.trans_le (le_max_left ..)
  let U := 1 + 2 * (|(n : ℝ) * log K - M| + 1)
  have hU : 0 < U := by positivity
  refine ⟨B, K, U, hB, hK, hU, ?_⟩
  intro b c μ hbB hc hM
  have hb := hB.trans hbB
  let z := fun l ↦ b * (y l - μ)
  let u := fun l ↦ c * softplus (z l)
  let k := b * c
  have hk : 0 < k := mul_pos hb hc
  let f := fun l ↦ log (k * logistic (z l)) + loss (u l)
  have hpt (l : Fin n) := point_bounds (z := z l) hb hc
  have hzu (l : Fin n) : 0 < u l := mul_pos hc (softplus_pos _)
  have hdiff : z j - z i = b * D := by dsimp [z, D]; ring
  have hz : - (b * D) / 2 < z i := by
    by_contra! hz
    have hratio := softplus_ratio (mul_pos hb hD) hdiff hz
    have hi : log (u i) = log c + log (softplus (z i)) :=
      log_mul hc.ne' (softplus_pos _).ne'
    have hj : log (u j) = log c + log (softplus (z j)) :=
      log_mul hc.ne' (softplus_pos _).ne'
    have hne : i ≠ j := fun he ↦ hij.ne (congrArg y he)
    have hpair := pair_bound (f := f) (fun l ↦ (hpt l).1) hne
    have hM' : M ≤ ∑ l, f l := hM
    have hfi : f i ≤ log b + log (u i) := (hpt i).2.1
    have hfj : f j ≤ log b - log (u j) := (hpt j).2.2.1
    have hlog := log_le_sub_one_of_pos (mul_pos hA hb)
    rw [log_mul hA.ne' hb.ne'] at hlog
    have hlog' := mul_le_mul_of_nonneg_left hlog hnR.le
    have hAeq : (n : ℝ) * (A * b) = b * D / 4 := by dsimp [A]; field_simp
    rw [mul_sub, mul_one, hAeq] at hlog'
    have hb' := (le_max_right (1 : ℝ) _).trans_lt hbB
    have hb'' := (div_lt_iff₀ hD).mp hb'
    linarith
  have huz : k * D / 2 ≤ u j := by
    have hh : b * D / 2 ≤ softplus (z j) := by linarith [softplus_lower (z j)]
    have he := mul_le_mul_of_nonneg_left hh hc.le
    change _ ≤ c * softplus (z j)
    unfold k
    linarith
  have hsingle (l : Fin n) :
      M ≤ (n : ℝ) * log k - log k + f l :=
    hM.trans (single_bound (f := f) (fun v ↦ (hpt v).2.2.2) l)
  have hfJ : f j ≤ log k - u j ^ 2 / 2 := by
    have h := log_le_log (mul_pos hk (logistic_pos (z j)))
      (mul_le_of_le_one_right hk.le (logistic_le_one (z j)))
    dsimp [f]
    linarith [loss_bound (hzu j).le]
  have hquad : M ≤ (n : ℝ) * log k - (k * D) ^ 2 / 8 := by
    have hsq : (k * D / 2) ^ 2 ≤ u j ^ 2 := by gcongr
    linarith [hsingle j]
  have hkK : k ≤ K := by
    by_contra! h
    have h₁ : 16 * n / D ^ 2 < k :=
      (le_max_left _ _).trans_lt ((le_max_right _ _).trans_lt h)
    have h₂ : 8 * (|M| + 1) / D ^ 2 < k :=
      (le_max_right _ _).trans_lt ((le_max_right _ _).trans_lt h)
    have h₃ : 1 < k := (le_max_left _ _).trans_lt h
    have h₁' := (div_lt_iff₀ (sq_pos_of_pos hD)).mp h₁
    have h₂' := (div_lt_iff₀ (sq_pos_of_pos hD)).mp h₂
    have ha := mul_lt_mul_of_pos_right h₁' hk
    have hh := mul_lt_mul_of_pos_left h₃ (mul_pos hk (sq_pos_of_pos hD))
    have hl := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hk) hnR.le
    nlinarith only [ha, hh, h₂', hl, hquad, neg_abs_le M, hnR.le]
  have hlogK := mul_le_mul_of_nonneg_left (log_le_log hk hkK) hnR.le
  have hi : f i ≤ log k + log (logistic (z i)) := by
    dsimp [f]
    rw [log_mul hk.ne' (logistic_pos _).ne']
    nlinarith only [loss_bound (hzu i).le, sq_nonneg (u i)]
  have hmin : exp (M / n) ≤ k := by
    rw [← exp_log hk, exp_le_exp]
    apply (div_le_iff₀ hnR).mpr
    rw [mul_comm]
    exact hM.trans (logProduct_le (y := y) (μ := μ) hb hc).2
  refine ⟨hmin, hkK, by linarith [hsingle i], ?_⟩
  by_contra! h
  have hU' : 1 < u j := by dsimp [U] at h; linarith [abs_nonneg ((n : ℝ) * log K - M)]
  have hsq := mul_lt_mul_of_pos_right hU' (hzu j)
  dsimp [U] at h
  linarith [hsingle j, le_abs_self ((n : ℝ) * log K - M)]


def thresholdProduct (y : Fin n → ℝ) (s k : ℝ) : ℝ :=
  ∑ i, (log k + loss (k * (y i - s)))

private theorem loss_antitone {v u : ℝ} (hv : 0 ≤ v) (hvu : v ≤ u) :
    loss u ≤ loss v := by
  have he := add_one_le_exp (u - v)
  have hm := mul_le_mul_of_nonneg_left he (exp_pos v).le
  have hv' : 1 ≤ exp v := by simpa using exp_le_exp.mpr hv
  have hh := mul_le_mul_of_nonneg_right hv' (sub_nonneg.mpr hvu)
  rw [mul_comm, ← exp_add, show v + (u - v) = u by ring] at hm
  unfold loss
  nlinarith only [hm, hh]

private theorem loss_shift_bound {u v τ A : ℝ}
    (_hu : 0 ≤ u) (hv : 0 ≤ v) (hτ : 0 ≤ τ) (huv : v - τ ≤ u) (hA : v ≤ A) :
    loss u ≤ loss v + τ * exp A := by
  by_cases he : v ≤ u
  · exact (loss_antitone hv he).trans (le_add_of_nonneg_right (by positivity))
  have hvu : u ≤ v := le_of_not_ge he
  have he' := mul_le_mul_of_nonneg_right (add_one_le_exp (u - v)) (exp_pos v).le
  rw [← exp_add, sub_add_cancel] at he'
  have hs := mul_le_mul (show v - u ≤ τ by linarith)
    (exp_le_exp.mpr hA) (exp_pos v).le hτ
  unfold loss
  linarith only [he', hs, hvu,
    show (u - v + 1) * exp v = exp v - (v - u) * exp v by ring]

/-- At large shape, every competitive point lies strictly below the
closed threshold envelope at its own product rate. Only one observation at
the minimum is needed for the strict bound, so tied minima are included. -/
theorem strict_threshold_tail {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j)
    (M : ℝ) :
    ∃ B > 0, ∀ {b c μ : ℝ}, B < b → 0 < c → M ≤ logProduct y b c μ →
      logProduct y b c μ < thresholdProduct y (y i) (b * c) := by
  obtain ⟨B₀, K, U, hB₀, hK, _, henv⟩ := large_shape_envelope hij M
  let C := |M - (n : ℝ) * log K|
  let A := K * (y j - y i)
  have h₂ : 0 < log 2 := log_pos (by norm_num)
  let B := max B₀ ((n : ℝ) * K * C * exp A / log 2)
  have hB : 0 < B := hB₀.trans_le (le_max_left ..)
  refine ⟨B, hB, ?_⟩
  intro b c μ hb hc hM
  have hb' := hB.trans hb
  let z := fun l ↦ b * (y l - μ)
  let u := fun l ↦ c * softplus (z l)
  let k := b * c
  let v := fun l ↦ k * (y l - y i)
  have hk : 0 < k := mul_pos hb' hc
  have henv' := henv ((le_max_left _ _).trans_lt hb) hc hM
  have hK' : k ≤ K := henv'.2.1
  have hz : -C ≤ z i := by
    have h := log_le_log (logistic_pos (z i)) (logistic_le_exp (z i))
    rw [log_exp] at h
    linarith only [h, henv'.2.2.1, neg_abs_le (M - (n : ℝ) * log K)]
  have hv₀ (l : Fin n) : 0 ≤ v l := mul_nonneg hk.le (sub_nonneg.mpr (hmin l))
  have hu₀ (l : Fin n) : 0 ≤ u l := (mul_pos hc (softplus_pos _)).le
  have hvA (l : Fin n) : v l ≤ A := by
    exact mul_le_mul hK' (sub_le_sub_right (hmax l) _) (sub_nonneg.mpr (hmin l)) hK.le
  have hlinear (l : Fin n) : v l + c * z i ≤ u l := by
    have h := mul_le_mul_of_nonneg_left (softplus_lower (z l)) hc.le
    change _ ≤ u l at h
    convert h using 1
    dsimp [v, z, k]
    ring
  have hrate (l : Fin n) : log (k * logistic (z l)) ≤ log k :=
    log_le_log (mul_pos hk (logistic_pos _))
      (mul_le_of_le_one_right hk.le (logistic_le_one _))
  by_cases hz₀ : 0 ≤ z i
  · unfold logProduct thresholdProduct
    apply Finset.sum_lt_sum
    · intro l _
      have h := hlinear l
      have h' := mul_nonneg hc.le hz₀
      exact add_le_add (hrate l) (loss_antitone (hv₀ l) (by linarith))
    · refine ⟨i, Finset.mem_univ _, ?_⟩
      have hq : logistic (z i) < 1 := (div_lt_one (by positivity)).mpr (by linarith)
      have h := hlinear i
      have h' := mul_nonneg hc.le hz₀
      exact add_lt_add_of_lt_of_le
        (log_lt_log (mul_pos hk (logistic_pos _)) (mul_lt_of_lt_one_right hk hq))
        (loss_antitone (hv₀ i) (by linarith))
  · let τ := K * C / b
    have hτ : 0 ≤ τ := by positivity
    have hck : c ≤ K / b := (le_div_iff₀ hb').mpr (by simpa [mul_comm] using hK')
    have hcC : c * C ≤ τ := by
      simpa [τ, div_mul_eq_mul_div, mul_comm C] using
        mul_le_mul_of_nonneg_right hck (abs_nonneg (M - (n : ℝ) * log K))
    have hshift (l : Fin n) : loss (u l) ≤ loss (v l) + τ * exp A := by
      apply loss_shift_bound (hu₀ l) (hv₀ l) hτ _ (hvA l)
      have h := mul_le_mul_of_nonneg_left hz hc.le
      linarith only [h, hlinear l, hcC, mul_neg c C]
    have hq : logistic (z i) ≤ 1 / 2 := by
      have he : exp (z i) ≤ 1 := by simpa using exp_le_exp.mpr (le_of_not_ge hz₀)
      apply (div_le_iff₀ (by positivity)).mpr
      linarith
    have hi := log_le_log (mul_pos hk (logistic_pos (z i)))
      (mul_le_mul_of_nonneg_left hq hk.le)
    rw [log_mul hk.ne' (by norm_num : (1 : ℝ) / 2 ≠ 0), log_div one_ne_zero two_ne_zero,
      log_one, zero_sub] at hi
    have hsum := single_bound
      (f := fun l ↦ log (k * logistic (z l)) - log k)
      (C := 0) (fun l ↦ sub_nonpos.mpr (hrate l)) i
    simp only [mul_zero, sub_zero, zero_add, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hsum
    have hsum' := Finset.sum_le_sum (s := (Finset.univ : Finset (Fin n)))
      (fun l _ ↦ hshift l)
    simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul] at hsum'
    have hb₁ := (le_max_right _ _).trans_lt hb
    have hsmall : (n : ℝ) * (τ * exp A) < log 2 := by
      dsimp [τ]
      rw [div_mul_eq_mul_div, ← mul_div_assoc, div_lt_iff₀ hb']
      calc
        _ = (n : ℝ) * K * C * exp A := by ring
        _ < _ := (div_lt_iff₀ h₂).mp hb₁ |>.trans_eq (mul_comm b _)
    unfold logProduct thresholdProduct
    simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    linarith only [hsum, hsum', hi, hsmall]


private theorem softplus_max_bound (z : ℝ) :
    0 ≤ softplus z - max z 0 ∧ softplus z - max z 0 ≤ log 2 := by
  constructor
  · exact sub_nonneg.mpr (max_le (softplus_lower z) (softplus_pos z).le)
  · by_cases hz : 0 ≤ z
    · rw [max_eq_left hz]
      have h : 1 + exp z ≤ 2 * exp z := by
        have : 1 ≤ exp z := by simpa using exp_le_exp.mpr hz
        linarith
      have he := log_le_log (show 0 < 1 + exp z by positivity) h
      rw [log_mul two_ne_zero (exp_pos z).ne', log_exp] at he
      exact sub_le_iff_le_add'.mpr (by simpa [softplus, add_comm] using he)
    · rw [max_eq_right (le_of_not_ge hz), sub_zero]
      apply log_le_log (by positivity)
      have : exp z ≤ 1 := by simpa using exp_le_exp.mpr (le_of_not_ge hz)
      linarith

private theorem logistic_tendsto : Tendsto logistic atTop (𝓝 1) := by
  have h := tendsto_exp_atBot.comp tendsto_neg_atTop_atBot
  have he : ∀ z : ℝ, logistic z = (1 + exp (-z))⁻¹ := by
    intro z
    unfold logistic
    rw [exp_neg]
    field_simp [ne_of_gt (exp_pos z), ne_of_gt (show 0 < 1 + exp z by positivity)]; ring
  rw [show logistic = fun z ↦ (1 + exp (-z))⁻¹ from funext he]
  simpa [he, Function.comp_def] using (h.const_add 1).inv₀ (by norm_num)

/-- The large-shape face has an exact contact penalty. Every tied minimum
has the same limiting logistic factor, while all strict excesses have
factor one. The statement applies to moving scales as well as fixed scales. -/
theorem threshold_contact {ι : Type*} {l : Filter ι}
    {y : Fin n → ℝ} {i : Fin n} (hmin : ∀ j, y i ≤ y j)
    {b c μ : ι → ℝ} {s k ρ : ℝ} (hs : s ≤ y i) (hk : 0 < k) (hρ : 0 < ρ)
    (hb : Tendsto b l atTop) (hbc : Tendsto (fun t ↦ b t * c t) l (𝓝 k))
    (hμ : Tendsto μ l (𝓝 s))
    (hcontact : s = y i → Tendsto (fun t ↦ logistic (b t * (y i - μ t))) l (𝓝 ρ)) :
    Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) l
      (𝓝 (thresholdProduct y s k +
        if s = y i then ((Finset.univ.filter fun j ↦ y j = y i).card : ℝ) * log ρ else 0)) := by
  classical
  let z := fun (j : Fin n) t ↦ b t * (y j - μ t)
  let u := fun (j : Fin n) t ↦ c t * softplus (z j t)
  have hb₀ : ∀ᶠ t in l, 0 < b t := hb.eventually (eventually_gt_atTop 0)
  have hc : Tendsto c l (𝓝 0) := by
    have h := hbc.mul (tendsto_inv_atTop_zero.comp hb)
    rw [mul_zero] at h
    apply h.congr'
    filter_upwards [hb₀] with t ht
    simp only [Function.comp_apply]
    field_simp
  have hu (j : Fin n) : Tendsto (u j) l (𝓝 (k * (y j - s))) := by
    have hmain : Tendsto (fun t ↦ (b t * c t) * max (y j - μ t) 0) l
        (𝓝 (k * (y j - s))) := by
      have hs' := sub_nonneg.mpr (hs.trans (hmin j))
      simpa [max_comm (0 : ℝ), max_eq_left hs'] using
        hbc.mul ((hμ.const_sub (y j)).max (tendsto_const_nhds (x := (0 : ℝ))))
    have he : Tendsto (fun t ↦ c t * (softplus (z j t) - max (z j t) 0)) l (𝓝 0) := by
      have h := (hc.abs.mul (tendsto_const_nhds (x := log 2)))
      simp only [abs_zero, zero_mul] at h
      refine squeeze_zero_norm' (a := fun t ↦ |c t| * log 2) ?_ h
      filter_upwards with t
      rw [Real.norm_eq_abs, abs_mul, abs_of_nonneg (softplus_max_bound _).1]
      exact mul_le_mul_of_nonneg_left (softplus_max_bound _).2 (abs_nonneg _)
    have h := hmain.add he
    rw [add_zero] at h
    apply h.congr'
    filter_upwards [hb₀] with t ht
    have hmax' : max (z j t) 0 = b t * max (y j - μ t) 0 := by
      dsimp [z]
      simpa only [mul_zero] using (mul_max_of_nonneg (y j - μ t) 0 ht.le).symm
    rw [hmax']
    dsimp [u]
    ring
  let R := fun j ↦ if s = y i ∧ y j = y i then ρ else 1
  have hR₀ (j : Fin n) : 0 < R j := by dsimp [R]; split <;> positivity
  have hR (j : Fin n) : Tendsto (fun t ↦ logistic (z j t)) l (𝓝 (R j)) := by
    by_cases he : s = y i ∧ y j = y i
    · simpa [R, he, z, he.2] using hcontact he.1
    · have hlt : s < y j := by
        rcases (hs.trans (hmin j)).lt_or_eq with h | h
        · exact h
        · exact False.elim (he ⟨le_antisymm hs (h ▸ hmin j), le_antisymm (h ▸ hs) (hmin j)⟩)
      have hz : Tendsto (z j) l atTop :=
        (hμ.const_sub (y j)).pos_mul_atTop (sub_pos.mpr hlt) hb |>.congr (by
          intro t
          simp [z, mul_comm])
      simpa [R, he, Function.comp_def] using logistic_tendsto.comp hz
  have hpoint (j : Fin n) : Tendsto
      (fun t ↦ log (b t * c t * logistic (z j t)) + loss (u j t)) l
      (𝓝 (log k + loss (k * (y j - s)) + log (R j))) := by
    have h := ((hbc.mul (hR j)).log (mul_pos hk (hR₀ j)).ne').add
      (((hu j).add_const 1).sub (continuous_exp.continuousAt.tendsto.comp (hu j)))
    rw [log_mul hk.ne' (hR₀ j).ne'] at h
    simpa only [loss, add_right_comm, Function.comp_apply] using h
  have h := tendsto_finsetSum Finset.univ (fun j _ ↦ hpoint j)
  rw [Finset.sum_add_distrib] at h
  change Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) l
    (𝓝 (thresholdProduct y s k + ∑ j, log (R j))) at h
  by_cases he : s = y i
  · simpa [R, he, apply_ite log, ← Finset.sum_filter] using h
  · simpa [R, he] using h


/-- Every strictly positive logistic contact factor is realized along a
moving free-scale threshold path. The closed envelope uses factor one. -/
theorem threshold_contact_reachable {y : Fin n → ℝ} {i : Fin n}
    (hmin : ∀ j, y i ≤ y j) {k ρ : ℝ} (hk : 0 < k) (hρ : ρ ∈ Ioc 0 1) :
    ∃ b c μ : ℝ → ℝ,
      (∀ᶠ t in atTop, 0 < b t) ∧ (∀ᶠ t in atTop, 0 < c t) ∧
      Tendsto b atTop atTop ∧ Tendsto μ atTop (𝓝 (y i)) ∧
      Tendsto (fun t ↦ b t * c t) atTop (𝓝 k) ∧
      Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) atTop
        (𝓝 (thresholdProduct y (y i) k +
          ((Finset.univ.filter fun j ↦ y j = y i).card : ℝ) * log ρ)) := by
  classical
  have hρ₀ : 0 < ρ := hρ.1
  by_cases he : ρ = 1
  · subst ρ
    let b := fun t : ℝ ↦ t ^ 2
    let c := fun t : ℝ ↦ k / t ^ 2
    let μ := fun t : ℝ ↦ y i - t⁻¹
    have hb : Tendsto b atTop atTop := tendsto_pow_atTop (by decide : 2 ≠ 0)
    have hμ : Tendsto μ atTop (𝓝 (y i)) := by
      simpa [μ] using tendsto_inv_atTop_zero.const_sub (y i)
    have hbc : Tendsto (fun t ↦ b t * c t) atTop (𝓝 k) := by
      apply tendsto_const_nhds.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      dsimp [b, c]
      field_simp
    have hq : Tendsto (fun t ↦ logistic (b t * (y i - μ t))) atTop (𝓝 1) := by
      apply logistic_tendsto.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      congr 1
      dsimp [b, μ]
      field_simp
      ring
    refine ⟨b, c, μ, hb.eventually (eventually_gt_atTop 0), ?_, hb, hμ, hbc, ?_⟩
    · exact (hb.eventually (eventually_gt_atTop 0)).mono fun _ ht ↦ div_pos hk ht
    · simpa using threshold_contact hmin le_rfl hk zero_lt_one hb hbc hμ (fun _ ↦ hq)
  · let z := log (ρ / (1 - ρ))
    let c := fun t : ℝ ↦ k / t
    let μ := fun t : ℝ ↦ y i - z * t⁻¹
    have hz : logistic z = ρ := by
      have hden : 0 < 1 - ρ := sub_pos.mpr (lt_of_le_of_ne hρ.2 he)
      unfold logistic z
      rw [exp_log (div_pos hρ₀ hden)]
      rw [div_eq_iff (by positivity : 1 + ρ / (1 - ρ) ≠ 0)]
      field_simp
      ring
    have hμ : Tendsto μ atTop (𝓝 (y i)) := by
      simpa [μ] using (tendsto_inv_atTop_zero.const_mul z).const_sub (y i)
    have hbc : Tendsto (fun t ↦ t * c t) atTop (𝓝 k) := by
      apply tendsto_const_nhds.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      dsimp [c]
      field_simp
    have hq : Tendsto (fun t ↦ logistic (t * (y i - μ t))) atTop (𝓝 ρ) := by
      apply tendsto_const_nhds.congr'
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      have h : t * (y i - μ t) = z := by dsimp [μ]; field_simp; ring
      rw [h, hz]
    refine ⟨id, c, μ, eventually_gt_atTop 0,
      (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ div_pos hk ht,
      tendsto_id, hμ, hbc, ?_⟩
    simpa using threshold_contact hmin le_rfl hk hρ₀ tendsto_id hbc hμ (fun _ ↦ hq)

def logSlope (z : ℝ) : ℝ := if z = 0 then 1 else log (1 + z) / z

private theorem logSlope_analytic {z : ℝ} (hz : -1 < z) : AnalyticAt ℝ logSlope z := by
  have h : AnalyticAt ℝ (fun t : ℝ ↦ log (1 + t)) z :=
    (analyticAt_const.add analyticAt_id).log (by change 0 < 1 + z; linarith)
  have hD : HasDerivAt (fun t : ℝ ↦ log (1 + t)) 1 0 := by
    simpa using ((hasDerivAt_id (0 : ℝ)).const_add 1).log
      (by simp : (1 : ℝ) + id 0 ≠ 0)
  have he : logSlope = dslope (fun t : ℝ ↦ log (1 + t)) 0 := by
    funext t
    by_cases ht : t = 0
    · subst t
      simp [logSlope, dslope_same, hD.deriv]
    · rw [logSlope, ite_eq_right ht, dslope_of_ne _ ht, slope_def_field]
      simp
  rw [he]
  exact h.dslope 0


def chartDomain : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ Ici 0)

def chart (y : Fin n → ℝ) (p : ℝ × ℝ × ℝ) : ℝ :=
  ∑ i, let t := p.2.1 * exp (p.1 * y i)
       log (p.1 * t / (1 + p.2.2 * t)) + loss (t * logSlope (p.2.2 * t))

def gompertzProduct (y : Fin n → ℝ) (b r : ℝ) : ℝ := chart y (b, r, 0)

/-- Analyticity of the unprofiled PGW likelihood at every regular chart
point, including inverse exponent zero. -/
theorem analytic_chart (y : Fin n → ℝ) {p : ℝ × ℝ × ℝ} (hp : p ∈ chartDomain) :
    AnalyticAt ℝ (chart y) p := by
  have hb : 0 < p.1 := hp.1
  have hr : 0 < p.2.1 := hp.2.1
  have hh : 0 ≤ p.2.2 := hp.2.2
  unfold chart
  apply Finset.analyticAt_fun_sum
  intro i _
  let t : ℝ × ℝ × ℝ → ℝ := fun q ↦ q.2.1 * exp (q.1 * y i)
  let w : ℝ × ℝ × ℝ → ℝ := fun q ↦ q.2.2 * t q
  have h1 : AnalyticAt ℝ (fun q : ℝ × ℝ × ℝ ↦ q.1) p := analyticAt_fst
  have h2 : AnalyticAt ℝ (fun q : ℝ × ℝ × ℝ ↦ q.2) p := analyticAt_snd
  have ht : AnalyticAt ℝ t p :=
    (analyticAt_fst.comp h2).mul (analyticAt_rexp.comp (h1.mul analyticAt_const))
  have hw : AnalyticAt ℝ w p := (analyticAt_snd.comp h2).mul ht
  have ht₀ : 0 < t p := mul_pos hr (exp_pos _)
  have hw₀ : 0 ≤ w p := mul_nonneg hh ht₀.le
  have hden : 0 < 1 + w p := by linarith
  have hlog : AnalyticAt ℝ (fun q : ℝ × ℝ × ℝ ↦ log (q.1 * t q / (1 + w q))) p := by
    have hd : AnalyticAt ℝ (fun q : ℝ × ℝ × ℝ ↦ q.1 * t q / (1 + w q)) p :=
      (h1.mul ht).div (analyticAt_const.add hw) hden.ne'
    exact (analyticAt_log (div_pos (mul_pos hb ht₀) hden)).comp_of_eq hd rfl
  have hu := ht.mul ((logSlope_analytic (show -1 < w p by linarith)).comp hw)
  exact hlog.add ((hu.add (analyticAt_const (v := (1 : ℝ)))).sub (analyticAt_rexp.comp hu))

private theorem chart_continuous (y : Fin n → ℝ) : ContinuousOn (chart y) chartDomain :=
  fun _ hp ↦ (analytic_chart y hp).continuousAt.continuousWithinAt

theorem chart_physical (y : Fin n → ℝ) {b c μ : ℝ} (hb : 0 < b) (hc : 0 < c) :
    chart y (b, c * exp (-b * μ), c⁻¹) = logProduct y b c μ := by
  unfold chart logProduct
  apply Finset.sum_congr rfl
  intro i _
  dsimp only
  let z := b * (y i - μ)
  have he : c * exp (-b * μ) * exp (b * y i) = c * exp z := by
    rw [mul_assoc, ← exp_add]
    congr 2
    dsimp [z]
    ring
  rw [he, show c⁻¹ * (c * exp z) = exp z by rw [← mul_assoc, inv_mul_cancel₀ hc.ne', one_mul]]
  have h₀ : exp z ≠ 0 := (exp_pos _).ne'
  simp only [logSlope, h₀, ite_false, softplus, logistic]
  congr 2 <;> (field_simp [ne_of_gt (show 0 < 1 + exp z by positivity)]; ring)

theorem chart_surjective (y : Fin n → ℝ) {b r h : ℝ}
    (hb : 0 < b) (hr : 0 < r) (hh : 0 < h) :
    chart y (b, r, h) = logProduct y b h⁻¹ (-(log (h * r)) / b) := by
  rw [← chart_physical y hb (inv_pos.mpr hh), inv_inv]
  congr 2
  rw [show -b * (-(log (h * r)) / b) = log (h * r) by field_simp]
  rw [exp_log (mul_pos hh hr), ← mul_assoc, inv_mul_cancel₀ hh.ne', one_mul]

private theorem chart_anchor (j : Fin n) {y : Fin n → ℝ} {b c μ B M : ℝ}
    (hn : 0 < n) (hb : 0 < b) (hc : 0 < c) (hB : b ≤ B)
    (hM : M ≤ logProduct y b c μ) :
    let m := exp (M / n)
    let C := (n : ℝ) * log B - M
    (b, c * exp (-b * μ), c⁻¹) ∈
      Icc m B ×ˢ (Icc (exp (-C - B * |y j|)) (exp (C + B * |y j|) * exp (exp (2 * C))) ×ˢ
        Icc 0 (B / m)) := by
  dsimp only
  let C := (n : ℝ) * log B - M
  obtain ⟨hbb, _, hh, hlo, hhi⟩ := bounded_shape_envelope hn j hb hc hB hM
  refine ⟨hbb, ?_, hh⟩
  have he : c * exp (-b * μ) = c * exp (b * (y j - μ)) * exp (-(b * y j)) := by
    rw [mul_assoc, ← exp_add]
    congr 2
    ring
  have habs : |b * y j| ≤ B * |y j| := by
    rw [abs_mul, abs_of_pos hb]
    exact mul_le_mul_of_nonneg_right hB (abs_nonneg _)
  rw [he]
  constructor
  · rw [exp_sub]
    have hx : exp (-(B * |y j|)) ≤ exp (-(b * y j)) :=
      exp_le_exp.mpr (by linarith [le_abs_self (b * y j)])
    rw [div_eq_mul_inv, ← exp_neg]
    exact mul_le_mul hlo hx (exp_pos _).le (mul_pos hc (exp_pos _)).le
  · rw [exp_add]
    have hx : exp (-(b * y j)) ≤ exp (B * |y j|) :=
      exp_le_exp.mpr (by linarith [neg_abs_le (b * y j)])
    calc
      _ ≤ (exp C * exp (exp (2 * C))) * exp (B * |y j|) :=
        mul_le_mul hhi hx (exp_pos _).le (by positivity)
      _ = _ := by ring

def physicalDomain : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ univ)

private theorem fixed_chart_path (y : Fin n → ℝ) {b r : ℝ} (hb : 0 < b) (hr : 0 < r) :
    Tendsto (fun h : ℝ ↦ logProduct y b h⁻¹ (-(log (h * r)) / b))
      (𝓝[>] 0) (𝓝 (gompertzProduct y b r)) := by
  have h₀ : Tendsto (fun h : ℝ ↦ (b, r, h)) (𝓝[>] 0) (𝓝 (b, r, 0)) :=
    tendsto_const_nhds.prodMk_nhds (tendsto_const_nhds.prodMk_nhds nhdsWithin_le_nhds)
  have hdom : Tendsto (fun h : ℝ ↦ (b, r, h)) (𝓝[>] 0) (𝓝[chartDomain] (b, r, 0)) := by
    apply tendsto_nhdsWithin_iff.mpr
    refine ⟨h₀, ?_⟩
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    exact ⟨hb, hr, hh.le⟩
  have ht : Tendsto (chart y) (𝓝[chartDomain] (b, r, 0)) (𝓝 (gompertzProduct y b r)) :=
    chart_continuous y (b, r, 0) ⟨hb, hr, le_rfl⟩
  apply (ht.comp hdom).congr'
  filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
  exact chart_surjective y hb hr hh

theorem faces_le_of_upper_bound {y : Fin n → ℝ} {i : Fin n}
    (hmin : ∀ l, y i ≤ y l) {M : ℝ}
    (hbound : ∀ p ∈ physicalDomain, logProduct y p.1 p.2.1 p.2.2 ≤ M) :
    (∀ b > 0, ∀ r > 0, gompertzProduct y b r ≤ M) ∧
      (∀ k > 0, thresholdProduct y (y i) k ≤ M) := by
  constructor
  · intro b hb r hr
    apply le_of_tendsto (fixed_chart_path y hb hr)
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    exact hbound _ (show (b, h⁻¹, -(log (h * r)) / b) ∈ physicalDomain from
      ⟨hb, inv_pos.mpr hh, mem_univ _⟩)
  · intro k hk
    let x := fun l ↦ exp (y l)
    have hx : ∀ l, 0 < x l := fun _ ↦ exp_pos _
    have hpath := (AllFreePGW.free_threshold_path (exp_pos (y i)) hk
      (fun l ↦ exp_le_exp.mpr (hmin l))).add_const (∑ l, log (x l))
    have htlim : (∑ l : Fin n, (log k - log (exp (y l)) +
        k * log (exp (y l) / exp (y i)) - (exp (k * log (exp (y l) / exp (y i))) - 1))) +
        ∑ l, log (x l) = thresholdProduct y (y i) k := by
      simp only [thresholdProduct, ← Finset.sum_add_distrib]
      apply Finset.sum_congr rfl
      intro l _
      simp only [log_div (exp_pos _).ne' (exp_pos _).ne', log_exp, x, loss]
      ring
    rw [htlim] at hpath
    apply le_of_tendsto hpath
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    have hc : 0 < k * h ^ 2 := mul_pos hk (sq_pos_of_pos hh)
    have hb : 0 < (h ^ 2)⁻¹ := inv_pos.mpr (sq_pos_of_pos hh)
    rw [← physical_eq hx hb hc (mul_pos (exp_pos _) (exp_pos _))]
    simp only [x, log_exp]
    exact hbound _ (show ((h ^ 2)⁻¹, k * h ^ 2, log (x i * exp (-h))) ∈ physicalDomain from
      ⟨hb, hc, mem_univ _⟩)

/-- All-free attainment for nonconstant data reduces to a finite witness
tying or beating both faces. The threshold comparison includes the closed
minimum contact. The finite stationary-list reduction is separate. -/
theorem all_free_attains_iff_witness {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j) :
    let D : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ univ)
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      ∃ p ∈ D, (∀ b > 0, ∀ r > 0, gompertzProduct y b r ≤ L p) ∧
        (∀ k > 0, thresholdProduct y (y i) k ≤ L p) := by
  intro D L
  constructor
  · rintro ⟨p, hp, hm⟩
    exact ⟨p, hp, faces_le_of_upper_bound hmin (fun _ hq ↦ hm hq)⟩
  · rintro ⟨p, hp, hG, hT⟩
    let M := L p
    obtain ⟨B₀, hB₀, htail⟩ := strict_threshold_tail hij hmin hmax M
    let B := max p.1 B₀
    let m := exp (M / n)
    let C := (n : ℝ) * log B - M
    let K : Set (ℝ × ℝ × ℝ) :=
      Icc m B ×ˢ (Icc (exp (-C - B * |y i|)) (exp (C + B * |y i|) * exp (exp (2 * C))) ×ˢ
        Icc 0 (B / m))
    let E := K ∩ {q | 0 < q.2.2}
    let w := (p.1, p.2.1 * exp (-p.1 * p.2.2), p.2.1⁻¹)
    have hw : w ∈ E := ⟨chart_anchor i (Fin.pos i) hp.1 hp.2.1 (le_max_left ..) le_rfl,
      inv_pos.mpr hp.2.1⟩
    have hwL : chart y w = M := chart_physical y hp.1 hp.2.1
    have hK : IsCompact K := isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc)
    have hkD : K ⊆ chartDomain := fun q hq ↦
      ⟨(exp_pos _).trans_le hq.1.1, (exp_pos _).trans_le hq.2.1.1, hq.2.2.1⟩
    obtain ⟨q, hq, hm⟩ := Generic.exists_isMaxOn_of_boundary_le
      (D := E) (K := K) (fun _ h ↦ h.1) hK ((chart_continuous y).mono hkD) hw
      (fun t ht ↦ by
        have htd := hkD ht.1
        have h₀ : t.2.2 = 0 := le_antisymm
          (le_of_not_gt (fun h ↦ ht.2 ⟨ht.1, h⟩)) htd.2.2
        rw [hwL]
        have h := hG t.1 htd.1 t.2.1 htd.2.1
        simpa [gompertzProduct, ← h₀] using h)
    have hqD := hkD hq.1
    let z := (q.1, q.2.2⁻¹, -(log (q.2.2 * q.2.1)) / q.1)
    have hzD : z ∈ D := ⟨hqD.1, inv_pos.mpr hq.2, mem_univ _⟩
    have hzq : L z = chart y q := (chart_surjective y hqD.1 hqD.2.1 hq.2).symm
    have hwq : M ≤ L z := by rw [hzq, ← hwL]; exact hm hw
    refine ⟨z, hzD, fun v hv ↦ ?_⟩
    change L v ≤ L z
    by_cases hM : M ≤ L v
    · have hvB : v.1 ≤ B := by
        by_contra! h
        have ht := htail ((le_max_right _ _).trans_lt h) hv.2.1 hM
        exact ht.not_ge ((hT (v.1 * v.2.1) (mul_pos hv.1 hv.2.1)).trans hM)
      let V := (v.1, v.2.1 * exp (-v.1 * v.2.2), v.2.1⁻¹)
      have hvE : V ∈ E := ⟨chart_anchor i (Fin.pos i) hv.1 hv.2.1 hvB hM,
        inv_pos.mpr hv.2.1⟩
      rw [hzq]
      change logProduct y v.1 v.2.1 v.2.2 ≤ chart y q
      rw [← chart_physical y hv.1 hv.2.1]
      exact hm hvE
    · exact (le_of_not_ge hM).trans hwq


/-- Every competitive sequence whose shape diverges has a subsequence
with a positive rate and logistic contact, and its likelihood converges
to the corresponding threshold trace. -/
theorem large_shape_subsequence {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (M : ℝ)
    {b c μ : ℕ → ℝ} (hb : Tendsto b atTop atTop)
    (hc : ∀ᶠ t in atTop, 0 < c t)
    (hM : ∀ᶠ t in atTop, M ≤ logProduct y (b t) (c t) (μ t)) :
    ∃ (s k ρ : ℝ) (φ : ℕ → ℕ), StrictMono φ ∧ s ≤ y i ∧ 0 < k ∧ ρ ∈ Ioc 0 1 ∧
      Tendsto (μ ∘ φ) atTop (𝓝 s) ∧
      Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) ∧
      Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 ρ) ∧
      Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
        (𝓝 (thresholdProduct y s k +
          if s = y i then ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) * log ρ else 0)) := by
  classical
  obtain ⟨B, K, U, hB, hK, hU, henv⟩ := large_shape_envelope hij M
  let m := exp (M / n)
  let C := |M - (n : ℝ) * log K|
  let δ := exp (M - (n : ℝ) * log K)
  let v := fun t ↦ (μ t, b t * c t, logistic (b t * (y i - μ t)))
  let S := Icc (y j - U / m) (y i + C) ×ˢ (Icc m K ×ˢ Icc δ 1)
  have hm : 0 < m := exp_pos _
  have hC : 0 ≤ C := abs_nonneg _
  have hbox : ∀ᶠ t in atTop, v t ∈ S := by
    filter_upwards [hb.eventually (eventually_gt_atTop B),
      hb.eventually (eventually_ge_atTop (1 : ℝ)), hc, hM] with t ht ht₁ hct hMt
    obtain ⟨hlo, hhi, hq, hu⟩ := henv ht hct hMt
    have hq' : δ ≤ logistic (b t * (y i - μ t)) := by
      rw [← exp_log (logistic_pos (b t * (y i - μ t)))]
      exact exp_le_exp.mpr hq
    have hz : -C ≤ b t * (y i - μ t) := by
      have h := log_le_log (logistic_pos (b t * (y i - μ t))) (logistic_le_exp _)
      rw [log_exp] at h
      linarith only [h, hq, neg_abs_le (M - (n : ℝ) * log K)]
    have hmu : μ t ≤ y i + C := by
      by_contra! h
      have h' := mul_le_mul_of_nonneg_right ht₁ hC
      have h'' := mul_lt_mul_of_pos_left (show C < μ t - y i by linarith) (hB.trans ht)
      nlinarith only [hz, h', h'']
    have hmu' : y j - U / m ≤ μ t := by
      by_contra! h
      have h' : U < m * (y j - μ t) := by
        have := (div_lt_iff₀ hm).mp (show U / m < y j - μ t by linarith)
        linarith
      have h'' := mul_le_mul_of_nonneg_right hlo
        (show 0 ≤ y j - μ t by linarith [div_pos hU hm])
      have hl := mul_le_mul_of_nonneg_left (softplus_lower (b t * (y j - μ t))) hct.le
      nlinarith only [h', h'', hl, hu]
    exact ⟨⟨hmu', hmu⟩, ⟨hlo, hhi⟩, ⟨hq', logistic_le_one _⟩⟩
  have hS : IsCompact S := isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc)
  obtain ⟨⟨s, k, ρ⟩, hs, φ, hφ, ht⟩ := hS.tendsto_subseq' hbox.frequently
  have hμ : Tendsto (μ ∘ φ) atTop (𝓝 s) := continuous_fst.continuousAt.tendsto.comp ht
  have hk : Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) :=
    continuous_fst.continuousAt.tendsto.comp (continuous_snd.continuousAt.tendsto.comp ht)
  have hρ : Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 ρ) :=
    continuous_snd.continuousAt.tendsto.comp (continuous_snd.continuousAt.tendsto.comp ht)
  have hb' := hb.comp hφ.tendsto_atTop
  have hs' : s ≤ y i := by
    by_contra! he
    have hpos : Tendsto (fun t ↦ μ (φ t) - y i) atTop (𝓝 (s - y i)) :=
      hμ.sub_const _
    have hz := tendsto_neg_atTop_atBot.comp (hpos.pos_mul_atTop (sub_pos.mpr he) hb')
    have hq := tendsto_exp_atBot.comp hz
    have hρ₀ : 0 < ρ := (exp_pos _).trans_le hs.2.2.1
    have hq' : Tendsto (fun t ↦ exp (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 0) := by
      apply hq.congr
      intro t
      simp only [Function.comp_apply]
      congr 1
      ring
    have hle := le_of_tendsto_of_tendsto' hρ hq' (fun _ ↦ logistic_le_exp _)
    exact hρ₀.not_ge hle
  have hk₀ : 0 < k := hm.trans_le hs.2.1.1
  have hρ₀ : 0 < ρ := (exp_pos _).trans_le hs.2.2.1
  refine ⟨s, k, ρ, φ, hφ, hs', hk₀, ⟨hρ₀, hs.2.2.2⟩, hμ, hk, hρ, ?_⟩
  exact threshold_contact hmin hs' hk₀ hρ₀ hb' hk hμ (fun _ ↦ hρ)

/-- Bounded-shape competitive sequences have a limit in the closed
inverse-exponent chart after taking a subsequence. Positive inverse
exponent gives an interior point and zero gives a Gompertz point. -/
theorem bounded_shape_subsequence {y : Fin n → ℝ} (j : Fin n)
    {b c μ : ℕ → ℝ} {B M : ℝ}
    (h : ∃ᶠ t in atTop, 0 < b t ∧ 0 < c t ∧ b t ≤ B ∧
      M ≤ logProduct y (b t) (c t) (μ t)) :
    ∃ p ∈ chartDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
        atTop (𝓝 p) := by
  let m := exp (M / n)
  let C := (n : ℝ) * log B - M
  let K : Set (ℝ × ℝ × ℝ) :=
    Icc m B ×ˢ (Icc (exp (-C - B * |y j|)) (exp (C + B * |y j|) * exp (exp (2 * C))) ×ˢ
      Icc 0 (B / m))
  have hK : IsCompact K := isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc)
  have hdom : K ⊆ chartDomain := fun _ hq ↦
    ⟨(exp_pos _).trans_le hq.1.1, (exp_pos _).trans_le hq.2.1.1, hq.2.2.1⟩
  have hmem := h.mono fun t ht ↦ chart_anchor j (Fin.pos j) ht.1 ht.2.1 ht.2.2.1 ht.2.2.2
  obtain ⟨p, hp, φ, hφ, hlim⟩ := hK.tendsto_subseq' hmem
  exact ⟨p, hdom hp, φ, hφ, hlim⟩


private theorem physical_bddAbove {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) :
    BddAbove ((fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2) '' physicalDomain) := by
  obtain ⟨B, K, U, hB, hK, _, henv⟩ := large_shape_envelope hij 0
  refine ⟨max 0 (max ((n : ℝ) * log B) ((n : ℝ) * log K)), ?_⟩
  rintro _ ⟨⟨b, c, μ⟩, hp, rfl⟩
  have hb : 0 < b := hp.1
  have hc : 0 < c := hp.2.1
  have hB' := logProduct_le (y := y) (μ := μ) hb hc
  rcases le_or_gt b B with hl | hl
  · exact hB'.1.trans ((mul_le_mul_of_nonneg_left (log_le_log hb hl) (by positivity)).trans
      ((le_max_left _ _).trans (le_max_right _ _)))
  · by_cases h : 0 ≤ logProduct y b c μ
    · have hk := (henv hl hc h).2.1
      exact hB'.2.trans ((mul_le_mul_of_nonneg_left (log_le_log (mul_pos hb hc) hk)
        (by positivity)).trans ((le_max_right _ _).trans (le_max_right _ _)))
    · exact (le_of_not_ge h).trans (le_max_left _ _)

/-- A sequence on a positive-likelihood superlevel has a subsequence in
the closed Gompertz chart or a subsequence with the exact threshold trace. -/
theorem competitive_subsequence {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (M : ℝ)
    {b c μ : ℕ → ℝ} (hb : ∀ᶠ t in atTop, 0 < b t) (hc : ∀ᶠ t in atTop, 0 < c t)
    (hM : ∀ᶠ t in atTop, M ≤ logProduct y (b t) (c t) (μ t)) :
    (∃ p ∈ chartDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
        Tendsto (fun t ↦
          (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
          atTop (𝓝 p) ∧
        Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
          (𝓝 (chart y p))) ∨
      (∃ (s k ρ : ℝ) (φ : ℕ → ℕ), StrictMono φ ∧ s ≤ y i ∧ 0 < k ∧ ρ ∈ Ioc 0 1 ∧
        Tendsto (b ∘ φ) atTop atTop ∧
        Tendsto (μ ∘ φ) atTop (𝓝 s) ∧
        Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) ∧
        Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 ρ) ∧
        Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
          (𝓝 (thresholdProduct y s k +
            if s = y i then ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) * log ρ
            else 0))) := by
  classical
  by_cases h : Tendsto b atTop atTop
  · obtain ⟨s, k, ρ, φ, hφ, hs, hk, hρ, hμ, hbc, hq, hL⟩ :=
      large_shape_subsequence hij hmin M h hc hM
    exact Or.inr ⟨s, k, ρ, φ, hφ, hs, hk, hρ, h.comp hφ.tendsto_atTop, hμ, hbc, hq, hL⟩
  · rw [tendsto_atTop] at h
    push Not at h
    obtain ⟨B, hB⟩ := h
    have h' := hB.and_eventually (hb.and (hc.and hM))
    obtain ⟨p, hp, φ, hφ, ht⟩ := bounded_shape_subsequence i
      (h'.mono fun t ⟨hlt, hbt, hct, hMt⟩ ↦ ⟨hbt, hct, hlt.le, hMt⟩)
    let P := fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹)
    have hpos := hφ.tendsto_atTop.eventually (hb.and hc)
    have hP : Tendsto P atTop (𝓝[chartDomain] p) := by
      apply tendsto_nhdsWithin_iff.mpr
      exact ⟨ht, hpos.mono fun t ⟨hbt, hct⟩ ↦
        ⟨hbt, mul_pos hct (exp_pos _), inv_nonneg.mpr hct.le⟩⟩
    have hcont : Tendsto (chart y) (𝓝[chartDomain] p) (𝓝 (chart y p)) :=
      chart_continuous y p hp
    refine Or.inl ⟨p, hp, φ, hφ, ht, (hcont.comp hP).congr' ?_⟩
    exact hpos.mono fun t ⟨hbt, hct⟩ ↦ chart_physical y hbt hct

private theorem constant_concentration (hn : 0 < n) (s : ℝ) :
    Tendsto (fun b ↦ logProduct (fun _ : Fin n ↦ s) b 1 s) atTop atTop := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have h := tendsto_atTop_add_const_right _ ((n : ℝ) *
    (log (logistic 0) + loss (softplus 0))) (tendsto_log_atTop.const_mul_atTop hnR)
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  simp only [logProduct, sub_self, mul_zero, mul_one, one_mul,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    log_mul hb.ne' (logistic_pos 0).ne']
  ring

/-- The all-free likelihood is unbounded on constants. Otherwise its
supremum is finite, and nonattainment means every finite point is below
the larger supremum of the two reachable faces. Finite equality preserves
attainment. -/
theorem all_free_failure (hn : 0 < n) (y : Fin n → ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    ((∀ i j, y i = y j) →
      Tendsto (fun b ↦ logProduct y b 1 (y ⟨0, hn⟩)) atTop atTop ∧
      ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ i : Fin n, (∀ j, y i ≤ y j) ∧
        let F := (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) '' Weibull.domain ∪
          thresholdProduct y (y i) '' Ioi 0
        BddAbove (L '' physicalDomain) ∧ BddAbove F ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
          (∀ p ∈ physicalDomain, L p < sSup F) ∧ sSup (L '' physicalDomain) = sSup F)) := by
  intro L
  constructor
  · intro h
    let s := y ⟨0, hn⟩
    have he : y = fun _ ↦ s := funext fun i ↦ h i ⟨0, hn⟩
    have ht : Tendsto (fun b ↦ logProduct y b 1 s) atTop atTop := by
      rw [he]
      exact constant_concentration hn s
    refine ⟨ht, ?_⟩
    rintro ⟨M, hM⟩
    obtain ⟨b, hb, hb'⟩ := ((ht.eventually (eventually_gt_atTop M)).and
      (eventually_gt_atTop (0 : ℝ))).exists
    exact hb.not_ge (hM (show L (b, 1, s) ∈ L '' physicalDomain from
      mem_image_of_mem _ ⟨hb', zero_lt_one, mem_univ _⟩))
  · rintro ⟨v, w, hvw⟩
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ y ⟨v, Finset.mem_univ _⟩
    obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ y ⟨v, Finset.mem_univ _⟩
    have hmin (k : Fin n) := hi k (Finset.mem_univ k)
    have hmax (k : Fin n) := hj k (Finset.mem_univ k)
    have hij : y i < y j := by
      have hle := (hmin v).trans (hmax v)
      by_contra! h
      exact hvw ((le_antisymm (hmax v) (h.trans (hmin v))).trans
        (le_antisymm (hmax w) (h.trans (hmin w))).symm)
    refine ⟨i, hmin, ?_⟩
    dsimp only
    let F := (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) '' Weibull.domain ∪
      thresholdProduct y (y i) '' Ioi 0
    have hbL : BddAbove (L '' physicalDomain) := physical_bddAbove hij
    have hsup : ∀ p ∈ physicalDomain, L p ≤ sSup (L '' physicalDomain) :=
      fun p hp ↦ le_csSup hbL (mem_image_of_mem _ hp)
    obtain ⟨hG, hT⟩ := faces_le_of_upper_bound hmin hsup
    have hF : ∀ v ∈ F, v ≤ sSup (L '' physicalDomain) := by
      rintro _ (⟨p, hp, rfl⟩ | ⟨k, hk, rfl⟩)
      · exact hG p.1 hp.1 p.2 hp.2
      · exact hT k hk
    have hbF : BddAbove F := ⟨_, hF⟩
    have hF₀ : F.Nonempty :=
      ⟨thresholdProduct y (y i) 1, Or.inr (mem_image_of_mem _ zero_lt_one)⟩
    refine ⟨hbL, hbF, ?_⟩
    have hface : sSup F ≤ sSup (L '' physicalDomain) := csSup_le hF₀ hF
    have hiff := all_free_attains_iff_witness hij hmin hmax
    constructor
    · intro h
      have hlt (p : ℝ × ℝ × ℝ) (hp : p ∈ physicalDomain) : L p < sSup F := by
        by_contra! h'
        apply h
        exact hiff.mpr ⟨p, hp,
          fun b hb r hr ↦ (le_csSup hbF (show gompertzProduct y b r ∈ F from
            Or.inl ⟨(b, r), ⟨hb, hr⟩, rfl⟩)).trans h',
          fun k hk ↦ (le_csSup hbF (Or.inr (mem_image_of_mem _ hk))).trans h'⟩
      refine ⟨hlt, le_antisymm (csSup_le ?_ (fun _ ⟨p, hp, he⟩ ↦ he ▸ (hlt p hp).le)) hface⟩
      exact ⟨L (1, 1, 0), mem_image_of_mem _ ⟨zero_lt_one, zero_lt_one, mem_univ _⟩⟩
    · rintro ⟨hlt, _⟩ ⟨p, hp, hm⟩
      exact (hlt p hp).not_ge (csSup_le hF₀ (fun _ hv ↦ (hF _ hv).trans
        (csSup_le ⟨L p, mem_image_of_mem _ hp⟩ (fun v hv ↦ by
          obtain ⟨q, hq, hv'⟩ := hv
          rw [← hv']
          exact hm hq))))


/-- Every nonconstant sample has an optimized all-free Gompertz face.
Its competitive points inherit compact positive nuisance bounds from PGW. -/
theorem gompertz_face_attains {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain p := by
  let G := fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2
  let M := G (1, 1) - 1
  obtain ⟨B, K, U, hB, _, _, henv⟩ := large_shape_envelope hij M
  let m := exp (M / n)
  let C := (n : ℝ) * log B - M
  let S : Set (ℝ × ℝ) :=
    Icc m B ×ˢ Icc (exp (-C - B * |y i|)) (exp (C + B * |y i|) * exp (exp (2*C)))
  have hs : IsCompact S := isCompact_Icc.prod isCompact_Icc
  have hdom : S ⊆ Weibull.domain := fun _ h ↦
    ⟨(exp_pos _).trans_le h.1.1, (exp_pos _).trans_le h.2.1⟩
  have hbox (b r : ℝ) (hb : 0 < b) (hr : 0 < r) (hM : M < gompertzProduct y b r) :
      (b, r) ∈ S := by
    have hgood : ∀ᶠ h : ℝ in 𝓝[>] 0, M ≤ logProduct y b h⁻¹ (-(log (h*r)) / b) :=
      (fixed_chart_path y hb hr).eventually (Ici_mem_nhds hM)
    have hpos : ∀ᶠ h : ℝ in 𝓝[>] 0, 0 < h := self_mem_nhdsWithin
    have hbB : b ≤ B := by
      by_contra! h
      have hk : Tendsto (fun t : ℝ ↦ b * t⁻¹) (𝓝[>] 0) atTop :=
        (tendsto_inv_nhdsGT_zero (𝕜 := ℝ)).const_mul_atTop hb
      have ht := hk.eventually (eventually_gt_atTop K)
      obtain ⟨t, ht₀, htk, htg⟩ := (hpos.and (ht.and hgood)).exists
      exact htk.not_ge (henv h (inv_pos.mpr ht₀) htg).2.1
    obtain ⟨t, ht, ht'⟩ := (hpos.and hgood).exists
    have ha := chart_anchor (y := y) i (Fin.pos i) hb (inv_pos.mpr ht) hbB ht'
    have hr' : t⁻¹ * exp (-b * (-(log (t * r)) / b)) = r := by
      rw [show -b * (-(log (t * r)) / b) = log (t * r) by field_simp]
      rw [exp_log (mul_pos ht hr), ← mul_assoc, inv_mul_cancel₀ ht.ne', one_mul]
    rw [hr'] at ha
    exact ⟨ha.1, ha.2.1⟩
  have h1 : (1, 1) ∈ S := hbox 1 1 zero_lt_one zero_lt_one (by dsimp [M]; linarith)
  have hc : ContinuousOn G S := by
    have hmap : MapsTo (fun p : ℝ × ℝ ↦ (p.1, p.2, 0)) S chartDomain :=
      fun _ hp ↦ ⟨(hdom hp).1, (hdom hp).2, le_rfl⟩
    exact (chart_continuous y).comp
      ((continuous_fst.prodMk (continuous_snd.prodMk continuous_const)).continuousOn) hmap
  obtain ⟨p, hp, hm⟩ := hs.exists_isMaxOn ⟨(1, 1), h1⟩ hc
  refine ⟨p, hdom hp, ?_⟩
  intro q hq
  change G q ≤ G p
  by_cases h : M < G q
  · exact hm (hbox _ _ hq.1 hq.2 h)
  · exact (le_of_not_gt h).trans ((sub_le_self _ zero_le_one).trans (hm h1))

/-- The optimized moving-threshold envelope is at the sample minimum. -/
theorem threshold_face_attains {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) :
    ∃ k ∈ Ioi 0, IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
      ∀ s ≤ y i, ∀ v > 0, thresholdProduct y s v ≤ thresholdProduct y (y i) k := by
  let x := fun l ↦ exp (y l)
  let a := exp (y i)
  have ha : 0 < a := exp_pos _
  have hx : ∀ l, a ≤ x l := fun l ↦ exp_le_exp.mpr (hmin l)
  have he (k : ℝ) : thresholdProduct y (y i) k =
      AllFreePGW.thresholdLogLik x a k +
        ∑ l, (log (x l) + if x l = a then log 2 else 0) := by
    unfold thresholdProduct AllFreePGW.thresholdLogLik
    rw [← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro l _
    simp [x, a, loss, log_div (exp_pos _).ne' (exp_pos _).ne']
    ring
  obtain ⟨k, hk, hm⟩ := (AllFreePGW.threshold_unique ha hx ⟨j, exp_lt_exp.mpr hij⟩).exists
  have hm' : IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k := by
    intro v hv
    change thresholdProduct y (y i) v ≤ thresholdProduct y (y i) k
    rw [he, he]
    exact add_le_add_left (hm hv) _
  refine ⟨k, hk, hm', fun s hs v hv ↦ ?_⟩
  apply le_trans _ (hm' hv)
  unfold thresholdProduct
  exact Finset.sum_le_sum (fun l _ ↦ add_le_add_right
    (loss_antitone (mul_nonneg hv.le (sub_nonneg.mpr (hmin l)))
      (mul_le_mul_of_nonneg_left (sub_le_sub_left hs _) hv.le)) _)

/-- Near the Gompertz face, every point on a fixed positive-level
superlevel lies in one compact nuisance box, uniformly in inverse exponent. -/
theorem gompertz_nuisance_compact {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    (M : ℝ) :
    ∃ ε > 0, ∃ S : Set (ℝ × ℝ), IsCompact S ∧ S ⊆ Weibull.domain ∧
      ∀ {b c μ : ℝ}, 0 < b → 0 < c → c⁻¹ < ε → M ≤ logProduct y b c μ →
        (b, c * exp (-b * μ)) ∈ S := by
  obtain ⟨B, K, U, hB, hK, _, henv⟩ := large_shape_envelope hij M
  let m := exp (M / n)
  let C := (n : ℝ) * log B - M
  let S : Set (ℝ × ℝ) :=
    Icc m B ×ˢ Icc (exp (-C - B * |y i|)) (exp (C + B * |y i|) * exp (exp (2 * C)))
  refine ⟨B / K, div_pos hB hK, S, isCompact_Icc.prod isCompact_Icc,
    fun _ h ↦ ⟨(exp_pos _).trans_le h.1.1, (exp_pos _).trans_le h.2.1⟩, ?_⟩
  intro b c μ hb hc hh hM
  have hbb : b ≤ B := by
    by_contra! h
    have hk := (henv h hc hM).2.1
    have hk' := mul_le_mul_of_nonneg_left hk (inv_pos.mpr hc).le
    rw [show c⁻¹ * (b * c) = b by field_simp] at hk'
    have h' := (lt_div_iff₀ hK).mp hh
    exact h.not_ge (hk'.trans (by simpa [mul_comm] using h'.le))
  have ha := chart_anchor i (Fin.pos i) hb hc hbb hM
  exact ⟨ha.1, ha.2.1⟩

/-- For sufficiently large inverse exponent the entire finite nuisance
slice is strictly below the optimized threshold, including points with
shape or scale depending on the inverse exponent. -/
theorem strict_inverse_exponent_tail {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j)
    {k : ℝ}
    (hm : IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k) :
    ∃ H > 0, ∀ {b h μ : ℝ}, 0 < b → H < h →
      logProduct y b h⁻¹ μ < thresholdProduct y (y i) k := by
  let M := thresholdProduct y (y i) k
  obtain ⟨B, hB, ht⟩ := strict_threshold_tail hij hmin hmax M
  let m := exp (M / n)
  have hm₀ : 0 < m := exp_pos _
  refine ⟨B / m, div_pos hB hm₀, ?_⟩
  intro b h μ hb hh
  have hh₀ := (div_pos hB hm₀).trans hh
  by_contra! hn
  have hbB : B < b := by
    by_contra! h'
    have ha := (bounded_shape_envelope (Fin.pos i) i hb (inv_pos.mpr hh₀) h' hn).2.2.1.2
    rw [inv_inv] at ha
    exact hh.not_ge ha
  have hlt := ht hbB (inv_pos.mpr hh₀) hn
  exact hlt.not_ge ((hm (mul_pos hb (inv_pos.mpr hh₀))).trans hn)


private theorem loss_strict {u v : ℝ} (hv : 0 ≤ v) (hvu : v < u) :
    loss u < loss v := by
  have he := mul_lt_mul_of_pos_right (add_one_lt_exp (sub_ne_zero.mpr hvu.ne'))
    (exp_pos v)
  rw [← exp_add, sub_add_cancel] at he
  have hv' : 1 ≤ exp v := by simpa using exp_le_exp.mpr hv
  have hh := mul_le_mul_of_nonneg_right hv' (sub_nonneg.mpr hvu.le)
  unfold loss
  nlinarith only [he, hh]

private theorem threshold_strict_location {y : Fin n → ℝ} {i : Fin n}
    (hmin : ∀ l, y i ≤ y l) {s k : ℝ} (hs : s < y i) (hk : 0 < k) :
    thresholdProduct y s k < thresholdProduct y (y i) k := by
  unfold thresholdProduct
  have h (l : Fin n) : log k + loss (k * (y l - s)) <
      log k + loss (k * (y l - y i)) := by
    apply add_lt_add_right
    exact loss_strict (mul_nonneg hk.le (sub_nonneg.mpr (hmin l)))
      (mul_lt_mul_of_pos_left (sub_lt_sub_left hs _) hk)
  exact Finset.sum_lt_sum (fun l _ ↦ (h l).le) ⟨i, Finset.mem_univ _, h i⟩

private theorem nonattaining_sequence {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l)
    (hno : ¬ ∃ p ∈ physicalDomain,
      IsMaxOn (fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2) physicalDomain p)
    {b c μ : ℕ → ℝ} (hb : ∀ᶠ t in atTop, 0 < b t) (hc : ∀ᶠ t in atTop, 0 < c t)
    (hlim : Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) atTop
      (𝓝 (sSup ((fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2) '' physicalDomain)))) :
    (∃ p ∈ Weibull.domain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
        Tendsto (fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
          atTop (𝓝 (p.1, p.2, 0)) ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain p ∧
        gompertzProduct y p.1 p.2 =
          sSup ((fun q : ℝ × ℝ × ℝ ↦ logProduct y q.1 q.2.1 q.2.2) '' physicalDomain)) ∨
      (∃ k > 0, ∃ φ : ℕ → ℕ, StrictMono φ ∧ Tendsto (b ∘ φ) atTop atTop ∧
        Tendsto (μ ∘ φ) atTop (𝓝 (y i)) ∧
        Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) ∧
        Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 1) ∧
        IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
        thresholdProduct y (y i) k =
          sSup ((fun q : ℝ × ℝ × ℝ ↦ logProduct y q.1 q.2.1 q.2.2) '' physicalDomain)) := by
  classical
  let L := fun q : ℝ × ℝ × ℝ ↦ logProduct y q.1 q.2.1 q.2.2
  let C := sSup (L '' physicalDomain)
  have hB : BddAbove (L '' physicalDomain) := physical_bddAbove hij
  have hub (p : ℝ × ℝ × ℝ) (hp : p ∈ physicalDomain) : L p ≤ C :=
    le_csSup hB (mem_image_of_mem _ hp)
  obtain ⟨hG, hT⟩ := faces_le_of_upper_bound hmin hub
  have hM : ∀ᶠ t in atTop, C - 1 ≤ logProduct y (b t) (c t) (μ t) :=
    hlim.eventually (Ici_mem_nhds (by dsimp [C]; linarith))
  rcases competitive_subsequence hij hmin (C - 1) hb hc hM with
    ⟨⟨B, r, h⟩, hp, φ, hφ, hpL, hL⟩ | ⟨s, k, ρ, φ, hφ, hs, hk, hρ, hbφ, hμφ, hkφ, hqφ, hL⟩
  · have hval : chart y (B, r, h) = C := tendsto_nhds_unique hL (hlim.comp hφ.tendsto_atTop)
    have h₀ : h = 0 := by
      by_contra! hh
      have hh' : 0 < h := lt_of_le_of_ne hp.2.2 (Ne.symm hh)
      have hd : (B, h⁻¹, -(log (h * r)) / B) ∈ physicalDomain :=
        ⟨hp.1, inv_pos.mpr hh', mem_univ _⟩
      have hv := chart_surjective y hp.1 hp.2.1 hh'
      exact hno ⟨_, hd, fun q hq ↦ (hub q hq).trans_eq (hv.symm.trans hval).symm⟩
    subst h
    have hv : gompertzProduct y B r = C := hval
    refine Or.inl ⟨(B, r), ⟨hp.1, hp.2.1⟩, φ, hφ, hpL, ?_, hv⟩
    intro q hq
    exact (hG q.1 hq.1 q.2 hq.2).trans_eq hv.symm
  · have hv : thresholdProduct y s k +
        (if s = y i then ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) * log ρ else 0) = C :=
      tendsto_nhds_unique hL (hlim.comp hφ.tendsto_atTop)
    have hsy : s = y i := by
      by_contra! h
      have hlt := threshold_strict_location hmin (lt_of_le_of_ne hs h) hk
      simp [h] at hv
      exact hlt.not_ge ((hT k hk).trans_eq hv.symm)
    subst s
    have hm : 0 < ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) := by
      exact_mod_cast Finset.card_pos.mpr ⟨i, by simp⟩
    have hρ₁ : ρ = 1 := by
      by_contra! h
      have hl : log ρ < 0 := log_neg hρ.1 (lt_of_le_of_ne hρ.2 h)
      have hneg := mul_neg_of_pos_of_neg hm hl
      have hle := hT k hk
      simp only [ite_true] at hv
      linarith only [hv, hneg, hle]
    simp only [hρ₁, log_one, mul_zero, ite_true, add_zero] at hv
    refine Or.inr ⟨k, hk, φ, hφ, hbφ, hμφ, hkφ, hρ₁ ▸ hqφ, ?_, hv⟩
    exact fun t ht ↦ (hT t ht).trans_eq hv.symm

/-- The all-free failure diagnosis includes concentration on constants
and the optimized faces on nonconstants. Every maximizing sequence in a
nonattaining nonconstant sample has a subsequence at a winning face. -/
theorem all_free_escape_classification (hn : 0 < n) (y : Fin n → ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    let V := L '' physicalDomain
    ((∀ i j, y i = y j) →
      Tendsto (fun b ↦ logProduct y b 1 (y ⟨0, hn⟩)) atTop atTop ∧ ¬ BddAbove V) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ i : Fin n, (∀ j, y i ≤ y j) ∧
      ∃ g ∈ Weibull.domain, ∃ k > 0,
        IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain g ∧
        IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
        let G := gompertzProduct y g.1 g.2
        let T := thresholdProduct y (y i) k
        BddAbove V ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
          (∀ p ∈ physicalDomain, L p < max G T) ∧ sSup V = max G T) ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) →
          ∀ {b c μ : ℕ → ℝ}, (∀ᶠ t in atTop, 0 < b t) → (∀ᶠ t in atTop, 0 < c t) →
          Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) atTop (𝓝 (sSup V)) →
          (∃ p ∈ Weibull.domain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
            Tendsto (fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
              atTop (𝓝 (p.1, p.2, 0)) ∧ gompertzProduct y p.1 p.2 = sSup V) ∨
          (∃ v > 0, ∃ φ : ℕ → ℕ, StrictMono φ ∧ Tendsto (b ∘ φ) atTop atTop ∧
            Tendsto (μ ∘ φ) atTop (𝓝 (y i)) ∧
            Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 v) ∧
            Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 1) ∧
            thresholdProduct y (y i) v = sSup V))) := by
  intro L V
  refine ⟨(all_free_failure hn y).1, ?_⟩
  intro hne
  obtain ⟨i, hmin, hV, hF, hfail⟩ := (all_free_failure hn y).2 hne
  obtain ⟨v, w, hvw⟩ := hne
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ y ⟨v, Finset.mem_univ _⟩
  have hmax (t : Fin n) := hj t (Finset.mem_univ t)
  have hij : y i < y j := by
    by_contra! h
    exact hvw ((le_antisymm (hmax v) (h.trans (hmin v))).trans
      (le_antisymm (hmax w) (h.trans (hmin w))).symm)
  obtain ⟨g, hg, hG⟩ := gompertz_face_attains hij
  obtain ⟨k, hk, hT, _⟩ := threshold_face_attains hij hmin
  refine ⟨i, hmin, g, hg, k, hk, hG, hT, hV, ?_, ?_⟩
  · let F := (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) '' Weibull.domain ∪
      thresholdProduct y (y i) '' Ioi 0
    have hfg : gompertzProduct y g.1 g.2 ∈ F := Or.inl ⟨g, hg, rfl⟩
    have hft : thresholdProduct y (y i) k ∈ F := Or.inr ⟨k, hk, rfl⟩
    have he : sSup F = max (gompertzProduct y g.1 g.2) (thresholdProduct y (y i) k) := by
      apply le_antisymm (csSup_le ⟨_, hfg⟩ ?_) (max_le (le_csSup hF hfg) (le_csSup hF hft))
      rintro _ (⟨p, hp, rfl⟩ | ⟨v, hv, rfl⟩)
      · exact (hG hp).trans (le_max_left ..)
      · exact (hT hv).trans (le_max_right ..)
    change (¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∀ p ∈ physicalDomain, L p < _) ∧ sSup V = _
    rw [← he]
    exact hfail
  · intro hno b c μ hb hc ht
    rcases nonattaining_sequence hij hmin hno hb hc ht with
      ⟨p, hp, φ, hφ, hpL, _, hL⟩ | ⟨v, hv, φ, hφ, hbL, hμ, hkL, hqL, _, hL⟩
    · exact Or.inl ⟨p, hp, φ, hφ, hpL, hL⟩
    · exact Or.inr ⟨v, hv, φ, hφ, hbL, hμ, hkL, hqL, hL⟩


end

end UnivariateML.PGWEnvelope

end
