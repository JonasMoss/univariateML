module

public import UnivariateML.Family.Shape2InvBurr
public import Mathlib.Analysis.SpecialFunctions.BinaryEntropy
public import Mathlib.Analysis.SpecialFunctions.Integrals.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exact inverse-Burr face comparisons

The comparisons use the Burr chart on reflected log observations, with a
single data-only Jacobian on transport back to physical inverse Burr.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.BPInverseBurr

noncomputable section

variable {n : ℕ}

local notation "avg" => BurrHeld.avg
local notation "sp" => BurrHeld.softplus
local notation "L" => BurrFree.likelihood
local notation "P" => BurrFree.paretoValue
local notation "W" => BurrFree.weibullLik

private theorem npos (hn : 0 < n) : (0 : ℝ) < n := by exact_mod_cast hn

private theorem avg_const (hn : 0 < n) (a : ℝ) : avg (fun _ : Fin n ↦ a) = a := by
  simp [BurrHeld.avg, nsmul_eq_mul, (npos hn).ne']

private theorem avg_add (a b : Fin n → ℝ) : avg (fun i ↦ a i + b i) = avg a + avg b := by
  simp [BurrHeld.avg, Finset.sum_add_distrib, add_div]

private theorem avg_sub (a b : Fin n → ℝ) : avg (fun i ↦ a i - b i) = avg a - avg b := by
  simp [BurrHeld.avg, Finset.sum_sub_distrib, sub_div]

private theorem avg_mul (a : ℝ) (b : Fin n → ℝ) : avg (fun i ↦ a * b i) = a * avg b := by
  simp [BurrHeld.avg, ← Finset.mul_sum, mul_div_assoc]

private theorem avg_mono {a b : Fin n → ℝ} (h : ∀ i, a i ≤ b i) : avg a ≤ avg b :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

private theorem sp_pos (v : ℝ) : 0 < sp v := log_pos (lt_add_of_pos_right _ (exp_pos _))

private theorem sp_shift (v : ℝ) : sp v = v + sp (-v) := by
  unfold BurrHeld.softplus
  rw [exp_neg]
  have hp : 0 < exp v := exp_pos _
  have hh : 0 < 1 + (exp v)⁻¹ := add_pos one_pos (inv_pos.mpr hp)
  calc
    log (1 + exp v) = log (exp v * (1 + (exp v)⁻¹)) := by
      congr 1
      field_simp [hp.ne']
      ring
    _ = v + log (1 + (exp v)⁻¹) := by rw [log_mul hp.ne' hh.ne', log_exp]

private theorem likelihood_affine (hn : 0 < n) (y : Fin n → ℝ) (k c b : ℝ) :
    L y (k, c, b) = log k + log c + c * avg y + b -
      (1 + k) * avg (fun i ↦ sp (c * y i + b)) := by
  unfold BurrFree.likelihood BurrHeld.firstLik
  dsimp only [Prod.fst, Prod.snd]
  have he (i : Fin n) : sp (-(c * y i + b)) + k * sp (c * y i + b) =
      (1 + k) * sp (c * y i + b) - (c * y i + b) := by
    linarith [sp_shift (c * y i + b)]
  simp_rw [he]
  rw [avg_sub, avg_mul, avg_add, avg_mul, avg_const hn]
  ring

/-! ## Dual bounds and the small-shape cutoff -/

private theorem entropy_support {a : ℝ} (ha : a ∈ Icc (0 : ℝ) 1) (z : ℝ) :
    a * z + binEntropy a ≤ sp z := by
  have ha0 : 0 ≤ a := ha.1
  have ha1 : a ≤ 1 := ha.2
  by_cases h0 : a = 0
  · subst a
    simpa using (sp_pos z).le
  by_cases h1 : a = 1
  · subst a
    simpa using (show z ≤ sp z by rw [sp_shift]; linarith [sp_pos (-z)])
  have hp : 0 < a := ha0.lt_of_ne (Ne.symm h0)
  have hq : 0 < 1 - a := sub_pos.mpr (ha1.lt_of_ne h1)
  let v := exp z / (1 + exp z)
  have hd : 0 < 1 + exp z := add_pos one_pos (exp_pos z)
  have hv : 0 < v := div_pos (exp_pos _) hd
  have hw : 0 < 1 - v := sub_pos.mpr ((div_lt_one hd).mpr (lt_add_of_pos_left _ one_pos))
  have H₁ := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (div_pos hv hp)) hp.le
  have H₂ := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (div_pos hw hq)) hq.le
  have hc₁ : a * (v / a - 1) = v - a := by field_simp [hp.ne']
  have hc₂ : (1 - a) * ((1 - v) / (1 - a) - 1) = (1 - v) - (1 - a) := by
    field_simp [hq.ne']
  rw [log_div hv.ne' hp.ne', hc₁] at H₁
  rw [log_div hw.ne' hq.ne', hc₂] at H₂
  have he : 1 - v = 1 / (1 + exp z) := by
    dsimp [v]
    field_simp [hd.ne']
    ring
  have hlogv : log v = z - sp z := by
    dsimp [v]
    rw [log_div (exp_pos z).ne' hd.ne', log_exp]
    rfl
  have hlogw : log (1 - v) = -sp z := by rw [he, one_div, log_inv]; rfl
  rw [hlogv] at H₁
  rw [hlogw] at H₂
  rw [binEntropy_eq_negMulLog_add_negMulLog_one_sub]
  unfold negMulLog
  linarith only [H₁, H₂]

/-- The positive logarithmic decay coefficient supplied by dual weights. -/
def dualRate (y w : Fin n → ℝ) (k : ℝ) : ℝ :=
  avg (fun i ↦ ((1 + k) * w i - 1) * y i)

/-- Entropy dual upper bound on the profiled height at a fixed first shape. -/
def dualValue (y w : Fin n → ℝ) (k : ℝ) : ℝ :=
  log (k / dualRate y w k) - 1 - (1 + k) * avg (fun i ↦ binEntropy (w i))

/-- Weights with the stated mean cancel the intercept and give an upper
bound simultaneously for all finite powers and scales at that shape. -/
theorem dual_upper (hn : 0 < n) (y w : Fin n → ℝ) {k c : ℝ} (b : ℝ)
    (hk : 0 < k) (hc : 0 < c) (hw : ∀ i, w i ∈ Icc (0 : ℝ) 1)
    (havg : avg w = 1 / (1 + k)) (ha : 0 < dualRate y w k) :
    L y (k, c, b) ≤ dualValue y w k := by
  let E := avg (fun i ↦ binEntropy (w i))
  have h := avg_mono (fun i ↦ entropy_support (hw i) (c * y i + b))
  have h₁ (i : Fin n) : w i * (c * y i + b) = c * (w i * y i) + b * w i := by ring
  simp_rw [h₁] at h
  rw [avg_add, avg_add, avg_mul, avg_mul, havg] at h
  have hh := mul_le_mul_of_nonneg_left h (show 0 ≤ 1 + k by positivity)
  have hnorm : (1 + k) * (b * (1 / (1 + k))) = b := by field_simp
  have hR : dualRate y w k = (1 + k) * avg (fun i ↦ w i * y i) - avg y := by
    unfold dualRate
    simp_rw [sub_mul, one_mul, mul_assoc]
    rw [avg_sub, avg_mul]
  rw [mul_add, mul_add, hnorm] at hh
  have hel := log_le_sub_one_of_pos (mul_pos hc ha)
  rw [log_mul hc.ne' ha.ne'] at hel
  rw [likelihood_affine hn]
  unfold dualValue
  rw [log_div hk.ne' ha.ne']
  nlinarith only [hh, hR, hel, congrArg (fun t ↦ c * t) hR]

private theorem entropy_quadratic {a : ℝ} (ha : a ∈ Icc (0 : ℝ) 1) :
    2 * a * (1 - a) ≤ binEntropy a := by
  have ha0 : 0 ≤ a := ha.1
  have ha1 : a ≤ 1 := ha.2
  by_cases h0 : a = 0
  · simp [h0]
  by_cases h1 : a = 1
  · simp [h1]
  have h := mul_le_mul_of_nonneg_left
    (log_le_sub_one_of_pos (ha0.lt_of_ne (Ne.symm h0))) ha0
  have h' := mul_le_mul_of_nonneg_left
    (log_le_sub_one_of_pos (sub_pos.mpr (ha1.lt_of_ne h1))) (sub_nonneg.mpr ha1)
  rw [binEntropy_eq_negMulLog_add_negMulLog_one_sub]
  unfold negMulLog
  nlinarith only [h, h']

private theorem entropy_one_side {a : ℝ} (ha : a ∈ Ioo (0 : ℝ) 1) :
    a * (1 - a) - a * log a ≤ binEntropy a := by
  have h := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (sub_pos.mpr ha.2))
    (sub_nonneg.mpr ha.2.le)
  rw [binEntropy_eq_negMulLog_add_negMulLog_one_sub]
  unfold negMulLog
  nlinarith only [h]

/-- The repeated zero log observations exclude the small-shape region.
The strict bound includes the equality case of the shape cutoff. -/
theorem entropy_small_shape (hn : 0 < n) {y : Fin n → ℝ}
    (hy : 0 < avg y) {k c b : ℝ} (hk : 0 < k) (hc : 0 < c)
    (hsmall : (n : ℝ) * k ≤ (1 + k) * BurrHeld.zeroCount y) :
    L y (k, c, b) < P y := by
  classical
  let E : Finset (Fin n) := Finset.univ.filter (fun i ↦ y i = 0)
  have hE : E.card = BurrHeld.zeroCount y := rfl
  let h := fun i ↦ sp (-(c * y i + b))
  have hsum : (E.card : ℝ) * sp (-b) ≤ ∑ i, h i := by
    calc
      _ = ∑ i ∈ E, h i := by
        simp only [h]
        rw [← nsmul_eq_mul, ← Finset.sum_const]
        apply Finset.sum_congr rfl
        intro i hi
        rw [(Finset.mem_filter.mp hi).2, mul_zero, zero_add]
      _ ≤ _ := Finset.sum_le_sum_of_subset_of_nonneg (Finset.subset_univ E)
        (fun i _ _ ↦ (sp_pos _).le)
  have hbnd : k * sp (-b) ≤ (1 + k) * avg h := by
    apply le_of_mul_le_mul_left _ (npos hn)
    calc
      (n : ℝ) * (k * sp (-b)) = ((n : ℝ) * k) * sp (-b) := by ring
      _ ≤ ((1 + k) * E.card) * sp (-b) := by
        rw [hE]
        exact mul_le_mul_of_nonneg_right hsmall (sp_pos _).le
      _ ≤ (1 + k) * ∑ i, h i := by
        rw [mul_assoc]
        exact mul_le_mul_of_nonneg_left hsum (by positivity)
      _ = n * ((1 + k) * avg h) := by
        unfold BurrHeld.avg
        field_simp [(npos hn).ne']
  have hrel : avg (fun i ↦ sp (c * y i + b)) = c * avg y + b + avg h := by
    simp_rw [sp_shift (c * y _ + b), avg_add, avg_mul, avg_const hn, h]
  have hstrict : 0 < k * (b + sp (-b)) := by rw [← sp_shift]; exact mul_pos hk (sp_pos b)
  have hP := (BurrFree.pareto_optimized hy (mul_pos hk hc)).1
  rw [log_mul hk.ne' hc.ne'] at hP
  rw [likelihood_affine hn, hrel]
  nlinarith only [hP, hbnd, hstrict]

/-! ## Exact power reference -/

/-- The exact seven-log reference used by the normalized-log convex-order screen. -/
def powerReference : Fin 7 → ℝ :=
  ![0, (23826 / 100000) / 64, (7 / 15) / 64, (8 / 15) / 64, (8 / 15) / 64, 1, 1]

private theorem reference_mean : 0 < avg powerReference ∧ avg powerReference ≤ 29 / 100 := by
  norm_num [BurrHeld.avg, powerReference, Fin.sum_univ_succ]

private theorem reference_weights {k : ℝ} (lo hi : ℝ)
    (h : (5 * lo + 2 * hi) / 7 = 1 / (1 + k)) :
    let w : Fin 7 → ℝ := ![lo, lo, lo, lo, lo, hi, hi]
    avg w = 1 / (1 + k) ∧
    dualRate powerReference w k =
      ((1 + k) * lo - 1) * (avg powerReference - 2 / 7) +
        ((1 + k) * hi - 1) * (2 / 7) ∧
    avg (fun i ↦ binEntropy (w i)) = (5 * binEntropy lo + 2 * binEntropy hi) / 7 := by
  dsimp
  norm_num [BurrHeld.avg, dualRate, powerReference, Fin.sum_univ_succ] at h ⊢
  refine ⟨by linarith, by ring, by ring⟩

/-- The exact convex-order reference has strict power dominance.
Two elementary sets of dual weights suffice outside the small-shape region. -/
theorem power_reference_dominance :
    ∀ v ∈ BurrFree.domain, L powerReference v < P powerReference := by
  intro v hv
  rcases v with ⟨k, c, b⟩
  have hk : 0 < k := hv.1
  have hc : 0 < c := hv.2.1
  obtain ⟨hD0, hD⟩ := reference_mean
  let D := avg powerReference
  change 0 < D at hD0
  change D ≤ 29 / 100 at hD
  by_cases hs : k ≤ 1 / 6
  · apply BurrFree.small_shape_lt_pareto (j := 0) (by decide) hD0 (show powerReference 0 = 0 by rfl)
      hk hc
    norm_num
    linarith
  have hK : 0 < 1 + k := by positivity
  by_cases hm : k ≤ 11 / 4
  · let a := (1 - (9 / 25) * k) / (1 + k)
    let d := (1 + (9 / 10) * k) / (1 + k)
    let w : Fin 7 → ℝ := ![a, a, a, a, a, d, d]
    have ha : a ∈ Icc (0 : ℝ) 1 := by
      change 0 ≤ a ∧ a ≤ 1
      exact ⟨div_nonneg (by linarith) hK.le,
        (div_le_one hK).mpr (by linarith)⟩
    have hd : d ∈ Icc (0 : ℝ) 1 := by
      change 0 ≤ d ∧ d ≤ 1
      exact ⟨div_nonneg (by positivity) hK.le, (div_le_one hK).mpr (by linarith)⟩
    have hw (i : Fin 7) : w i ∈ Icc (0 : ℝ) 1 := by
      fin_cases i <;> first | exact ha | exact hd
    have he : (5 * a + 2 * d) / 7 = 1 / (1 + k) := by
      dsimp [a, d]
      field_simp [hK.ne']
      ring
    obtain ⟨havg, hR, hE⟩ := reference_weights a d he
    change avg w = _ at havg
    change dualRate powerReference w k = _ at hR
    change avg (fun i ↦ binEntropy (w i)) = _ at hE
    let R := (9 / 25 : ℝ) * (2 / 7 - D) + (9 / 10) * (2 / 7)
    have hR' : dualRate powerReference w k = k * R := by
      rw [hR]
      dsimp [a, d, R, D]
      field_simp [hK.ne']
      ring
    have hRlo : 51 / 200 ≤ R := by dsimp [R]; linarith
    have hRpos : 0 < R := by linarith
    have hrate : 0 < dualRate powerReference w k := by rw [hR']; positivity
    have hlog : log (D / R) ≤ 7 / 50 := by
      have h := log_le_sub_one_of_pos (div_pos hD0 hRpos)
      have h' : D / R ≤ 57 / 50 := (div_le_iff₀ hRpos).mpr (by linarith)
      linarith
    have hent : 3 / 20 ≤ (1 + k) * avg (fun i ↦ binEntropy (w i)) := by
      rw [hE]
      have h0 := entropy_quadratic ha
      have h1 := entropy_quadratic hd
      have hq : (1 + k) * ((5 * (2 * a * (1 - a)) + 2 * (2 * d * (1 - d))) / 7) =
          2 * (k - (81 / 250) * k ^ 2) / (1 + k) := by
        dsimp [a, d]
        field_simp [hK.ne']
        ring
      have hprod := mul_nonneg (show 0 ≤ k - 1 / 6 by linarith)
        (show 0 ≤ 11 / 4 - k by linarith)
      have hq' : (3 / 20 : ℝ) ≤ 2 * (k - (81 / 250) * k ^ 2) / (1 + k) := by
        apply (le_div_iff₀ hK).mpr
        nlinarith only [hprod, hm]
      rw [← hq] at hq'
      exact hq'.trans (mul_le_mul_of_nonneg_left (by linarith only [h0, h1]) hK.le)
    have hupper := dual_upper (by decide : 0 < 7) powerReference w b hk hc hw havg hrate
    apply hupper.trans_lt
    unfold dualValue BurrFree.paretoValue
    rw [hR', show k / (k * R) = 1 / R by field_simp [hk.ne', hRpos.ne'], one_div, log_inv]
    rw [log_div hD0.ne' hRpos.ne'] at hlog
    change _ < -log D - 1
    linarith only [hlog, hent]
  · let d := 7 / (2 * (1 + k))
    let w : Fin 7 → ℝ := ![0, 0, 0, 0, 0, d, d]
    have hd0 : 0 < d := by dsimp [d]; positivity
    have hdtop : d ≤ 14 / 15 := (div_le_iff₀ (by positivity : 0 < 2 * (1 + k))).mpr
      (by linarith)
    have hd1 : d < 1 := hdtop.trans_lt (by norm_num)
    have hw (i : Fin 7) : w i ∈ Icc (0 : ℝ) 1 := by
      fin_cases i <;> first | exact ⟨hd0.le, hd1.le⟩ | exact ⟨le_rfl, zero_le_one⟩
    have he : (5 * (0 : ℝ) + 2 * d) / 7 = 1 / (1 + k) := by
      dsimp [d]
      field_simp [hK.ne']
      ring
    obtain ⟨havg, hR, hE⟩ := reference_weights 0 d he
    change avg w = _ at havg
    change dualRate powerReference w k = _ at hR
    change avg (fun i ↦ binEntropy (w i)) = _ at hE
    have hnorm : (1 + k) * (2 * d / 7) = 1 := by
      dsimp [d]
      field_simp [hK.ne']
    have hR' : dualRate powerReference w k = 1 - D := by
      rw [hR]
      change ((1 + k) * 0 - 1) * (D - 2 / 7) + ((1 + k) * d - 1) * (2 / 7) = _
      linarith only [hnorm]
    have hRpos : 0 < 1 - D := by linarith
    have hrate : 0 < dualRate powerReference w k := by rw [hR']; exact hRpos
    have hent : 1 - d - log d ≤ (1 + k) * avg (fun i ↦ binEntropy (w i)) := by
      rw [hE, binEntropy_zero, mul_zero, zero_add]
      have h := entropy_one_side (show d ∈ Ioo (0 : ℝ) 1 from ⟨hd0, hd1⟩)
      have hh := mul_le_mul_of_nonneg_left h (show 0 ≤ (1 + k) * 2 / 7 by positivity)
      have hnorm' : ((1 + k) * 2 / 7) * d = 1 := by linarith only [hnorm]
      calc
        _ = ((1 + k) * 2 / 7) * (d * (1 - d) - d * log d) := by
          rw [mul_sub, ← mul_assoc, hnorm', ← mul_assoc, hnorm', one_mul, one_mul]
        _ ≤ _ := by linarith only [hh]
    have hlog : log (k / (1 - D) * D * d) < 1 - d := by
      have hk' : k * d = 7 / 2 - d := by linarith only [hnorm]
      have heq : k / (1 - D) * D * d = ((7 / 2 - d) * D) / (1 - D) := by
        rw [← hk']
        ring
      have hbnd : k / (1 - D) * D * d < 2 - d := by
        rw [heq, div_lt_iff₀ hRpos]
        nlinarith only [hdtop, hD, mul_nonneg (show 0 ≤ 29 / 100 - D by linarith)
          (show 0 ≤ 11 / 2 - 2 * d by linarith)]
      exact (log_le_sub_one_of_pos (by positivity :
        0 < k / (1 - D) * D * d)).trans_lt (by linarith)
    have hupper := dual_upper (by decide : 0 < 7) powerReference w b hk hc hw havg hrate
    apply hupper.trans_lt
    unfold dualValue BurrFree.paretoValue
    rw [hR']
    rw [log_mul (mul_pos (div_pos hk hRpos) hD0).ne' hd0.ne',
      log_mul (div_pos hk hRpos).ne' hD0.ne'] at hlog
    change _ < -log D - 1
    linarith only [hlog, hent]

private theorem sp_deriv (z : ℝ) :
    HasDerivAt sp (exp z / (1 + exp z)) z := by
  have hd : 0 < 1 + exp z := add_pos one_pos (exp_pos _)
  unfold BurrHeld.softplus
  exact ((hasDerivAt_exp z).const_add 1).log hd.ne'

private theorem affine_softplus_convex (a b : ℝ) :
    ConvexOn ℝ univ (fun t ↦ sp (a * t + b)) := by
  have h : ConvexOn ℝ univ sp := by
    apply MonotoneOn.convexOn_of_deriv convex_univ
      (fun z _ ↦ (sp_deriv z).continuousAt.continuousWithinAt)
    · exact fun u _ ↦ (sp_deriv u).differentiableAt.differentiableWithinAt
    · intro u _ v _ huv
      rw [(sp_deriv u).deriv, (sp_deriv v).deriv, div_le_div_iff₀
        (add_pos one_pos (exp_pos _)) (add_pos one_pos (exp_pos _))]
      linarith [exp_le_exp.mpr huv]
  refine ⟨convex_univ, fun u _ v _ s t hs ht hst ↦ ?_⟩
  simp only [smul_eq_mul]
  have he : a * (s * u + t * v) + b = s * (a * u + b) + t * (a * v + b) := by
    linear_combination -b * hst
  rw [he]
  simpa only [smul_eq_mul] using h.2 (mem_univ (a * u + b)) (mem_univ (a * v + b)) hs ht hst

/-! ## Finite convex-order test -/

/-- Equal-weight empirical convex order after division by each mean.
The larger sample in convex order is the first argument. -/
def normalizedConvexOrder {m : ℕ} (y : Fin n → ℝ) (z : Fin m → ℝ) : Prop :=
  ∀ g : ℝ → ℝ, ConvexOn ℝ univ g →
    avg (fun i ↦ g (z i / avg z)) ≤ avg (fun i ↦ g (y i / avg y))

private theorem hinge_interpolation {f : ℝ → ℝ} (hf : ConvexOn ℝ univ f) (S : Finset ℝ) :
    ∀ a d : ℝ, (∀ x ∈ S, a < x) →
      (∀ x ∈ S, d ≤ (f x - f a) / (x - a)) →
      ∃ w : ℝ → ℝ, (∀ z ∈ insert a S, 0 ≤ w z) ∧
        ∀ x ∈ insert a S,
          f x = f a + d * (x - a) + ∑ z ∈ insert a S, w z * max (x - z) 0 := by
  classical
  refine S.strongInductionOn ?_
  intro S ih a d hlt hsec
  by_cases he : S.Nonempty
  · let b := S.min' he
    have hb : b ∈ S := S.min'_mem he
    have hab : a < b := hlt b hb
    have hna : a ∉ S := fun ha ↦ (lt_irrefl a) (hlt a ha)
    have hmin (x : ℝ) (hx : x ∈ S.erase b) : b < x :=
      (S.min'_le x (Finset.mem_of_mem_erase hx)).lt_of_ne
        (Ne.symm (Finset.ne_of_mem_erase hx))
    let d' := (f b - f a) / (b - a)
    have hdd : d ≤ d' := hsec b hb
    obtain ⟨w, hw, hrep⟩ := ih (S.erase b) (Finset.erase_ssubset hb) b d' hmin
      (fun x hx ↦ hf.slope_mono_adjacent (mem_univ a) (mem_univ x) hab (hmin x hx))
    rw [Finset.insert_erase hb] at hw hrep
    let w' := fun z : ℝ ↦ if z = a then d' - d else w z
    have hw' (z : ℝ) (hz : z ∈ S) : w' z = w z := by
      have h : z ≠ a := fun ha ↦ hna (ha ▸ hz)
      simp [w', h]
    refine ⟨w', fun z hz ↦ ?_, fun x hx ↦ ?_⟩
    · rcases Finset.mem_insert.mp hz with rfl | h
      · simpa [w'] using sub_nonneg.mpr hdd
      · rw [hw' z h]
        exact hw z h
    · rcases Finset.mem_insert.mp hx with hxa | hx'
      · subst x
        have hzero : ∑ z ∈ insert a S, w' z * max (a - z) 0 = 0 := by
          apply Finset.sum_eq_zero
          intro z hz
          have hz' : a ≤ z := (Finset.mem_insert.mp hz).elim
            (fun h ↦ h.ge) (fun h ↦ (hlt z h).le)
          rw [max_eq_right (sub_nonpos.mpr hz'), mul_zero]
        rw [hzero, sub_self, mul_zero, add_zero, add_zero]
      · have hsum : ∑ z ∈ S, w' z * max (x - z) 0 =
            ∑ z ∈ S, w z * max (x - z) 0 :=
          Finset.sum_congr rfl (fun z hz ↦ by rw [hw' z hz])
        have hlin : d' * (b - a) = f b - f a := div_mul_cancel₀ _ (sub_pos.mpr hab).ne'
        rw [Finset.sum_insert hna, hsum, max_eq_left (sub_nonneg.mpr (hlt x hx').le)]
        have hwa : w' a = d' - d := by simp [w']
        rw [hwa, hrep x hx']
        linarith only [hlin]
  · have hz : S = ∅ := Finset.not_nonempty_iff_eq_empty.mp he
    subst S
    exact ⟨fun _ ↦ 0, fun _ _ ↦ le_rfl, by simp⟩

private theorem avg_hinge_interpolation {m : ℕ} (hm : 0 < m) (x : Fin m → ℝ)
    {f : ℝ → ℝ} {S : Finset ℝ} {a d : ℝ} {w : ℝ → ℝ}
    (hx : ∀ i, x i ∈ S)
    (hrep : ∀ z ∈ S, f z = f a + d * (z - a) + ∑ t ∈ S, w t * max (z - t) 0) :
    avg (fun i ↦ f (x i)) = f a + d * (avg x - a) +
      ∑ t ∈ S, w t * avg (fun i ↦ max (x i - t) 0) := by
  simp_rw [fun i ↦ hrep (x i) (hx i)]
  rw [avg_add, avg_add, avg_const hm, avg_mul, avg_sub, avg_const hm]
  congr 1
  unfold BurrHeld.avg
  rw [Finset.sum_comm, Finset.sum_div]
  apply Finset.sum_congr rfl
  intro t _
  rw [← Finset.mul_sum, mul_div_assoc]

/-- Finite stop-loss form of normalized convex order. Test only the
observed normalized values of the two data sets, including ties. -/
def FiniteNormalizedOrder {m : ℕ} (y : Fin n → ℝ) (z : Fin m → ℝ) : Prop :=
  ∀ t ∈ range (fun i ↦ y i / avg y) ∪ range (fun i ↦ z i / avg z),
    avg (fun i ↦ max (z i / avg z - t) 0) ≤ avg (fun i ↦ max (y i / avg y - t) 0)

/-- On nonempty samples with positive means, the finite hinge checks
are equivalent to empirical convex order of normalized observations. -/
theorem finite_normalized_order_iff {m : ℕ} (hn : 0 < n) (hm : 0 < m)
    {y : Fin n → ℝ} {z : Fin m → ℝ} (hy : 0 < avg y) (hz : 0 < avg z) :
    FiniteNormalizedOrder y z ↔ normalizedConvexOrder y z := by
  classical
  constructor
  · intro hord f hf
    let y' := fun i ↦ y i / avg y
    let z' := fun i ↦ z i / avg z
    let S := Finset.univ.image y' ∪ Finset.univ.image z'
    have hy' (i : Fin n) : y' i ∈ S :=
      Finset.mem_union_left _ (Finset.mem_image_of_mem y' (Finset.mem_univ i))
    have hz' (i : Fin m) : z' i ∈ S :=
      Finset.mem_union_right _ (Finset.mem_image_of_mem z' (Finset.mem_univ i))
    have hne : S.Nonempty := ⟨y' ⟨0, hn⟩, hy' _⟩
    let a := S.min' hne
    let d := (f a - f (a - 1)) / (a - (a - 1))
    have ha : a ∈ S := S.min'_mem _
    have hlt (x : ℝ) (hx : x ∈ S.erase a) : a < x :=
      (S.min'_le _ (Finset.mem_of_mem_erase hx)).lt_of_ne (Ne.symm (Finset.ne_of_mem_erase hx))
    obtain ⟨w, hw, hrep⟩ := hinge_interpolation hf (S.erase a) a d hlt
      (fun x hx ↦ hf.slope_mono_adjacent (mem_univ (a - 1)) (mem_univ x) (by linarith) (hlt x hx))
    rw [Finset.insert_erase ha] at hw hrep
    have haY : avg y' = 1 := by
      change (∑ i, y i / avg y) / n = _
      rw [← Finset.sum_div, div_right_comm]
      exact div_self hy.ne'
    have haZ : avg z' = 1 := by
      change (∑ i, z i / avg z) / m = _
      rw [← Finset.sum_div, div_right_comm]
      exact div_self hz.ne'
    have h := Finset.sum_le_sum (s := S) (fun t ht ↦ mul_le_mul_of_nonneg_left
      (hord t (by simpa [S, y', z'] using ht)) (hw t ht))
    change avg (fun i ↦ f (z' i)) ≤ avg (fun i ↦ f (y' i))
    rw [avg_hinge_interpolation hm z' hz' hrep, avg_hinge_interpolation hn y' hy' hrep, haY, haZ]
    exact add_le_add_right h _
  · intro hord t _
    have h : ConvexOn ℝ univ (fun x : ℝ ↦ x - t) := by
      refine ⟨convex_univ, fun a _ b _ u v _ _ huv ↦ ?_⟩
      simp only [smul_eq_mul]
      exact (by linear_combination t * huv : u * a + v * b - t = u * (a - t) + v * (b - t)).le
    exact hord _ (h.sup (convexOn_const 0 convex_univ))

private theorem normalized_dominance (hn : 0 < n) {y : Fin n → ℝ}
    (hy : 0 < avg y) (hord : normalizedConvexOrder y powerReference) :
    ∀ p ∈ BurrFree.domain, L y p < P y := by
  rintro ⟨k, c, b⟩ hp
  have h0 := reference_mean.1
  have hc : 0 < c := hp.2.1
  let d := c * avg y / avg powerReference
  have hd : 0 < d := div_pos (mul_pos hc hy) h0
  have H := hord _ (affine_softplus_convex (c * avg y) b)
  have h₁ (i : Fin n) : c * avg y * (y i / avg y) = c * y i := by
    field_simp [hy.ne']
  have h₂ (i : Fin 7) : c * avg y * (powerReference i / avg powerReference) =
      d * powerReference i := by dsimp [d]; ring
  simp_rw [h₁, h₂] at H
  have hm : d * avg powerReference = c * avg y := div_mul_cancel₀ _ h0.ne'
  have hP := power_reference_dominance (k, d, b)
    (show (k, d, b) ∈ BurrFree.domain from ⟨hp.1, hd, mem_univ _⟩)
  rw [likelihood_affine hn, BurrFree.paretoValue]
  rw [likelihood_affine (by decide : 0 < 7), BurrFree.paretoValue, hm] at hP
  have hlog : log d = log c + log (avg y) - log (avg powerReference) := by
    dsimp [d]
    rw [log_div (mul_pos hc hy).ne' h0.ne', log_mul hc.ne' hy.ne']
  rw [hlog] at hP
  have hS := mul_le_mul_of_nonneg_left H (show 0 ≤ 1 + k from add_nonneg zero_le_one
    (le_of_lt hp.1))
  linarith only [hP, hS]

private theorem weibull_high_mass_upper (hn : 0 < n) (y : Fin n → ℝ) {c b d : ℝ}
    (hd : 0 < d)
    (hmass : d * exp c ≤ avg (fun i ↦ exp (c * y i))) :
    W y c b ≤ log c + c * (avg y - 1) - log d - 1 := by
  obtain ⟨hval, hopt⟩ := BurrFree.weibull_profile_value hn y c
  have hlog := log_le_log (mul_pos hd (exp_pos _)) hmass
  rw [log_mul hd.ne' (exp_pos c).ne', log_exp] at hlog
  exact (hopt b).trans (by rw [hval]; linarith)

private theorem reference_weibull_loses {c b : ℝ} (hc : 0 < c) :
    W powerReference c b < P powerReference := by
  let D := avg powerReference
  obtain ⟨hD0, hD⟩ := reference_mean
  change 0 < D at hD0
  change D ≤ 29 / 100 at hD
  have hmass : (2 / 7 : ℝ) * exp c ≤ avg (fun i ↦ exp (c * powerReference i)) := by
    norm_num [BurrHeld.avg, powerReference, Fin.sum_univ_succ]
    linarith only [exp_pos (c * (23826 / 100000 / 64)), exp_pos (c * (7 / 15 / 64)),
      exp_pos (c * (8 / 15 / 64))]
  have H := weibull_high_mass_upper (by decide : 0 < 7) powerReference (b := b)
    (by norm_num : 0 < (2 / 7 : ℝ)) hmass
  have hD1 : 0 < 1 - D := by linarith
  have h1 := log_le_sub_one_of_pos (mul_pos hc hD1)
  rw [log_mul hc.ne' hD1.ne'] at h1
  have h2 : log (D / ((2 / 7) * (1 - D))) < 1 :=
    (log_le_sub_one_of_pos (div_pos hD0 (by positivity))).trans_lt (by
      have h : D / ((2 / 7) * (1 - D)) < 2 := (div_lt_iff₀ (by positivity)).mpr (by linarith)
      linarith)
  rw [log_div hD0.ne' (by positivity), log_mul (by norm_num : (2 / 7 : ℝ) ≠ 0) hD1.ne'] at h2
  change W powerReference c b < -log D - 1
  change W powerReference c b ≤ log c + c * (D - 1) - log (2 / 7) - 1 at H
  linarith only [H, h1, h2]

private theorem normalized_weibull_loses {y : Fin n → ℝ} (hn : 0 < n)
    (hy : 0 < avg y) (hord : normalizedConvexOrder y powerReference) {c b : ℝ} (hc : 0 < c) :
    W y c b < P y := by
  have h0 := reference_mean.1
  let d := c * avg y / avg powerReference
  have hd : 0 < d := div_pos (mul_pos hc hy) h0
  have hconv : ConvexOn ℝ univ (fun t ↦ exp (c * avg y * t + b)) := by
    refine ⟨convex_univ, fun u _ v _ s t hs ht hst ↦ ?_⟩
    simp only [smul_eq_mul]
    rw [show c * avg y * (s * u + t * v) + b =
        s * (c * avg y * u + b) + t * (c * avg y * v + b) by linear_combination -b * hst]
    simpa only [smul_eq_mul] using convexOn_exp.2 (mem_univ _) (mem_univ _) hs ht hst
  have H := hord _ hconv
  have h₁ (i : Fin n) : c * avg y * (y i / avg y) = c * y i := by field_simp [hy.ne']
  have h₂ (i : Fin 7) : c * avg y * (powerReference i / avg powerReference) =
      d * powerReference i := by dsimp [d]; ring
  simp_rw [h₁, h₂] at H
  have hm : d * avg powerReference = c * avg y := div_mul_cancel₀ _ h0.ne'
  have gap := reference_weibull_loses (b := b) hd
  have hlog : log d = log c + log (avg y) - log (avg powerReference) := by
    dsimp [d]
    rw [log_div (mul_pos hc hy).ne' h0.ne', log_mul hc.ne' hy.ne']
  unfold BurrFree.weibullLik BurrFree.paretoValue at gap ⊢
  rw [avg_sub, avg_add, avg_mul, avg_const hn]
  rw [avg_sub, avg_add, avg_mul, avg_const (by decide : 0 < 7), hm, hlog] at gap
  linarith only [gap, H]

/-! ## Reciprocal transport -/

private def inverseObjective (x : Fin n → ℝ) (v : ℝ × ℝ × ℝ) : ℝ :=
  ShapeFacesInverseBurr.logLik x v.1 v.2.1 v.2.2

private def chart (x : Fin n → ℝ) (j : Fin n) (v : ℝ × ℝ × ℝ) : ℝ × ℝ × ℝ :=
  (v.1, v.2.1, v.2.1 * (log v.2.2 - log (x j)))

private theorem chart_maps (x : Fin n → ℝ) (j : Fin n) :
    MapsTo (chart x j) BurrFree.physicalDomain BurrFree.domain :=
  fun _ hv ↦ ⟨hv.1, hv.2.1, mem_univ _⟩

private theorem chart_surj (x : Fin n → ℝ) (j : Fin n) :
    SurjOn (chart x j) BurrFree.physicalDomain BurrFree.domain := by
  rintro ⟨k, c, b⟩ hv
  have hc : 0 < c := hv.2.1
  refine ⟨(k, c, exp (log (x j) + b / c)), ⟨hv.1, hc, exp_pos _⟩, ?_⟩
  unfold chart
  simp only [log_exp, add_sub_cancel_left, Prod.mk.injEq, true_and]
  field_simp [hc.ne']

private theorem inverse_chart_value (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) {v : ℝ × ℝ × ℝ} (hv : v ∈ BurrFree.physicalDomain) :
    inverseObjective x v = n * L (BurrFree.logExcess (Transfer.reciprocal x) j) (chart x j v)
      - ∑ i, log (x i) := by
  rcases v with ⟨k, c, s⟩
  let y := BurrFree.logExcess (Transfer.reciprocal x) j
  let b := c * (log s - log (x j))
  have h (i : Fin n) : c * y i + b = c * log (s / x i) := by
    simp only [y, BurrFree.logExcess, Transfer.reciprocal, log_inv, b,
      log_div (show 0 < s from hv.2.2).ne' (hx i).ne']
    ring
  have hExpand : L y (k, c, b) =
      avg (fun i ↦ log k + log c + (c * y i + b) - (1 + k) * sp (c * y i + b)) := by
    simp only [likelihood_affine hn, avg_sub, avg_add, avg_const hn, avg_mul]
    ring
  change ShapeFacesInverseBurr.logLik x k c s = n * L y (k, c, b) - ∑ i, log (x i)
  rw [hExpand]
  simp_rw [h]
  unfold BurrHeld.avg ShapeFacesInverseBurr.logLik
  rw [mul_div_cancel₀ _ (npos hn).ne', ← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  unfold ShapeFacesInverseBurr.logDensity BurrHeld.softplus
  rw [rpow_def_of_pos (div_pos (show 0 < s from hv.2.2) (hx i))]
  ring_nf

private theorem strict_face_transfer (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) {V : ℝ}
    (hstrict : ∀ p ∈ BurrFree.domain, L (BurrFree.logExcess (Transfer.reciprocal x) j) p < V)
    {u : ℝ → ℝ × ℝ × ℝ} (hu : ∀ᶠ t in atTop, u t ∈ BurrFree.domain)
    (ht : Tendsto (fun t ↦ L (BurrFree.logExcess (Transfer.reciprocal x) j) (u t))
      atTop (𝓝 V)) :
    let F := inverseObjective x
    let B := n * V - ∑ i, log (x i)
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro F B
  have hlt (p : ℝ × ℝ × ℝ) (hp : p ∈ BurrFree.physicalDomain) : F p < B := by
    change inverseObjective x p < _
    rw [inverse_chart_value hn hx j hp]
    exact sub_lt_sub_right (mul_lt_mul_of_pos_left (hstrict _ (chart_maps x j hp)) (npos hn)) _
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro U hU
    apply le_of_tendsto ((ht.const_mul (n : ℝ)).sub_const (∑ i, log (x i)))
    filter_upwards [hu] with t htu
    obtain ⟨p, hp, he⟩ := chart_surj x j htu
    have hb := hU (mem_image_of_mem F hp)
    change inverseObjective x p ≤ U at hb
    rw [inverse_chart_value hn hx j hp, he] at hb
    exact hb

private theorem reciprocal_log_properties (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n}
    (hj : ∀ i, x i ≤ x j) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    (∀ i, 0 ≤ y i) ∧ (∃ i k, y i ≠ y k) ∧ 0 < avg y := by
  intro y
  have he (i : Fin n) : y i = log (x j) - log (x i) := by
    simp only [y, BurrFree.logExcess, Transfer.reciprocal, log_inv]
    ring
  have hy (i : Fin n) : 0 ≤ y i := by rw [he]; exact sub_nonneg.mpr (log_le_log (hx i) (hj i))
  obtain ⟨i, k, hik⟩ := hne
  have hne' : y i ≠ y k := by
    intro heq
    have h : log (x i) = log (x k) := by rw [he i, he k] at heq; linarith
    exact hik (by simpa only [exp_log (hx i), exp_log (hx k)] using congrArg exp h)
  have hpos : ∃ i, 0 < y i := by
    by_contra! Hz
    exact hne' ((le_antisymm (Hz i) (hy i)).trans (le_antisymm (Hz k) (hy k)).symm)
  obtain ⟨a, ha⟩ := hpos
  have ha' := Finset.single_le_sum (s := Finset.univ) (fun i _ ↦ hy i) (Finset.mem_univ a)
  exact ⟨hy, ⟨i, k, hne'⟩, div_pos (ha.trans_le ha') (npos hn)⟩

/-- On nonconstant inverse-Burr data, convex domination of the exact
reference by the normalized reciprocal logs gives strict power escape. -/
theorem inverse_power_convex_order (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * P y - ∑ i, log (x i)
    normalizedConvexOrder y powerReference →
    (∀ c > 0, ∀ b, W y c b < P y) ∧
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro y F B horder
  obtain ⟨hy, _, hmean⟩ := reciprocal_log_properties hn hx hne hj
  have hstrict := normalized_dominance hn hmean horder
  have hp := BurrFree.pareto_path hn hy hmean
  have hmem : ∀ᶠ t : ℝ in atTop,
      ((avg y)⁻¹ / t ^ 2, t ^ 2, t) ∈ BurrFree.domain := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact ⟨div_pos (inv_pos.mpr hmean) (sq_pos_of_pos ht), sq_pos_of_pos ht, mem_univ _⟩
  exact ⟨fun _ hc _ ↦ normalized_weibull_loses hn hmean horder hc,
    strict_face_transfer hn hx j hstrict hmem hp⟩

/-! ## A signed tail bound from the fitted Weibull weights -/

private theorem avg_deriv {f : ℝ → Fin n → ℝ} {d : Fin n → ℝ} {t : ℝ}
    (h : ∀ i, HasDerivAt (fun q ↦ f q i) (d i) t) :
    HasDerivAt (fun q ↦ avg (f q)) (avg d) t := by
  unfold BurrHeld.avg
  exact (HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ h i).div_const n

private theorem weibull_scores (hn : 0 < n) (y : Fin n → ℝ) {c b : ℝ}
    (hc : 0 < c) (hW : ∀ d > 0, ∀ t, W y d t ≤ W y c b) :
    let v := fun i ↦ exp (c * y i + b)
    avg v = 1 ∧ avg (fun i ↦ (v i - 1) * y i) = 1 / c := by
  intro v
  have hb (i : Fin n) :
      HasDerivAt (fun t ↦ c * y i + t - exp (c * y i + t)) (1 - v i) b := by
    have h := (hasDerivAt_id b).const_add (c * y i)
    exact h.sub ((hasDerivAt_exp (c * y i + b)).comp b h) |>.congr_deriv (by simp [v])
  have hdb := (avg_deriv hb).const_add (log c)
  have hLb : IsLocalMax (fun t ↦ W y c t) b :=
    (show IsMaxOn (fun t ↦ W y c t) univ b from fun t _ ↦ hW c hc t).isLocalMax (by simp)
  have hB : avg (fun i ↦ 1 - v i) = 0 := hdb.deriv.symm.trans hLb.deriv_eq_zero
  rw [avg_sub, avg_const hn] at hB
  have hd (i : Fin n) :
      HasDerivAt (fun t ↦ t * y i + b - exp (t * y i + b)) (y i - v i * y i) c := by
    have h := ((hasDerivAt_id c).mul_const (y i)).add_const b
    have H := h.sub ((hasDerivAt_exp (c * y i + b)).comp c h)
    exact H.congr_deriv (by simp only [one_mul]; rfl)
  have hdc := (hasDerivAt_log hc.ne').add (avg_deriv hd)
  have hLc : IsLocalMax (fun t ↦ W y t b) c :=
    (show IsMaxOn (fun t ↦ W y t b) (Ioi 0) c from fun t ht ↦ hW t ht b).isLocalMax
      (Ioi_mem_nhds hc)
  have hC : c⁻¹ + avg (fun i ↦ y i - v i * y i) = 0 :=
    hdc.deriv.symm.trans hLc.deriv_eq_zero
  refine ⟨by linarith, ?_⟩
  have he (i : Fin n) : (v i - 1) * y i = v i * y i - y i := by ring
  simp only [he, one_div]
  rw [avg_sub]
  rw [avg_sub] at hC
  linarith

private theorem entropy_remainder {M t : ℝ} (ht : t ∈ Icc (0 : ℝ) M) (hM : M < 1) :
    (1 - t) * log (1 - t) ≤ -t + t ^ 2 / (2 * (1 - M)) := by
  let F := fun t : ℝ ↦ -t + t ^ 2 / (2 * (1 - M)) - (1 - t) * log (1 - t)
  have hder (u : ℝ) (hu : u < 1) :
      HasDerivAt F (u / (1 - M) + log (1 - u)) u := by
    have h1 := (hasDerivAt_id u).const_sub 1
    have hlog := h1.log (sub_pos.mpr hu).ne'
    have hd := (((hasDerivAt_id u).neg).add (((hasDerivAt_id u).pow 2).div_const
      (2 * (1 - M)))).sub (h1.mul hlog)
    convert hd using 1
    · rfl
    · simp only [id_eq, Nat.cast_ofNat, mul_one]
      field_simp [(sub_pos.mpr hu).ne', (sub_pos.mpr hM).ne']
      ring
  have hmono : MonotoneOn F (Icc 0 M) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    · exact fun u hu ↦ (hder u (hu.2.trans_lt hM)).continuousAt.continuousWithinAt
    · exact fun u hu ↦
        (hder u ((interior_subset hu).2.trans_lt hM)).differentiableAt.differentiableWithinAt
    · intro u hu
      have hu' : u ∈ Icc 0 M := interior_subset hu
      have hup : 0 < 1 - u := sub_pos.mpr (hu'.2.trans_lt hM)
      rw [(hder u (hu'.2.trans_lt hM)).deriv]
      have h := one_sub_inv_le_log_of_pos hup
      have he : u / (1 - u) = (1 - u)⁻¹ - 1 := by field_simp [hup.ne']; ring
      have H := div_le_div_of_nonneg_left hu'.1 (sub_pos.mpr hM)
        (sub_le_sub_left hu'.2 1)
      rw [he] at H
      linarith
  have h := hmono (show (0 : ℝ) ∈ Icc 0 M from ⟨le_rfl, ht.1.trans ht.2⟩) ht ht.1
  have hzero : F 0 = 0 := by simp [F]
  rw [hzero] at h
  exact sub_nonneg.mp h

/-- Signed Weibull seam from the exact Weibull optimizer's weights.
One uniform inequality on the second moment and a bound on the largest
weight suffices for all shapes at or above the cutoff. -/
theorem weibull_dual_seam (hn : 0 < n) (y : Fin n → ℝ) {cW bW V K : ℝ}
    (hcW : 0 < cW) (hW : ∀ d > 0, ∀ t, W y d t ≤ W y cW bW)
    (hK : 0 < K) (hV : ∀ i, exp (cW * y i + bW) ≤ V)
    (hgap : avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1) :
    ∀ {k c b : ℝ}, K ≤ k → 0 < c → L y (k, c, b) < W y cW bW := by
  intro k c b hk hc
  have hk0 := hK.trans_le hk
  have hk1 : 0 < 1 + k := by positivity
  let v := fun i ↦ exp (cW * y i + bW)
  let m₂ := avg (fun i ↦ v i ^ 2)
  let M := V / (1 + k)
  have hV0 : 0 < V := (exp_pos _).trans_le (hV ⟨0, hn⟩)
  have hM : m₂ / 2 + M < 1 := by
    have hex : V / (1 + k) ≤ V / (1 + K) :=
      div_le_div_of_nonneg_left hV0.le (by positivity) (by linarith)
    exact lt_of_le_of_lt (add_le_add_right hex _) hgap
  have hm₂ : 0 ≤ m₂ := div_nonneg (Finset.sum_nonneg fun _ _ ↦ sq_nonneg _) (npos hn).le
  have hM1 : M < 1 := by linarith
  let w := fun i ↦ v i / (1 + k)
  have hw0 (i : Fin n) : 0 < w i := div_pos (exp_pos _) hk1
  have hwM (i : Fin n) : w i ≤ M := div_le_div_of_nonneg_right (hV i) hk1.le
  have hw1 (i : Fin n) : w i < 1 := (hwM i).trans_lt hM1
  obtain ⟨hmean, hscore⟩ := weibull_scores hn y hcW hW
  have hwmean : avg w = 1 / (1 + k) := by
    change (∑ i, v i / (1 + k)) / (n : ℝ) = _
    rw [← Finset.sum_div, div_right_comm]
    change avg v / (1 + k) = _
    rw [hmean]
  have hmul (i : Fin n) : (1 + k) * w i = v i := by
    dsimp [w]
    field_simp [hk1.ne']
  have hR : dualRate y w k = 1 / cW := by
    unfold dualRate
    simp_rw [hmul]
    exact hscore
  have hR0 : 0 < dualRate y w k := hR.symm ▸ one_div_pos.mpr hcW
  have hlog_v (i : Fin n) : log (v i) = cW * y i + bW := log_exp _
  have hnorm : avg (fun i ↦ v i * log (v i)) = avg (fun i ↦ log (v i)) + 1 := by
    have he (i : Fin n) : v i * log (v i) - log (v i) =
        cW * ((v i - 1) * y i) + bW * (v i - 1) := by rw [hlog_v]; ring
    have h := congrArg avg (funext he)
    rw [avg_sub, avg_add, avg_mul, avg_mul, avg_sub, hmean, hscore, avg_const hn,
      sub_self, mul_zero, add_zero, one_div, mul_inv_cancel₀ hcW.ne'] at h
    linarith
  have hWval : W y cW bW = log cW + avg (fun i ↦ log (v i)) - 1 := by
    unfold BurrFree.weibullLik
    simp_rw [← hlog_v]
    simp only [fun i ↦ exp_log (show 0 < v i from exp_pos _)]
    rw [avg_sub, show avg v = 1 from hmean]
    ring
  have hE (i : Fin n) : (1 + k) * binEntropy (w i) = v i * log (1 + k) -
      v i * log (v i) - (1 + k) * ((1 - w i) * log (1 - w i)) := by
    rw [binEntropy_eq_negMulLog_add_negMulLog_one_sub]
    unfold negMulLog
    have he : log (w i) = log (v i) - log (1 + k) :=
      log_div (exp_pos _).ne' hk1.ne'
    rw [he]
    calc
      _ = (1 + k) * w i * (log (1 + k) - log (v i)) -
          (1 + k) * ((1 - w i) * log (1 - w i)) := by ring
      _ = _ := by rw [hmul]; ring
  have hE' := congrArg avg (funext hE)
  rw [avg_mul, avg_sub, avg_sub, avg_mul,
    show (fun i ↦ v i * log (1 + k)) = (fun i ↦ log (1 + k) * v i) by
      funext i; ring, avg_mul, hmean, mul_one, hnorm] at hE'
  have hrem := avg_mono (fun i ↦ entropy_remainder ⟨(hw0 i).le, hwM i⟩ hM1)
  have hrew (i : Fin n) : -(w i) + w i ^ 2 / (2 * (1 - M)) =
      (-1 : ℝ) * w i + (1 / ((1 + k) ^ 2 * (2 * (1 - M)))) * v i ^ 2 := by
    dsimp [w]
    field_simp [hk1.ne', (sub_pos.mpr hM1).ne']
  simp_rw [hrew] at hrem
  rw [avg_add, avg_mul, avg_mul, hwmean] at hrem
  have hrem' := mul_le_mul_of_nonneg_left hrem hk1.le
  have hrem_eq :
      (1 + k) * ((-1) * (1 / (1 + k)) +
        1 / ((1 + k) ^ 2 * (2 * (1 - M))) * m₂) =
      -1 + (m₂ / (2 * (1 - M))) / (1 + k) := by
    field_simp [hk1.ne', (sub_pos.mpr hM1).ne']
  change _ ≤ (1 + k) * ((-1) * (1 / (1 + k)) +
    1 / ((1 + k) ^ 2 * (2 * (1 - M))) * m₂) at hrem'
  rw [hrem_eq] at hrem'
  have hstrict : m₂ / (2 * (1 - M)) < 1 :=
    (div_lt_one (by linarith)).mpr (by linarith)
  have hs := (div_lt_div_iff_of_pos_right hk1).mpr hstrict
  have hklog := log_le_sub_one_of_pos (div_pos hk0 hk1)
  have he : k / (1 + k) - 1 = -(1 / (1 + k)) := by field_simp [hk1.ne']; ring
  rw [he, log_div hk0.ne' hk1.ne'] at hklog
  have hupper := dual_upper hn y w b hk0 hc
    (fun i ↦ ⟨(hw0 i).le, (hw1 i).le⟩) hwmean hR0
  apply hupper.trans_lt
  rw [dualValue, hR, show k / (1 / cW) = k * cW by field_simp [hcW.ne'],
    log_mul hk0.ne' hcW.ne', hE', hWval]
  linarith only [hrem', hs, hklog]

/-! ## Finite cover and three-region comparison -/

private theorem log_chord_bound {l u t : ℝ} (hl : 0 < l) (hlu : l ≤ u)
    (ht : t ∈ Icc (0 : ℝ) 1) :
    log ((1 - t) * l + t * u) - ((1 - t) * log l + t * log u) ≤
      (u - l) ^ 2 / (8 * l ^ 2) := by
  let F := fun x : ℝ ↦ log x + x ^ 2 / (2 * l ^ 2)
  have hder {x : ℝ} (hx : 0 < x) : HasDerivAt F (x⁻¹ + x / l ^ 2) x := by
    have H := (hasDerivAt_log hx.ne').add
      (((hasDerivAt_id x).pow 2).div_const (2 * l ^ 2))
    convert H using 1
    · rfl
    · simp only [id_eq, Nat.cast_ofNat, show (2 : ℕ) - 1 = 1 by decide, pow_one, mul_one]
      field_simp [hl.ne']
  have hm : MonotoneOn (deriv F) (Icc l u) := by
    intro a ha b hb hab
    have ha0 := hl.trans_le ha.1
    have hb0 := ha0.trans_le hab
    rw [(hder ha0).deriv, (hder hb0).deriv]
    have hprod : l ^ 2 ≤ a * b := by
      simpa only [pow_two] using (mul_self_le_mul_self hl.le ha.1).trans
        (mul_le_mul_of_nonneg_left hab ha0.le)
    have h1 : 0 ≤ (b - a) * (a * b - l ^ 2) := mul_nonneg (sub_nonneg.mpr hab)
      (sub_nonneg.mpr hprod)
    apply (le_of_mul_le_mul_left _ (by positivity : 0 < a * b * l ^ 2))
    field_simp [ha0.ne', hb0.ne', hl.ne']
    nlinarith only [h1]
  have hconv : ConvexOn ℝ (Icc l u) F :=
    (hm.mono interior_subset).convexOn_of_deriv (convex_Icc _ _)
      (fun _ hx ↦ (hder (hl.trans_le hx.1)).continuousAt.continuousWithinAt)
      (fun _ hx ↦
        (hder (hl.trans_le (interior_subset hx).1)).differentiableAt.differentiableWithinAt)
  have H := hconv.2 (left_mem_Icc.mpr hlu) (right_mem_Icc.mpr hlu)
    (sub_nonneg.mpr ht.2) ht.1 (by ring : 1 - t + t = 1)
  simp only [smul_eq_mul, F] at H
  have he :
      (1 - t) * (l ^ 2 / (2 * l ^ 2)) + t * (u ^ 2 / (2 * l ^ 2)) -
        (((1 - t) * l + t * u) ^ 2 / (2 * l ^ 2)) =
      (t * (1 - t)) * ((u - l) ^ 2 / (2 * l ^ 2)) := by
    ring
  have htq : t * (1 - t) ≤ 1 / 4 := by nlinarith [sq_nonneg (t - 1 / 2)]
  have hB := mul_le_mul_of_nonneg_right htq
    (show 0 ≤ (u - l) ^ 2 / (2 * l ^ 2) by positivity)
  have hE : (1 / 4 : ℝ) * ((u - l) ^ 2 / (2 * l ^ 2)) = (u - l) ^ 2 / (8 * l ^ 2) := by ring
  linarith only [H, he, hB, hE]

private theorem dual_chord (hn : 0 < n) (y : Fin n → ℝ) {l u k c : ℝ} (b : ℝ)
    (hl : 0 < l) (hk : k ∈ Icc l u) {Uₗ Uᵤ : ℝ}
    (hₗ : L y (l, c, b) ≤ Uₗ) (hᵤ : L y (u, c, b) ≤ Uᵤ) :
    L y (k, c, b) ≤ max Uₗ Uᵤ + (u - l) ^ 2 / (8 * l ^ 2) := by
  have hlu := hk.1.trans hk.2
  by_cases he : l = u
  · have h := le_antisymm (he ▸ hk.2) hk.1
    rw [h, ← he, sub_self, zero_pow (by decide : 2 ≠ 0), zero_div, add_zero]
    exact hₗ.trans (le_max_left _ _)
  have hpos := sub_pos.mpr (hlu.lt_of_ne he)
  let t := (k - l) / (u - l)
  have ht : t ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg (sub_nonneg.mpr hk.1) hpos.le, (div_le_one hpos).mpr
      (sub_le_sub_right hk.2 _)⟩
  have hlin : (1 - t) * l + t * u = k := by
    dsimp [t]
    field_simp [hpos.ne']
    ring
  have hlog := log_chord_bound hl hlu ht
  rw [hlin] at hlog
  have hval : L y (k, c, b) =
      (1 - t) * L y (l, c, b) + t * L y (u, c, b) +
        (log k - ((1 - t) * log l + t * log u)) := by
    simp only [likelihood_affine hn]
    linear_combination (avg (fun i ↦ sp (c * y i + b))) * hlin
  rw [hval]
  have h1 := mul_le_mul_of_nonneg_left (hₗ.trans (le_max_left Uₗ Uᵤ)) (sub_nonneg.mpr ht.2)
  have h2 := mul_le_mul_of_nonneg_left (hᵤ.trans (le_max_right Uₗ Uᵤ)) ht.1
  linarith only [hlog, h1, h2]

/-- Each interval in the compact comparison has exact dual weights at
both endpoints and strict separation after the logarithmic chord error. -/
def DualCover (y : Fin n → ℝ) (l u B : ℝ) : Prop :=
  ∃ (N : ℕ) (a b : Fin N → ℝ) (wₐ wᵦ : Fin N → Fin n → ℝ),
    (∀ k ∈ Icc l u, ∃ j, k ∈ Icc (a j) (b j)) ∧
    (∀ j, 0 < a j ∧ a j ≤ b j ∧
      (∀ i, wₐ j i ∈ Icc (0 : ℝ) 1 ∧ wᵦ j i ∈ Icc (0 : ℝ) 1) ∧
      avg (wₐ j) = 1 / (1 + a j) ∧ avg (wᵦ j) = 1 / (1 + b j) ∧
      0 < dualRate y (wₐ j) (a j) ∧ 0 < dualRate y (wᵦ j) (b j) ∧
      max (dualValue y (wₐ j) (a j)) (dualValue y (wᵦ j) (b j)) +
        (b j - a j) ^ 2 / (8 * (a j) ^ 2) < B)

/-- The finite dual cover rules out every fit whose first shape falls
in the covered interval. -/
theorem dual_cover_upper (hn : 0 < n) {y : Fin n → ℝ} {l u B : ℝ}
    (hcover : DualCover y l u B) {k c b : ℝ} (hk : k ∈ Icc l u) (hc : 0 < c) :
    L y (k, c, b) < B := by
  obtain ⟨N, a, d, wₐ, wᵦ, hdom, hweights⟩ := hcover
  obtain ⟨j, hj⟩ := hdom k hk
  obtain ⟨ha, hd, hw, hmeanₐ, hmeanᵦ, hrₐ, hrᵦ, H⟩ := hweights j
  exact (dual_chord hn y b ha hj
    (dual_upper hn y (wₐ j) b ha hc (fun i ↦ (hw i).1) hmeanₐ hrₐ)
    (dual_upper hn y (wᵦ j) b (ha.trans_le hd) hc (fun i ↦ (hw i).2) hmeanᵦ hrᵦ)).trans_lt H

/-- The entropy cutoff from the zero log observations. -/
def zeroCutoff (y : Fin n → ℝ) : ℝ :=
  (BurrHeld.zeroCount y : ℝ) / (n - (BurrHeld.zeroCount y : ℝ))

private theorem small_shape_of_cutoff (hn : 0 < n) {y : Fin n → ℝ}
    (hmean : 0 < avg y) {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) (hcut : k ≤ zeroCutoff y) :
    L y (k, c, b) < P y := by
  classical
  set m := (BurrHeld.zeroCount y : ℝ)
  have hcard : m ≤ n := by
    have h : BurrHeld.zeroCount y ≤ n := by
      unfold BurrHeld.zeroCount
      exact (Finset.card_filter_le _ _).trans_eq (by simp)
    change (BurrHeld.zeroCount y : ℝ) ≤ (n : ℝ)
    exact_mod_cast h
  have hm : (n : ℝ) * k ≤ (1 + k) * m := by
    rcases hcard.lt_or_eq with h | h
    · have h' := (le_div_iff₀ (sub_pos.mpr h)).mp hcut
      nlinarith only [h']
    · rw [h]
      nlinarith [npos hn]
  exact entropy_small_shape hn hmean hk hc hm

private theorem inverse_weibull_regions (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j)
    {cW bW K : ℝ} (hcW : 0 < cW) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * W y cW bW - ∑ i, log (x i)
    P y < W y cW bW →
    (∀ {k c b : ℝ}, K ≤ k → 0 < c → L y (k, c, b) < W y cW bW) →
    DualCover y (zeroCutoff y) K (W y cW bW) →
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro y F B hdom htail hcover
  have hmean := (reciprocal_log_properties hn hx hne hj).2.2
  have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ BurrFree.domain) : L y p < W y cW bW := by
    rcases p with ⟨k, c, b⟩
    by_cases hlo : k ≤ zeroCutoff y
    · exact (small_shape_of_cutoff hn hmean hp.1 hp.2.1 hlo).trans hdom
    by_cases hhi : K ≤ k
    · exact htail hhi hp.2.1
    · exact dual_cover_upper hn hcover ⟨(le_of_not_ge hlo), (le_of_not_ge hhi)⟩ hp.2.1
  have hmem : ∀ᶠ k : ℝ in atTop, (k, cW, bW - log k) ∈ BurrFree.domain := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
    exact ⟨hk, hcW, mem_univ _⟩
  exact strict_face_transfer hn hx j hstrict hmem (BurrFree.weibull_path hn y cW bW)

/-- A signed dual seam and finite cover certify that inverse Weibull
strictly dominates every finite inverse-Burr fit and the power face. -/
theorem inverse_weibull_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j)
    {cW bW K V : ℝ} (hcW : 0 < cW) (hK : 0 < K) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * W y cW bW - ∑ i, log (x i)
    (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) →
    P y < W y cW bW →
    (∀ i, exp (cW * y i + bW) ≤ V) →
    avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1 →
    DualCover y (zeroCutoff y) K (W y cW bW) →
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro y F B hW hdom hV hs hcover
  exact inverse_weibull_regions hn hx hne hj hcW hdom
    (weibull_dual_seam hn y hcW hW hK hV hs) hcover

/-! ## The written Hessian and score test -/

private theorem radial_upper {f g j : ℝ → ℝ} {N ρ μ q : ℝ}
    (hN : 0 < N) (hρ : 0 < ρ) (hμ : 0 < μ) (hq : q < μ * ρ)
    (hf : ∀ t ∈ Icc (0 : ℝ) 1, HasDerivAt f (g t) t)
    (hg : ∀ t ∈ Icc (0 : ℝ) 1, HasDerivAt g (j t) t)
    (hj : ∀ t ∈ Icc (0 : ℝ) 1, j t ≤ 0)
    (hloc : ∀ t ∈ Icc (0 : ℝ) 1, t * N ≤ ρ → j t ≤ -μ * N ^ 2)
    (hzero : g 0 ≤ q * N) :
    f 1 ≤ f 0 + q ^ 2 / (2 * μ) := by
  let s := min (1 : ℝ) (ρ / N)
  have hs0 : 0 < s := lt_min zero_lt_one (div_pos hρ hN)
  have hs1 : s ≤ 1 := min_le_left _ _
  have hr (t : ℝ) (ht : t ∈ Icc 0 s) : t * N ≤ ρ :=
    (le_div_iff₀ hN).mp (ht.2.trans (min_le_right _ _))
  have hsDom : Icc (0 : ℝ) s ⊆ Icc 0 1 := Icc_subset_Icc_right hs1
  let G : ℝ → ℝ := fun t ↦ g t + μ * N ^ 2 * t
  have hG {t : ℝ} (ht : t ∈ Icc 0 s) : HasDerivAt G (j t + μ * N ^ 2) t := by
    have H := (hg t (hsDom ht)).add ((hasDerivAt_id t).const_mul (μ * N ^ 2))
    change HasDerivAt G _ t at H
    simpa only [mul_one] using H
  have hm : AntitoneOn G (Icc 0 s) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun _ ht ↦ (hG ht).continuousAt.continuousWithinAt
    · exact fun _ ht ↦ (hG (interior_subset ht)).differentiableAt.differentiableWithinAt
    · intro t ht
      have ht' := interior_subset ht
      rw [(hG ht').deriv]
      linarith [hloc t (hsDom ht') (hr t ht')]
  have hb {t : ℝ} (ht : t ∈ Icc 0 s) : g t ≤ g 0 - μ * N ^ 2 * t := by
    have H := hm (show (0 : ℝ) ∈ Icc 0 s from ⟨le_rfl, hs0.le⟩) ht ht.1
    dsimp [G] at H
    linarith
  let F : ℝ → ℝ := fun t ↦ f t - g 0 * t + μ * N ^ 2 * t ^ 2 / 2
  have hF {t : ℝ} (ht : t ∈ Icc 0 s) :
      HasDerivAt F (g t - g 0 + μ * N ^ 2 * t) t := by
    have H := ((hf t (hsDom ht)).sub ((hasDerivAt_id t).const_mul (g 0))).add
      ((((hasDerivAt_id t).pow 2).const_mul (μ * N ^ 2)).div_const 2)
    convert H using 1
    · rfl
    · norm_num
      ring
  have hMF : AntitoneOn F (Icc 0 s) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun _ ht ↦ (hF ht).continuousAt.continuousWithinAt
    · exact fun _ ht ↦ (hF (interior_subset ht)).differentiableAt.differentiableWithinAt
    · intro t ht
      rw [(hF (interior_subset ht)).deriv]
      linarith [hb (interior_subset ht)]
  have H := hMF (show (0 : ℝ) ∈ Icc 0 s from ⟨le_rfl, hs0.le⟩)
    (show s ∈ Icc 0 s from ⟨hs0.le, le_rfl⟩) hs0.le
  have hFs : f s ≤ f 0 + q ^ 2 / (2 * μ) := by
    have hq' := mul_le_mul_of_nonneg_right hzero hs0.le
    have hsq : q * N * s - μ * N ^ 2 * s ^ 2 / 2 ≤ q ^ 2 / (2 * μ) := by
      apply (le_div_iff₀ (by positivity : 0 < 2 * μ)).mpr
      nlinarith only [sq_nonneg (q - μ * (N * s))]
    simp only [F, mul_zero, zero_pow (by decide : 2 ≠ 0), zero_div, sub_zero, add_zero] at H
    linarith only [H, hq', hsq]
  by_cases hs : s = 1
  · simpa only [hs] using hFs
  have he : s = ρ / N := min_eq_right (le_of_not_ge (fun h ↦
    hs (min_eq_left h)))
  have hgs : g s < 0 := by
    have h := hb (show s ∈ Icc 0 s from ⟨hs0.le, le_rfl⟩)
    have hR : μ * N ^ 2 * s = μ * ρ * N := by rw [he]; field_simp [hN.ne']
    rw [hR] at h
    exact h.trans_lt (by linarith [mul_lt_mul_of_pos_right hq hN])
  have hMg : AntitoneOn g (Icc (0 : ℝ) 1) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun t ht ↦ (hg t ht).continuousAt.continuousWithinAt
    · exact fun t ht ↦ (hg t (interior_subset ht)).differentiableAt.differentiableWithinAt
    · intro t ht
      rw [(hg t (interior_subset ht)).deriv]
      exact hj t (interior_subset ht)
  have hMf : AntitoneOn f (Icc s 1) := by
    have hsub : Icc s (1 : ℝ) ⊆ Icc 0 1 := Icc_subset_Icc_left hs0.le
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun t ht ↦ (hf t (hsub ht)).continuousAt.continuousWithinAt
    · exact fun t ht ↦ (hf t (hsub (interior_subset ht))).differentiableAt.differentiableWithinAt
    · intro t ht
      have ht' := interior_subset ht
      rw [(hf t (hsub ht')).deriv]
      exact (hMg ⟨hs0.le, hs1⟩ (hsub ht') ht'.1).trans hgs.le
  exact (hMf ⟨le_rfl, hs1⟩ ⟨hs1, le_rfl⟩ hs1).trans hFs

private theorem model_seam_remainder {ξ v : ℝ} (hξ : 0 < ξ) (hv : 0 ≤ v) :
    v - (1 + ξ) / ξ * log (1 + ξ * v) ≤ ξ * (v ^ 2 / 2 - v) + ξ ^ 2 / 6 := by
  let F := fun v : ℝ ↦ v - (1 + ξ) / ξ * log (1 + ξ * v) - ξ * (v ^ 2 / 2 - v)
  have hd {u : ℝ} (hu : 0 ≤ u) :
      HasDerivAt F (ξ ^ 2 * u * (1 - u) / (1 + ξ * u)) u := by
    have hs : 0 < 1 + ξ * u := by positivity
    have hlog := (((hasDerivAt_id u).const_mul ξ).const_add 1).log hs.ne'
    have H := ((hasDerivAt_id u).sub (hlog.const_mul ((1 + ξ) / ξ))).sub
      (((((hasDerivAt_id u).pow 2).div_const 2).sub (hasDerivAt_id u)).const_mul ξ)
    convert H using 1
    · rfl
    · simp only [id_eq, mul_one, Nat.cast_ofNat, show (2 : ℕ) - 1 = 1 by decide, pow_one]
      field_simp [hξ.ne', hs.ne']
      ring
  let B := fun u : ℝ ↦ ξ ^ 2 * (u ^ 2 / 2 - u ^ 3 / 3)
  have hB (u : ℝ) : HasDerivAt B (ξ ^ 2 * u * (1 - u)) u := by
    have H := ((((hasDerivAt_id u).pow 2).div_const 2).sub
      (((hasDerivAt_id u).pow 3).div_const 3)).const_mul (ξ ^ 2)
    convert H using 1
    · rfl
    · norm_num
      ring
  have hm : MonotoneOn B (Icc (0 : ℝ) 1) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    · exact fun u _ ↦ (hB u).continuousAt.continuousWithinAt
    · exact fun u _ ↦ (hB u).differentiableAt.differentiableWithinAt
    · intro u hu
      have hu' : u ∈ Icc (0 : ℝ) 1 := interior_subset hu
      rw [(hB u).deriv]
      exact mul_nonneg (mul_nonneg (sq_nonneg _) hu'.1) (sub_nonneg.mpr hu'.2)
  have hf : MonotoneOn (B - F) (Icc (0 : ℝ) 1) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    · exact fun u hu ↦ ((hB u).sub (hd hu.1)).continuousAt.continuousWithinAt
    · exact fun u hu ↦
        ((hB u).sub (hd (interior_subset hu).1)).differentiableAt.differentiableWithinAt
    · intro u hu
      have hu' : u ∈ Icc (0 : ℝ) 1 := interior_subset hu
      rw [((hB u).sub (hd hu'.1)).deriv]
      exact sub_nonneg.mpr (div_le_self (mul_nonneg (mul_nonneg (sq_nonneg _) hu'.1)
        (sub_nonneg.mpr hu'.2)) (by linarith [mul_nonneg hξ.le hu'.1]))
  have h0 : (B - F) 0 = 0 := by simp [B, F]
  have hsmall (u : ℝ) (hu : u ∈ Icc (0 : ℝ) 1) : F u ≤ ξ ^ 2 / 6 := by
    have H := hf (show (0 : ℝ) ∈ Icc 0 1 from ⟨le_rfl, zero_le_one⟩) hu hu.1
    rw [h0, Pi.sub_apply] at H
    have H' := hm hu (show (1 : ℝ) ∈ Icc 0 1 from ⟨zero_le_one, le_rfl⟩) hu.2
    have he : B 1 = ξ ^ 2 / 6 := by dsimp [B]; ring
    linarith only [H, H', he]
  have hfinal : F v ≤ ξ ^ 2 / 6 := by
    by_cases h : v ≤ 1
    · exact hsmall v ⟨hv, h⟩
    · have hn : AntitoneOn F (Ici 1) := by
        apply antitoneOn_of_deriv_nonpos (convex_Ici _)
        · exact fun u hu ↦ (hd (zero_le_one.trans hu)).continuousAt.continuousWithinAt
        · exact fun u hu ↦
            (hd (zero_le_one.trans (interior_subset hu))).differentiableAt.differentiableWithinAt
        · intro u hu
          have hu' : 1 ≤ u := interior_subset hu
          have hu0 := (zero_le_one : (0 : ℝ) ≤ 1).trans hu'
          rw [(hd hu0).deriv]
          exact div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonneg_of_nonpos
            (mul_nonneg (sq_nonneg _) hu0) (sub_nonpos.mpr hu')) (by positivity)
      exact (hn le_rfl (le_of_not_ge h) (le_of_not_ge h)).trans (hsmall 1 ⟨zero_le_one, le_rfl⟩)
  exact sub_le_iff_le_add.mp hfinal |>.trans_eq (add_comm _ _)

private def seamLik (y : Fin n → ℝ) (ξ c b : ℝ) : ℝ :=
  log c + avg (fun i ↦ c * y i + b -
    ((1 + ξ) / ξ) * log (1 + ξ * exp (c * y i + b)))

private theorem seam_likelihood_eq (hn : 0 < n) (y : Fin n → ℝ) {k : ℝ}
    (hk : 0 < k) (c b : ℝ) :
    L y (k, c, b - log k) = seamLik y k⁻¹ c b := by
  rw [likelihood_affine hn]
  unfold seamLik
  rw [avg_sub, avg_mul, avg_add, avg_mul, avg_const hn]
  have h (i : Fin n) : sp (c * y i + (b - log k)) =
      log (1 + k⁻¹ * exp (c * y i + b)) := by
    unfold BurrHeld.softplus
    rw [show c * y i + (b - log k) = (c * y i + b) - log k by ring,
      exp_sub, exp_log hk]
    simp only [div_eq_mul_inv, mul_comm]
  simp_rw [h]
  have hK : (1 + k⁻¹) / k⁻¹ = 1 + k := by field_simp [hk.ne']; ring
  rw [hK]
  ring

private theorem seam_directional (y : Fin n → ℝ) {ξ : ℝ} (hξ : 0 < ξ)
    (a b d e t : ℝ) (hc : 0 < a + t * d) :
    let C := fun t : ℝ ↦ a + t * d
    let Z := fun t i ↦ C t * y i + (b + t * e)
    let v := fun t i ↦ exp (Z t i)
    let w := fun i ↦ d * y i + e
    let g := fun t ↦ d / C t + avg (fun i ↦ w i * ((1 - v t i) / (1 + ξ * v t i)))
    HasDerivAt (fun t ↦ seamLik y ξ (C t) (b + t * e)) (g t) t ∧
      HasDerivAt g
        (-(d ^ 2 / (C t) ^ 2) -
          (1 + ξ) * avg (fun i ↦ (v t i / (1 + ξ * v t i) ^ 2) * (w i) ^ 2)) t := by
  intro C Z v w g
  have hC : HasDerivAt C d t := by
    have H := ((hasDerivAt_id t).mul_const d).const_add a
    change HasDerivAt C _ t at H
    simpa only [one_mul] using H
  have hZ (i : Fin n) : HasDerivAt (fun t ↦ Z t i) (w i) t := by
    have H := (hC.mul_const (y i)).add (((hasDerivAt_id t).mul_const e).const_add b)
    change HasDerivAt (fun t ↦ Z t i) _ t at H
    simpa only [one_mul] using H
  have hv (i : Fin n) : HasDerivAt (fun t ↦ v t i) (v t i * w i) t :=
    (hasDerivAt_exp _).comp t (hZ i)
  have hP (i : Fin n) : 0 < 1 + ξ * v t i := add_pos one_pos (mul_pos hξ (exp_pos _))
  have hU (i : Fin n) : HasDerivAt
      (fun t ↦ Z t i - (1 + ξ) / ξ * log (1 + ξ * v t i))
      (w i * ((1 - v t i) / (1 + ξ * v t i))) t := by
    have h := (hZ i).sub (((((hv i).const_mul ξ).const_add 1).log (hP i).ne').const_mul
      ((1 + ξ) / ξ))
    convert h using 1
    simp only [mul_comm ξ (v t i)]
    have hden : 1 + v t i * ξ ≠ 0 := by simpa only [mul_comm ξ (v t i)] using (hP i).ne'
    field_simp [hξ.ne', hden]
    ring
  constructor
  · have h := ((hasDerivAt_log (show C t ≠ 0 from hc.ne')).comp t hC).add (avg_deriv hU)
    change HasDerivAt (fun t ↦ seamLik y ξ (C t) (b + t * e)) _ t at h
    simpa only [g, div_eq_mul_inv, mul_comm] using h
  · have hD (i : Fin n) :
        HasDerivAt (fun t ↦ w i * ((1 - v t i) / (1 + ξ * v t i)))
          (-(1 + ξ) * ((v t i / (1 + ξ * v t i) ^ 2) * (w i) ^ 2)) t := by
      have h := (((hv i).const_sub 1).div (((hv i).const_mul ξ).const_add 1)
        (hP i).ne').const_mul (w i)
      convert h using 1
      ring
    have hbase := (hasDerivAt_const t d).div hC hc.ne'
    have h := hbase.add (avg_deriv hD)
    change HasDerivAt g _ t at h
    rw [avg_mul] at h
    convert h using 1
    ring

private theorem abs_dot_le (d e u v : ℝ) :
    |d * u + e * v| ≤ sqrt (d ^ 2 + e ^ 2) * sqrt (u ^ 2 + v ^ 2) := by
  have h : (d * u + e * v) ^ 2 ≤ (d ^ 2 + e ^ 2) * (u ^ 2 + v ^ 2) := by
    nlinarith only [sq_nonneg (d * v - e * u)]
  rw [← sqrt_mul (by positivity : 0 ≤ d ^ 2 + e ^ 2), ← sqrt_sq_eq_abs]
  exact sqrt_le_sqrt h

private theorem center_gradient_bound (hn : 0 < n) (y : Fin n → ℝ)
    {a b ξ h : ℝ} (ha : 0 < a) (hξ : 0 < ξ) (hξh : ξ ≤ h)
    (hW : ∀ c > 0, ∀ b', W y c b' ≤ W y a b) (d e : ℝ) :
    let v := fun i ↦ exp (a * y i + b)
    let r := fun i ↦ v i * (v i - 1)
    let E := avg (fun i ↦ v i ^ 2 * |v i - 1| * sqrt (y i ^ 2 + 1))
    let D := sqrt ((avg (fun i ↦ r i * y i)) ^ 2 + (avg r) ^ 2) + h * E
    d / a + avg (fun i ↦ (d * y i + e) * ((1 - v i) / (1 + ξ * v i))) ≤
      (ξ * D) * sqrt (d ^ 2 + e ^ 2) := by
  intro v r E D
  let N := sqrt (d ^ 2 + e ^ 2)
  let w := fun i ↦ d * y i + e
  let s := fun i ↦ r i * v i * w i / (1 + ξ * v i)
  obtain ⟨hm, hscore⟩ := weibull_scores hn y ha hW
  have hcancel : d / a + avg (fun i ↦ w i * (1 - v i)) = 0 := by
    have he (i : Fin n) : w i * (1 - v i) =
        -d * ((v i - 1) * y i) + e * (1 - v i) := by dsimp [w]; ring
    simp_rw [he]
    rw [avg_add, avg_mul, avg_mul, avg_sub, avg_const hn, show avg v = 1 from hm,
      show avg (fun i ↦ (v i - 1) * y i) = 1 / a from hscore]
    ring
  have hstep (i : Fin n) :
      w i * ((1 - v i) / (1 + ξ * v i)) =
        w i * (1 - v i) + ξ * (r i * w i) - ξ ^ 2 * s i := by
    have hp : 0 < 1 + ξ * v i := add_pos one_pos (mul_pos hξ (exp_pos _))
    dsimp [s, r]
    simp only [mul_comm ξ (v i)]
    have hden : 1 + v i * ξ ≠ 0 := by simpa only [mul_comm ξ (v i)] using hp.ne'
    field_simp [hden]
    ring
  have hrw : avg (fun i ↦ r i * w i) =
      d * avg (fun i ↦ r i * y i) + e * avg r := by
    have h (i : Fin n) : r i * w i = d * (r i * y i) + e * r i := by dsimp [w]; ring
    simp_rw [h, avg_add, avg_mul]
  have hdot : avg (fun i ↦ r i * w i) ≤
      N * sqrt ((avg (fun i ↦ r i * y i)) ^ 2 + (avg r) ^ 2) := by
    rw [hrw]
    exact (le_abs_self _).trans (abs_dot_le _ _ _ _)
  have hcorr (i : Fin n) : -s i ≤ N * (v i ^ 2 * |v i - 1| * sqrt (y i ^ 2 + 1)) := by
    have hV : 0 < v i := exp_pos _
    have hP : 1 ≤ 1 + ξ * v i := by linarith [mul_pos hξ hV]
    have hP0 : 0 < 1 + ξ * v i := zero_lt_one.trans_le hP
    have hn' : |w i| ≤ N * sqrt (y i ^ 2 + 1) := by
      simpa only [one_pow, mul_one] using abs_dot_le d e (y i) 1
    calc
      -s i ≤ |s i| := neg_le_abs _
      _ = v i ^ 2 * |v i - 1| * |w i| / (1 + ξ * v i) := by
        simp only [s, r, abs_div, abs_mul, abs_of_pos hV, abs_of_pos hP0]
        ring
      _ ≤ v i ^ 2 * |v i - 1| * |w i| :=
        div_le_self (by positivity) hP
      _ ≤ v i ^ 2 * |v i - 1| * (N * sqrt (y i ^ 2 + 1)) :=
        mul_le_mul_of_nonneg_left hn' (mul_nonneg (sq_nonneg _) (abs_nonneg _))
      _ = _ := by ring
  have hx := avg_mono hcorr
  have hnz : avg (fun i ↦ -s i) = -avg s := by
    simp [BurrHeld.avg, Finset.sum_neg_distrib, neg_div]
  rw [avg_mul, hnz] at hx
  have hE : 0 ≤ E := by
    apply div_nonneg _ (npos hn).le
    exact Finset.sum_nonneg (fun _ _ ↦ by positivity)
  have hN : 0 ≤ N := sqrt_nonneg _
  have hcorr' : ξ * (-avg s) ≤ h * (N * E) :=
    (mul_le_mul_of_nonneg_left hx hξ.le).trans
      (mul_le_mul_of_nonneg_right hξh (mul_nonneg hN hE))
  change d / a + avg (fun i ↦ w i * ((1 - v i) / (1 + ξ * v i))) ≤ _
  simp_rw [hstep, avg_sub, avg_add, avg_mul]
  have htot := mul_le_mul_of_nonneg_left (add_le_add hdot hcorr') hξ.le
  dsimp [D]
  nlinarith only [hcancel, htot]

/-- The radius, Hessian lower bound, gradient norm, and signed remainder
inequalities in the written inverse-Weibull seam test. The quadratic-form
inequality expresses the lower eigenvalue bound in two nuisance coordinates. -/
def SourceSignedSeam (y : Fin n → ℝ) (c b h : ℝ) : Prop :=
  ∃ ρ μ : ℝ, 0 < ρ ∧ ρ < c ∧ 0 < μ ∧
    let v := fun i ↦ exp (c * y i + b)
    let t := fun i ↦ sqrt (y i ^ 2 + 1)
    let α := fun i ↦ v i * exp (-ρ * t i)
    let β := fun i ↦ v i * exp (ρ * t i)
    let A := avg (fun i ↦ v i ^ 2) / 2 - 1
    let r := fun i ↦ v i * (v i - 1)
    let D := sqrt ((avg (fun i ↦ r i * y i)) ^ 2 + (avg r) ^ 2) +
      h * avg (fun i ↦ v i ^ 2 * |v i - 1| * t i)
    (∀ d e : ℝ, μ * (d ^ 2 + e ^ 2) ≤ d ^ 2 / (c + ρ) ^ 2 +
      avg (fun i ↦ (α i / (1 + h * β i) ^ 2) * (d * y i + e) ^ 2)) ∧
      A < 0 ∧ h * D < μ * ρ ∧ h * (1 / 6 + D ^ 2 / (2 * μ)) < -A / 2

private theorem center_value_bound (hn : 0 < n) (y : Fin n → ℝ) {ξ : ℝ}
    (hξ : 0 < ξ) (a b : ℝ) (hm : avg (fun i ↦ exp (a * y i + b)) = 1) :
    seamLik y ξ a b ≤ W y a b + ξ * (avg (fun i ↦ exp (a * y i + b) ^ 2) / 2 - 1) +
      ξ ^ 2 / 6 := by
  let v := fun i ↦ exp (a * y i + b)
  have H := avg_mono (fun i ↦ model_seam_remainder hξ (exp_pos (a * y i + b)).le)
  rw [avg_sub, avg_mul, avg_add, avg_mul, avg_sub, avg_const hn, hm] at H
  have hv (i : Fin n) : exp (a * y i + b) ^ 2 / 2 = (1 / 2 : ℝ) * v i ^ 2 := by
    dsimp only [v]
    ring
  simp_rw [hv, avg_mul] at H
  unfold seamLik BurrFree.weibullLik
  rw [avg_sub, avg_mul, avg_sub, hm]
  dsimp only [v] at H
  linarith only [H]

/-- The original signed collar test excludes every finite Burr point
at inverse shape at most the seam width. -/
theorem source_signed_seam_upper (hn : 0 < n) (y : Fin n → ℝ) {a b h : ℝ}
    (ha : 0 < a) (hh : 0 < h) (hW : ∀ c > 0, ∀ b', W y c b' ≤ W y a b)
    (hsource : SourceSignedSeam y a b h) :
    ∀ {k c b' : ℝ}, 1 / h ≤ k → 0 < c → L y (k, c, b') < W y a b := by
  intro k c b' hk hc
  obtain ⟨ρ, μ, hρ, _, hμ, hmatrix, hA, hDρ, hsign⟩ := hsource
  let ξ := k⁻¹
  have hk0 : 0 < k := (one_div_pos.mpr hh).trans_le hk
  have hξ : 0 < ξ := inv_pos.mpr hk0
  have hξh : ξ ≤ h := by
    change k⁻¹ ≤ h
    exact (inv_le_comm₀ hk0 hh).mpr (by simpa only [one_div] using hk)
  let v := fun i ↦ exp (a * y i + b)
  let tlen := fun i ↦ sqrt (y i ^ 2 + 1)
  let α := fun i ↦ v i * exp (-ρ * tlen i)
  let β := fun i ↦ v i * exp (ρ * tlen i)
  let A := avg (fun i ↦ v i ^ 2) / 2 - 1
  let r := fun i ↦ v i * (v i - 1)
  let D := sqrt ((avg (fun i ↦ r i * y i)) ^ 2 + (avg r) ^ 2) +
    h * avg (fun i ↦ v i ^ 2 * |v i - 1| * tlen i)
  have hD0 : 0 ≤ D := by
    have hE : 0 ≤ avg (fun i ↦ v i ^ 2 * |v i - 1| * tlen i) :=
      div_nonneg (Finset.sum_nonneg fun _ _ ↦ by positivity) (npos hn).le
    exact add_nonneg (sqrt_nonneg _) (mul_nonneg hh.le hE)
  let d := c - a
  let e := b' + log k - b
  let N := sqrt (d ^ 2 + e ^ 2)
  have hN0 : 0 ≤ N := sqrt_nonneg _
  have hN2 : N ^ 2 = d ^ 2 + e ^ 2 := sq_sqrt (by positivity)
  let C := fun t : ℝ ↦ a + t * d
  let B := fun t : ℝ ↦ b + t * e
  let V := fun t i ↦ exp (C t * y i + B t)
  let w := fun i ↦ d * y i + e
  let f := fun t ↦ seamLik y ξ (C t) (B t)
  let g := fun t ↦ d / C t + avg (fun i ↦ w i * ((1 - V t i) / (1 + ξ * V t i)))
  let J := fun t ↦ -(d ^ 2 / (C t) ^ 2) -
    (1 + ξ) * avg (fun i ↦ (V t i / (1 + ξ * V t i) ^ 2) * (w i) ^ 2)
  have hC0 : C 0 = a := by simp [C]
  have hB0 : B 0 = b := by simp [B]
  have hC1 : C 1 = c := by simp [C, d]
  have hB1 : B 1 = b' + log k := by simp [B, e]
  have hCpos (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) : 0 < C t := by
    by_cases ht0 : t = 0
    · rw [ht0, hC0]
      exact ha
    have h : C t = (1 - t) * a + t * c := by dsimp [C, d]; ring
    rw [h]
    exact add_pos_of_nonneg_of_pos (mul_nonneg (sub_nonneg.mpr ht.2) ha.le)
      (mul_pos (ht.1.lt_of_ne (Ne.symm ht0)) hc)
  have hder (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) :
      HasDerivAt f (g t) t ∧ HasDerivAt g (J t) t :=
    seam_directional y hξ a b d e t (hCpos t ht)
  have hj (t : ℝ) : J t ≤ 0 := by
    have h : 0 ≤ avg (fun i ↦ (V t i / (1 + ξ * V t i) ^ 2) * (w i) ^ 2) :=
      div_nonneg (Finset.sum_nonneg fun _ _ ↦ by positivity) (npos hn).le
    exact sub_nonpos.mpr (le_trans (neg_nonpos.mpr (div_nonneg (sq_nonneg _) (sq_nonneg _)))
      (mul_nonneg (by positivity) h))
  have hloc (t : ℝ) (ht : t ∈ Icc (0 : ℝ) 1) (htρ : t * N ≤ ρ) : J t ≤ -μ * N ^ 2 := by
    have hCd : C t ≤ a + ρ := by
      have habs : |d| ≤ N := by simpa [N] using abs_dot_le d e 1 0
      have h := (mul_le_mul_of_nonneg_left ((le_abs_self d).trans habs) ht.1).trans htρ
      dsimp [C]
      linarith only [h]
    have hC2 : (C t) ^ 2 ≤ (a + ρ) ^ 2 := by
      simpa only [pow_two] using mul_self_le_mul_self (hCpos t ht).le hCd
    have hterm₁ : d ^ 2 / (a + ρ) ^ 2 ≤ d ^ 2 / (C t) ^ 2 :=
      div_le_div_of_nonneg_left (sq_nonneg _) (pow_pos (hCpos t ht) _) hC2
    have hVbounds (i : Fin n) : α i ≤ V t i ∧ V t i ≤ β i := by
      have hdot : |w i| ≤ N * tlen i := by
        simpa only [one_pow, mul_one] using abs_dot_le d e (y i) 1
      have hnorm : |t * w i| ≤ ρ * tlen i := by
        rw [abs_mul, abs_of_nonneg ht.1]
        exact (mul_le_mul_of_nonneg_left hdot ht.1).trans
          (by rw [← mul_assoc]; exact mul_le_mul_of_nonneg_right htρ (sqrt_nonneg _))
      have heq : V t i = v i * exp (t * w i) := by
        unfold V C B v w
        rw [← exp_add]
        congr 1
        ring
      rw [heq]
      exact ⟨mul_le_mul_of_nonneg_left
        (exp_le_exp.mpr (by simpa only [neg_mul] using (abs_le.mp hnorm).1)) (exp_pos _).le,
        mul_le_mul_of_nonneg_left (exp_le_exp.mpr ((abs_le.mp hnorm).2)) (exp_pos _).le⟩
    have hterm₂ := avg_mono (fun i ↦ show
        (α i / (1 + h * β i) ^ 2) * (w i) ^ 2 ≤
          (V t i / (1 + ξ * V t i) ^ 2) * (w i) ^ 2 from by
      have hv1 : 0 < V t i := exp_pos _
      have hβ : 0 < β i := mul_pos (exp_pos _) (exp_pos _)
      have hA0 : 0 < 1 + ξ * V t i := by positivity
      have hden : 1 + ξ * V t i ≤ 1 + h * β i :=
        add_le_add_right ((mul_le_mul_of_nonneg_left (hVbounds i).2 hξ.le).trans
          (mul_le_mul_of_nonneg_right hξh hβ.le)) _
      have hden' : (1 + ξ * V t i) ^ 2 ≤ (1 + h * β i) ^ 2 := by
        simpa only [pow_two] using mul_self_le_mul_self hA0.le hden
      apply mul_le_mul_of_nonneg_right _ (sq_nonneg _)
      exact (div_le_div_of_nonneg_right (hVbounds i).1 (sq_nonneg _)).trans
        (div_le_div_of_nonneg_left hv1.le (pow_pos hA0 _) hden'))
    have hM := hmatrix d e
    change μ * (d ^ 2 + e ^ 2) ≤ d ^ 2 / (a + ρ) ^ 2 +
      avg (fun i ↦ (α i / (1 + h * β i) ^ 2) * (w i) ^ 2) at hM
    have hQ : 0 ≤ avg (fun i ↦ V t i / (1 + ξ * V t i) ^ 2 * (w i) ^ 2) :=
      div_nonneg (Finset.sum_nonneg fun _ _ ↦ by positivity) (npos hn).le
    dsimp only [J]
    rw [hN2]
    nlinarith only [hM, hterm₁, hterm₂, mul_nonneg hξ.le hQ]
  have hdelta : f 1 ≤ f 0 + (ξ * D) ^ 2 / (2 * μ) := by
    by_cases hn0 : N = 0
    · have hsum : d ^ 2 + e ^ 2 = 0 := by rw [← hN2, hn0]; ring
      have hd0 : d = 0 := by nlinarith [sq_nonneg e]
      have he0 : e = 0 := by nlinarith [sq_nonneg d]
      have hF : f 1 = f 0 := by simp [f, C, B, hd0, he0]
      exact hF.le.trans (le_add_of_nonneg_right (div_nonneg (sq_nonneg _) (by positivity)))
    · have hq : ξ * D < μ * ρ := (mul_le_mul_of_nonneg_right hξh hD0).trans_lt hDρ
      have h0 := center_gradient_bound hn y ha hξ hξh hW d e
      have hz (i : Fin n) : V 0 i = v i := by simp [V, hC0, hB0, v]
      have hgz : g 0 ≤ (ξ * D) * N := by
        dsimp only [g]
        simp_rw [hC0, hz]
        exact h0
      exact radial_upper (hN0.lt_of_ne (Ne.symm hn0)) hρ hμ hq
        (fun t ht ↦ (hder t ht).1) (fun t ht ↦ (hder t ht).2) (fun t _ ↦ hj t) hloc hgz
  have hbase := center_value_bound hn y hξ a b (weibull_scores hn y ha hW).1
  have hF0 : f 0 ≤ W y a b + ξ * A + ξ ^ 2 / 6 := by
    simpa only [f, hC0, hB0, A, v] using hbase
  have hF1 : f 1 = L y (k, c, b') := by
    change seamLik y k⁻¹ (C 1) (B 1) = _
    rw [hC1, hB1, ← seam_likelihood_eq hn y hk0, add_sub_cancel_right]
  have hrem : ξ ^ 2 / 6 + (ξ * D) ^ 2 / (2 * μ) = ξ ^ 2 * (1 / 6 + D ^ 2 / (2 * μ)) := by ring
  have hm : 0 ≤ 1 / 6 + D ^ 2 / (2 * μ) := by positivity
  change h * (1 / 6 + D ^ 2 / (2 * μ)) < -A / 2 at hsign
  have H := (mul_le_mul_of_nonneg_right hξh hm).trans_lt hsign
  have H' : ξ ^ 2 * (1 / 6 + D ^ 2 / (2 * μ)) < -(ξ * A) / 2 := by
    nlinarith only [mul_lt_mul_of_pos_left H hξ]
  have hA' : A < 0 := hA
  rw [← hF1]
  linarith only [hF0, hdelta, hrem, H', mul_neg_of_pos_of_neg hξ hA']

/-- For a positive definite symmetric two-by-two form, determinant over
trace is an explicit positive lower bound for its smallest eigenvalue. -/
theorem det_over_trace_bound {a b c : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hdet : 0 < a * c - b ^ 2) :
    let μ := (a * c - b ^ 2) / (a + c)
    0 < μ ∧ ∀ d e : ℝ, μ * (d ^ 2 + e ^ 2) ≤ a * d ^ 2 + 2 * b * d * e + c * e ^ 2 := by
  intro μ
  have ht := add_pos ha hc
  have hμ : 0 < μ := div_pos hdet ht
  have hprod : μ * (a + c) = a * c - b ^ 2 := div_mul_cancel₀ _ ht.ne'
  have hpos : 0 < a - μ := by
    apply (mul_pos_iff_of_pos_right ht).mp
    nlinarith only [hprod, sq_pos_of_pos ha, sq_nonneg b]
  have hD : (a - μ) * (c - μ) - b ^ 2 = μ ^ 2 := by
    nlinarith only [hprod]
  refine ⟨hμ, fun d e ↦ ?_⟩
  have h := add_nonneg (div_nonneg (sq_nonneg ((a - μ) * d + b * e)) hpos.le)
    (mul_nonneg (div_nonneg (sq_nonneg μ) hpos.le) (sq_nonneg e))
  have he : ((a - μ) * d + b * e) ^ 2 / (a - μ) + (μ ^ 2 / (a - μ)) * e ^ 2 =
      (a - μ) * d ^ 2 + 2 * b * d * e + (c - μ) * e ^ 2 := by
    rw [← hD]
    field_simp [hpos.ne']
    ring
  rw [he] at h
  linarith only [h]

/-- The written small-shape, signed-collar, and compact-cover checks
transport to strict inverse-Weibull dominance on the original sample. -/
theorem inverse_weibull_source_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j)
    {cW bW h : ℝ} (hcW : 0 < cW) (hh : 0 < h) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * W y cW bW - ∑ i, log (x i)
    (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) →
    P y < W y cW bW → SourceSignedSeam y cW bW h →
    DualCover y (zeroCutoff y) (1 / h) (W y cW bW) →
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro y F B hW hdom hsource hcover
  exact inverse_weibull_regions hn hx hne hj hcW hdom
    (source_signed_seam_upper hn y hcW hh hW hsource) hcover

/-! ## Joined failure statement -/

/-- The all-free inverse-Burr failure split. Constant samples diverge.
For nonconstant samples the larger reachable face is the strict
unattained supremum exactly when every finite fit loses. Contact with
that height suffices for attainment. The convex-order power screen and
the signed dual inverse-Weibull screen both imply strict escape. -/
theorem inverse_all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let J := ∑ i, log (x i)
    let D := BurrFree.physicalDomain
    ((∀ i k, x i = x k) → ¬ BddAbove (F '' D)) ∧
    ((∃ i k, x i ≠ x k) → ∃ j : Fin n, (∀ i, x i ≤ x j) ∧
      let y := BurrFree.logExcess (Transfer.reciprocal x) j
      ∃ cW > 0, ∃ bW, (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) ∧
        let B := n * max (P y) (W y cW bW) - J
        ((∃ p ∈ D, IsMaxOn F D p) ↔ ∃ p ∈ D, B ≤ F p) ∧
        ((¬ ∃ p ∈ D, IsMaxOn F D p) → IsLUB (F '' D) B ∧ ∀ p ∈ D, F p < B) ∧
        (FiniteNormalizedOrder y powerReference →
          W y cW bW < P y ∧ IsLUB (F '' D) (n * P y - J) ∧ ∀ p ∈ D, F p < n * P y - J) ∧
        (∀ h > 0, P y < W y cW bW → SourceSignedSeam y cW bW h →
          DualCover y (zeroCutoff y) (1 / h) (W y cW bW) →
          IsLUB (F '' D) (n * W y cW bW - J) ∧ ∀ p ∈ D, F p < n * W y cW bW - J) ∧
        (∀ K > 0, ∀ V, P y < W y cW bW →
          (∀ i, exp (cW * y i + bW) ≤ V) →
          avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1 →
          DualCover y (zeroCutoff y) K (W y cW bW) →
          IsLUB (F '' D) (n * W y cW bW - J) ∧ ∀ p ∈ D, F p < n * W y cW bW - J)) := by
  intro F J D
  constructor
  · intro he
    let j : Fin n := ⟨0, hn⟩
    let v := x j
    have hv : 0 < v := hx j
    have he' : x = fun _ ↦ v := funext fun i ↦ he i j
    change ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦
      ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2) '' BurrFree.physicalDomain)
    rw [he']
    exact ShapeFacesInverseBurr.joint_constant_unbounded hn hv
  · intro hne
    obtain ⟨j, -, hj⟩ := Finset.exists_max_image (Finset.univ : Finset (Fin n)) x
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    have hj' (i : Fin n) : x i ≤ x j := hj i (Finset.mem_univ i)
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    obtain ⟨cW, hcW, bW, hW, hpair, _⟩ := BurrFree.inverse_criterion_and_escape hn hx hne hj'
    let B := n * max (P y) (W y cW bW) - J
    refine ⟨j, hj', cW, hcW, bW, hW, hpair, ?_, ?_, ?_, ?_⟩
    · intro hnmax
      have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ BurrFree.domain) :
          L y p < max (P y) (W y cW bW) := by
        obtain ⟨q, hq, he⟩ := chart_surj x j hp
        have hq' : F q < B := lt_of_not_ge (fun h ↦ hnmax (hpair.mpr ⟨q, hq, h⟩))
        change inverseObjective x q < _ at hq'
        rw [inverse_chart_value hn hx j hq, he] at hq'
        exact lt_of_mul_lt_mul_left ((sub_lt_sub_iff_right J).mp hq') (npos hn).le
      by_cases hface : W y cW bW ≤ P y
      · have hh (p : ℝ × ℝ × ℝ) (hp : p ∈ BurrFree.domain) : L y p < P y :=
          (hstrict p hp).trans_le (le_of_eq (max_eq_left hface))
        obtain ⟨hy, _, hmean⟩ := reciprocal_log_properties hn hx hne hj'
        have hmem : ∀ᶠ t : ℝ in atTop, ((avg y)⁻¹ / t ^ 2, t ^ 2, t) ∈ BurrFree.domain := by
          filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
          exact ⟨div_pos (inv_pos.mpr hmean) (sq_pos_of_pos ht), sq_pos_of_pos ht, mem_univ _⟩
        change IsLUB (F '' D) B ∧ ∀ p ∈ D, F p < B
        dsimp only [B]
        rw [max_eq_left hface]
        exact strict_face_transfer hn hx j hh hmem (BurrFree.pareto_path hn hy hmean)
      · have hface' := le_of_not_ge hface
        have hh (p : ℝ × ℝ × ℝ) (hp : p ∈ BurrFree.domain) : L y p < W y cW bW :=
          (hstrict p hp).trans_le (le_of_eq (max_eq_right hface'))
        have hmem : ∀ᶠ t : ℝ in atTop, (t, cW, bW - log t) ∈ BurrFree.domain := by
          filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
          exact ⟨ht, hcW, mem_univ _⟩
        change IsLUB (F '' D) B ∧ ∀ p ∈ D, F p < B
        dsimp only [B]
        rw [max_eq_right hface']
        exact strict_face_transfer hn hx j hh hmem (BurrFree.weibull_path hn y cW bW)
    · intro hord
      obtain ⟨hless, hpow⟩ := inverse_power_convex_order hn hx hne hj'
        ((finite_normalized_order_iff hn (by decide : 0 < 7)
          (reciprocal_log_properties hn hx hne hj').2.2 reference_mean.1).mp hord)
      exact ⟨hless cW hcW bW, hpow⟩
    · intro h hh hdom hseam hcover
      exact inverse_weibull_source_comparison hn hx hne hj' hcW hh hW hdom hseam hcover
    · intro K hK V hdom hV hs hcover
      exact inverse_weibull_comparison hn hx hne hj' hcW hK hW hdom hV hs hcover

/-! ## An exact inverse-Weibull example -/

private theorem log_one_sub_quadratic {t : ℝ} (ht : 0 ≤ t) (ht' : t < 1) :
    log (1 - t) ≤ -t - t ^ 2 / 2 := by
  let F := fun s : ℝ ↦ -s - s ^ 2 / 2 - log (1 - s)
  have hder {s : ℝ} (hs : s < 1) :
      HasDerivAt F (s ^ 2 / (1 - s)) s := by
    have H := (((hasDerivAt_id s).neg).sub (((hasDerivAt_id s).pow 2).div_const 2)).sub
      (((hasDerivAt_id s).const_sub 1).log (sub_pos.mpr hs).ne')
    convert H using 1
    · rfl
    · norm_num
      field_simp [(sub_pos.mpr hs).ne']
      ring
  have hm : MonotoneOn F (Icc 0 t) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    · exact fun s hs ↦ (hder (hs.2.trans_lt ht')).continuousAt.continuousWithinAt
    · exact fun s hs ↦
        (hder ((interior_subset hs).2.trans_lt ht')).differentiableAt.differentiableWithinAt
    · intro s hs
      have h : s < 1 := (interior_subset hs).2.trans_lt ht'
      rw [(hder h).deriv]
      exact div_nonneg (sq_nonneg _) (sub_pos.mpr h).le
  have H := hm (show (0 : ℝ) ∈ Icc 0 t from ⟨le_rfl, ht⟩)
    (show t ∈ Icc 0 t from ⟨ht, le_rfl⟩) ht
  have he : F 0 = 0 := by simp [F]
  rw [he] at H
  exact sub_nonneg.mp H

private def rareLog : Fin 8 → ℝ := ![0, 1, 1, 1, 1, 1, 1, 1]

private theorem rare_avg (f : ℝ → ℝ) :
    avg (fun i ↦ f (rareLog i)) = (f 0 + 7 * f 1) / 8 := by
  norm_num [BurrHeld.avg, rareLog, Fin.sum_univ_succ]
  ring

private theorem rare_mean : avg rareLog = 7 / 8 := by
  simpa using rare_avg (fun z ↦ z)

private theorem rare_weibull_wins {a b : ℝ} (ha : 0 < a)
    (hW : ∀ c > 0, ∀ u, W rareLog c u ≤ W rareLog a b) :
    P rareLog < W rareLog a b ∧
      ∀ p ∈ BurrFree.domain, L rareLog p < W rareLog a b := by
  let W₀ := log (8 : ℝ) - log (7 / 8) - 2
  have hN : 0 < 8 := by decide
  let b₀ := -(8 : ℝ) - log (7 / 8)
  have hface : W₀ - 1 / 126 < W rareLog a b := by
    have hW₀ : W rareLog 8 b₀ = W₀ - exp (-8) / 7 := by
      unfold BurrFree.weibullLik
      rw [rare_avg (fun z ↦ (8 : ℝ) * z + b₀ - exp (8 * z + b₀))]
      dsimp [b₀, W₀]
      rw [show (8 : ℝ) * 0 + (-8 - log (7 / 8)) = -8 - log (7 / 8) by ring,
        show (8 : ℝ) * 1 + (-8 - log (7 / 8)) = -log (7 / 8) by ring,
        exp_sub, exp_neg, exp_log (by norm_num : 0 < (7 / 8 : ℝ)),
        show exp (-log (7 / 8)) = (8 / 7 : ℝ) from by
          rw [exp_neg, exp_log (by norm_num : 0 < (7 / 8 : ℝ))]; norm_num]
      ring
    have hpos : 5 ≤ exp (4 : ℝ) := by linarith [add_one_le_exp (4 : ℝ)]
    have hlarge : (18 : ℝ) < exp 8 := by
      rw [show (8 : ℝ) = 4 + 4 by norm_num, exp_add]
      nlinarith [sq_nonneg (exp (4 : ℝ) - 5)]
    have hsmall : exp (-8) / 7 < 1 / 126 := by
      rw [exp_neg]
      apply (div_lt_iff₀ (by norm_num : (0 : ℝ) < 7)).mpr
      rw [show (1 / 126 : ℝ) * 7 = (18 : ℝ)⁻¹ by norm_num]
      exact (inv_lt_inv₀ (exp_pos (8 : ℝ)) (by norm_num : 0 < (18 : ℝ))).mpr hlarge
    exact (show W₀ - 1 / 126 < W rareLog 8 b₀ from by linarith).trans_le
      (hW 8 (by norm_num) b₀)
  have hP : P rareLog < W rareLog a b := by
    have hlog : 3 / 2 ≤ log (8 : ℝ) := by
      rw [show (8 : ℝ) = 2 ^ 3 by norm_num, log_pow]
      linarith [one_sub_inv_le_log_of_pos (by norm_num : 0 < (2 : ℝ))]
    apply lt_trans _ hface
    rw [BurrFree.paretoValue, rare_mean]
    dsimp [W₀]
    linarith
  refine ⟨hP, ?_⟩
  rintro ⟨k, c, b'⟩ hp
  have hk : 0 < k := hp.1
  have hc : 0 < c := hp.2.1
  by_cases hlo : k ≤ 1 / 7
  · have hm : 0 < avg rareLog := by rw [rare_mean]; norm_num
    exact (BurrFree.small_shape_lt_pareto hN hm (j := 0) (by rfl) hk hc
      (by norm_num; linarith)).trans hP
  by_cases hhi : 2 ≤ k
  · have hv₀ (i : Fin 8) : exp (a * rareLog i + b) ≤ (8 / 7 : ℝ) := by
      have hm := (weibull_scores hN rareLog ha hW).1
      rw [rare_avg (fun z ↦ exp (a * z + b))] at hm
      have hv1 : exp (a * 1 + b) ≤ (8 / 7 : ℝ) := by
        linarith only [hm, exp_pos (a * 0 + b)]
      have hi : rareLog i ≤ 1 := by fin_cases i <;> norm_num [rareLog]
      have H : a * rareLog i + b ≤ a * 1 + b :=
        add_le_add_left (mul_le_mul_of_nonneg_left hi ha.le) b
      exact (exp_le_exp.mpr H).trans hv1
    have h2 : avg (fun i ↦ exp (a * rareLog i + b) ^ 2) ≤ 8 / 7 := by
      have h := avg_mono (fun i ↦ mul_le_mul_of_nonneg_right (hv₀ i)
        (exp_pos (a * rareLog i + b)).le)
      simp only [← pow_two, avg_mul, (weibull_scores hN rareLog ha hW).1, mul_one] at h
      exact h
    exact weibull_dual_seam hN rareLog ha hW (K := 2) (by norm_num) hv₀
      (by linarith only [h2]) hhi hc
  have hk1 : 0 < 1 + k := by positivity
  let t := 1 / (1 + k)
  let d := 8 / (7 * (1 + k))
  let w : Fin 8 → ℝ := ![0, d, d, d, d, d, d, d]
  have hdt : d = (8 / 7) * t := by dsimp [d, t]; field_simp [hk1.ne']
  have ht : 0 < t := one_div_pos.mpr hk1
  have ht1 : t < 1 := (div_lt_one hk1).mpr (by linarith)
  have htt : 1 / 3 ≤ t := (le_div_iff₀ hk1).mpr (by linarith)
  have hd0 : 0 < d := by dsimp [d]; positivity
  have hd1 : d < 1 := (div_lt_one (by positivity : 0 < 7 * (1 + k))).mpr (by linarith)
  have hw : ∀ i, w i ∈ Icc (0 : ℝ) 1 := by
    intro i
    fin_cases i <;> first | exact ⟨hd0.le, hd1.le⟩ | exact ⟨le_rfl, zero_le_one⟩
  have hmean : avg w = t := by
    norm_num [BurrHeld.avg, w, Fin.sum_univ_succ]
    rw [hdt]
    ring
  have hrate : dualRate rareLog w k = 1 / 8 := by
    norm_num [BurrHeld.avg, w, rareLog, dualRate, Fin.sum_univ_succ]
    dsimp [d]
    field_simp [hk1.ne']
    ring
  have hE : (1 + k) * avg (fun i ↦ binEntropy (w i)) = (1 + k) * (7 * binEntropy d / 8) := by
    congr 1
    norm_num [BurrHeld.avg, w, Fin.sum_univ_succ]
    ring
  have hnorm : (1 + k) * (7 / 8) * d = 1 := by
    dsimp [d]
    field_simp [hk1.ne']
  have hent : 1 - d - log d ≤ (1 + k) * avg (fun i ↦ binEntropy (w i)) := by
    rw [hE]
    have h := mul_le_mul_of_nonneg_left (entropy_one_side ⟨hd0, hd1⟩)
      (show 0 ≤ (1 + k) * (7 / 8) by positivity)
    rw [mul_sub, ← mul_assoc, hnorm, ← mul_assoc, hnorm, one_mul, one_mul] at h
    linarith only [h]
  have hz : 1 - t = k / (1 + k) := by dsimp [t]; field_simp [hk1.ne']; ring
  have hlog := log_one_sub_quadratic ht.le ht1
  have he : log (k / (1 / 8)) + log d = W₀ + 2 + log (1 - t) := by
    rw [hz, log_div hk.ne' hk1.ne']
    dsimp [d, W₀]
    rw [log_div hk.ne' (by norm_num : (1 / 8 : ℝ) ≠ 0), log_div (by norm_num : (8 : ℝ) ≠ 0)
      (by positivity : 7 * (1 + k) ≠ 0), log_mul (by norm_num : (7 : ℝ) ≠ 0) hk1.ne',
      log_div (by norm_num : (7 : ℝ) ≠ 0) (by norm_num : (8 : ℝ) ≠ 0),
      one_div, log_inv]
    ring
  have hGap : t / 7 - t ^ 2 / 2 ≤ -(1 / 126) := by
    nlinarith only [htt, mul_nonneg (show 0 ≤ t - 1 / 3 by linarith)
      (show 0 ≤ t / 2 + 1 / 42 by linarith)]
  have hupper := dual_upper hN rareLog w b' hk hc hw hmean
    (show 0 < dualRate rareLog w k by rw [hrate]; norm_num)
  apply hupper.trans_lt (show dualValue rareLog w k < W rareLog a b from ?_)
  unfold dualValue
  rw [hrate]
  linarith only [hent, hlog, he, hGap, hface, hdt]

/-- An exact inverse-Weibull-only example has one observation at one
and seven observations at the reciprocal of Euler's number. -/
theorem inverse_weibull_exact_example :
    let x : Fin 8 → ℝ := fun i ↦ if i = 0 then 1 else exp (-1)
    let y := BurrFree.logExcess (Transfer.reciprocal x) 0
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    ∃ a > 0, ∃ b, (∀ c > 0, ∀ t, W y c t ≤ W y a b) ∧ P y < W y a b ∧
      let B := 8 * W y a b - ∑ i, log (x i)
      IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B := by
  intro x y F
  have hx (i : Fin 8) : 0 < x i := by dsimp [x]; split <;> positivity
  have he : y = rareLog := by
    funext i
    fin_cases i <;>
      norm_num [x, y, rareLog, BurrFree.logExcess, Transfer.reciprocal, log_inv, log_exp]
  have hne : ∃ i j, rareLog i ≠ rareLog j := ⟨0, 1, by norm_num [rareLog]⟩
  obtain ⟨a, ha, b, hW⟩ := BurrFree.weibull_optimized (by decide : 0 < 8) hne
  obtain ⟨hP, hstrict⟩ := rare_weibull_wins ha hW
  have ht : ∀ᶠ k : ℝ in atTop, (k, a, b - log k) ∈ BurrFree.domain :=
    (eventually_gt_atTop (0 : ℝ)).mono (fun _ hk ↦ ⟨hk, ha, mem_univ _⟩)
  have H := strict_face_transfer (by decide : 0 < 8) hx 0 (V := W rareLog a b)
    (fun p hp ↦ by change L y p < _; rw [he]; exact hstrict p hp) ht
    (show Tendsto (fun k : ℝ ↦ L y (k, a, b - log k)) atTop (𝓝 (W rareLog a b)) from by
      rw [he]
      exact BurrFree.weibull_path (by decide) rareLog a b)
  rw [he]
  exact ⟨a, ha, b, hW, hP, H⟩

end

end UnivariateML.BPInverseBurr

end
