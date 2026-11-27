module

public import UnivariateML.Generic.UniqueAE
import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Analysis.Convex.SpecificFunctions.Deriv
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-count Birnbaum–Saunders likelihood charts

The location parameter is the threshold. Contact with the threshold is
excluded by the positive-support density convention.
-/

@[expose] public section

open Set MeasureTheory ContinuousLinearMap
open scoped Topology

namespace UnivariateML.AE2Fatigue

noncomputable section

variable {n : ℕ}

/-- The coordinates stand for shape, scale and threshold, respectively. -/
structure Coordinates where
  shape : ℝ
  scale : ℝ
  threshold : ℝ

/-- The strictly positive support of the sample in a parameter chart. -/
def domain {P : Type*} (p : P → Coordinates) : Set (P × (Fin n → ℝ)) :=
  {z | 0 < (p z.1).shape ∧ 0 < (p z.1).scale ∧ ∀ i, (p z.1).threshold < z.2 i}

/-- The fixed-count log likelihood with the universal normalization omitted. -/
def likelihood {P : Type*} (p : P → Coordinates) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (Real.log (x i - (p v).threshold + (p v).scale) -
    3 * Real.log (x i - (p v).threshold) / 2 - Real.log (p v).scale / 2 -
    Real.log (p v).shape -
    ((x i - (p v).threshold) / (p v).scale + (p v).scale / (x i - (p v).threshold) - 2) /
      (2 * (p v).shape ^ 2))

private def score (a b t : ℝ) : ℝ :=
  1 / (t + b) - 3 / (2 * t) - (1 / b - b / t ^ 2) / (2 * a ^ 2)

private def sampleDeriv {P : Type*} (p : P → Coordinates) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score (p v).shape (p v).scale (x i - (p v).threshold)) • proj i

private theorem hasSampleDeriv {P : Type*} (p : P → Coordinates) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) (hz : (v, x) ∈ domain p) :
    HasFDerivAt (likelihood p w v) (sampleDeriv p w v x) x := by
  unfold likelihood sampleDeriv
  apply HasFDerivAt.fun_sum
  intro i _
  let a := (p v).shape
  let b := (p v).scale
  let c := (p v).threshold
  have ha : 0 < a := hz.1
  have hb : 0 < b := hz.2.1
  have ht : 0 < x i - c := sub_pos.mpr (hz.2.2 i)
  have h := (hasDerivAt_id (x i)).sub_const c
  have hl := (h.add_const b).log (show x i - c + b ≠ 0 from (add_pos ht hb).ne')
  have hl' := ((h.log ht.ne').const_mul 3).div_const 2
  have hr := (((h.div_const b).add ((hasDerivAt_const (x i) b).div h ht.ne')).sub_const 2).div_const
    (2 * a ^ 2)
  have hd := ((((hl.sub hl').sub_const (Real.log b / 2)).sub_const (Real.log a)).sub hr).const_mul
    (w i : ℝ)
  have hterm : HasDerivAt (fun y : ℝ ↦ (w i : ℝ) *
      (Real.log (y - c + b) - 3 * Real.log (y - c) / 2 - Real.log b / 2 - Real.log a -
        ((y - c) / b + b / (y - c) - 2) / (2 * a ^ 2)))
      ((w i : ℝ) * score a b (x i - c)) (x i) := by
    convert hd using 1
    · rfl
    · simp only [score, id_eq, zero_mul, zero_sub]
      field_simp [ht.ne', hb.ne', ha.ne', (add_pos ht hb).ne']; ring
  let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
  have hcomp := hterm.comp_hasFDerivAt x f.hasFDerivAt
  exact hcomp

private theorem chart {P : Type*} [TopologicalSpace P] (p : P → Coordinates)
    (ha : Continuous fun v ↦ (p v).shape) (hb : Continuous fun v ↦ (p v).scale)
    (hc : Continuous fun v ↦ (p v).threshold) (w : Fin n → ℕ) :
    IsOpen (domain (n := n) p) ∧
      ContinuousOn (fun z : P × (Fin n → ℝ) ↦ likelihood p w z.1 z.2) (domain p) ∧
      ContinuousOn (fun z : P × (Fin n → ℝ) ↦ sampleDeriv p w z.1 z.2) (domain p) := by
  let a (z : P × (Fin n → ℝ)) := (p z.1).shape
  let b (z : P × (Fin n → ℝ)) := (p z.1).scale
  let t (i : Fin n) (z : P × (Fin n → ℝ)) := z.2 i - (p z.1).threshold
  have ca : Continuous a := ha.comp continuous_fst
  have cb : Continuous b := hb.comp continuous_fst
  have ct (i : Fin n) : Continuous (t i) :=
    ((continuous_apply i).comp continuous_snd).sub (hc.comp continuous_fst)
  have pa (z : P × (Fin n → ℝ)) (hz : z ∈ domain p) : 0 < a z := hz.1
  have pb (z : P × (Fin n → ℝ)) (hz : z ∈ domain p) : 0 < b z := hz.2.1
  have pt (i : Fin n) (z : P × (Fin n → ℝ)) (hz : z ∈ domain p) : 0 < t i z :=
    sub_pos.mpr (hz.2.2 i)
  have na z hz := (pa z hz).ne'
  have nb z hz := (pb z hz).ne'
  have nt i z hz := (pt i z hz).ne'
  have ntb i z hz := (add_pos (pt i z hz) (pb z hz)).ne'
  have nq z (hz : z ∈ domain p) : 2 * a z ^ 2 ≠ 0 := by have := pa z hz; positivity
  refine ⟨?_, ?_, ?_⟩
  · have hs : domain (n := n) p =
        {z | 0 < a z} ∩ {z | 0 < b z} ∩ ⋂ i, {z | 0 < t i z} := by
      ext z
      simp [domain, a, b, t, sub_pos, and_assoc]
    rw [hs]
    exact ((isOpen_lt continuous_const ca).inter (isOpen_lt continuous_const cb)).inter
      (isOpen_iInter_of_finite fun i ↦ isOpen_lt continuous_const (ct i))
  · unfold likelihood
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.mul
      ((((((ct i).add cb).continuousOn.log (ntb i)).sub
        ((continuousOn_const.mul ((ct i).continuousOn.log (nt i))).div_const 2)).sub
        ((cb.continuousOn.log nb).div_const 2)).sub (ca.continuousOn.log na) |>.sub
        ((((ct i).continuousOn.div cb.continuousOn nb).add
          (cb.continuousOn.div (ct i).continuousOn (nt i))).sub continuousOn_const |>.div
          (continuousOn_const.mul (ca.continuousOn.pow 2)) nq))
  · unfold sampleDeriv
    apply continuousOn_finsetSum
    intro i _
    have n₂ z (hz : z ∈ domain p) : 2 * t i z ≠ 0 := by have := pt i z hz; positivity
    have nt₂ z (hz : z ∈ domain p) : t i z ^ 2 ≠ 0 := pow_ne_zero _ (nt i z hz)
    have cs : ContinuousOn (fun z ↦ score (a z) (b z) (t i z)) (domain p) :=
      ((continuousOn_const.div ((ct i).add cb).continuousOn (ntb i)).sub
        (continuousOn_const.div (continuousOn_const.mul (ct i).continuousOn) n₂)).sub
        (((continuousOn_const.div cb.continuousOn nb).sub
          (cb.continuousOn.div ((ct i).continuousOn.pow 2) nt₂)).div
          (continuousOn_const.mul (ca.continuousOn.pow 2)) nq)
    exact (continuousOn_const.mul cs).smul continuousOn_const

private def crossing (b d t : ℝ) : ℝ :=
  t ^ 2 + (b + d) * t + (b * d * (b + d)) * (t ^ (-1 : ℤ)) + (b * d) ^ 2 * (t ^ (-2 : ℤ))

private theorem crossing_eq {a b d t : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hd : 0 < d) (ht : 0 < t) (hbd : b ≠ d)
    (h : score a b t = score a d t) :
    crossing b d t = 2 * a ^ 2 * (b * d) - 2 * (b * d) := by
  have htb : t + b ≠ 0 := (add_pos ht hb).ne'
  have htd : t + d ≠ 0 := (add_pos ht hd).ne'
  have he : (b - d) *
      ((t + b) * (t + d) * (t ^ 2 + b * d) - 2 * a ^ 2 * b * d * t ^ 2) = 0 := by
    unfold score at h
    field_simp [ha.ne', hb.ne', hd.ne', ht.ne', htb, htd] at h
    linear_combination h
  have hzero := (mul_eq_zero.mp he).resolve_left (sub_ne_zero.mpr hbd)
  unfold crossing
  rw [zpow_neg_one, zpow_neg, zpow_ofNat]
  field_simp [ht.ne']
  linear_combination hzero

private theorem crossing_strictConvex {b d : ℝ} (hb : 0 < b) (hd : 0 < d) :
    StrictConvexOn ℝ (Ioi 0) (crossing b d) := by
  have hsq : StrictConvexOn ℝ (Ioi 0) (fun t : ℝ ↦ t ^ 2) :=
    (strictConvexOn_pow (n := 2) (by decide)).subset
      (fun _ hx ↦ le_of_lt hx) (convex_Ioi _)
  have hid : ConvexOn ℝ (Ioi 0) (fun t : ℝ ↦ t) := ⟨convex_Ioi _, by intros; rfl⟩
  have hlin := (hid.smul (c := b + d) (by positivity))
  have hinv := (strictConvexOn_zpow (m := -1) (by decide) (by decide)).convexOn.smul
    (c := b * d * (b + d)) (by positivity)
  have hinv₂ := (strictConvexOn_zpow (m := -2) (by decide) (by decide)).convexOn.smul
    (c := (b * d) ^ 2) (by positivity)
  exact ((hsq.add_convexOn hlin).add_convexOn hinv).add_convexOn hinv₂

private theorem evalSampleDeriv {P : Type*} (p : P → Coordinates) (w : Fin n → ℕ)
    (b : P) (x : Fin n → ℝ) (i : Fin n) :
    sampleDeriv p w b x (Pi.single i 1) =
      (w i : ℝ) * score (p b).shape (p b).scale (x i - (p b).threshold) := by
  simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji
    simp [Pi.single_eq_of_ne hji]
  · simp

/-- At positive held shape and fixed threshold, an attained scale is unique
for Lebesgue-almost every fixed-count sample with three ordered distinct levels. -/
theorem held_alpha_mu_ae_unique {a c : ℝ} (ha : 0 < a) (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ b d, UniqueAE.Winner (domain fun s ↦ ⟨a, s, c⟩)
        (likelihood (fun s ↦ ⟨a, s, c⟩) w) x b →
        UniqueAE.Winner (domain fun s ↦ ⟨a, s, c⟩)
        (likelihood (fun s ↦ ⟨a, s, c⟩) w) x d → b = d := by
  let p (s : ℝ) : Coordinates := ⟨a, s, c⟩
  obtain ⟨ho, hl, hd⟩ := chart p continuous_const continuous_id continuous_const w
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hd
    (hasSampleDeriv p w)] with x he hlevels b d hb hd
  obtain ⟨i, j, k, hij, hjk⟩ := hlevels
  have hb0 : 0 < b := hb.1.2.1
  have hd0 : 0 < d := hd.1.2.1
  by_contra hbd
  have hx (l : Fin n) : 0 < x l - c := sub_pos.mpr (hb.1.2.2 l)
  have hz (l : Fin n) : crossing b d (x l - c) = 2 * a ^ 2 * (b * d) - 2 * (b * d) := by
    apply crossing_eq ha hb0 hd0 (hx l) hbd
    have hh := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single l 1)) (he b d hb hd)
    simp only [evalSampleDeriv] at hh
    exact mul_left_cancel₀ (by exact_mod_cast (hw l).ne') hh
  have hs : x j - c ∈ openSegment ℝ (x i - c) (x k - c) := by
    rw [openSegment_eq_Ioo (sub_lt_sub_right (hij.trans hjk) c)]
    exact ⟨sub_lt_sub_right hij c, sub_lt_sub_right hjk c⟩
  have hlt := (crossing_strictConvex hb0 hd0).lt_on_openSegment (hx i) (hx k)
    (ne_of_lt (sub_lt_sub_right (hij.trans hjk) c)) hs
  rw [hz i, hz j, hz k, max_self] at hlt
  exact (lt_irrefl _ hlt)

private def thresholdRatio (b u v : ℝ) : ℝ :=
  let p := (1 + b * u) * (1 + b * v)
  (u + v) * p / (3 * p - 2)

private theorem thresholdRatio_comm (b u v : ℝ) :
    thresholdRatio b u v = thresholdRatio b v u := by
  simp only [thresholdRatio, add_comm u v, mul_comm (1 + b * u)]

private theorem thresholdRatio_lt {b u U v : ℝ} (hb : 0 < b) (hu : 0 < u)
    (hU : u < U) (hv : 0 < v) :
    thresholdRatio b u v < thresholdRatio b U v := by
  let p := (1 + b * u) * (1 + b * v)
  let P := (1 + b * U) * (1 + b * v)
  let k := 1 + b * v
  have hk : 0 < k := by dsimp [k]; positivity
  have hkp : k ≤ p := by
    dsimp [p, k]
    nlinarith [mul_nonneg (show 0 ≤ b * u by positivity) (show 0 ≤ 1 + b * v by positivity)]
  have hs : b * (u + v) ≤ p - 1 := by
    dsimp [p]
    nlinarith [show 0 ≤ (b * u) * (b * v) by positivity]
  have hp : 1 < p := by linarith [show 0 < b * (u + v) by positivity]
  have hpP : p < P := by
    dsimp [p, P]
    exact mul_lt_mul_of_pos_right (by linarith [mul_pos hb (sub_pos.mpr hU)]) hk
  have hd : 0 < 3 * p - 2 := by linarith
  have hD : 0 < 3 * P - 2 := by linarith
  have hbnd : b * (u + v) * k ≤ (p - 1) * p :=
    mul_le_mul hs hkp hk.le (by linarith)
  have hpos : 0 < P * (3 * p - 2) - 2 * (b * (u + v) * k) := by
    nlinarith [mul_pos (sub_pos.mpr hpP) hd, sq_pos_of_pos (show 0 < p by linarith)]
  change (u + v) * p / (3 * p - 2) < (U + v) * P / (3 * P - 2)
  apply (div_lt_div_iff₀ hd hD).mpr
  have he : (U + v) * P * (3 * p - 2) - (u + v) * p * (3 * P - 2) =
      (U - u) * (P * (3 * p - 2) - 2 * (b * (u + v) * k)) := by
    dsimp [P, p, k]
    ring
  linarith [mul_pos (sub_pos.mpr hU) hpos]

private theorem threshold_crossing {a b t s : ℝ} (ha : 0 < a) (hb : 0 < b)
    (ht : 0 < t) (hs : 0 < s) (hts : t ≠ s)
    (h : score a b t = score a b s) :
    b * thresholdRatio b t⁻¹ s⁻¹ = a ^ 2 := by
  have htb : t + b ≠ 0 := (add_pos ht hb).ne'
  have hsb : s + b ≠ 0 := (add_pos hs hb).ne'
  have he : b * ((s - t) * (b * (t + s) * (t + b) * (s + b) -
    a ^ 2 * t * s * (3 * (t + b) * (s + b) - 2 * t * s))) = 0 := by
    unfold score at h
    field_simp [ha.ne', hb.ne', ht.ne', hs.ne', htb, hsb] at h
    linear_combination h
  have hz := (mul_eq_zero.mp ((mul_eq_zero.mp he).resolve_left hb.ne')).resolve_left
    (sub_ne_zero.mpr hts.symm)
  have hP : 1 ≤ (1 + b * t⁻¹) * (1 + b * s⁻¹) := by
    exact one_le_mul_of_one_le_of_one_le
      (le_add_of_nonneg_right (by positivity)) (le_add_of_nonneg_right (by positivity))
  have hD : 3 * ((1 + b * t⁻¹) * (1 + b * s⁻¹)) - 2 ≠ 0 := by linarith
  unfold thresholdRatio
  rw [← mul_div_assoc, div_eq_iff hD]
  field_simp [ht.ne', hs.ne']
  linear_combination hz

/-- At positive held shape and scale, the attained threshold is unique for
Lebesgue-almost every fixed-count sample with at least three distinct levels. -/
theorem held_alpha_beta_ae_unique {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ c d, UniqueAE.Winner (domain fun s ↦ ⟨a, b, s⟩)
        (likelihood (fun s ↦ ⟨a, b, s⟩) w) x c →
        UniqueAE.Winner (domain fun s ↦ ⟨a, b, s⟩)
        (likelihood (fun s ↦ ⟨a, b, s⟩) w) x d → c = d := by
  let p (s : ℝ) : Coordinates := ⟨a, b, s⟩
  obtain ⟨ho, hl, hd⟩ := chart p continuous_const continuous_const continuous_id w
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hd
    (hasSampleDeriv p w)] with x he hlevels c d hc hd
  obtain ⟨i, j, _, hij, _⟩ := hlevels
  by_contra hcd
  have hx (l : Fin n) : 0 < x l - c := sub_pos.mpr (hc.1.2.2 l)
  have hy (l : Fin n) : 0 < x l - d := sub_pos.mpr (hd.1.2.2 l)
  have hz (l : Fin n) : b * thresholdRatio b (x l - c)⁻¹ (x l - d)⁻¹ = a ^ 2 := by
    apply threshold_crossing ha hb (hx l) (hy l) (by intro hh; apply hcd; linarith)
    have hh := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single l 1)) (he c d hc hd)
    simp only [evalSampleDeriv] at hh
    exact mul_left_cancel₀ (by exact_mod_cast (hw l).ne') hh
  have hu := inv_lt_inv₀ (hx j) (hx i) |>.mpr (sub_lt_sub_right hij c)
  have hv := inv_lt_inv₀ (hy j) (hy i) |>.mpr (sub_lt_sub_right hij d)
  have h₁ := thresholdRatio_lt hb (inv_pos.mpr (hx j)) hu (inv_pos.mpr (hy j))
  have h₂ := thresholdRatio_lt hb (inv_pos.mpr (hy j)) hv (inv_pos.mpr (hx i))
  rw [thresholdRatio_comm b (x j - d)⁻¹, thresholdRatio_comm b (x i - d)⁻¹] at h₂
  have hh := mul_lt_mul_of_pos_left (h₁.trans h₂) hb
  rw [hz j, hz i] at hh
  exact lt_irrefl _ hh

private def tieSample : Fin 2 → ℝ := ![(1 : ℝ) / 2, 2]

private theorem tie_height {b : ℝ} (hb : 0 < b) :
    let v := b + b⁻¹ + 5 / 2
    likelihood (fun s ↦ ⟨3, s, 0⟩) (fun _ ↦ 1) b tieSample =
      Real.log v - v / (36 / 5) + (41 / 4) / 18 - 2 * Real.log 3 := by
  dsimp only
  have hp : 0 < b + b⁻¹ + 5 / 2 := by positivity
  have he : ((1 : ℝ) / 2 + b) * (2 + b) = b * (b + b⁻¹ + 5 / 2) := by
    field_simp [hb.ne']; ring
  have hlogs : Real.log ((1 : ℝ) / 2 + b) + Real.log (2 + b) =
      Real.log b + Real.log (b + b⁻¹ + 5 / 2) := by
    rw [← Real.log_mul (by positivity : (1 : ℝ) / 2 + b ≠ 0)
      (by positivity : (2 : ℝ) + b ≠ 0), he, Real.log_mul hb.ne' hp.ne']
  have hlogTwo : Real.log ((1 : ℝ) / 2) = -Real.log 2 := by rw [one_div, Real.log_inv]
  norm_num [likelihood, tieSample, Fin.sum_univ_two, hlogTwo]
  calc
    _ = (Real.log ((1 : ℝ) / 2 + b) + Real.log (2 + b)) -
        Real.log b - (b + b⁻¹) * (5 / 2) / 18 + 4 / 18 - 2 * Real.log 3 := by ring
    _ = _ := by rw [hlogs]; ring

/-- At shape three and threshold zero the sample (1/2, 2) has exactly
the two indicated scales as its global maximizers. -/
theorem held_alpha_mu_two_level_tie :
    {b | UniqueAE.Winner (domain fun s ↦ ⟨3, s, 0⟩)
      (likelihood (fun s ↦ ⟨3, s, 0⟩) fun _ ↦ 1) ![(1 : ℝ) / 2, 2] b} =
      {((47 : ℝ) - Real.sqrt 1809) / 20, ((47 : ℝ) + Real.sqrt 1809) / 20} := by
  let l : ℝ := (47 - Real.sqrt 1809) / 20
  let r : ℝ := (47 + Real.sqrt 1809) / 20
  change {b | UniqueAE.Winner (domain fun s ↦ ⟨3, s, 0⟩)
      (likelihood (fun s ↦ ⟨3, s, 0⟩) fun _ ↦ 1) tieSample b} = {l, r}
  let v (b : ℝ) := b + b⁻¹ + 5 / 2
  have hsq : Real.sqrt 1809 ^ 2 = (1809 : ℝ) := Real.sq_sqrt (by norm_num)
  have hnonneg := Real.sqrt_nonneg (1809 : ℝ)
  have hs47 : Real.sqrt 1809 < 47 := by
    by_contra hn
    have hp := mul_nonneg (sub_nonneg.mpr (le_of_not_gt hn))
      (show 0 ≤ Real.sqrt 1809 + 47 by positivity)
    nlinarith
  have hl : 0 < l := by dsimp [l]; positivity
  have hr : 0 < r := by dsimp [r]; positivity
  have hrel {b : ℝ} (hb : 0 < b) : v b = 36 / 5 ↔ b = l ∨ b = r := by
    have he : (v b - 36 / 5) * b = (b - l) * (b - r) := by
      dsimp [v, l, r]
      field_simp [hb.ne']
      linear_combination 10 * hsq
    constructor
    · intro h
      rw [h, sub_self, zero_mul] at he
      exact (mul_eq_zero.mp he.symm).imp sub_eq_zero.mp sub_eq_zero.mp
    · intro h
      rcases h with rfl | rfl <;> simp only [sub_self, zero_mul, mul_zero] at he
      all_goals exact sub_eq_zero.mp ((mul_eq_zero.mp he).resolve_right (by positivity))
  have hfeasible (b : ℝ) (hb : 0 < b) : (b, tieSample) ∈ domain (fun s ↦ ⟨3, s, 0⟩) := by
    refine ⟨by norm_num, hb, ?_⟩
    intro i
    fin_cases i <;> norm_num [tieSample]
  let L : ℝ → (Fin 2 → ℝ) → ℝ := likelihood (fun s ↦ ⟨3, s, 0⟩) (fun _ ↦ 1)
  have hheight {b : ℝ} (hb : 0 < b) {d : ℝ} (hd : 0 < d) (hvd : v d = 36 / 5) :
      L b tieSample ≤ L d tieSample ∧ (L b tieSample = L d tieSample → v b = 36 / 5) := by
    have hv : 0 < v b := by dsimp [v]; positivity
    have hconstant : 0 < (36 / 5 : ℝ) := by norm_num
    have he : Real.log (v b / (36 / 5)) = Real.log (v b) - Real.log (36 / 5) :=
      Real.log_div hv.ne' hconstant.ne'
    have hmain := Real.log_le_sub_one_of_pos (div_pos hv hconstant)
    rw [he] at hmain
    have hleft := tie_height hb
    have hright := tie_height hd
    change L b tieSample = Real.log (v b) - v b / (36 / 5) + (41 / 4) / 18 -
      2 * Real.log 3 at hleft
    change L d tieSample = Real.log (v d) - v d / (36 / 5) + (41 / 4) / 18 -
      2 * Real.log 3 at hright
    rw [hvd] at hright
    constructor
    · linarith
    · intro h
      by_contra hn
      have hne : v b / (36 / 5) ≠ 1 := by
        intro heq
        exact hn (by simpa only [one_mul] using (div_eq_iff hconstant.ne').mp heq)
      have hstrict := Real.log_lt_sub_one_of_pos (div_pos hv hconstant) hne
      rw [he] at hstrict
      linarith
  ext b
  change UniqueAE.Winner _ L tieSample b ↔ b ∈ ({l, r} : Set ℝ)
  simp only [mem_insert_iff, mem_singleton_iff]
  constructor
  · intro hb
    have hb₀ : 0 < b := hb.1.2.1
    obtain ⟨hle, heq⟩ := hheight hb₀ hl ((hrel hl).2 (Or.inl rfl))
    exact (hrel hb₀).1 (heq (le_antisymm hle (hb.2 l (hfeasible l hl))))
  · intro hb
    have hb₀ : 0 < b := hb.elim (fun h ↦ h.symm ▸ hl) (fun h ↦ h.symm ▸ hr)
    refine ⟨hfeasible b hb₀, fun q hq ↦ ?_⟩
    exact (hheight hq.2.1 hb₀ ((hrel hb₀).2 hb)).1

end

end UnivariateML.AE2Fatigue

end
