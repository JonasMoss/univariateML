module

public import UnivariateML.Family.Transfers3
public import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
public import Mathlib.Analysis.Convex.SpecificFunctions.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Nonstandard beta with both endpoints free and shapes fixed

The data are normalized by their positive range. The two coordinates are
the beta argument at the sample minimum and the sample range divided by
width. They give an affine chart for the regular support constraints.
-/

@[expose] public section

open Set Real
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2NonstandardBetaAffine

noncomputable section

variable {n : ℕ}

/-- Positive finite support in the affine inverse-width chart. -/
def domain (a b : ℝ) : Set (ℝ × ℝ) :=
  {p | 0 ≤ p.1 ∧ 0 < p.2 ∧ p.1 + p.2 ≤ 1 ∧
    (a = 1 ∨ 0 < p.1) ∧ (b = 1 ∨ p.1 + p.2 < 1)}

/-- Chart log likelihood up to its parameter-independent normalizer. -/
def logLik (z : Fin n → ℝ) (a b : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (log p.2 + (a - 1) * log (p.1 + p.2 * z i) +
    (b - 1) * log (1 - (p.1 + p.2 * z i)))

private def closure : Set (ℝ × ℝ) :=
  {p | 0 ≤ p.1 ∧ 0 ≤ p.2 ∧ p.1 + p.2 ≤ 1}

private def density (z : Fin n → ℝ) (a b : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∏ i, (p.2 * (p.1 + p.2 * z i) ^ (a - 1) * (1 - (p.1 + p.2 * z i)) ^ (b - 1))

private theorem exp_logLik {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    {a b : ℝ} {p : ℝ × ℝ} (hp : p ∈ domain a b) :
    exp (logLik z a b p) = density z a b p := by
  unfold logLik density
  rw [Real.exp_sum]
  apply Finset.prod_congr rfl
  intro i _
  rw [exp_add, exp_add, exp_log hp.2.1]
  have ha : exp ((a - 1) * log (p.1 + p.2 * z i)) = (p.1 + p.2 * z i) ^ (a - 1) := by
    rcases hp.2.2.2.1 with rfl | hh
    · simp
    · rw [rpow_def_of_pos (hh.trans_le (le_add_of_nonneg_right
        (mul_nonneg hp.2.1.le (hz i).1))), mul_comm]
  have hb : exp ((b - 1) * log (1 - (p.1 + p.2 * z i))) =
      (1 - (p.1 + p.2 * z i)) ^ (b - 1) := by
    rcases hp.2.2.2.2 with rfl | hh
    · simp
    · have hlt : p.1 + p.2 * z i < 1 := by
        linarith [mul_le_of_le_one_right hp.2.1.le (hz i).2]
      rw [rpow_def_of_pos (sub_pos.mpr hlt), mul_comm]
  rw [ha, hb]

private theorem midpoint_log {c x y : ℝ} (hc : 0 ≤ c)
    (hx : c = 0 ∨ 0 < x) (hy : c = 0 ∨ 0 < y) :
    (c * log x + c * log y) / 2 ≤ c * log ((x + y) / 2) ∧
      (0 < c → x ≠ y → (c * log x + c * log y) / 2 < c * log ((x + y) / 2)) := by
  by_cases he : c = 0
  · simp [he]
  have hxp := hx.resolve_left he
  have hyp := hy.resolve_left he
  have ho := strictConcaveOn_log_Ioi.concaveOn.2 hxp hyp
    (by norm_num : 0 ≤ (1 / 2 : ℝ)) (by norm_num : 0 ≤ (1 / 2 : ℝ))
    (by norm_num : (1 / 2 : ℝ) + 1 / 2 = 1)
  change 1 / 2 * log x + 1 / 2 * log y ≤ log (1 / 2 * x + 1 / 2 * y) at ho
  rw [show 1 / 2 * x + 1 / 2 * y = (x + y) / 2 by ring] at ho
  refine ⟨by linarith [mul_le_mul_of_nonneg_left ho hc], ?_⟩
  intro hc' hne
  have hp := strictConcaveOn_log_Ioi.2 hxp hyp hne
    (by norm_num : 0 < (1 / 2 : ℝ)) (by norm_num : 0 < (1 / 2 : ℝ))
    (by norm_num : (1 / 2 : ℝ) + 1 / 2 = 1)
  change 1 / 2 * log x + 1 / 2 * log y < log (1 / 2 * x + 1 / 2 * y) at hp
  rw [show 1 / 2 * x + 1 / 2 * y = (x + y) / 2 by ring] at hp
  linarith [mul_lt_mul_of_pos_left hp hc']

/-- For fixed shapes at least one, nonconstant samples have a unique
fit in the affine endpoint chart, including closed uniform contact. -/
theorem fixed_shapes_affine_fit {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hzero : ∃ i, z i = 0) (hone : ∃ i, z i = 1) {a b : ℝ} (ha : 1 ≤ a) (hb : 1 ≤ b) :
    ∃! p, p ∈ domain a b ∧ IsMaxOn (logLik z a b) (domain a b) p := by
  obtain ⟨i₀, hi₀⟩ := hzero
  obtain ⟨i₁, hi₁⟩ := hone
  have hN : (0 : ℝ) < n := Nat.cast_pos.mpr i₀.isLt.pos
  have hsub : domain a b ⊆ closure := fun _ hp ↦ ⟨hp.1, hp.2.1.le, hp.2.2.1⟩
  let v : ℝ × ℝ := (1/3,1/3)
  have hv : v ∈ domain a b := by
    exact ⟨by norm_num [v], by norm_num [v], by norm_num [v],
      Or.inr (by norm_num [v]), Or.inr (by norm_num [v])⟩
  have hK : IsCompact closure := by
    have he : closure = (Icc 0 1 ×ˢ Icc 0 1) ∩ {p : ℝ × ℝ | p.1 + p.2 ≤ 1} := by
      ext p
      change (0 ≤ p.1 ∧ 0 ≤ p.2 ∧ p.1 + p.2 ≤ 1) ↔
        (p.1 ∈ Icc 0 1 ∧ p.2 ∈ Icc 0 1) ∧ p.1 + p.2 ≤ 1
      constructor
      · rintro ⟨h₁,h₂,h₃⟩
        exact ⟨⟨⟨h₁, by linarith⟩, ⟨h₂, by linarith⟩⟩, h₃⟩
      · rintro ⟨⟨h₁,h₂⟩,h₃⟩
        exact ⟨h₁.1,h₂.1,h₃⟩
    rw [he]
    exact (isCompact_Icc.prod isCompact_Icc).inter_right
      (isClosed_le (continuous_fst.add continuous_snd) continuous_const)
  have hc : Continuous (density z a b) := by
    apply continuous_finsetProd
    intro i _
    have H : Continuous (fun p : ℝ × ℝ ↦ p.1 + p.2 * z i) := by fun_prop
    exact (continuous_snd.mul ((continuous_rpow_const (sub_nonneg.mpr ha)).comp H)).mul
      ((continuous_rpow_const (sub_nonneg.mpr hb)).comp (continuous_const.sub H))
  obtain ⟨p, hp, hm⟩ := hK.exists_isMaxOn ⟨v, hsub hv⟩ hc.continuousOn
  have hpos : 0 < density z a b p := by
    have h := hm (hsub hv)
    have hle : density z a b v ≤ density z a b p := h
    rw [← exp_logLik hz hv] at hle
    exact (exp_pos _).trans_le hle
  have hno (j : Fin n) : p.2 * (p.1 + p.2 * z j) ^ (a - 1) *
      (1 - (p.1 + p.2 * z j)) ^ (b - 1) ≠ 0 := by
    intro h
    exact hpos.ne' (Finset.prod_eq_zero (Finset.mem_univ j) h)
  have hpD : p ∈ domain a b := by
    refine ⟨hp.1, lt_of_le_of_ne hp.2.1 ?_, hp.2.2, ?_, ?_⟩
    · intro he
      exact hno i₀ (by simp [← he])
    · by_cases h : a = 1
      · exact Or.inl h
      · apply Or.inr (lt_of_le_of_ne hp.1 ?_)
        intro he
        have ha' : a - 1 ≠ 0 := sub_ne_zero.mpr h
        exact hno i₀ (by simp [hi₀, ← he, zero_rpow ha'])
    · by_cases h : b = 1
      · exact Or.inl h
      · apply Or.inr (lt_of_le_of_ne hp.2.2 ?_)
        intro he
        have hb' : b - 1 ≠ 0 := sub_ne_zero.mpr h
        exact hno i₁ (by simp [hi₁, he, zero_rpow hb'])
  have hmD : IsMaxOn (logLik z a b) (domain a b) p := by
    intro q hq
    apply Real.exp_le_exp.mp
    rw [exp_logLik hz hq, exp_logLik hz hpD]
    exact hm (hsub hq)
  refine ⟨p, ⟨hpD, hmD⟩, ?_⟩
  rintro q ⟨hq, hqm⟩
  by_cases hab : a = 1 ∧ b = 1
  · rcases hab with ⟨rfl,rfl⟩
    have h1 : (0,1) ∈ domain 1 1 := by
      exact ⟨le_rfl, one_pos, by norm_num, Or.inl rfl, Or.inl rfl⟩
    have hbest (w : ℝ × ℝ) (hw : w ∈ domain 1 1)
        (hm' : IsMaxOn (logLik z 1 1) (domain 1 1) w) : w = (0,1) := by
      have hle : logLik z 1 1 (0,1) ≤ logLik z 1 1 w := hm' h1
      have hform (u : ℝ × ℝ) : logLik z 1 1 u = (n : ℝ) * log u.2 := by
        simp [logLik, nsmul_eq_mul]
      rw [hform, hform] at hle
      simp only [log_one, mul_zero] at hle
      have hwle : w.2 ≤ 1 := by linarith [hw.1, hw.2.2.1]
      have hw1 : w.2 = 1 := by
        by_contra! hh
        have hlog := log_neg hw.2.1 (lt_of_le_of_ne hwle hh)
        exact (mul_neg_of_pos_of_neg hN hlog).not_ge hle
      exact Prod.ext (by linarith [hw.1, hw.2.2.1]) hw1
    exact (hbest q hq hqm).trans (hbest p hpD hmD).symm
  · by_contra! hne
    let w : ℝ × ℝ := ((q.1 + p.1) / 2, (q.2 + p.2) / 2)
    have hw : w ∈ domain a b := by
      refine ⟨by dsimp [w]; linarith [hq.1,hpD.1],
        by dsimp [w]; linarith [hq.2.1,hpD.2.1],
        by dsimp [w]; linarith [hq.2.2.1,hpD.2.2.1], ?_, ?_⟩
      · by_cases h : a = 1
        · exact Or.inl h
        · exact Or.inr (by dsimp [w]; linarith [hq.2.2.2.1.resolve_left h, hpD.1])
      · by_cases h : b = 1
        · exact Or.inl h
        · exact Or.inr (by dsimp [w]; linarith [hq.2.2.2.2.resolve_left h, hpD.2.2.1])
    have arg₁ (u : ℝ × ℝ) (hu : u ∈ domain a b) (i : Fin n) :
        a - 1 = 0 ∨ 0 < u.1 + u.2 * z i :=
      hu.2.2.2.1.imp (fun h ↦ sub_eq_zero.mpr h)
        (fun h ↦ h.trans_le (le_add_of_nonneg_right (mul_nonneg hu.2.1.le (hz i).1)))
    have arg₂ (u : ℝ × ℝ) (hu : u ∈ domain a b) (i : Fin n) :
        b - 1 = 0 ∨ 0 < 1 - (u.1 + u.2 * z i) :=
      hu.2.2.2.2.imp (fun h ↦ sub_eq_zero.mpr h) (fun h ↦ sub_pos.mpr
        (by linarith [mul_le_of_le_one_right hu.2.1.le (hz i).2]))
    let f (u : ℝ × ℝ) (i : Fin n) :=
      log u.2 + (a - 1) * log (u.1 + u.2 * z i) + (b - 1) * log (1 - (u.1 + u.2 * z i))
    have hbetter (i : Fin n) : (f q i + f p i) / 2 < f w i := by
      have H₀ := midpoint_log (c := 1) zero_le_one (Or.inr hq.2.1) (Or.inr hpD.2.1)
      have H₁ := midpoint_log (sub_nonneg.mpr ha) (arg₁ q hq i) (arg₁ p hpD i)
      have H₂ := midpoint_log (sub_nonneg.mpr hb) (arg₂ q hq i) (arg₂ p hpD i)
      have harg : (q.1 + q.2 * z i + (p.1 + p.2 * z i)) / 2 = w.1 + w.2 * z i := by
        dsimp [w]
        ring
      have harg' : (1 - (q.1 + q.2 * z i) + (1 - (p.1 + p.2 * z i))) / 2 =
          1 - (w.1 + w.2 * z i) := by
        rw [← harg]
        ring
      rw [harg] at H₁
      rw [harg'] at H₂
      change (log q.2 + (a - 1) * log (q.1 + q.2 * z i) +
        (b - 1) * log (1 - (q.1 + q.2 * z i)) +
        (log p.2 + (a - 1) * log (p.1 + p.2 * z i) +
        (b - 1) * log (1 - (p.1 + p.2 * z i)))) / 2 <
        log w.2 + (a - 1) * log (w.1 + w.2 * z i) + (b - 1) * log (1 - (w.1 + w.2 * z i))
      by_cases he : q.2 = p.2
      · have hh : q.1 ≠ p.1 := fun h ↦ hne (Prod.ext h he)
        by_cases h : a = 1
        · have hbp : 0 < b - 1 := sub_pos.mpr (lt_of_le_of_ne hb (Ne.symm (fun h' ↦ hab ⟨h,h'⟩)))
          have hne' : 1 - (q.1 + q.2 * z i) ≠ 1 - (p.1 + p.2 * z i) := by
            rw [he]
            intro h'
            exact hh (by linarith)
          linarith [H₀.1, H₁.1, H₂.2 hbp hne']
        · have hap : 0 < a - 1 := sub_pos.mpr (lt_of_le_of_ne ha (Ne.symm h))
          have hne' : q.1 + q.2 * z i ≠ p.1 + p.2 * z i := by
            rw [he]
            intro hh'
            exact hh (by linarith)
          linarith [H₀.1, H₁.2 hap hne', H₂.1]
      · linarith [H₀.2 one_pos he, H₁.1, H₂.1]
    let : Nonempty (Fin n) := ⟨i₀⟩
    have HH := Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty (fun i _ ↦ hbetter i)
    rw [← Finset.sum_div, Finset.sum_add_distrib] at HH
    have hq' : logLik z a b w ≤ logLik z a b q := hqm hw
    have hp' : logLik z a b w ≤ logLik z a b p := hmD hw
    change (logLik z a b q + logLik z a b p) / 2 < logLik z a b w at HH
    linarith

/-- Endpoint pairs containing the sample with all density factors positive. -/
def endpointDomain (m M a b : ℝ) : Set (ℝ × ℝ) :=
  {v | v.1 ≤ m ∧ M ≤ v.2 ∧ (a = 1 ∨ v.1 < m) ∧ (b = 1 ∨ M < v.2)}

private def toChart (m M : ℝ) (v : ℝ × ℝ) : ℝ × ℝ :=
  ((m - v.1) / (v.2 - v.1), (M - m) / (v.2 - v.1))

private theorem endpoint_bij {m M : ℝ} (hm : m < M) (a b : ℝ) :
    BijOn (toChart m M) (endpointDomain m M a b) (domain a b) := by
  let fromChart (p : ℝ × ℝ) : ℝ × ℝ :=
    (m - p.1 * ((M - m) / p.2), m + (1 - p.1) * ((M - m) / p.2))
  have H := sub_pos.mpr hm
  have hwidth {v : ℝ × ℝ} (hv : v ∈ endpointDomain m M a b) : 0 < v.2 - v.1 :=
    sub_pos.mpr (hv.1.trans_lt (hm.trans_le hv.2.1))
  refine ⟨?_, ?_, ?_⟩
  · intro v hv
    have hw := hwidth hv
    have he : (toChart m M v).1 + (toChart m M v).2 = (M - v.1) / (v.2 - v.1) := by
      simp only [toChart, ← add_div]
      congr 1
      ring
    have hg : (toChart m M v).1 + (toChart m M v).2 ≤ 1 := by
      rw [he]
      exact (div_le_one hw).mpr (sub_le_sub_right hv.2.1 _)
    refine ⟨div_nonneg (sub_nonneg.mpr hv.1) hw.le, div_pos H hw, hg, ?_, ?_⟩
    · exact hv.2.2.1.imp id (fun h ↦ div_pos (sub_pos.mpr h) hw)
    · apply hv.2.2.2.imp id
      intro h
      rw [he]
      exact (div_lt_one hw).mpr (sub_lt_sub_right h _)
  · intro u hu v hv he
    have hinv (v : ℝ × ℝ) (hv : v ∈ endpointDomain m M a b) :
        fromChart (toChart m M v) = v := by
      unfold toChart
      dsimp [fromChart]
      rw [div_div_cancel₀ H.ne']
      exact Prod.ext (by dsimp; field_simp [(hwidth hv).ne']; ring)
        (by dsimp; field_simp [(hwidth hv).ne']; ring)
    exact (hinv u hu).symm.trans ((congrArg fromChart he).trans (hinv v hv))
  · intro p hp
    let W := (M - m) / p.2
    have hW : 0 < W := div_pos H hp.2.1
    have he : M = m + p.2 * W := by
      dsimp [W]
      rw [mul_div_cancel₀ _ hp.2.1.ne']
      ring
    have hv : fromChart p ∈ endpointDomain m M a b := by
      change m - p.1 * W ≤ m ∧ M ≤ m + (1-p.1)*W ∧
        (a = 1 ∨ m-p.1*W < m) ∧ (b = 1 ∨ M < m+(1-p.1)*W)
      refine ⟨by linarith [mul_nonneg hp.1 hW.le], ?_, ?_, ?_⟩
      · rw [he]
        linarith [mul_le_mul_of_nonneg_right hp.2.2.1 hW.le]
      · exact hp.2.2.2.1.imp id (fun h ↦ by linarith [mul_pos h hW])
      · refine hp.2.2.2.2.imp id fun h ↦ ?_
        rw [he]
        linarith [mul_lt_mul_of_pos_right h hW]
    have hd : (fromChart p).2 - (fromChart p).1 = W := by
      change m + (1-p.1)*W - (m-p.1*W) = W
      ring
    refine ⟨fromChart p, hv, Prod.ext ?_ ?_⟩
    · change (m - (m - p.1 * W)) / ((fromChart p).2 - (fromChart p).1) = p.1
      rw [hd, show m - (m - p.1 * W) = p.1 * W by ring, mul_div_cancel_right₀ _ hW.ne']
    · change (M - m) / ((fromChart p).2 - (fromChart p).1) = p.2
      rw [hd, div_div_cancel₀ H.ne']

private theorem endpoint_likelihood {x : Fin n → ℝ} {m M a b : ℝ} (hm : m < M)
    {v : ℝ × ℝ} (hv : v ∈ endpointDomain m M a b) :
    NonstandardBeta.logLik x v.1 v.2 (a,b) =
      logLik (fun i ↦ (x i - m) / (M - m)) a b (toChart m M v) +
        (-(n : ℝ) * (log (M - m) + Beta.logBeta (a,b))) := by
  have hH := sub_pos.mpr hm
  have hW := sub_pos.mpr (hv.1.trans_lt (hm.trans_le hv.2.1))
  have hleft (i : Fin n) : (toChart m M v).1 + (toChart m M v).2 * ((x i-m)/(M-m)) =
      (x i - v.1) / (v.2 - v.1) := by
    dsimp [toChart]
    field_simp [hW.ne',hH.ne']
    ring
  have hright (i : Fin n) :
      1 - (x i - v.1) / (v.2 - v.1) = (v.2 - x i) / (v.2 - v.1) := by
    field_simp [hW.ne']
    ring
  have hlog : log (toChart m M v).2 = log (M-m) - log (v.2-v.1) :=
    log_div hH.ne' hW.ne'
  unfold NonstandardBeta.logLik logLik
  simp only [hleft, hright, hlog, Finset.sum_add_distrib, Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

/-- On nonconstant samples, both regular fixed shapes give a unique
positive finite endpoint pair in the original coordinates. -/
theorem fixed_shapes_endpoint_fit {x : Fin n → ℝ} {m M a b : ℝ} (hspan : m < M)
    (hx : ∀ i, x i ∈ Icc m M) (hmin : ∃ i, x i = m) (hmax : ∃ i, x i = M)
    (ha : 1 ≤ a) (hb : 1 ≤ b) :
    let D := endpointDomain m M a b
    ∃! v, v ∈ D ∧ IsMaxOn (fun v : ℝ × ℝ ↦ NonstandardBeta.logLik x v.1 v.2 (a,b)) D v := by
  intro D
  let z := fun i ↦ (x i - m) / (M - m)
  have hH := sub_pos.mpr hspan
  have hz (i : Fin n) : z i ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg (sub_nonneg.mpr (hx i).1) hH.le,
      (div_le_one hH).mpr (sub_le_sub_right (hx i).2 _)⟩
  have h0 : ∃ i, z i = 0 := hmin.imp fun i hi ↦ by simp [z, hi]
  have h1 : ∃ i, z i = 1 := hmax.imp fun i hi ↦ by simp [z, hi, hH.ne']
  exact (Transfer.existsUnique_max_iff (endpoint_bij hspan a b) (fun _ hv ↦
    endpoint_likelihood hspan hv)).mpr (fixed_shapes_affine_fit hz h0 h1 ha hb)

end

end UnivariateML.Endpoint2NonstandardBetaAffine

end
