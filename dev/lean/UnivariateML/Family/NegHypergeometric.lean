module

public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Data.Nat.Factorial.Basic
public import Mathlib.Data.Nat.Choose.Basic
public import Mathlib.Analysis.SpecificLimits.Basic
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Negative hypergeometric scalar likelihoods

The observations are the black draws before the positive stopping count
of whites is reached. The likelihood omits the sample-only binomial factors.

## Deviations

The domains are unbounded integer rays. The held-black flat case has every
feasible white count as a maximizer. The joint fit is outside the scope here.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.NegHypergeometric

noncomputable section

variable {h : ℕ}

def reducedMass (r m n y : ℕ) : ℝ :=
  (m.descFactorial r : ℝ) * (n.descFactorial y : ℝ) / ((m + n).descFactorial (r + y) : ℝ)

def likelihood (r m n : ℕ) (y : Fin h → ℕ) : ℝ :=
  ∏ i, reducedMass r m n (y i)

/-- The stopped-draw mass for positive `r`, at total draw count `r + y`. -/
def mass (r m n y : ℕ) : ℝ :=
  ((r+y-1).choose (r-1) : ℝ) * reducedMass r m n y

/-- The omitted factors give a positive constant common to every urn. -/
theorem mass_product_eq (r : ℕ) (y : Fin h → ℕ) :
    ∃ c : ℝ, 0 < c ∧ ∀ m n, (∏ i, mass r m n (y i)) = c * likelihood r m n y := by
  refine ⟨∏ i, ((r+y i-1).choose (r-1) : ℝ), Finset.prod_pos ?_, ?_⟩
  · exact fun i _ ↦ Nat.cast_pos.mpr (Nat.choose_pos (by omega))
  · intro m n
    simp only [mass, Finset.prod_mul_distrib, likelihood]

/-- One factor of the held-white ratio, in the coordinate `A = m + n + 1`. -/
def whiteFactor (r m A t : ℝ) : ℝ :=
  (A - m) * (A - t) / (A * (A - m - t + r))

/-- One factor of the held-black ratio, in the coordinate `u = m + 1 - r`. -/
def blackFactor (r n u y : ℝ) : ℝ :=
  (u + n - y) * (u + r) / (u * (u + r + n))

def whiteRatio (r m A : ℝ) (y : Fin h → ℕ) : ℝ :=
  ∏ i, whiteFactor r m A (r + y i)

def blackRatio (r n u : ℝ) (y : Fin h → ℕ) : ℝ :=
  ∏ i, blackFactor r n u (y i)

private theorem white_pos {r m A t : ℝ} (hr : 0 < r) (hm : r ≤ m)
    (hA : m < A) (ht : t < A - m + r) : 0 < whiteFactor r m A t :=
  div_pos (mul_pos (by linarith) (by linarith)) (mul_pos (by linarith) (by linarith))

private theorem white_one {r m A t : ℝ} (hr : 0 < r) (hm : r ≤ m)
    (hA : m < A) (ht : t < A - m + r) :
    whiteFactor r m A t < 1 ↔ m * t < r * A := by
  unfold whiteFactor
  rw [div_lt_iff₀ (mul_pos (by linarith : 0 < A) (by linarith)), one_mul]
  constructor <;> intro H <;> linarith only [H,
    show (A - m) * (A - t) - A * (A - m - t + r) = m * t - r * A by ring]

private def whiteLog (r m A t : ℝ) : ℝ :=
  log (A - m) + log (A - t) - log A - log (A - m - t + r)

private theorem whiteLog_eq {r m A t : ℝ} (hr : 0 < r) (hm : r ≤ m)
    (hA : m < A) (ht : t < A - m + r) :
    whiteLog r m A t = log (whiteFactor r m A t) := by
  have p : A-m ≠ 0 := by linarith
  have q : A-t ≠ 0 := by linarith
  have s : A ≠ 0 := by linarith
  have w : A-m-t+r ≠ 0 := by linarith
  rw [whiteLog, whiteFactor, log_div (mul_ne_zero p q) (mul_ne_zero s w),
    log_mul p q, log_mul s w]
  ring

private theorem white_deriv {r m A t : ℝ} (hm : r ≤ m) (ht : t < A - m + r) :
    HasDerivAt (whiteLog r m A) ((m-r) / ((A-t)*(A-m-t+r))) t := by
  have p : A - t ≠ 0 := by linarith
  have q : A - m - t + r ≠ 0 := by linarith
  have d₁ := ((hasDerivAt_const t A).sub (hasDerivAt_id t)).log p
  have d₂ := (((hasDerivAt_const t (A-m)).sub (hasDerivAt_id t)).add_const r).log q
  convert (((hasDerivAt_const t (log (A-m))).add d₁).sub_const (log A)).sub d₂ using 1
  · rfl
  · change (m-r)/((A-t)*(A-m-t+r)) = (0 + (0-1)/(A-t)) - (0-1)/(A-m-t+r)
    field_simp
    ring

private theorem quotient_antitone {a b x z : ℝ} (hab : a < b) (hx : x < a)
    (hz : z < a) (hxz : x ≤ z) : (a-z)/(b-z) ≤ (a-x)/(b-x) := by
  rw [div_le_div_iff₀ (by linarith : 0 < b-z) (by linarith : 0 < b-x)]
  nlinarith [mul_nonneg (sub_nonneg.mpr hxz) (sub_nonneg.mpr hab.le)]

/-- Normalize at the old root. The difference of the two log factors reaches
its maximum there, and that maximum is strictly negative. -/
private theorem white_compare {r m A B : ℝ} (hr : 0 < r) (hm : r < m)
    (hA : m < A) (hAB : A < B) :
    ∃ c > 0, ∀ t < A-m+r, whiteLog r m B t < c * whiteLog r m A t := by
  let z := r*A/m
  have mp : 0 < m := hr.trans hm
  have hzEq : m*z = r*A := by dsimp [z]; field_simp
  have hz : z < A-m+r := by
    dsimp [z]
    apply (div_lt_iff₀ mp).2
    nlinarith [mul_pos (sub_pos.mpr hm) (sub_pos.mpr hA)]
  let q (t : ℝ) := (A-t)/(B-t) * ((A-m+r-t)/(B-m+r-t))
  have qpos (t : ℝ) (ht : t < A-m+r) : 0 < q t :=
    mul_pos (div_pos (by linarith) (by linarith)) (div_pos (by linarith) (by linarith))
  have qanti (x t : ℝ) (hx : x < A-m+r) (ht : t < A-m+r) (hxt : x ≤ t) :
      q t ≤ q x := by
    have h₁ := quotient_antitone (a := A) (b := B) hAB (by linarith) (by linarith) hxt
    have h₂ := quotient_antitone (a := A-m+r) (b := B-m+r) (by linarith) hx ht hxt
    exact mul_le_mul h₁ h₂ (div_nonneg (by linarith) (by linarith))
      (div_nonneg (by linarith) (by linarith))
  let d (t : ℝ) := (m-r)/((A-t)*(A-m-t+r))
  let g (t : ℝ) := whiteLog r m B t - q z * whiteLog r m A t
  have dg (t : ℝ) (ht : t < A-m+r) : HasDerivAt g ((q t - q z) * d t) t := by
    convert (white_deriv hm.le (show t < B-m+r by linarith)).sub
      ((white_deriv hm.le ht).const_mul (q z)) using 1
    dsimp [q, d]
    have h₁ : A-t ≠ 0 := by linarith
    have h₂ : A-m-t+r = A-m+r-t := by ring
    have h₃ : B-m-t+r = B-m+r-t := by ring
    have h₄ : A-m+r-t ≠ 0 := by linarith
    have h₅ : B-t ≠ 0 := by linarith
    have h₆ : B-m+r-t ≠ 0 := by linarith
    rw [h₂, h₃]
    field_simp
  have dp (t : ℝ) (ht : t < A-m+r) : 0 < d t :=
    div_pos (sub_pos.mpr hm) (mul_pos (by linarith) (by linarith))
  have gz : g z < 0 := by
    have hzero : whiteFactor r m A z = 1 := by
      unfold whiteFactor
      apply (div_eq_iff (mul_ne_zero (by linarith) (by linarith))).2
      nlinarith only [hzEq]
    have hb : whiteFactor r m B z < 1 :=
      (white_one hr hm.le (hA.trans hAB) (by linarith)).2 (by
        rw [hzEq]; exact mul_lt_mul_of_pos_left hAB hr)
    dsimp [g]
    rw [whiteLog_eq hr hm.le hA hz, hzero, log_one, mul_zero, sub_zero,
      whiteLog_eq hr hm.le (hA.trans hAB) (by linarith)]
    exact log_neg (white_pos hr hm.le (hA.trans hAB) (by linarith)) hb
  refine ⟨q z, qpos z hz, fun t ht ↦ ?_⟩
  suffices g t ≤ g z by dsimp [g] at *; linarith
  rcases le_total t z with htz | hzt
  · have H : MonotoneOn g (Icc t z) := by
      apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
      · exact fun x hx ↦ (dg x (hx.2.trans_lt hz)).continuousAt.continuousWithinAt
      · intro x hx
        have hx' : x ∈ Icc t z := interior_subset hx
        exact (dg x (hx'.2.trans_lt hz)).differentiableAt.differentiableWithinAt
      · intro x hx
        have hx' : x ∈ Icc t z := interior_subset hx
        have hxx := hx'.2.trans_lt hz
        rw [(dg x hxx).deriv]
        exact mul_nonneg (sub_nonneg.mpr (qanti x z hxx hz hx'.2)) (dp x hxx).le
    exact H ⟨le_rfl, htz⟩ ⟨htz, le_rfl⟩ htz
  · have H : AntitoneOn g (Icc z t) := by
      apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
      · exact fun x hx ↦ (dg x (hx.2.trans_lt ht)).continuousAt.continuousWithinAt
      · intro x hx
        have hx' : x ∈ Icc z t := interior_subset hx
        exact (dg x (hx'.2.trans_lt ht)).differentiableAt.differentiableWithinAt
      · intro x hx
        have hx' : x ∈ Icc z t := interior_subset hx
        have hxx := hx'.2.trans_lt ht
        rw [(dg x hxx).deriv]
        exact mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr (qanti z x hz hxx hx'.1)) (dp x hxx).le
    exact H ⟨le_rfl, hzt⟩ ⟨hzt, le_rfl⟩ hzt

private theorem prod_cross (hh : 0 < h) {f g : Fin h → ℝ}
    (hf : ∀ i, 0 < f i) (hg : ∀ i, 0 < g i) {c : ℝ} (hc : 0 < c)
    (H : ∀ i, log (g i) < c * log (f i)) (hle : ∏ i, f i ≤ 1) :
    ∏ i, g i < 1 := by
  have hs := Finset.sum_lt_sum
    (fun i (_ : i ∈ (Finset.univ : Finset (Fin h))) ↦ (H i).le)
    ⟨⟨0, hh⟩, Finset.mem_univ _, H _⟩
  rw [← Finset.mul_sum, ← log_prod (fun i _ ↦ (hf i).ne'),
    ← log_prod (fun i _ ↦ (hg i).ne')] at hs
  have hf' := Finset.prod_pos (s := Finset.univ) (fun i _ ↦ hf i)
  have hg' := Finset.prod_pos (s := Finset.univ) (fun i _ ↦ hg i)
  exact (log_neg_iff hg').mp (hs.trans_le (mul_nonpos_of_nonneg_of_nonpos hc.le
    ((log_nonpos_iff hf'.le).2 hle)))

theorem white_single_crossing (hh : 0 < h) (y : Fin h → ℕ) {r m A B : ℝ}
    (hr : 0 < r) (hm : r ≤ m) (hA : m < A) (hAB : A < B)
    (hy : ∀ i, (y i : ℝ) < A-m) (hle : whiteRatio r m A y ≤ 1) :
    whiteRatio r m B y < 1 := by
  rcases hm.eq_or_lt with he | hs
  · unfold whiteRatio
    conv_rhs => rw [← show (∏ _ : Fin h, (1 : ℝ)) = 1 by simp]
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      exact white_pos hr hm (hA.trans hAB) (by linarith [hy i])
    · intro i _
      apply (white_one hr hm (hA.trans hAB) (by linarith [hy i])).2
      rw [← he]
      exact mul_lt_mul_of_pos_left (by linarith [hy i]) hr
    · exact ⟨⟨0, hh⟩, Finset.mem_univ _⟩
  · obtain ⟨c, hc, H⟩ := white_compare hr hs hA hAB
    refine prod_cross hh (fun i ↦ white_pos hr hm hA (by linarith [hy i]))
      (fun i ↦ white_pos hr hm (hA.trans hAB) (by linarith [hy i])) hc ?_ hle
    intro i
    have hi : r + (y i : ℝ) < A-m+r := by linarith [hy i]
    simpa only [whiteLog_eq hr hm hA hi,
      whiteLog_eq hr hm (hA.trans hAB) (show r + (y i : ℝ) < B-m+r by linarith)] using H _ hi

private theorem black_pos {r n u y : ℝ} (hr : 0 < r) (hu : 0 < u)
    (hn : 0 ≤ n) (hy : y ≤ n) : 0 < blackFactor r n u y :=
  div_pos (mul_pos (by linarith) (by linarith)) (mul_pos hu (by linarith))

/-- The scaled log difference falls up to `r` and rises thereafter. Comparing
its increments gives the strict inequality from the source integral argument. -/
private theorem black_compare {r n u v : ℝ} (hr : 0 < r) (hn : 0 < n)
    (hu : 0 < u) (huv : u < v) :
    ∃ c > 0, ∀ y ∈ Icc (0 : ℝ) n, log (blackFactor r n v y) <
      c * log (blackFactor r n u y) := by
  let c := (u+r)/(v+r)
  let g (z : ℝ) := log (v+z) - c * log (u+z)
  have cp : 0 < c := div_pos (by linarith) (by linarith)
  have dg (z : ℝ) (hz : 0 ≤ z) :
      HasDerivAt g ((v-u)*(z-r)/((u+z)*(v+z)*(v+r))) z := by
    have p : 0 < u+z := by linarith
    have q : 0 < v+z := by linarith
    have s : 0 < v+r := by linarith
    convert (((hasDerivAt_id z).const_add v).log q.ne').sub
      ((((hasDerivAt_id z).const_add u).log p.ne').const_mul c) using 1
    · rfl
    · simp only [id_eq, c]
      field_simp
      ring
  have anti : AntitoneOn g (Icc 0 r) := by
    apply antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    · exact fun z hz ↦ (dg z hz.1).continuousAt.continuousWithinAt
    · intro z hz
      have hz' : z ∈ Icc 0 r := interior_subset hz
      exact (dg z hz'.1).differentiableAt.differentiableWithinAt
    · intro z hz
      have hz' : z ∈ Icc 0 r := interior_subset hz
      rw [(dg z hz'.1).deriv]
      exact div_nonpos_of_nonpos_of_nonneg
        (mul_nonpos_of_nonneg_of_nonpos (by linarith) (by linarith [hz'.2]))
        (mul_nonneg (mul_nonneg (by linarith [hz'.1]) (by linarith [hz'.1])) (by linarith))
  have mono : StrictMonoOn g (Icc r (r+n)) := by
    apply strictMonoOn_of_deriv_pos (convex_Icc _ _)
    · exact fun z hz ↦ (dg z (hr.le.trans hz.1)).continuousAt.continuousWithinAt
    · intro z hz
      rw [interior_Icc] at hz
      rw [(dg z (hr.le.trans hz.1.le)).deriv]
      have hz₀ := hz.1
      exact div_pos (mul_pos (sub_pos.mpr huv) (sub_pos.mpr hz₀))
        (mul_pos (mul_pos (by linarith) (by linarith)) (by linarith))
  refine ⟨c, cp, fun y hy ↦ ?_⟩
  have hgr : g r ≤ g 0 := anti ⟨le_rfl, hr.le⟩ ⟨hr.le, le_rfl⟩ hr.le
  have H : g (n-y) - g 0 < g (r+n) - g r := by
    by_cases hy' : n-y ≤ r
    · have h₁ := anti (show 0 ∈ Icc 0 r from ⟨le_rfl, hr.le⟩)
        (show n-y ∈ Icc 0 r from ⟨by linarith [hy.2], hy'⟩) (by linarith [hy.2])
      have h₂ := mono (show r ∈ Icc r (r+n) from ⟨le_rfl, by linarith⟩)
        (show r+n ∈ Icc r (r+n) from ⟨by linarith, le_rfl⟩) (by linarith)
      linarith
    · have h₁ := mono (show n-y ∈ Icc r (r+n) from ⟨by linarith, by linarith [hy.1]⟩)
        (show r+n ∈ Icc r (r+n) from ⟨by linarith, le_rfl⟩) (by linarith [hy.1])
      linarith
  have E (w : ℝ) (hw : 0 < w) : log (blackFactor r n w y) =
      log (w+(n-y)) - log w - (log (w+(r+n)) - log (w+r)) := by
    have he : w+n-y = w+(n-y) := by ring
    have he' : w+r+n = w+(r+n) := by ring
    have p : w+(n-y) ≠ 0 := by linarith [hy.2]
    have q : w+r ≠ 0 := by linarith
    have s : w+(r+n) ≠ 0 := by linarith
    rw [blackFactor, he, he', log_div (mul_ne_zero p q) (mul_ne_zero hw.ne' s),
      log_mul p q, log_mul hw.ne' s]
    ring
  rw [E v (hu.trans huv), E u hu]
  dsimp [g] at H
  simp only [add_zero] at H
  nlinarith only [H]

theorem black_single_crossing (hh : 0 < h) (y : Fin h → ℕ) {r n u v : ℝ}
    (hr : 0 < r) (hn : 0 < n) (hu : 0 < u) (huv : u < v)
    (hy : ∀ i, (y i : ℝ) ≤ n) (hle : blackRatio r n u y ≤ 1) :
    blackRatio r n v y < 1 := by
  obtain ⟨c, hc, H⟩ := black_compare hr hn hu huv
  exact prod_cross hh (fun i ↦ black_pos hr hu hn.le (hy i))
    (fun i ↦ black_pos hr (hu.trans huv) hn.le (hy i)) hc
    (fun i ↦ H _ ⟨Nat.cast_nonneg _, hy i⟩) hle

private theorem cast_desc_pos {m k : ℕ} (hk : k ≤ m) : (0 : ℝ) < m.descFactorial k :=
  Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hk)

private theorem desc_step {m k : ℕ} (hk : k ≤ m) :
    ((m+1).descFactorial k : ℝ) =
    ((m : ℝ)+1) / ((m : ℝ)+1-k) * (m.descFactorial k : ℝ) := by
  have H := congrArg (fun j : ℕ ↦ (j : ℝ)) (Nat.succ_descFactorial m k)
  have hk' : k ≤ m+1 := by omega
  push_cast [Nat.cast_sub hk'] at H
  have hp : (0 : ℝ) < (m : ℝ)+1-k := by
    have : (k : ℝ) ≤ m := by exact_mod_cast hk
    linarith
  rw [div_mul_eq_mul_div, eq_div_iff hp.ne']
  linear_combination H

theorem likelihood_pos {r m n : ℕ} (y : Fin h → ℕ) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) : 0 < likelihood r m n y := by
  exact Finset.prod_pos fun i _ ↦ div_pos (mul_pos (cast_desc_pos hm) (cast_desc_pos (hn i)))
    (cast_desc_pos (Nat.add_le_add hm (hn i)))

theorem white_adjacent {r m n : ℕ} (y : Fin h → ℕ) (_hr : 0 < r) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) :
    likelihood r m (n+1) y = likelihood r m n y * whiteRatio r m (m+n+1) y := by
  rw [likelihood, likelihood, whiteRatio, ← Finset.prod_mul_distrib]
  apply Finset.prod_congr rfl
  intro i _
  have hy := hn i
  have hs := Nat.add_le_add hm hy
  have p := (cast_desc_pos hs).ne'
  have q : (0 : ℝ) < (n : ℝ)+1-y i := by
    have : (y i : ℝ) ≤ n := by exact_mod_cast hy
    linarith
  have v : (0 : ℝ) < (m : ℝ)+n+1 := by positivity
  have w : (0 : ℝ) < (m : ℝ)+n+1-(r+y i : ℝ) := by
    have hs' : (r : ℝ)+y i ≤ (m : ℝ)+n := by exact_mod_cast hs
    linarith
  simp only [reducedMass, ← Nat.add_assoc, desc_step hy, desc_step hs, Nat.cast_add, whiteFactor]
  field_simp (discharger := linarith)
  ring

theorem black_adjacent {r m n : ℕ} (y : Fin h → ℕ) (_hr : 0 < r) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) :
    likelihood r (m+1) n y = likelihood r m n y * blackRatio r n (m+1-r) y := by
  rw [likelihood, likelihood, blackRatio, ← Finset.prod_mul_distrib]
  apply Finset.prod_congr rfl
  intro i _
  have hy := hn i
  have hs := Nat.add_le_add hm hy
  have p := (cast_desc_pos hs).ne'
  have q : (0 : ℝ) < (m : ℝ)+1-r := by
    have : (r : ℝ) ≤ m := by exact_mod_cast hm
    linarith
  have v : (0 : ℝ) < (m : ℝ)+n+1 := by positivity
  have w : (0 : ℝ) < (m : ℝ)+n+1-(r+y i : ℝ) := by
    have hs' : (r : ℝ)+y i ≤ (m : ℝ)+n := by exact_mod_cast hs
    linarith
  have E : m+1+n = m+n+1 := by omega
  simp only [reducedMass, E, desc_step hm, desc_step hs, Nat.cast_add, blackFactor]
  field_simp
  ring

/-- A positive sequence with a first nonpositive ratio comparison and strict
single crossing has at most the crossing index and its successor as maxima. -/
private theorem ray_maximizers (L R : ℕ → ℝ) (b : ℕ)
    (hp : ∀ k ≥ b, 0 < L k) (step : ∀ k ≥ b, L (k+1) = L k * R k)
    (cross : ∀ k j, b ≤ k → k < j → R k ≤ 1 → R j < 1)
    (hit : ∃ k, b ≤ k ∧ R k ≤ 1) :
    ∃ k ≥ b, R k ≤ 1 ∧ (∀ j, b ≤ j → j < k → 1 < R j) ∧
      ∀ j, IsMaxOn L (Ici b) j ∧ j ∈ Ici b ↔ j = k ∨ (j = k+1 ∧ R k = 1) := by
  classical
  let k := Nat.find hit
  obtain ⟨hb, hk⟩ := Nat.find_spec hit
  have before (j : ℕ) (hj : b ≤ j) (hjk : j < k) : 1 < R j :=
    lt_of_not_ge (fun H ↦ (Nat.find_min hit hjk) ⟨hj, H⟩)
  have riseAux (s : ℕ) (hs : s ≤ k) (j : ℕ) (hj : b ≤ j) (hjs : j < s) :
      L j < L s := by
    induction s with
    | zero => omega
    | succ s ih =>
      have hbs : b ≤ s := by omega
      have hstep : L s < L (s+1) := by
        rw [step s hbs]
        simpa using mul_lt_mul_of_pos_left (before s hbs (by omega)) (hp s hbs)
      by_cases h : j = s
      · simpa [h] using hstep
      · exact (ih (by omega) (by omega)).trans hstep
  have rise (j : ℕ) (hj : b ≤ j) (hjk : j < k) : L j < L k :=
    riseAux k le_rfl j hj hjk
  have fall (j : ℕ) (hj : k+1 < j) : L j < L (k+1) := by
    apply Nat.rel_of_forall_rel_succ_of_le_of_lt (fun a b : ℝ ↦ b < a)
      (f := L) (a := k+1) _ le_rfl hj
    intro t ht
    have hbt : b ≤ t := by omega
    rw [step t hbt]
    simpa using mul_lt_mul_of_pos_left (cross k t hb (by omega) hk) (hp t hbt)
  have next : L (k+1) ≤ L k := by
    rw [step k hb]
    simpa using mul_le_mul_of_nonneg_left hk (hp k hb).le
  have tie : L (k+1) = L k ↔ R k = 1 := by
    rw [step k hb]
    constructor
    · intro H
      apply (mul_left_cancel₀ (hp k hb).ne')
      simpa using H
    · intro H
      simp [H]
  have best : IsMaxOn L (Ici b) k := by
    intro j hj
    change b ≤ j at hj
    change L j ≤ L k
    by_cases hlt : j < k
    · exact (rise j hj hlt).le
    by_cases he : j = k
    · simp [he]
    by_cases he' : j = k+1
    · simpa [he'] using next
    exact (fall j (by omega)).le.trans next
  refine ⟨k, hb, hk, before, fun j ↦ ⟨?_, ?_⟩⟩
  · rintro ⟨hj, hbj⟩
    have hjk : L k ≤ L j := hj hb
    by_cases hlt : j < k
    · exact False.elim ((rise j hbj hlt).not_ge hjk)
    by_cases he : j = k
    · exact Or.inl he
    by_cases he' : j = k+1
    · exact Or.inr ⟨he', tie.mp (next.antisymm (he' ▸ hjk))⟩
    exact False.elim (((fall j (by omega)).trans_le next).not_ge hjk)
  · rintro (rfl | ⟨rfl, he⟩)
    · exact ⟨best, hb⟩
    · exact ⟨fun j hj ↦ (best hj).trans_eq (tie.mpr he).symm, hb.trans (Nat.le_succ _)⟩

private theorem white_maximizers_on_ray {r m b : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) (hb : ∀ i, y i ≤ b) :
    ∃ k ≥ b, whiteRatio r m (m+k+1) y ≤ 1 ∧
      (∀ j, b ≤ j → j < k → 1 < whiteRatio r m (m+j+1) y) ∧
      ∀ j, IsMaxOn (fun n ↦ likelihood r m n y) (Ici b) j ∧ j ∈ Ici b ↔
        j = k ∨ (j = k+1 ∧ whiteRatio r m (m+k+1) y = 1) := by
  have hr' : (0 : ℝ) < r := by exact_mod_cast hr
  have hm' : (r : ℝ) ≤ m := by exact_mod_cast hm
  have support (k : ℕ) (hk : b ≤ k) (i : Fin h) : (y i : ℝ) ≤ k := by
    exact_mod_cast (hb i).trans hk
  apply ray_maximizers _ _ b (fun k hk ↦ likelihood_pos y hm (fun i ↦ (hb i).trans hk))
    (fun k hk ↦ white_adjacent y hr hm (fun i ↦ (hb i).trans hk))
  · intro k j hk hkj H
    have hkj' : (k : ℝ) < j := by exact_mod_cast hkj
    exact white_single_crossing hh y (A := (m : ℝ)+k+1) (B := (m : ℝ)+j+1)
      hr' hm' (by linarith [show (0 : ℝ) ≤ k from Nat.cast_nonneg k]) (by linarith)
      (fun i ↦ by linarith [support k hk i]) H
  · obtain ⟨k, hk⟩ := exists_nat_gt ((m : ℝ)*(r+b)/r + b)
    have hcut : (m : ℝ)*(r+b)/r < k := by
      linarith [show (0 : ℝ) ≤ b from Nat.cast_nonneg b]
    have hbk : b ≤ k := by
      have hn : 0 ≤ (m : ℝ)*(r+b)/r := by positivity
      exact_mod_cast (show (b : ℝ) ≤ k by linarith)
    have hA : (m : ℝ) < (m : ℝ)+k+1 := by linarith [show (0 : ℝ) ≤ k from Nat.cast_nonneg k]
    refine ⟨k, hbk, ?_⟩
    unfold whiteRatio
    conv_rhs => rw [← show (∏ _ : Fin h, (1 : ℝ)) = 1 by simp]
    apply Finset.prod_le_prod₀
    · intro i _
      exact (white_pos hr' hm' hA (by linarith [support k hbk i])).le
    · intro i _
      apply le_of_lt
      apply (white_one hr' hm' hA (by linarith [support k hbk i])).2
      have hmul := (div_lt_iff₀ hr').1 hcut
      have hbi := support b le_rfl i
      have h₁ := mul_le_mul_of_nonneg_left (show (r : ℝ)+y i ≤ r+b by linarith)
        (Nat.cast_nonneg m)
      have h₂ : (k : ℝ) < (m : ℝ)+k+1 := by linarith [show (0 : ℝ) ≤ m from Nat.cast_nonneg m]
      nlinarith [mul_lt_mul_of_pos_left h₂ hr']

/-- The first feasible black count. -/
def maxBlackDraws (y : Fin h → ℕ) : ℕ := Finset.univ.sup y

theorem white_feasible_iff (y : Fin h → ℕ) (n : ℕ) :
    n ∈ Ici (maxBlackDraws y) ↔ ∀ i, y i ≤ n := by
  simp [maxBlackDraws, Finset.sup_le_iff]

theorem white_maximizers {r m : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) :
    ∃ k ≥ maxBlackDraws y, whiteRatio r m (m+k+1) y ≤ 1 ∧
      (∀ j, maxBlackDraws y ≤ j → j < k → 1 < whiteRatio r m (m+j+1) y) ∧
      ∀ j, IsMaxOn (fun n ↦ likelihood r m n y) (Ici (maxBlackDraws y)) j ∧
        j ∈ Ici (maxBlackDraws y) ↔
        j = k ∨ (j = k+1 ∧ whiteRatio r m (m+k+1) y = 1) :=
  white_maximizers_on_ray hh y hr hm ((white_feasible_iff y _).mp (mem_Ici.mpr le_rfl))

private theorem shifted_div_limit (a b c : ℝ) :
    Tendsto (fun m : ℕ ↦ (a + b*m)/(c + m)) atTop (𝓝 b) := by
  simpa using tendsto_add_mul_div_add_mul_atTop_nhds a c b (d := (1 : ℝ)) one_ne_zero

private theorem black_zero_mass_limit (n r : ℕ) :
    Tendsto (fun m : ℕ ↦ reducedMass r m n 0) atTop (𝓝 1) := by
  induction r with
  | zero => simp [reducedMass]
  | succ r ih =>
    have H := (shifted_div_limit (- (r : ℝ)) 1 ((n : ℝ)-r)).mul ih
    simp only [one_mul] at H
    apply H.congr'
    filter_upwards [eventually_ge_atTop r] with m hm
    have hs : r ≤ m+n := by omega
    simp only [reducedMass, Nat.descFactorial_zero, Nat.add_zero, Nat.cast_one, mul_one,
      Nat.descFactorial_succ, Nat.cast_mul, Nat.cast_sub hm, Nat.cast_sub hs, Nat.cast_add]
    rw [show -(r : ℝ) + m = m-r by ring, show (n : ℝ)-r+m = m+n-r by ring, div_mul_div_comm]

private theorem black_mass_limit (n r y : ℕ) :
    Tendsto (fun m : ℕ ↦ reducedMass r m n y) atTop (𝓝 (if y = 0 then 1 else 0)) := by
  induction y with
  | zero => simpa using black_zero_mass_limit n r
  | succ y ih =>
    have H := (shifted_div_limit (↑(n-y : ℕ)) 0 ((n : ℝ)-(r+y : ℕ))).mul ih
    simp only [zero_mul, add_zero, Nat.succ_ne_zero, ite_false] at H ⊢
    apply H.congr'
    filter_upwards [eventually_ge_atTop (r+y)] with m hm
    have hs : r+y ≤ m+n := by omega
    simp only [reducedMass, show r + (y+1) = r+y+1 by omega,
      Nat.descFactorial_succ, Nat.cast_mul, Nat.cast_sub hs, Nat.cast_add]
    rw [show (n : ℝ)-(r+y : ℝ)+m = m+n-(r+y) by ring, div_mul_div_comm]
    ring

theorem black_mass_pointwise_limit (r n y : ℕ) :
    Tendsto (fun m : ℕ ↦ mass r m n y) atTop (𝓝 (if y = 0 then 1 else 0)) := by
  have H := (tendsto_const_nhds (x := ((r+y-1).choose (r-1) : ℝ))).mul
    (black_mass_limit n r y)
  by_cases hy : y = 0
  · subst y
    simpa [mass] using H
  · simpa [mass, hy] using H

theorem black_likelihood_limit {r n : ℕ} (y : Fin h → ℕ) :
    Tendsto (fun m : ℕ ↦ likelihood r m n y) atTop
      (𝓝 (if (∀ i, y i = 0) then 1 else 0)) := by
  classical
  have H := tendsto_finsetProd Finset.univ (fun i _ ↦ black_mass_limit n r (y i))
  unfold likelihood
  convert H using 2
  split_ifs with hy
  · simp [hy]
  · push Not at hy
    obtain ⟨i, hi⟩ := hy
    symm
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    simp [hi]

theorem black_maximizers {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : ∀ i, y i ≤ n) (hy : ∃ i, 0 < y i) :
    ∃ k ≥ r, blackRatio r n (k+1-r) y ≤ 1 ∧
      (∀ j, r ≤ j → j < k → 1 < blackRatio r n (j+1-r) y) ∧
      ∀ j, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) j ∧ j ∈ Ici r ↔
        j = k ∨ (j = k+1 ∧ blackRatio r n (k+1-r) y = 1) := by
  have hp : 0 < n := by obtain ⟨i, hi⟩ := hy; exact hi.trans_le (hn i)
  have hpos (k : ℕ) (hk : r ≤ k) : (0 : ℝ) < (k : ℝ)+1-r := by
    have : (r : ℝ) ≤ k := by exact_mod_cast hk
    linarith
  apply ray_maximizers _ _ r (fun k hk ↦ likelihood_pos y hk hn)
    (fun k hk ↦ black_adjacent y hr hk hn)
  · intro k j hk hkj H
    have hkj' : (k : ℝ) < j := by exact_mod_cast hkj
    exact black_single_crossing hh y (by exact_mod_cast hr) (by exact_mod_cast hp)
      (hpos k hk) (by linarith) (fun i ↦ by exact_mod_cast hn i) H
  · by_contra! H
    have hm := monotoneOn_nat_Ici_of_le_succ (k := r) (f := fun m ↦ likelihood r m n y)
      (fun k hk ↦ by
        rw [black_adjacent y hr hk hn]
        simpa using mul_le_mul_of_nonneg_left (H k hk).le (likelihood_pos y hk hn).le)
    have hlim := black_likelihood_limit (r := r) (n := n) y
    have hnot : ¬ ∀ i, y i = 0 := by
      obtain ⟨i, hi⟩ := hy
      exact fun H ↦ hi.ne' (H i)
    simp only [hnot, ite_false] at hlim
    have hbad := ge_of_tendsto hlim ((eventually_ge_atTop r).mono fun k hk ↦
      hm (a := r) (b := k) (show r ∈ {x | r ≤ x} from Nat.le_refl r) hk hk)
    exact (likelihood_pos y (m := r) le_rfl hn).not_ge hbad

theorem black_flat {r : ℕ} (y : Fin h → ℕ) (hy : ∀ i, y i = 0) {m : ℕ}
    (hm : r ≤ m) : likelihood r m 0 y = 1 := by
  simp [likelihood, reducedMass, hy, (cast_desc_pos hm).ne']

theorem black_escape {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : 0 < n) (hy : ∀ i, y i = 0) :
    StrictMonoOn (fun m ↦ likelihood r m n y) (Ici r) ∧
    Tendsto (fun m ↦ likelihood r m n y) atTop (𝓝 1) ∧
    sSup ((fun m ↦ likelihood r m n y) '' Ici r) = 1 ∧
    ¬ ∃ m ∈ Ici r, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) m := by
  have mono : StrictMonoOn (fun m ↦ likelihood r m n y) (Ici r) := by
    intro m hm k _ hmk
    apply Nat.rel_of_forall_rel_succ_of_le_of_lt (· < ·) _ hm hmk
    intro j hj
    have hsupport (i : Fin h) : y i ≤ n := by simp [hy i]
    rw [black_adjacent y hr hj hsupport]
    have hp := likelihood_pos y hj hsupport
    have hu : (0 : ℝ) < (j : ℝ)+1-r := by
      have : (r : ℝ) ≤ j := by exact_mod_cast hj
      linarith
    have rp : (0 : ℝ) < r := by exact_mod_cast hr
    have np : (0 : ℝ) < n := by exact_mod_cast hn
    have H : 1 < blackRatio r n (j+1-r) y := by
      unfold blackRatio
      conv_lhs => rw [← show (∏ _ : Fin h, (1 : ℝ)) = 1 by simp]
      apply Finset.prod_lt_prod_of_nonempty₀
      · simp
      · intro i _
        simp only [hy i, Nat.cast_zero, blackFactor, sub_zero]
        rw [lt_div_iff₀ (mul_pos hu (by linarith)), one_mul]
        nlinarith [mul_pos rp np]
      · exact ⟨⟨0, hh⟩, Finset.mem_univ _⟩
    simpa using mul_lt_mul_of_pos_left H hp
  have lim : Tendsto (fun m ↦ likelihood r m n y) atTop (𝓝 1) := by
    simpa [hy] using black_likelihood_limit (r := r) (n := n) y
  have bound (m : ℕ) (hm : m ∈ Ici r) : likelihood r m n y ≤ 1 :=
    ge_of_tendsto lim ((eventually_ge_atTop m).mono fun k hk ↦
      mono.monotoneOn hm (hm.trans hk) hk)
  have bounded : BddAbove ((fun m ↦ likelihood r m n y) '' Ici r) :=
    ⟨1, by rintro _ ⟨m, hm, rfl⟩; exact bound m hm⟩
  refine ⟨mono, lim, le_antisymm ?_ ?_, ?_⟩
  · apply csSup_le (Set.image_nonempty.mpr ⟨r, mem_Ici.mpr le_rfl⟩)
    rintro _ ⟨m, hm, rfl⟩
    exact bound m hm
  · exact UnivariateML.Generic.limit_le_sSup (eventually_ge_atTop r) lim bounded
  · rintro ⟨m, hm, hmax⟩
    have hms : m+1 ∈ Ici r := hm.trans (Nat.le_succ _)
    exact (mono hm hms (Nat.lt_succ_self _)).not_ge (hmax hms)

theorem black_nonattainment_iff {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : ∀ i, y i ≤ n) :
    (¬ ∃ m ∈ Ici r, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) m) ↔
      0 < n ∧ ∀ i, y i = 0 := by
  refine ⟨?_, fun ⟨hp, hy⟩ ↦ (black_escape hh y hr hp hy).2.2.2⟩
  intro H
  have hz : ∀ i, y i = 0 := by
    by_contra! hy
    obtain ⟨i, hi⟩ := hy
    obtain ⟨k, _, _, _, hk⟩ := black_maximizers hh y hr hn ⟨i, Nat.pos_of_ne_zero hi⟩
    exact H ⟨k, ((hk k).2 (Or.inl rfl)).2, ((hk k).2 (Or.inl rfl)).1⟩
  refine ⟨Nat.pos_of_ne_zero ?_, hz⟩
  intro hn0
  subst n
  apply H
  refine ⟨r, mem_Ici.mpr le_rfl, fun m hm ↦ ?_⟩
  change likelihood r m 0 y ≤ likelihood r r 0 y
  rw [black_flat y hz hm, black_flat y hz le_rfl]

private theorem prod_ratio_eq_one {a b : Fin h → ℕ} (hb : ∀ i, 0 < b i) :
    (∏ i, (a i : ℝ)/(b i : ℝ)) = 1 ↔ (∏ i, a i) = ∏ i, b i := by
  have hp : 0 < ∏ i, (b i : ℝ) := Finset.prod_pos fun i _ ↦ Nat.cast_pos.mpr (hb i)
  rw [Finset.prod_div_distrib, div_eq_iff hp.ne', one_mul]
  exact_mod_cast (show ((∏ i, a i : ℕ) : ℝ) = ((∏ i, b i : ℕ) : ℝ) ↔
    (∏ i, a i) = ∏ i, b i from Nat.cast_inj)

theorem white_tie_iff {r m n : ℕ} (y : Fin h → ℕ) (hr : 0 < r)
    (hm : r ≤ m) (hn : ∀ i, y i ≤ n) :
    likelihood r m (n+1) y = likelihood r m n y ↔
      (∏ i, (n+1)*(m+n+1-(r+y i))) = ∏ i, (m+n+1)*(n+1-y i) := by
  rw [white_adjacent y hr hm hn]
  have hp := (likelihood_pos y hm hn).ne'
  have H : whiteRatio r m (m+n+1) y =
      ∏ i, ((n+1)*(m+n+1-(r+y i)) : ℕ) / (((m+n+1)*(n+1-y i) : ℕ) : ℝ) := by
    unfold whiteRatio whiteFactor
    apply Finset.prod_congr rfl
    intro i _
    have hi := hn i
    have he : r+y i ≤ m+n+1 := by omega
    have he' : y i ≤ n+1 := by omega
    push_cast [Nat.cast_sub he, Nat.cast_sub he']
    ring
  have cancel (R : ℝ) : likelihood r m n y * R = likelihood r m n y ↔ R = 1 := by
    constructor
    · intro H
      apply mul_left_cancel₀ hp
      simpa using H
    · intro H
      simp [H]
  rw [cancel, H]
  exact prod_ratio_eq_one fun i ↦
    Nat.mul_pos (by omega) (Nat.sub_pos_of_lt (by have := hn i; omega))

theorem black_tie_iff {r m n : ℕ} (y : Fin h → ℕ) (hr : 0 < r)
    (hm : r ≤ m) (hn : ∀ i, y i ≤ n) :
    likelihood r (m+1) n y = likelihood r m n y ↔
      (∏ i, (m+1)*(m+n+1-(r+y i))) = ∏ _ : Fin h, (m+1-r)*(m+n+1) := by
  rw [black_adjacent y hr hm hn]
  have hp := (likelihood_pos y hm hn).ne'
  have H : blackRatio r n (m+1-r) y =
      ∏ i, ((m+1)*(m+n+1-(r+y i)) : ℕ) / (((m+1-r)*(m+n+1) : ℕ) : ℝ) := by
    unfold blackRatio blackFactor
    apply Finset.prod_congr rfl
    intro i _
    have hi := hn i
    have he : r+y i ≤ m+n+1 := by omega
    have he' : r ≤ m+1 := by omega
    push_cast [Nat.cast_sub he, Nat.cast_sub he']
    ring
  have cancel (R : ℝ) : likelihood r m n y * R = likelihood r m n y ↔ R = 1 := by
    constructor
    · intro H
      apply mul_left_cancel₀ hp
      simpa using H
    · intro H
      simp [H]
  rw [cancel, H]
  exact prod_ratio_eq_one fun _ ↦ by
    exact Nat.mul_pos (Nat.sub_pos_of_lt (by omega)) (by omega)

end

end UnivariateML.NegHypergeometric
