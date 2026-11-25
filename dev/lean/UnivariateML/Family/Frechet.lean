module

public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Held-shape Fréchet likelihood

The input is the centered sample of gaps g i = x i - m, where m is the
sample minimum. The positive location gap e gives the original location
μ = m - e and each distance to it is g i + e.

## Deviations

The statements concern exact real arithmetic and nonempty uncensored samples.
The collapse threshold and supremum are expressed for the total log-likelihood.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Frechet

noncomputable section

variable {n : ℕ}

def logDensity (lam sigma x : ℝ) : ℝ :=
  log lam - log sigma - (1 + lam) * log (x / sigma) - (x / sigma) ^ (-lam)

def logLik (g : Fin n → ℝ) (lam e sigma : ℝ) : ℝ :=
  ∑ i, logDensity lam sigma (g i + e)

def powerSum (g : Fin n → ℝ) (p e : ℝ) : ℝ :=
  ∑ i, (g i + e) ^ (-p)

def minimumCount (g : Fin n → ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ g i = 0).card

def scale (g : Fin n → ℝ) (lam e : ℝ) : ℝ :=
  (n / powerSum g lam e) ^ lam⁻¹

def profile (g : Fin n → ℝ) (lam e : ℝ) : ℝ :=
  logLik g lam e (scale g lam e)

def ratio (g : Fin n → ℝ) (lam e : ℝ) : ℝ :=
  powerSum g (lam + 1) e / (powerSum g lam e * powerSum g 1 e)

def collapseExponent (g : Fin n → ℝ) (lam : ℝ) : ℝ :=
  lam * (n - (minimumCount g : ℝ)) - minimumCount g

def collapseValue (g : Fin n → ℝ) (lam : ℝ) : ℝ :=
  n * log (n * lam / minimumCount g) - n -
    (lam + 1) * ∑ i, log (g i)

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

variable {g : Fin n → ℝ} {lam e : ℝ}

private theorem distance_pos (hg : ∀ i, 0 ≤ g i) (he : 0 < e) (i : Fin n) :
    0 < g i + e := add_pos_of_nonneg_of_pos (hg i) he

private theorem powerSum_pos (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (he : 0 < e) (p : ℝ) :
    0 < powerSum g p e := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact Finset.sum_pos (fun i _ ↦ rpow_pos_of_pos (distance_pos hg he i) _) Finset.univ_nonempty

private theorem logLik_eq (hg : ∀ i, 0 ≤ g i) (he : 0 < e) {s : ℝ} (hs : 0 < s) :
    logLik g lam e s =
      n * log lam + n * log (s ^ lam) -
        (lam + 1) * (∑ i, log (g i + e)) - s ^ lam * powerSum g lam e := by
  unfold logLik logDensity powerSum
  simp only [log_div (distance_pos hg he _).ne' hs.ne',
    div_rpow (distance_pos hg he _).le hs.le, rpow_neg hs.le, div_inv_eq_mul]
  simp only [Finset.sum_sub_distrib, ← Finset.mul_sum,
    ← Finset.sum_mul, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    log_rpow hs]
  ring

/-- The explicit positive scale strictly maximizes the likelihood at a fixed gap. -/
theorem heldShape (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam) (he : 0 < e) :
    0 < scale g lam e ∧ scale g lam e ^ lam = n / powerSum g lam e ∧
      ∀ s > 0, s ≠ scale g lam e → logLik g lam e s < profile g lam e := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hS := powerSum_pos hn hg he lam
  have ht : 0 < (n : ℝ) / powerSum g lam e := div_pos hn' hS
  have hs₀ : 0 < scale g lam e := rpow_pos_of_pos ht _
  have hp : scale g lam e ^ lam = n / powerSum g lam e := by
    rw [scale, ← rpow_mul ht.le, inv_mul_cancel₀ hl.ne', rpow_one]
  refine ⟨hs₀, hp, fun s hs hne ↦ ?_⟩
  have hsp := rpow_pos_of_pos hs lam
  have hne' : s ^ lam / (n / powerSum g lam e) ≠ 1 := by
    intro h
    apply hne
    apply (rpow_left_inj hs.le hs₀.le hl.ne').mp
    rw [hp]
    simpa using (div_eq_iff ht.ne').mp h
  have h := mul_lt_mul_of_pos_left (log_lt_sub_one_of_pos (div_pos hsp ht) hne') hn'
  rw [log_div hsp.ne' ht.ne'] at h
  have hmul : (n : ℝ) * (s ^ lam / (n / powerSum g lam e)) = s ^ lam * powerSum g lam e := by
    field_simp
  have hopt : (n : ℝ) / powerSum g lam e * powerSum g lam e = n :=
    div_mul_cancel₀ _ hS.ne'
  unfold profile
  rw [logLik_eq hg he hs, logLik_eq hg he hs₀, hp, hopt]
  linarith only [h, hmul]

private theorem profile_eq (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam) (he : 0 < e) :
    profile g lam e = n * log lam + n * log (n / powerSum g lam e) -
      (lam + 1) * (∑ i, log (g i + e)) - n := by
  obtain ⟨hs, hp, _⟩ := heldShape hn hg hl he
  unfold profile
  rw [logLik_eq hg he hs, hp, div_mul_cancel₀ _ (powerSum_pos hn hg he lam).ne']

private theorem hasDerivAt_powerSum (hg : ∀ i, 0 ≤ g i) (he : 0 < e) (p : ℝ) :
    HasDerivAt (powerSum g p) (-p * powerSum g (p + 1) e) e := by
  unfold powerSum
  rw [Finset.mul_sum]
  refine HasDerivAt.fun_sum (A := fun i t ↦ (g i + t) ^ (-p)) (u := Finset.univ) ?_
  intro i _
  have h := (hasDerivAt_rpow_const (p := -p) (Or.inl (distance_pos hg he i).ne')).comp e
    ((hasDerivAt_id e).const_add (g i))
  refine h.congr_deriv ?_
  change -p * (g i + e) ^ (-p - 1) * 1 = -p * (g i + e) ^ (-(p + 1))
  rw [mul_one, show -p - 1 = -(p + 1) by ring]

/-- The profile derivative is the gap score times the positive sum of reciprocal distances. -/
theorem hasDerivAt_profile (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam) (he : 0 < e) :
    HasDerivAt (profile g lam)
      (powerSum g 1 e * (n * lam * ratio g lam e - (lam + 1))) e := by
  have hS := powerSum_pos hn hg he lam
  have h1 := powerSum_pos hn hg he 1
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hlog := ((hasDerivAt_const e (n : ℝ)).div (hasDerivAt_powerSum hg he lam) hS.ne').log
    (div_pos hn' hS).ne'
  have hsum : HasDerivAt (fun t ↦ ∑ i, log (g i + t)) (powerSum g 1 e) e := by
    unfold powerSum
    refine HasDerivAt.fun_sum (A := fun i t ↦ log (g i + t)) (u := Finset.univ) ?_
    intro i _
    simpa [rpow_neg_one] using (((hasDerivAt_id e).const_add (g i)).log
      (distance_pos hg he i).ne')
  have h := (((hlog.const_mul (n : ℝ)).const_add (n * log lam)).sub
    (hsum.const_mul (lam + 1))).sub_const (n : ℝ)
  have heq : profile g lam =ᶠ[𝓝 e] fun t ↦
      n * log lam + n * log (n / powerSum g lam t) -
        (lam + 1) * (∑ i, log (g i + t)) - n := by
    filter_upwards [Ioi_mem_nhds he] with t ht
    exact profile_eq hn hg hl ht
  refine (h.congr_of_eventuallyEq heq).congr_deriv ?_
  dsimp
  unfold ratio
  field_simp [hS.ne', h1.ne', hn'.ne']
  ring

private theorem weightedMean_lt {q : Fin n → ℝ} (hq : ∀ i, 0 < q i)
    (hne : ∃ i j, q i ≠ q j) {a b : ℝ} (hab : a < b) :
    (∑ i, q i ^ a * q i) / (∑ i, q i ^ a) <
      (∑ i, q i ^ b * q i) / (∑ i, q i ^ b) := by
  let w := fun i ↦ q i ^ a
  let v := fun i ↦ q i ^ b
  have hw (i : Fin n) : 0 < w i := rpow_pos_of_pos (hq i) _
  have hv (i : Fin n) : 0 < v i := rpow_pos_of_pos (hq i) _
  obtain ⟨i, j, hij⟩ := hne
  have hnon : (Finset.univ : Finset (Fin n)).Nonempty := ⟨i, Finset.mem_univ _⟩
  change (∑ i, w i * q i) / (∑ i, w i) < (∑ i, v i * q i) / (∑ i, v i)
  rw [div_lt_div_iff₀ (Finset.sum_pos (fun i _ ↦ hw i) hnon)
    (Finset.sum_pos (fun i _ ↦ hv i) hnon)]
  -- Symmetrizing the cross-multiplied difference gives a sum of nonnegative pairs.
  let d := fun i j ↦ (q j - q i) * (w i * v j - w j * v i)
  have hd (i j) : d i j = (w i * w j) * ((q j - q i) *
      (q j ^ (b - a) - q i ^ (b - a))) := by
    have hvw (k) : v k = w k * q k ^ (b - a) := by
      dsimp [v, w]
      rw [← rpow_add (hq k)]
      congr 1
      ring
    dsimp [d]
    rw [hvw j, hvw i]
    ring
  have hdpos (i j) (h : q i ≠ q j) : 0 < d i j := by
    rw [hd]
    refine mul_pos (mul_pos (hw i) (hw j)) ?_
    rcases lt_or_gt_of_ne h with h | h
    · exact mul_pos (sub_pos.mpr h) (sub_pos.mpr (rpow_lt_rpow (hq i).le h (sub_pos.mpr hab)))
    · exact mul_pos_of_neg_of_neg (sub_neg.mpr h)
        (sub_neg.mpr (rpow_lt_rpow (hq j).le h (sub_pos.mpr hab)))
  have hdnon (i j) : 0 ≤ d i j := by
    by_cases h : q i = q j
    · simp [d, h]
    · exact (hdpos i j h).le
  have hsum : 0 < ∑ i, ∑ j, d i j := by
    have hrow : 0 < ∑ j, d i j :=
      Finset.sum_pos' (fun j _ ↦ hdnon i j) ⟨j, Finset.mem_univ _, hdpos i j hij⟩
    exact Finset.sum_pos' (fun i _ ↦ Finset.sum_nonneg fun j _ ↦ hdnon i j)
      ⟨i, Finset.mem_univ _, hrow⟩
  have hswap : (∑ i, ∑ j, w j * v i * (q i - q j)) =
      ∑ i, ∑ j, w i * v j * (q j - q i) := Finset.sum_comm
  have hexpand : (∑ i, ∑ j, d i j) =
      2 * ((∑ i, v i * q i) * (∑ i, w i) - (∑ i, w i * q i) * (∑ i, v i)) := by
    simp only [show ∀ i j, d i j = w i * v j * (q j - q i) + w j * v i * (q i - q j)
      from fun i j ↦ by dsimp [d]; ring, Finset.sum_add_distrib, hswap]
    have ht (i) : (∑ j, w i * v j * (q j - q i)) =
        w i * (∑ j, v j * q j) - w i * q i * (∑ j, v j) := by
      simp only [Finset.mul_sum, ← Finset.sum_sub_distrib]
      exact Finset.sum_congr rfl fun j _ ↦ by ring
    simp only [ht, Finset.sum_sub_distrib, ← Finset.sum_mul]
    ring
  rw [hexpand] at hsum
  linarith

private theorem moment_lt (hg : ∀ i, 0 ≤ g i)
    (hne : ∃ i j, g i ≠ g j) (he : 0 < e) {a b : ℝ} (hab : a < b) :
    powerSum g (a + 1) e / powerSum g a e <
      powerSum g (b + 1) e / powerSum g b e := by
  have hp (i : Fin n) := distance_pos hg he i
  let q := fun i ↦ (g i + e)⁻¹
  have hq (i) : 0 < q i := inv_pos.mpr (hp i)
  have hne' : ∃ i j, q i ≠ q j := by
    obtain ⟨i, j, h⟩ := hne
    exact ⟨i, j, fun heq ↦ h (add_right_cancel (inv_injective heq))⟩
  have hs (p : ℝ) : powerSum g p e = ∑ i, q i ^ p := by
    exact Finset.sum_congr rfl fun i _ ↦ by dsimp [q]; rw [inv_rpow (hp i).le, rpow_neg (hp i).le]
  simp only [hs, rpow_add (hq _), rpow_one]
  exact weightedMean_lt hq hne' hab

private theorem hasDerivAt_logRatio (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (he : 0 < e) :
    HasDerivAt (fun t ↦ log (ratio g lam t))
      (-(lam + 1) * (powerSum g (lam + 1 + 1) e / powerSum g (lam + 1) e) +
        lam * (powerSum g (lam + 1) e / powerSum g lam e) +
        powerSum g (1 + 1) e / powerSum g 1 e) e := by
  have h (p : ℝ) := (hasDerivAt_powerSum hg he p).log (powerSum_pos hn hg he p).ne'
  have hder := ((h (lam + 1)).sub (h lam)).sub (h 1)
  have heq : (fun t ↦ log (ratio g lam t)) =ᶠ[𝓝 e] fun t ↦
      log (powerSum g (lam + 1) t) - log (powerSum g lam t) - log (powerSum g 1 t) := by
    filter_upwards [Ioi_mem_nhds he] with t ht
    have hS (p : ℝ) := (powerSum_pos hn hg ht p).ne'
    rw [ratio, log_div (hS _) (mul_ne_zero (hS _) (hS _)), log_mul (hS _) (hS _)]
    ring
  refine (hder.congr_of_eventuallyEq heq).congr_deriv ?_
  simp only [mul_div_assoc, neg_mul, one_mul, neg_div]
  ring

private theorem ratio_pos (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (he : 0 < e) :
    0 < ratio g lam e :=
  div_pos (powerSum_pos hn hg he _) (mul_pos (powerSum_pos hn hg he _) (powerSum_pos hn hg he _))

/-- With unequal sample gaps the ratio strictly decreases on the positive gap domain. -/
theorem ratio_strictAntiOn (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam)
    (hne : ∃ i j, g i ≠ g j) : StrictAntiOn (ratio g lam) (Ioi 0) := by
  have hlog : StrictAntiOn (fun t ↦ log (ratio g lam t)) (Ioi 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    · exact fun t ht ↦ (hasDerivAt_logRatio hn hg ht (lam := lam)).continuousAt.continuousWithinAt
    · intro t ht
      rw [interior_Ioi] at ht
      rw [(hasDerivAt_logRatio hn hg ht (lam := lam)).deriv]
      have ha := moment_lt hg hne ht (show lam < lam + 1 by linarith)
      have hb := moment_lt hg hne ht (show (1 : ℝ) < lam + 1 by linarith)
      nlinarith only [mul_lt_mul_of_pos_left ha hl, hb]
  intro x hx y hy hxy
  exact (log_lt_log_iff (ratio_pos hn hg hy) (ratio_pos hn hg hx)).mp (hlog hx hy hxy)

private def scaledSum (g : Fin n → ℝ) (p e : ℝ) : ℝ :=
  ∑ i, (e / (g i + e)) ^ p

private theorem scaledSum_eq (hg : ∀ i, 0 ≤ g i) (he : 0 < e) (p : ℝ) :
    scaledSum g p e = e ^ p * powerSum g p e := by
  unfold scaledSum powerSum
  rw [Finset.mul_sum]
  exact Finset.sum_congr rfl fun i _ ↦ by
    rw [div_rpow he.le (distance_pos hg he i).le, rpow_neg (distance_pos hg he i).le, div_eq_mul_inv]

private theorem ratio_eq (hg : ∀ i, 0 ≤ g i) (he : 0 < e) :
    ratio g lam e = scaledSum g (lam + 1) e / (scaledSum g lam e * scaledSum g 1 e) := by
  simp only [scaledSum_eq hg he, rpow_add he, ratio]
  have h₁ := (rpow_pos_of_pos he lam).ne'
  have h₂ := (rpow_pos_of_pos he 1).ne'
  rw [show e ^ lam * powerSum g lam e * (e ^ (1 : ℝ) * powerSum g 1 e) =
    (e ^ lam * e ^ (1 : ℝ)) * (powerSum g lam e * powerSum g 1 e) by ring,
    mul_div_mul_left _ _ (mul_ne_zero h₁ h₂)]

private theorem minimumCount_pos (hz : ∃ i, g i = 0) : 0 < (minimumCount g : ℝ) := by
  obtain ⟨i, hi⟩ := hz
  exact_mod_cast (Finset.card_pos.mpr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩ :
    0 < minimumCount g)

private theorem indicator_sum :
    (∑ i, if g i = 0 then (1 : ℝ) else 0) = minimumCount g := by
  rw [← Finset.sum_filter]
  simp [minimumCount]

private theorem scaledSum_zero (g : Fin n → ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (scaledSum g p) (𝓝[>] 0) (𝓝 (minimumCount g : ℝ)) := by
  rw [← indicator_sum]
  refine tendsto_finsetSum Finset.univ fun i _ ↦ ?_
  by_cases h : g i = 0
  · simp only [h, ite_true]
    refine tendsto_const_nhds.congr' ?_
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    simp [ht.ne']
  · simp only [ite_eq_right h]
    have hid : Tendsto (fun t : ℝ ↦ t) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    have hdiv : Tendsto (fun t ↦ t / (g i + t)) (𝓝[>] 0) (𝓝 0) := by
      have hden := hid.const_add (g i)
      rw [add_zero] at hden
      have ht := hid.div hden h
      rw [zero_div] at ht
      exact ht
    simpa [zero_rpow hp.ne', Function.comp_def] using (continuousAt_rpow_const 0 p (.inr hp.le)).tendsto.comp hdiv

private theorem scaledSum_top (g : Fin n → ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (scaledSum g p) atTop (𝓝 (n : ℝ)) := by
  have hlim (i : Fin n) : Tendsto (fun t ↦ t / (g i + t)) atTop (𝓝 1) := by
    have ht := (tendsto_inv_atTop_zero.const_mul (g i)).const_add 1
    simp only [mul_zero, add_zero] at ht
    refine (by simpa using ht.inv₀ one_ne_zero : Tendsto
      (fun t : ℝ ↦ (1 + g i * t⁻¹)⁻¹) atTop (𝓝 1)).congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have heq : 1 + g i * t⁻¹ = (g i + t) / t := by
      rw [div_eq_mul_inv, add_mul, mul_inv_cancel₀ ht.ne', add_comm]
    rw [heq, inv_div]
  unfold scaledSum
  simpa using tendsto_finsetSum Finset.univ (fun i _ ↦
    (show Tendsto (fun t ↦ (t / (g i + t)) ^ p) atTop (𝓝 1) by
      simpa only [one_rpow, Function.comp_def] using (continuousAt_rpow_const 1 p (.inr hp.le)).tendsto.comp (hlim i)))

/-- Normalization by powers of the gap yields the reciprocal count limits at both ends. -/
theorem ratio_limits (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hl : 0 < lam) :
    Tendsto (ratio g lam) (𝓝[>] 0) (𝓝 (1 / (minimumCount g : ℝ))) ∧
      Tendsto (ratio g lam) atTop (𝓝 (1 / (n : ℝ))) := by
  have hk := (minimumCount_pos hz).ne'
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hpl : 0 < lam + 1 := by positivity
  have hcancel (a : ℝ) (ha : a ≠ 0) : a / (a * a) = 1 / a := by field_simp
  constructor
  · have h := (scaledSum_zero g hpl).div
      ((scaledSum_zero g hl).mul (scaledSum_zero g zero_lt_one)) (mul_ne_zero hk hk)
    rw [hcancel _ hk] at h
    exact h.congr' (by filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t); exact (ratio_eq hg ht).symm)
  · have h := (scaledSum_top g hpl).div
      ((scaledSum_top g hl).mul (scaledSum_top g zero_lt_one)) (mul_ne_zero hn' hn')
    rw [hcancel _ hn'] at h
    exact h.congr' (by filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht; exact (ratio_eq hg ht).symm)

private theorem nonconstant_of_exponent_nonneg (hn : 0 < n) (hz : ∃ i, g i = 0)
    (hE : 0 ≤ collapseExponent g lam) : ∃ i j, g i ≠ g j := by
  by_contra! h
  obtain ⟨i, hi⟩ := hz
  have hall (j : Fin n) : g j = 0 := (h j i).trans hi
  have hk : minimumCount g = n := by simp [minimumCount, hall]
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  simp [collapseExponent, hk, hn'.not_ge] at hE

private theorem ratio_continuousOn (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) :
    ContinuousOn (ratio g lam) (Ioi 0) := by
  intro t ht
  exact (((hasDerivAt_powerSum hg ht _).div
    ((hasDerivAt_powerSum hg ht _).mul (hasDerivAt_powerSum hg ht _))
    (mul_ne_zero (powerSum_pos hn hg ht lam).ne' (powerSum_pos hn hg ht 1).ne')).continuousAt).continuousWithinAt

private theorem logLik_le_profile (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam)
    (he : 0 < e) {s : ℝ} (hs : 0 < s) : logLik g lam e s ≤ profile g lam e := by
  by_cases h : s = scale g lam e
  · exact h ▸ le_rfl
  · exact ((heldShape hn hg hl he).2.2 s hs h).le

/-- Above the collapse threshold, exactly one positive gap and scale maximize the likelihood. -/
theorem heldShape_joint_unique (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : 0 < collapseExponent g lam) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun p ↦ logLik g lam p.1 p.2) domain p := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hk := minimumCount_pos hz
  have hnl := mul_pos hn' hl
  let c : ℝ := (lam + 1) / (n * lam)
  have hc₀ : c < 1 / (minimumCount g : ℝ) := by
    dsimp [c]
    rw [div_lt_div_iff₀ hnl hk]
    unfold collapseExponent at hE
    linarith only [hE]
  have hcTop : 1 / (n : ℝ) < c := by
    dsimp [c]
    rw [div_lt_div_iff₀ hn' hnl]
    linarith only [hn']
  obtain ⟨hzero, htop⟩ := ratio_limits hn hg hz hl
  have hev : ∀ᶠ t in 𝓝[>] 0, 0 < t ∧ c < ratio g lam t := by
    filter_upwards [self_mem_nhdsWithin, hzero.eventually (Ioi_mem_nhds hc₀)] with t ht hr
    exact ⟨ht, hr⟩
  obtain ⟨a, ha, hsa⟩ := hev.exists
  obtain ⟨b, hab, hsb⟩ := ((eventually_gt_atTop a).and (htop.eventually (Iio_mem_nhds hcTop))).exists
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun z hz ↦ lt_of_lt_of_le ha hz.1
  obtain ⟨t, ht, hroot⟩ := intermediate_value_Icc' hab.le
    ((ratio_continuousOn (lam := lam) hn hg).mono hsub) ⟨hsb.le, hsa.le⟩
  have ht₀ := hsub ht
  have hc : (n : ℝ) * lam * ratio g lam t = lam + 1 := by
    rw [hroot]
    exact mul_div_cancel₀ _ hnl.ne'
  have hanti := ratio_strictAntiOn hn hg hl (nonconstant_of_exponent_nonneg hn hz hE.le)
  have hpcont : ContinuousOn (profile g lam) (Ioi 0) :=
    fun z hz ↦ (hasDerivAt_profile hn hg hl hz).continuousAt.continuousWithinAt
  have hbetter (z : ℝ) (hz₀ : 0 < z) (hne : z ≠ t) : profile g lam z < profile g lam t := by
    rcases lt_or_gt_of_ne hne with hzt | htz
    · have hs : Icc z t ⊆ Ioi (0 : ℝ) := fun y hy ↦ lt_of_lt_of_le hz₀ hy.1
      have hmono : StrictMonoOn (profile g lam) (Icc z t) := by
        apply strictMonoOn_of_deriv_pos (convex_Icc z t) (hpcont.mono hs)
        intro y hy
        rw [interior_Icc] at hy
        have hy₀ : 0 < y := hz₀.trans hy.1
        rw [(hasDerivAt_profile hn hg hl hy₀).deriv, ← hc]
        exact mul_pos (powerSum_pos hn hg hy₀ 1)
          (sub_pos.mpr (mul_lt_mul_of_pos_left (hanti hy₀ ht₀ hy.2) hnl))
      exact hmono ⟨le_rfl, hzt.le⟩ ⟨hzt.le, le_rfl⟩ hzt
    · have hs : Icc t z ⊆ Ioi (0 : ℝ) := fun y hy ↦ lt_of_lt_of_le ht₀ hy.1
      have hdec : StrictAntiOn (profile g lam) (Icc t z) := by
        apply strictAntiOn_of_deriv_neg (convex_Icc t z) (hpcont.mono hs)
        intro y hy
        rw [interior_Icc] at hy
        have hy₀ : 0 < y := ht₀.trans hy.1
        rw [(hasDerivAt_profile hn hg hl hy₀).deriv, ← hc]
        exact mul_neg_of_pos_of_neg (powerSum_pos hn hg hy₀ 1)
          (sub_neg.mpr (mul_lt_mul_of_pos_left (hanti ht₀ hy₀ hy.1) hnl))
      exact hdec ⟨le_rfl, htz.le⟩ ⟨htz.le, le_rfl⟩ htz
  let p := (t, scale g lam t)
  have hp : p ∈ domain := ⟨ht₀, (heldShape hn hg hl ht₀).1⟩
  have hstrict (q : ℝ × ℝ) (hq : q ∈ domain) (hne : q ≠ p) :
      logLik g lam q.1 q.2 < logLik g lam p.1 p.2 := by
    by_cases heq : q.1 = t
    · have hs : q.2 ≠ scale g lam t := fun hs ↦ hne (Prod.ext heq hs)
      simpa [p, heq, profile] using (heldShape hn hg hl ht₀).2.2 q.2 hq.2 hs
    · exact (logLik_le_profile hn hg hl hq.1 hq.2).trans_lt (hbetter q.1 hq.1 heq)
  refine ⟨p, ⟨hp, ?_⟩, ?_⟩
  · intro q hq
    by_cases h : q = p
    · exact h ▸ le_rfl
    · exact (hstrict q hq h).le
  · intro q ⟨hq, hm⟩
    by_contra hh
    exact (not_lt_of_ge (hm hp)) (hstrict q hq hh)

private def regularProfile (g : Fin n → ℝ) (lam e : ℝ) : ℝ :=
  n * log (n * lam / scaledSum g lam e) - n -
    (lam + 1) * ∑ i, if g i = 0 then 0 else log (g i + e)

private theorem profile_decomposition (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hl : 0 < lam)
    (he : 0 < e) : profile g lam e = collapseExponent g lam * log e + regularProfile g lam e := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hs := powerSum_pos hn hg he lam
  have hr := rpow_pos_of_pos he lam
  have hlogs : (∑ i, log (g i + e)) =
      minimumCount g * log e + ∑ i, if g i = 0 then 0 else log (g i + e) := by
    rw [← indicator_sum (g := g), Finset.sum_mul, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl fun i _ ↦ by by_cases hi : g i = 0 <;> simp [hi]
  rw [profile_eq hn hg hl he, regularProfile, scaledSum_eq hg he, hlogs,
    log_div hn'.ne' hs.ne', log_div (mul_ne_zero hn'.ne' hl.ne') (mul_ne_zero hr.ne' hs.ne'),
    log_mul hn'.ne' hl.ne', log_mul hr.ne' hs.ne', log_rpow he, collapseExponent]
  ring

private theorem regularProfile_limit (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hn : 0 < n) :
    Tendsto (regularProfile g lam) (𝓝[>] 0) (𝓝 (collapseValue g lam)) := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have ht := tendsto_const_nhds (x := (n : ℝ) * lam) |>.div (scaledSum_zero g hl)
    (minimumCount_pos hz).ne'
  have hlog := (continuousAt_log (div_pos (mul_pos hn' hl) (minimumCount_pos hz)).ne').tendsto.comp ht
  have hsum : Tendsto (fun t : ℝ ↦ ∑ i, if g i = 0 then 0 else log (g i + t))
      (𝓝[>] 0) (𝓝 (∑ i, log (g i))) := by
    refine tendsto_finsetSum Finset.univ fun i _ ↦ ?_
    by_cases hi : g i = 0
    · simp [hi]
    · have hid : Tendsto (fun t : ℝ ↦ t) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
      have h := hid.const_add (g i)
      rw [add_zero] at h
      simpa only [ite_eq_right hi, Function.comp_def] using (continuousAt_log hi).tendsto.comp h
  exact ((hlog.const_mul (n : ℝ)).sub_const (n : ℝ)).sub (hsum.const_mul (lam + 1))

/-- Below threshold, collapsing the gap along its optimal scale makes log-likelihood unbounded. -/
theorem heldShape_unbounded (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : collapseExponent g lam < 0) :
    Tendsto (profile g lam) (𝓝[>] 0) atTop ∧
      ∀ B : ℝ, ∃ p ∈ (domain : Set (ℝ × ℝ)), B < logLik g lam p.1 p.2 := by
  have ht : Tendsto (profile g lam) (𝓝[>] 0) atTop := by
    refine ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hE).atTop_add
      (regularProfile_limit hz hl hn)).congr' ?_
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    exact (profile_decomposition hn hg hl ht).symm
  refine ⟨ht, fun B ↦ ?_⟩
  have hev : ∀ᶠ t in 𝓝[>] 0, 0 < t ∧ B < profile g lam t := by
    filter_upwards [self_mem_nhdsWithin, ht.eventually (eventually_gt_atTop B)] with t ht₀ hB
    exact ⟨ht₀, hB⟩
  obtain ⟨t, ht₀, hB⟩ := hev.exists
  exact ⟨(t, scale g lam t), ⟨ht₀, (heldShape hn hg hl ht₀).1⟩, hB⟩

/-- At equality the decreasing profile approaches the strict, least upper bound. -/
theorem heldShape_collapse_eq (hn : 0 < n) (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hl : 0 < lam) (hE : collapseExponent g lam = 0) :
    StrictAntiOn (profile g lam) (Ioi 0) ∧
      Tendsto (profile g lam) (𝓝[>] 0) (𝓝 (collapseValue g lam)) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ logLik g lam p.1 p.2) '' domain) (collapseValue g lam) ∧
      ∀ p ∈ (domain : Set (ℝ × ℝ)), logLik g lam p.1 p.2 < collapseValue g lam := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hnl := mul_pos hn' hl
  have hk := minimumCount_pos hz
  have hanti := ratio_strictAntiOn hn hg hl
    (nonconstant_of_exponent_nonneg hn hz hE.ge)
  have hr := (ratio_limits hn hg hz hl).1
  have hratio (t : ℝ) (ht : 0 < t) : ratio g lam t < 1 / (minimumCount g : ℝ) := by
    have hm : 0 < t / 2 := by positivity
    apply (hanti hm ht (by linarith)).trans_le
    apply ge_of_tendsto hr
    filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hm)]
      with u (hu : 0 < u) (hu' : u < t / 2)
    exact (hanti hu hm hu').le
  have hthreshold : (n : ℝ) * lam * (1 / (minimumCount g : ℝ)) = lam + 1 := by
    unfold collapseExponent at hE
    calc
      _ = (n * lam) / (minimumCount g : ℝ) := by ring
      _ = _ := (div_eq_iff hk.ne').mpr (by linarith only [hE])
  have hpanti : StrictAntiOn (profile g lam) (Ioi 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    · exact fun t ht ↦ (hasDerivAt_profile hn hg hl ht).continuousAt.continuousWithinAt
    · intro t ht
      rw [interior_Ioi] at ht
      rw [(hasDerivAt_profile hn hg hl ht).deriv, ← hthreshold]
      exact mul_neg_of_pos_of_neg (powerSum_pos hn hg ht 1)
        (sub_neg.mpr (mul_lt_mul_of_pos_left (hratio t ht) hnl))
  have hlim : Tendsto (profile g lam) (𝓝[>] 0) (𝓝 (collapseValue g lam)) := by
    apply (regularProfile_limit hz hl hn).congr'
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    rw [profile_decomposition hn hg hl ht, hE, zero_mul, zero_add]
  have hlt (p : ℝ × ℝ) (hp : p ∈ domain) : logLik g lam p.1 p.2 < collapseValue g lam := by
    have h₀ : 0 < p.1 := hp.1
    have hm : 0 < p.1 / 2 := by positivity
    apply (logLik_le_profile hn hg hl hp.1 hp.2).trans_lt
    apply (hpanti hm h₀ (by linarith)).trans_le
    apply ge_of_tendsto hlim
    filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hm)]
      with u (hu : 0 < u) (hu' : u < p.1 / 2)
    exact (hpanti hu hm hu').le
  refine ⟨hpanti, hlim, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  · intro B hB
    apply le_of_tendsto hlim
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    exact hB ⟨(t, scale g lam t), ⟨ht, (heldShape hn hg hl ht).1⟩, rfl⟩

end
end UnivariateML.Frechet
