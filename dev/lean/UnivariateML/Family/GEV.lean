module

public import UnivariateML.Family.Frechet
public import UnivariateML.Family.LocScaleInstances

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# GEV with shape held

The parameters are (location, scale). The domain includes contact at shape -1
and uses strict support at every other nonzero shape. The real-valued objective
is used only on this domain and represents the total log likelihood.
Positive shapes use Fréchet coordinates. Negative shapes use inverse
reflected-Weibull scale and the standardized gap at the maximum.

## Deviations

These are exact-real results. Nonconstant samples of size two are included.
For shapes below -1 the dossier records an infinite endpoint density.
The unboundedness theorem uses a path within strict support.
The negative interior and Gumbel proofs use strict concavity in inverse-scale
coordinates. At -1 the optimum has zero gap and no interior gap root.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.GEV

noncomputable section

variable {n : ℕ} {x : Fin n → ℝ} {m M xi : ℝ}

def supportArg (xi y : ℝ) (p : ℝ × ℝ) : ℝ :=
  1 + xi * ((y - p.1) / p.2)

def domain (x : Fin n → ℝ) (xi : ℝ) : Set (ℝ × ℝ) :=
  {p | 0 < p.2 ∧ ∀ i, if xi = -1 then 0 ≤ supportArg xi (x i) p
    else 0 < supportArg xi (x i) p}

def logLik (x : Fin n → ℝ) (xi : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, if xi = 0 then
    -log p.2 - ((x i - p.1) / p.2) - exp (-((x i - p.1) / p.2))
  else
    -log p.2 + (-1 / xi - 1) * log (supportArg xi (x i) p) -
      supportArg xi (x i) p ^ (-1 / xi)

def lowerGaps (x : Fin n → ℝ) (m : ℝ) : Fin n → ℝ := fun i ↦ x i - m

private def upperGaps (x : Fin n → ℝ) (M : ℝ) : Fin n → ℝ := fun i ↦ M - x i

def IsMLE (x : Fin n → ℝ) (xi : ℝ) (p : ℝ × ℝ) : Prop :=
  p ∈ domain x xi ∧ IsMaxOn (logLik x xi) (domain x xi) p

private theorem transport_max {D E : Set (ℝ × ℝ)} {f g : ℝ × ℝ → ℝ}
    (e : D ≃ E) (he : ∀ p : D, g (e p) = f p) (p : D) :
    IsMaxOn f D p ↔ IsMaxOn g E (e p) := by
  constructor
  · intro hm y hy
    obtain ⟨q, hq⟩ := e.surjective ⟨y, hy⟩
    have hval : (e q).val = y := congrArg Subtype.val hq
    change g y ≤ g (e p)
    rw [← hval, he, he]
    exact hm q.property
  · intro hm y hy
    have h := hm (e ⟨y, hy⟩).property
    change g (e ⟨y, hy⟩) ≤ g (e p) at h
    change f y ≤ f p
    simpa only [he] using h

private theorem transport_unique {D E : Set (ℝ × ℝ)} {f g : ℝ × ℝ → ℝ}
    (e : D ≃ E) (he : ∀ p : D, g (e p) = f p)
    (h : ∃! p, p ∈ D ∧ IsMaxOn f D p) : ∃! p, p ∈ E ∧ IsMaxOn g E p := by
  obtain ⟨p, ⟨hp, hm⟩, hu⟩ := h
  refine ⟨e ⟨p, hp⟩, ⟨(e ⟨p, hp⟩).property, (transport_max e he ⟨p, hp⟩).mp hm⟩, ?_⟩
  intro y hy
  obtain ⟨q, hq⟩ := e.surjective ⟨y, hy.1⟩
  have hqmax : IsMaxOn g E (e q) := by rw [hq]; exact hy.2
  have hqp : q = (⟨p, hp⟩ : D) := Subtype.ext (hu q ⟨q.property, (transport_max e he q).mpr hqmax⟩)
  exact congrArg Subtype.val (hq.symm.trans (congrArg e hqp))

private theorem bddAbove_of_max {D : Set (ℝ × ℝ)} {f : ℝ × ℝ → ℝ} {p : ℝ × ℝ}
    (h : IsMaxOn f D p) : BddAbove (f '' D) :=
  ⟨f p, by rintro _ ⟨q, hq, rfl⟩; exact h hq⟩

private def positiveEquiv (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) : (Frechet.domain : Set (ℝ × ℝ)) ≃ domain x xi where
  toFun p := ⟨(m - p.val.1 + p.val.2, xi * p.val.2), by
    have he : 0 < p.val.1 := p.property.1
    have hs : 0 < p.val.2 := p.property.2
    refine ⟨mul_pos hxi hs, fun i ↦ ?_⟩
    simp only [show xi ≠ -1 by linarith, ite_false]
    have ht : supportArg xi (x i) (m - p.val.1 + p.val.2, xi * p.val.2) =
        (x i - m + p.val.1) / p.val.2 := by
      unfold supportArg
      dsimp
      field_simp
      ring
    rw [ht]
    exact div_pos (by linarith [hm i]) hs⟩
  invFun p := ⟨(m - p.val.1 + p.val.2 / xi, p.val.2 / xi), by
    have hs : 0 < p.val.2 := p.property.1
    obtain ⟨i, hi⟩ := hz
    have ht := p.property.2 i
    simp only [show xi ≠ -1 by linarith, ite_false, hi, supportArg] at ht
    have heq : p.val.2 / xi * (1 + xi * ((m - p.val.1) / p.val.2)) =
        m - p.val.1 + p.val.2 / xi := by
      field_simp
      ring
    exact ⟨heq ▸ mul_pos (div_pos hs hxi) ht, div_pos hs hxi⟩⟩
  left_inv p := by
    apply Subtype.ext
    apply Prod.ext
    · dsimp
      field_simp
      ring
    · dsimp
      field_simp
  right_inv p := by
    apply Subtype.ext
    apply Prod.ext
    · dsimp
      field_simp
      ring
    · dsimp
      field_simp

private theorem positive_value (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (p : (Frechet.domain : Set (ℝ × ℝ))) :
    logLik x xi (positiveEquiv hxi hm hz p) =
      Frechet.logLik (lowerGaps x m) xi⁻¹ p.val.1 p.val.2 := by
  have hs : 0 < p.val.2 := p.property.2
  unfold logLik Frechet.logLik
  apply Finset.sum_congr rfl
  intro i _
  change (if xi = 0 then _ else -log (xi * p.val.2) +
      (-1 / xi - 1) * log (supportArg xi (x i) (m - p.val.1 + p.val.2, xi * p.val.2)) -
        supportArg xi (x i) (m - p.val.1 + p.val.2, xi * p.val.2) ^ (-1 / xi)) = _
  have ht : supportArg xi (x i) (m - p.val.1 + p.val.2, xi * p.val.2) =
      (lowerGaps x m i + p.val.1) / p.val.2 := by
    unfold supportArg lowerGaps
    dsimp
    field_simp
    ring
  rw [ite_eq_right hxi.ne', ht, log_mul hxi.ne' hs.ne', Frechet.logDensity, log_inv]
  rw [show (-1 : ℝ) / xi = -xi⁻¹ by ring]
  ring

/-- Above the Fréchet collapse threshold there is exactly one GEV fit. -/
theorem positive_unique (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : 0 < Frechet.collapseExponent (lowerGaps x m) xi⁻¹) :
    ∃! p, IsMLE x xi p :=
  transport_unique (positiveEquiv hxi hm hz) (positive_value hxi hm hz)
    (Frechet.heldShape_joint_unique hn (fun i ↦ sub_nonneg.mpr (hm i))
      (by simpa [lowerGaps, sub_eq_zero] using hz) (inv_pos.mpr hxi) hE)

/-- Below the Fréchet collapse threshold the GEV objective is unbounded. -/
theorem positive_unbounded (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : Frechet.collapseExponent (lowerGaps x m) xi⁻¹ < 0) :
    ¬ BddAbove (logLik x xi '' domain x xi) := by
  rintro ⟨B, hB⟩
  have hzero : ∃ i, lowerGaps x m i = 0 := by simpa [lowerGaps, sub_eq_zero] using hz
  obtain ⟨p, hp, hlt⟩ := (Frechet.heldShape_unbounded hn
    (fun i ↦ sub_nonneg.mpr (hm i)) hzero (inv_pos.mpr hxi) hE).2 B
  let q := positiveEquiv hxi hm hz ⟨p, hp⟩
  exact (not_lt_of_ge (hB (mem_image_of_mem _ q.property)))
    (by rw [positive_value hxi hm hz]; exact hlt)

private theorem positive_collapse_value (hz : ∃ i, x i = m)
    (hE : Frechet.collapseExponent (lowerGaps x m) xi⁻¹ = 0) :
    Frechet.collapseValue (lowerGaps x m) xi⁻¹ =
      n * (log (1 + xi⁻¹) - 1) - (1 + xi⁻¹) * ∑ i, log (x i - m) := by
  obtain ⟨i, hi⟩ := hz
  have hk : 0 < Frechet.minimumCount (lowerGaps x m) :=
    Finset.card_pos.mpr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, by simp [lowerGaps, hi]⟩⟩
  have hk' : (Frechet.minimumCount (lowerGaps x m) : ℝ) ≠ 0 := by exact_mod_cast hk.ne'
  have he : n * xi⁻¹ / (Frechet.minimumCount (lowerGaps x m) : ℝ) = 1 + xi⁻¹ := by
    apply (div_eq_iff hk').mpr
    unfold Frechet.collapseExponent at hE
    linarith only [hE]
  unfold Frechet.collapseValue lowerGaps at *
  rw [he]
  ring

/-- The critical positive-shape collapse gives a finite strict supremum. -/
theorem positive_collapse_eq (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : Frechet.collapseExponent (lowerGaps x m) xi⁻¹ = 0) :
    let A := n * (log (1 + xi⁻¹) - 1) - (1 + xi⁻¹) * ∑ i, log (x i - m)
    IsLUB (logLik x xi '' domain x xi) A ∧ ∀ p ∈ domain x xi, logLik x xi p < A := by
  dsimp only
  rw [← positive_collapse_value hz hE]
  let e := positiveEquiv hxi hm hz
  have himage : logLik x xi '' domain x xi =
      (fun p : ℝ × ℝ ↦ Frechet.logLik (lowerGaps x m) xi⁻¹ p.1 p.2) '' Frechet.domain := by
    ext L
    constructor
    · rintro ⟨q, hq, rfl⟩
      obtain ⟨p, hp⟩ := e.surjective ⟨q, hq⟩
      exact ⟨p, p.property, (positive_value hxi hm hz p).symm.trans
        (congrArg (fun r : domain x xi ↦ logLik x xi r) hp)⟩
    · rintro ⟨p, hp, rfl⟩
      exact ⟨e ⟨p, hp⟩, (e ⟨p, hp⟩).property, positive_value hxi hm hz ⟨p, hp⟩⟩
  have hzero : ∃ i, lowerGaps x m i = 0 := by simpa [lowerGaps, sub_eq_zero] using hz
  obtain ⟨_, _, hbound, hstrict⟩ := Frechet.heldShape_collapse_eq hn
    (fun i ↦ sub_nonneg.mpr (hm i)) hzero (inv_pos.mpr hxi) hE
  refine ⟨himage ▸ hbound, fun p hp ↦ ?_⟩
  have hmem := mem_image_of_mem (logLik x xi) hp
  rw [himage] at hmem
  obtain ⟨q, hq, hval⟩ := hmem
  exact hval ▸ hstrict q hq

/-- The native GEV parameters along the positive-shape profiled collapse. -/
def collapsePath (x : Fin n → ℝ) (m xi e : ℝ) : ℝ × ℝ :=
  let s := Frechet.scale (lowerGaps x m) xi⁻¹ e
  (m - e + s, xi * s)

private theorem collapse_scale_tendsto (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) :
    Tendsto (Frechet.scale (lowerGaps x m) xi⁻¹) (𝓝[>] 0) (𝓝 0) := by
  obtain ⟨i, hi⟩ := hz
  let g := lowerGaps x m
  let a := xi⁻¹
  have hlim : Tendsto (fun e : ℝ ↦ (n : ℝ) ^ xi * e) (𝓝[>] 0) (𝓝 0) := by
    simpa only [mul_zero] using
      (show Tendsto (fun e : ℝ ↦ e) (𝓝[>] 0) (𝓝 0) from nhdsWithin_le_nhds).const_mul
        ((n : ℝ) ^ xi)
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hlim ?_ ?_
  · filter_upwards [self_mem_nhdsWithin] with e (he : 0 < e)
    exact (Frechet.heldShape hn (fun j ↦ sub_nonneg.mpr (hm j)) (inv_pos.mpr hxi) he).1.le
  · filter_upwards [self_mem_nhdsWithin] with e (he : 0 < e)
    have hs : e ^ (-a) ≤ Frechet.powerSum g a e := by
      unfold Frechet.powerSum
      have h := Finset.single_le_sum (f := fun j ↦ (g j + e) ^ (-a))
        (fun j _ ↦ rpow_nonneg (by dsimp [g, lowerGaps]; linarith [hm j]) (-a))
        (Finset.mem_univ i)
      simpa only [g, lowerGaps, hi, sub_self, zero_add] using h
    have hden := (rpow_pos_of_pos he (-a)).trans_le hs
    have hdiv : (n : ℝ) / Frechet.powerSum g a e ≤ n / e ^ (-a) :=
      div_le_div_of_nonneg_left (Nat.cast_nonneg _) (rpow_pos_of_pos he _) hs
    have hpow := rpow_le_rpow (div_nonneg (Nat.cast_nonneg _) hden.le) hdiv hxi.le
    rw [div_rpow (Nat.cast_nonneg _) (rpow_nonneg he.le _), ← rpow_mul he.le] at hpow
    have ha : -a * xi = -1 := by dsimp [a]; rw [neg_mul, inv_mul_cancel₀ hxi.ne']
    rw [ha, rpow_neg_one, div_inv_eq_mul] at hpow
    simpa only [Frechet.scale, inv_inv, g, a] using hpow

private theorem collapse_path_value (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) {e : ℝ} (he : 0 < e) :
    collapsePath x m xi e ∈ domain x xi ∧
      logLik x xi (collapsePath x m xi e) = Frechet.profile (lowerGaps x m) xi⁻¹ e := by
  let p : (Frechet.domain : Set (ℝ × ℝ)) := ⟨(e, Frechet.scale (lowerGaps x m) xi⁻¹ e), he,
    (Frechet.heldShape hn (fun i ↦ sub_nonneg.mpr (hm i)) (inv_pos.mpr hxi) he).1⟩
  let q := positiveEquiv hxi hm hz p
  exact ⟨q.property, positive_value hxi hm hz p⟩

/-- At or beyond the positive collapse threshold, the profiled path tends to
zero scale and location equal to the sample minimum. Its values converge to
the finite supremum at equality and diverge for a negative exponent. -/
theorem positive_collapse_path (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) :
    (∀ e > 0, collapsePath x m xi e ∈ domain x xi) ∧
    Tendsto (collapsePath x m xi) (𝓝[>] 0) (𝓝 (m, 0)) ∧
    (Frechet.collapseExponent (lowerGaps x m) xi⁻¹ = 0 →
      Tendsto (fun e ↦ logLik x xi (collapsePath x m xi e)) (𝓝[>] 0)
        (𝓝 (n * (log (1 + xi⁻¹) - 1) - (1 + xi⁻¹) * ∑ i, log (x i - m)))) ∧
    (Frechet.collapseExponent (lowerGaps x m) xi⁻¹ < 0 →
      Tendsto (fun e ↦ logLik x xi (collapsePath x m xi e)) (𝓝[>] 0) atTop) := by
  have hs := collapse_scale_tendsto hn hxi hm hz
  have he : Tendsto (fun e : ℝ ↦ e) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hg (i : Fin n) : 0 ≤ lowerGaps x m i := sub_nonneg.mpr (hm i)
  have hzero : ∃ i, lowerGaps x m i = 0 := by simpa [lowerGaps, sub_eq_zero] using hz
  have heq : Frechet.profile (lowerGaps x m) xi⁻¹ =ᶠ[𝓝[>] 0]
      fun e ↦ logLik x xi (collapsePath x m xi e) := by
    filter_upwards [self_mem_nhdsWithin] with _ h
    exact (collapse_path_value hn hxi hm hz h).2.symm
  refine ⟨fun _ h ↦ (collapse_path_value hn hxi hm hz h).1, ?_, ?_, ?_⟩
  · change Tendsto
      (fun e ↦ (m - e + Frechet.scale (lowerGaps x m) xi⁻¹ e,
        xi * Frechet.scale (lowerGaps x m) xi⁻¹ e)) (𝓝[>] 0) (𝓝 (m, 0))
    simpa only [sub_zero, add_zero, mul_zero] using
      ((tendsto_const_nhds.sub he).add hs).prodMk_nhds (hs.const_mul xi)
  · intro hE
    rw [← positive_collapse_value hz hE]
    exact (Frechet.heldShape_collapse_eq hn hg hzero (inv_pos.mpr hxi) hE).2.1.congr' heq
  · intro hE
    exact (Frechet.heldShape_unbounded hn hg hzero (inv_pos.mpr hxi) hE).1.congr' heq

/-- The Gumbel seam attains a unique fit on nonconstant data. -/
theorem zero_unique (hx : LocationScale.Nonconstant x) : ∃! p, IsMLE x 0 p := by
  have hdom : domain x 0 = LocationScale.domain := by
    ext p
    simp [domain, supportArg, LocationScale.domain]
  have hval : logLik x 0 = LocationScale.logLik LocScaleInstances.gumbelLoss x := by
    funext p
    unfold logLik LocationScale.logLik
    refine Finset.sum_congr rfl fun i _ ↦ ?_
    simp only [ite_true, LocScaleInstances.gumbelLoss]
    ring
  simpa only [IsMLE, hdom, hval] using LocScaleInstances.gumbel_existsUnique_mle hx

private def negativeDomain (closed : Prop) [Decidable closed] : Set (ℝ × ℝ) :=
  Ioi 0 ×ˢ (if closed then Ici 0 else Ioi 0)

private theorem negative_arg (hxi : xi < 0) {p : ℝ × ℝ} (hq : 0 < p.1) (y : ℝ) :
    supportArg xi y (M + (p.2 - 1) / p.1, -xi / p.1) = p.2 + p.1 * (M - y) := by
  unfold supportArg
  dsimp
  field_simp [hxi.ne, hq.ne']
  ring

private def negativeEquiv (hxi : xi < 0) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    negativeDomain (xi = -1) ≃ domain x xi where
  toFun p := ⟨(M + (p.val.2 - 1) / p.val.1, -xi / p.val.1), by
    have hq : 0 < p.val.1 := p.property.1
    refine ⟨div_pos (neg_pos.mpr hxi) hq, fun i ↦ ?_⟩
    rw [negative_arg hxi hq]
    have hb := p.property.2
    change p.val.2 ∈ (if xi = -1 then Ici 0 else Ioi 0) at hb
    split_ifs at hb ⊢ with h
    · exact add_nonneg hb (mul_nonneg hq.le (sub_nonneg.mpr (hM i)))
    · exact add_pos_of_pos_of_nonneg hb (mul_nonneg hq.le (sub_nonneg.mpr (hM i)))⟩
  invFun p := ⟨(-xi / p.val.2, supportArg xi M p), by
    obtain ⟨i, hi⟩ := hz
    refine ⟨div_pos (neg_pos.mpr hxi) p.property.1, ?_⟩
    have hb := p.property.2 i
    rw [hi] at hb
    change supportArg xi M p ∈ (if xi = -1 then Ici 0 else Ioi 0)
    split_ifs at hb ⊢ <;> exact hb⟩
  left_inv p := by
    have hq : 0 < p.val.1 := p.property.1
    apply Subtype.ext
    apply Prod.ext
    · dsimp
      field_simp [hxi.ne, hq.ne']
    · exact (negative_arg hxi hq M).trans (by simp)
  right_inv p := by
    have hs : 0 < p.val.2 := p.property.1
    apply Subtype.ext
    apply Prod.ext
    · dsimp [supportArg]
      field_simp [hxi.ne, hs.ne']
      ring
    · dsimp
      field_simp [hxi.ne, hs.ne']

private def weibullLoss (a t : ℝ) : ℝ := t ^ a - (a - 1) * log t

private def negativeLogLik (g : Fin n → ℝ) (xi : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, (log p.1 - log (-xi) - weibullLoss (-1 / xi) (p.2 + p.1 * g i))

private theorem negative_value (hxi : xi < 0) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M)
    (p : negativeDomain (xi = -1)) :
    logLik x xi (negativeEquiv hxi hM hz p) = negativeLogLik (upperGaps x M) xi p := by
  have hq : 0 < p.val.1 := p.property.1
  unfold logLik negativeLogLik
  refine Finset.sum_congr rfl fun i _ ↦ ?_
  change (if xi = 0 then _ else -log (-xi / p.val.1) +
      (-1 / xi - 1) * log (supportArg xi (x i) (M + (p.val.2 - 1) / p.val.1, -xi / p.val.1)) -
      supportArg xi (x i) (M + (p.val.2 - 1) / p.val.1, -xi / p.val.1) ^ (-1 / xi)) = _
  rw [ite_eq_right hxi.ne, negative_arg hxi hq, log_div (neg_ne_zero.mpr hxi.ne) hq.ne']
  unfold weibullLoss upperGaps
  ring

private theorem negative_shape_gt_one (hxi : xi < 0) (hlo : -1 < xi) : 1 < -1 / xi := by
  rw [lt_div_iff_of_neg hxi]
  linarith

private theorem weibullLoss_strictConvex {a : ℝ} (ha : 1 < a) :
    StrictConvexOn ℝ (Ioi 0) (weibullLoss a) := by
  refine ⟨convex_Ioi 0, fun t ht s hs hne u v hu hv huv ↦ ?_⟩
  have hp := (convexOn_rpow ha.le).2 (le_of_lt ht : t ∈ Ici 0)
    (le_of_lt hs : s ∈ Ici 0) hu.le hv.le huv
  have hl := strictConcaveOn_log_Ioi.2 ht hs hne hu hv huv
  simp only [smul_eq_mul, weibullLoss] at hp hl ⊢
  nlinarith only [hp, mul_lt_mul_of_pos_left hl (sub_pos.mpr ha)]

private theorem negative_strictConcave (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (ha : 1 < -1 / xi) : StrictConcaveOn ℝ (Ioi 0 ×ˢ Ioi 0) (negativeLogLik g xi) := by
  have hconv := weibullLoss_strictConvex ha
  refine ⟨(convex_Ioi 0).prod (convex_Ioi 0), ?_⟩
  intro p hp q hq hpq u v hu hv huv
  simp only [negativeLogLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  refine Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty fun i _ ↦ ?_
  have hpi : p.2 + p.1 * g i ∈ Ioi 0 :=
    add_pos_of_pos_of_nonneg hp.2 (mul_nonneg (le_of_lt hp.1) (hg i))
  have hqi : q.2 + q.1 * g i ∈ Ioi 0 :=
    add_pos_of_pos_of_nonneg hq.2 (mul_nonneg (le_of_lt hq.1) (hg i))
  rw [show (u • p + v • q).2 + (u • p + v • q).1 * g i =
    u • (p.2 + p.1 * g i) + v • (q.2 + q.1 * g i) by simp [smul_eq_mul]; ring]
  have huvp : (u • p + v • q).1 = u • p.1 + v • q.1 := rfl
  rw [huvp]
  by_cases he : p.1 = q.1
  · have hne : p.2 + p.1 * g i ≠ q.2 + q.1 * g i := by
      rw [he]
      exact fun h ↦ hpq (Prod.ext he (add_right_cancel h))
    have hl := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 hu.le hv.le huv
    have hw := hconv.2 hpi hqi hne hu hv huv
    simp only [smul_eq_mul] at hl hw ⊢
    nlinarith only [hl, hw, congrArg (fun t : ℝ ↦ t * log (-xi)) huv]
  · have hl := strictConcaveOn_log_Ioi.2 hp.1 hq.1 he hu hv huv
    have hw := hconv.convexOn.2 hpi hqi hu.le hv.le huv
    simp only [smul_eq_mul] at hl hw ⊢
    nlinarith only [hl, hw, congrArg (fun t : ℝ ↦ t * log (-xi)) huv]

private theorem weibullLoss_linear {a t : ℝ} (ha : 1 ≤ a) (ht : 0 < t) :
    t ≤ weibullLoss a t := by
  have hp := one_add_mul_self_le_rpow_one_add
    (show -1 ≤ t - 1 by linarith) ha
  rw [show 1 + (t - 1) = t by ring] at hp
  unfold weibullLoss
  linarith only [hp, mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos ht) (sub_nonneg.mpr ha)]

private theorem negative_escape (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) (hd : ∃ j, 0 < g j) (ha : 1 < -1 / xi) :
    Tendsto (fun p : (Ioi 0 ×ˢ Ioi 0 : Set (ℝ × ℝ)) ↦ negativeLogLik g xi p)
      (cocompact (Ioi 0 ×ˢ Ioi 0 : Set (ℝ × ℝ))) atBot := by
  obtain ⟨i, hi⟩ := hz
  obtain ⟨j, hj⟩ := hd
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  let a := -1 / xi
  let c := (n : ℝ) * log (-xi)
  let k := g j / (2 * n)
  have hk : 0 < k := div_pos hj (by positivity)
  have hk' : (n : ℝ) * k = g j / 2 := by dsimp [k]; field_simp
  apply tendsto_atBot.mpr
  intro level
  -- An observation with a positive gap bounds the inverse scale and gap above.
  -- An observation at the maximum supplies the logarithmic bound near contact.
  let U := (-n * (1 + log k) - c - level) / (g j / 2)
  let V := n * (U - 1) - c - level
  let qmin := exp ((level + c) / n)
  let bmin := exp ((level + c - n * (U - 1)) / (a - 1))
  let box := Icc qmin U ×ˢ Icc bmin V
  have hbox : box ⊆ (Ioi 0 ×ˢ Ioi 0 : Set (ℝ × ℝ)) := fun _ hp ↦
    ⟨(exp_pos _).trans_le hp.1.1, (exp_pos _).trans_le hp.2.1⟩
  have hcompact : IsCompact (Subtype.val ⁻¹' box : Set (Ioi 0 ×ˢ Ioi 0 : Set (ℝ × ℝ))) :=
    Topology.IsInducing.subtypeVal.isCompact_preimage'
      (isCompact_Icc.prod isCompact_Icc : IsCompact box) (by
        intro p hp
        exact ⟨⟨p, hbox hp⟩, rfl⟩)
  refine Filter.mem_cocompact.mpr ⟨_, hcompact, ?_⟩
  intro p hp
  change negativeLogLik g xi p ≤ level
  by_contra! hlevel
  change p.val ∉ box at hp
  apply hp
  let q := p.val.1
  let b := p.val.2
  have hq : 0 < q := p.property.1
  have hb : 0 < b := p.property.2
  let loss (l : Fin n) := weibullLoss a (b + q * g l)
  have hlin (l : Fin n) : b + q * g l ≤ loss l :=
    weibullLoss_linear ha.le (add_pos_of_pos_of_nonneg hb (mul_nonneg hq.le (hg l)))
  have hnon (l : Fin n) : 0 ≤ loss l := by linarith [hlin l, mul_nonneg hq.le (hg l)]
  have hs (l : Fin n) : loss l ≤ ∑ t, loss t :=
    Finset.single_le_sum (fun t _ ↦ hnon t) (Finset.mem_univ l)
  have hv : negativeLogLik g xi p = n * log q - c - ∑ t, loss t := by
    simp [negativeLogLik, loss, a, q, b, c, Finset.sum_sub_distrib]
  rw [hv] at hlevel
  have hlog : log q ≤ k * q - 1 - log k := by
    have h := log_le_sub_one_of_pos (mul_pos hk hq)
    rw [log_mul hk.ne' hq.ne'] at h
    linarith
  have hU : q ≤ U := by
    apply (le_div_iff₀ (show 0 < g j / 2 by positivity)).mpr
    have he := congrArg (fun t : ℝ ↦ t * q) hk'
    nlinarith only [he, hlevel, hlin j, hs j, hb,
      mul_le_mul_of_nonneg_left hlog hn'.le]
  have hlogU := mul_le_mul_of_nonneg_left ((log_le_sub_one_of_pos hq).trans
    (sub_le_sub_right hU 1)) hn'.le
  have hV : b ≤ V := by dsimp [V]; linarith [hlin j, hs j, mul_nonneg hq.le hj.le]
  have hqmin : qmin ≤ q := by
    dsimp only [qmin]
    rw [← exp_log hq, exp_le_exp, div_le_iff₀ hn']
    linarith [hnon j, hs j]
  have hbmin : bmin ≤ b := by
    dsimp only [bmin]
    rw [← exp_log hb, exp_le_exp, div_le_iff₀ (sub_pos.mpr ha)]
    have he : loss i = b ^ a - (a - 1) * log b := by simp [loss, hi, weibullLoss]
    linarith only [hlevel, hlogU, hs i, he, rpow_nonneg hb.le a]
  exact ⟨⟨hqmin, hU⟩, ⟨hbmin, hV⟩⟩

/-- The negative interior has a unique attained fit on nonconstant data. -/
theorem negative_interior_unique (hx : LocationScale.Nonconstant x)
    (hlo : -1 < xi) (hxi : xi < 0) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    ∃! p, IsMLE x xi p := by
  obtain ⟨i, j, hij⟩ := hx
  have hn := (Nat.zero_le i.val).trans_lt i.isLt
  have hg (l : Fin n) : 0 ≤ upperGaps x M l := sub_nonneg.mpr (hM l)
  have hzero : ∃ l, upperGaps x M l = 0 := by
    simpa [upperGaps, sub_eq_zero, eq_comm] using hz
  have hpos : ∃ l, 0 < upperGaps x M l := by
    rcases lt_or_gt_of_ne hij with h | h
    · exact ⟨i, sub_pos.mpr (h.trans_le (hM j))⟩
    · exact ⟨j, sub_pos.mpr (h.trans_le (hM i))⟩
  have ha := negative_shape_gt_one hxi hlo
  have h := Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    (D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ))
    ⟨(1, 1), one_pos, one_pos⟩ (isOpen_Ioi.prod isOpen_Ioi)
    (negative_strictConcave hn hg ha) (negative_escape hn hg hzero hpos ha)
  apply transport_unique (negativeEquiv hxi hM hz) (negative_value hxi hM hz)
  simpa only [negativeDomain, ite_eq_right (ne_of_gt hlo)] using h

private theorem reverse_value (g : Fin n → ℝ) (p : ℝ × ℝ) :
    negativeLogLik g (-1) p = n * log p.1 - n * p.2 - p.1 * ∑ i, g i := by
  simp [negativeLogLik, weibullLoss, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    Finset.mul_sum]
  ring

/-- At shape -1 the closed upper endpoint is the sample maximum. The location
is the sample mean and the scale is its distance from the maximum. -/
theorem reverse_exponential_unique (hx : LocationScale.Nonconstant x)
    (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    IsMLE x (-1) ((∑ i, x i) / n, M - (∑ i, x i) / n) ∧
      ∀ p, IsMLE x (-1) p → p = ((∑ i, x i) / n, M - (∑ i, x i) / n) := by
  obtain ⟨i, j, hij⟩ := hx
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_le i.val).trans_lt i.isLt
  let g := upperGaps x M
  have hpos : ∃ l, 0 < g l := by
    rcases lt_or_gt_of_ne hij with h | h
    · exact ⟨i, sub_pos.mpr (h.trans_le (hM j))⟩
    · exact ⟨j, sub_pos.mpr (h.trans_le (hM i))⟩
  obtain ⟨l, hl⟩ := hpos
  have hsum : 0 < ∑ i, g i :=
    Finset.sum_pos' (fun i _ ↦ sub_nonneg.mpr (hM i)) ⟨l, Finset.mem_univ _, hl⟩
  let d := (∑ i, g i) / n
  have hd : 0 < d := div_pos hsum hn
  have hnd : (n : ℝ) * d = ∑ i, g i := by dsimp [d]; field_simp
  have hmean : d = M - (∑ i, x i) / n := by
    simp [d, g, upperGaps, Finset.sum_sub_distrib]
    field_simp
  let p₀ : ℝ × ℝ := (d⁻¹, 0)
  have hp₀ : p₀ ∈ negativeDomain ((-1 : ℝ) = -1) := by
    exact ⟨inv_pos.mpr hd, by simp [p₀]⟩
  have hstrict {p : ℝ × ℝ} (hp : p ∈ negativeDomain ((-1 : ℝ) = -1)) (hne : p ≠ p₀) :
      negativeLogLik g (-1) p < negativeLogLik g (-1) p₀ := by
    have hq : 0 < p.1 := hp.1
    have hb := hp.2
    change p.2 ∈ (if (-1 : ℝ) = -1 then Ici 0 else Ioi 0) at hb
    simp only [ite_true, mem_Ici] at hb
    have hopt : d⁻¹ * (∑ i, g i) = n := by rw [← hnd]; field_simp
    rw [reverse_value, reverse_value]
    dsimp [p₀]
    rw [hopt, log_inv]
    have hlog := log_le_sub_one_of_pos (mul_pos hq hd)
    rw [log_mul hq.ne' hd.ne'] at hlog
    have hmul : (n : ℝ) * (p.1 * d) = p.1 * ∑ i, g i := by rw [← hnd]; ring
    by_cases hqd : p.1 * d = 1
    · have hq₀ : p.1 = d⁻¹ := by
        rw [← one_div, eq_div_iff hd.ne']
        exact hqd
      have hb₀ : 0 < p.2 :=
        lt_of_le_of_ne hb (fun he ↦ hne (Prod.ext hq₀ he.symm))
      nlinarith only [hmul, mul_le_mul_of_nonneg_left hlog hn.le, mul_pos hn hb₀]
    · have hlog' := log_lt_sub_one_of_pos (mul_pos hq hd) hqd
      rw [log_mul hq.ne' hd.ne'] at hlog'
      nlinarith only [hmul, mul_lt_mul_of_pos_left hlog' hn, mul_nonneg hn.le hb]
  have hmax : IsMaxOn (negativeLogLik g (-1)) (negativeDomain ((-1 : ℝ) = -1)) p₀ := by
    intro p hp
    by_cases he : p = p₀
    · exact he ▸ le_rfl
    · exact (hstrict hp he).le
  have h : ∃! p, p ∈ negativeDomain ((-1 : ℝ) = -1) ∧
      IsMaxOn (negativeLogLik g (-1)) (negativeDomain ((-1 : ℝ) = -1)) p :=
    ⟨p₀, ⟨hp₀, hmax⟩, fun _ hp ↦ by
      by_contra hne
      exact (not_lt_of_ge (hp.2 hp₀)) (hstrict hp.1 hne)⟩
  let e := negativeEquiv (show (-1 : ℝ) < 0 by norm_num) hM hz
  have he : (e ⟨p₀, hp₀⟩).val = ((∑ i, x i) / n, M - (∑ i, x i) / n) := by
    change (M + (0 - 1) / d⁻¹, -(-1 : ℝ) / d⁻¹) = _
    simp only [div_inv_eq_mul, zero_sub, neg_neg, neg_mul, one_mul, ← sub_eq_add_neg]
    rw [hmean]
    congr 1
    ring
  have hm : IsMLE x (-1) (e ⟨p₀, hp₀⟩) :=
    ⟨(e ⟨p₀, hp₀⟩).property, (transport_max e (negative_value _ hM hz) ⟨p₀, hp₀⟩).mp hmax⟩
  rw [he] at hm
  obtain ⟨p, _, hu⟩ := transport_unique e (negative_value _ hM hz) h
  exact ⟨hm, fun q hq ↦ (hu q hq).trans (hu _ hm).symm⟩

/-- Below shape -1, moving the upper endpoint to the maximum at fixed scale
makes the total log likelihood unbounded from within strict support. -/
theorem negative_unbounded (hlo : xi < -1) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    ¬ BddAbove (logLik x xi '' domain x xi) := by
  have hxi : xi < 0 := by linarith
  let a := -1 / xi
  have ha : 0 < a := div_pos_of_neg_of_neg (by norm_num) hxi
  have ha' : a < 1 := by dsimp [a]; rw [div_lt_iff_of_neg hxi]; linarith
  let g := upperGaps x M
  let f (b : ℝ) (i : Fin n) := -log (-xi) - weibullLoss a (b + g i)
  have hg (i : Fin n) : 0 ≤ g i := sub_nonneg.mpr (hM i)
  obtain ⟨i, hi⟩ := hz
  have hi' : g i = 0 := by simp [g, upperGaps, hi]
  have ht : Tendsto (fun b ↦ f b i) (𝓝[>] 0) atTop := by
    have hp : Tendsto (fun b : ℝ ↦ b ^ a) (𝓝[>] 0) (𝓝 0) := by
      simpa only [zero_rpow ha.ne'] using
        (continuousAt_rpow_const 0 a (.inr ha.le)).tendsto.mono_left nhdsWithin_le_nhds
    have hl := tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg (sub_neg.mpr ha')
    have h := hl.atTop_add (tendsto_const_nhds.sub hp :
      Tendsto (fun b ↦ -log (-xi) - b ^ a) (𝓝[>] 0) (𝓝 (-log (-xi) - 0)))
    convert h using 1
    funext b
    simp only [f, hi', add_zero, weibullLoss]
    ring
  have hmono (b : ℝ) (hb : 0 < b) (hb' : b ≤ 1) (j : Fin n) : f 1 j ≤ f b j := by
    have hbg := add_pos_of_pos_of_nonneg hb (hg j)
    have hle : b + g j ≤ 1 + g j := add_le_add_left hb' _
    have hp := rpow_le_rpow hbg.le hle ha.le
    have hl := log_le_log hbg hle
    dsimp [f, weibullLoss]
    linarith only [hp, mul_le_mul_of_nonpos_left hl (sub_nonpos.mpr ha'.le)]
  rintro ⟨B, hB⟩
  let C := ∑ j ∈ (Finset.univ.erase i), f 1 j
  have hev : ∀ᶠ b : ℝ in 𝓝[>] 0, 0 < b ∧ b < 1 ∧ B - C < f b i := by
    filter_upwards [self_mem_nhdsWithin, mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one),
      ht.eventually (eventually_gt_atTop (B - C))] with b hb hb' h
    exact ⟨hb, hb', h⟩
  obtain ⟨b, hb, hb', hf⟩ := hev.exists
  let p : negativeDomain (xi = -1) := ⟨(1, b), by
    refine ⟨one_pos, ?_⟩
    change b ∈ (if xi = -1 then Ici 0 else Ioi 0)
    simpa only [ite_eq_right hlo.ne, mem_Ioi] using hb⟩
  let q := negativeEquiv hxi hM ⟨i, hi⟩ p
  have h := hB (mem_image_of_mem _ q.property)
  change logLik x xi q ≤ B at h
  rw [negative_value hxi hM ⟨i, hi⟩] at h
  have hsum : negativeLogLik g xi p = ∑ j, f b j := by simp [p, f, negativeLogLik, a]
  rw [hsum, ← Finset.sum_erase_add (f := f b) _ (Finset.mem_univ i)] at h
  have hC : C ≤ ∑ j ∈ (Finset.univ.erase i), f b j :=
    Finset.sum_le_sum fun j _ ↦ hmono b hb hb'.le j
  linarith

/-- At a held positive shape, the sample minimum count determines attainment. -/
theorem positive_exists_iff (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) :
    (∃ p, IsMLE x xi p) ↔
      xi * Frechet.minimumCount (lowerGaps x m) < n - Frechet.minimumCount (lowerGaps x m) := by
  have hthreshold : 0 < Frechet.collapseExponent (lowerGaps x m) xi⁻¹ ↔
      xi * Frechet.minimumCount (lowerGaps x m) < n - Frechet.minimumCount (lowerGaps x m) := by
    rw [Frechet.collapseExponent, sub_pos, inv_mul_eq_div, lt_div_iff₀ hxi, mul_comm]
  rw [← hthreshold]
  constructor
  · rintro ⟨p, hp⟩
    rcases lt_trichotomy (Frechet.collapseExponent (lowerGaps x m) xi⁻¹) 0 with h | h | h
    · exact (positive_unbounded hn hxi hm hz h (bddAbove_of_max hp.2)).elim
    · obtain ⟨hL, hlt⟩ := positive_collapse_eq hn hxi hm hz h
      have hb : logLik x xi p ∈ upperBounds (logLik x xi '' domain x xi) := by
        rintro _ ⟨q, hq, rfl⟩
        exact hp.2 hq
      exact ((not_lt_of_ge (hL.2 hb)) (hlt p hp.1)).elim
    · exact h
  · intro h
    exact (positive_unique hn hxi hm hz h).exists

/-- Constant nonempty samples give an unbounded likelihood at every held shape. -/
theorem constant_unbounded (hn : 0 < n) (hc : ∀ i, x i = m) :
    ¬ BddAbove (logLik x xi '' domain x xi) := by
  rintro ⟨B, hB⟩
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  let s := exp (-(B / n + 2))
  have hp : (m, s) ∈ domain x xi := by
    refine ⟨exp_pos _, fun i ↦ ?_⟩
    simp only [supportArg, hc, sub_self, zero_div, mul_zero, add_zero]
    split_ifs <;> norm_num
  have h := hB (mem_image_of_mem (logLik x xi) hp)
  change logLik x xi (m, s) ≤ B at h
  have hvalue : logLik x xi (m, s) = n * (-log s - 1) := by
    simp only [logLik, supportArg, hc, sub_self, zero_div, neg_zero, exp_zero, sub_zero,
      mul_zero, add_zero, log_one, add_zero, one_rpow, ite_self, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [hvalue] at h
  dsimp [s] at h
  rw [log_exp] at h
  have hmul : (n : ℝ) * (B / n) = B := by field_simp
  nlinarith only [h, hmul, hn']

/-- For nonconstant data, the endpoint at -1 is included, and the sole
additional attainment condition is the minimum multiplicity at positive shape. -/
theorem fixed_shape_exists_iff (hx : LocationScale.Nonconstant x)
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m) :
    (∃ p, IsMLE x xi p) ↔ -1 ≤ xi ∧
      (0 < xi → xi * Frechet.minimumCount (lowerGaps x m) <
        n - Frechet.minimumCount (lowerGaps x m)) := by
  have hsample := hx
  obtain ⟨i, _, _⟩ := hsample
  have hn := (Nat.zero_le i.val).trans_lt i.isLt
  let : Nonempty (Fin n) := ⟨i⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x Finset.univ_nonempty
  have hM (i : Fin n) := hj i (Finset.mem_univ i)
  have hmax : ∃ i, x i = x j := ⟨j, rfl⟩
  constructor
  · rintro ⟨p, hp⟩
    refine ⟨?_, fun hxi ↦ (positive_exists_iff hn hxi hm hz).mp ⟨p, hp⟩⟩
    by_contra! hlo
    exact negative_unbounded hlo hM hmax (bddAbove_of_max hp.2)
  · rintro ⟨hlo, hthreshold⟩
    rcases lt_trichotomy xi 0 with hxi | rfl | hxi
    · rcases eq_or_lt_of_le hlo with rfl | hlo
      · exact ⟨_, (reverse_exponential_unique hx hM hmax).1⟩
      · exact (negative_interior_unique hx hlo hxi hM hmax).exists
    · exact (zero_unique hx).exists
    · exact (positive_exists_iff hn hxi hm hz).mpr (hthreshold hxi)

/-- For any held shape on nonconstant data, two finite maximizers coincide. -/
theorem fixed_shape_unique (hx : LocationScale.Nonconstant x) {p q : ℝ × ℝ}
    (hp : IsMLE x xi p) (hq : IsMLE x xi q) : p = q := by
  have hsample := hx
  obtain ⟨i, _, _⟩ := hsample
  have hn : 0 < n := (Nat.zero_le i.val).trans_lt i.isLt
  let : Nonempty (Fin n) := ⟨i⟩
  obtain ⟨imin, _, hmin⟩ := Finset.exists_min_image Finset.univ x Finset.univ_nonempty
  obtain ⟨imax, _, hmax⟩ := Finset.exists_max_image Finset.univ x Finset.univ_nonempty
  have hm (i : Fin n) := hmin i (Finset.mem_univ i)
  have hM (i : Fin n) := hmax i (Finset.mem_univ i)
  have hminatt : ∃ i, x i = x imin := ⟨imin, rfl⟩
  have hmaxatt : ∃ i, x i = x imax := ⟨imax, rfl⟩
  rcases lt_trichotomy xi 0 with hxi | rfl | hxi
  · rcases lt_trichotomy xi (-1) with hlo | rfl | hlo
    · exact (negative_unbounded hlo hM hmaxatt (bddAbove_of_max hp.2)).elim
    · have hu := (reverse_exponential_unique hx hM hmaxatt).2
      exact (hu p hp).trans (hu q hq).symm
    · exact (negative_interior_unique hx hlo hxi hM hmaxatt).unique hp hq
  · exact (zero_unique hx).unique hp hq
  · have ht := (positive_exists_iff hn hxi hm hminatt).mp ⟨p, hp⟩
    have hE : 0 < Frechet.collapseExponent (lowerGaps x (x imin)) xi⁻¹ := by
      rwa [Frechet.collapseExponent, sub_pos, inv_mul_eq_div, lt_div_iff₀ hxi, mul_comm]
    exact (positive_unique hn hxi hm hminatt hE).unique hp hq

end

end UnivariateML.GEV
