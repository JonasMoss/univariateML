module

public import UnivariateML.Family.Transfers
public import UnivariateML.Generic.LocationScale

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Transformed gamma, its reciprocal, and log-gamma

The objectives are mean log likelihoods. Parameters are positive, with
transformed-gamma scale in the raw observation units and log-gamma in
shape–rate coordinates.

## Deviations

Strict concavity for the held-scale, held-shape fit is in power, as in the
dossiers. It does not in general survive the change to log-power.
With just one shape held and scale free, nonconstancy is necessary for a
finite fit. Holding both shapes gives a unique scale even on constant data.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.TransformedGamma

noncomputable section

variable {n : ℕ}

def loss (k t : ℝ) : ℝ := exp t - k * t

def logLik (x : Fin n → ℝ) (k p a : ℝ) : ℝ :=
  log p - log (Real.Gamma k) - Gamma.avgLog x -
    Gamma.avg (fun i ↦ loss k (p * (log (x i) - log a)))

private theorem nat_pos (hn : 0 < n) : (0 : ℝ) < n := by exact_mod_cast hn

private theorem avg_const (hn : 0 < n) (c : ℝ) :
    Gamma.avg (fun _ : Fin n ↦ c) = c := by
  simp [Gamma.avg, mul_comm _ c, (nat_pos hn).ne']

private theorem avg_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 < Gamma.avg x := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact div_pos (Finset.sum_pos (fun i _ ↦ hx i) Finset.univ_nonempty) (nat_pos hn)

private theorem avg_mono (x y : Fin n → ℝ) (h : ∀ i, x i ≤ y i) :
    Gamma.avg x ≤ Gamma.avg y :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

def powerSample (x : Fin n → ℝ) (p : ℝ) : Fin n → ℝ :=
  fun i ↦ exp (p * log (x i))

def powerRate (p a : ℝ) : ℝ := exp (-p * log a)

private theorem rate_bijOn {p : ℝ} (hp : 0 < p) :
    BijOn (powerRate p) (Ioi 0) (Ioi 0) := by
  refine ⟨fun _ _ ↦ exp_pos _, ?_, ?_⟩
  · intro a ha b hb h
    have h' := exp_injective h
    change -p * log a = -p * log b at h'
    exact log_injOn_pos ha hb (mul_left_cancel₀ (neg_ne_zero.mpr hp.ne') h')
  · intro β hβ
    refine ⟨exp (log β / (-p)), exp_pos _, ?_⟩
    have hβ' : 0 < β := hβ
    rw [powerRate, log_exp, show -p * (log β / (-p)) = log β by field_simp [hp.ne'],
      exp_log hβ']

/-- The powered sample has a Gamma likelihood plus the mean log Jacobian. -/
theorem logLik_power (hn : 0 < n) (x : Fin n → ℝ) (k p a : ℝ) :
    logLik x k p a =
      Gamma.logLik (powerSample x p) k (powerRate p a) +
        (log p + (p - 1) * Gamma.avgLog x) := by
  rw [Gamma.logLik_eq_avg hn]
  have hterm (i : Fin n) :
      Gamma.logDensity k (powerRate p a) (powerSample x p i) =
        (-p) * log (x i) - log (Real.Gamma k) - loss k (p * (log (x i) - log a)) := by
    simp only [Gamma.logDensity, powerRate, powerSample, loss, log_exp, ← exp_add]
    rw [show -p * log a + p * log (x i) = p * (log (x i) - log a) by ring]
    ring
  simp_rw [hterm]
  simp only [logLik, Gamma.avgLog, Gamma.avg, Finset.sum_sub_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp [(nat_pos hn).ne']
  ring

def scale (x : Fin n → ℝ) (k p : ℝ) : ℝ :=
  exp (log (Gamma.avg (powerSample x p) / k) / p)

private theorem scale_rate (hn : 0 < n) (x : Fin n → ℝ) {k p : ℝ}
    (hk : 0 < k) (hp : 0 < p) :
    powerRate p (scale x k p) = Gamma.rate (powerSample x p) k := by
  have hm := avg_pos hn (x := powerSample x p) (fun _ ↦ exp_pos _)
  have he : -p * (log (Gamma.avg (powerSample x p) / k) / p) =
      log (k / Gamma.avg (powerSample x p)) := by
    rw [log_div hm.ne' hk.ne', log_div hk.ne' hm.ne']
    field_simp [hp.ne']
    ring
  rw [powerRate, scale, log_exp, he, Gamma.rate, exp_log (div_pos hk hm)]

/-- With both shapes held the closed-form scale strictly beats every other positive scale. -/
theorem heldShapes_scale (hn : 0 < n) (x : Fin n → ℝ) {k p : ℝ}
    (hk : 0 < k) (hp : 0 < p) :
    0 < scale x k p ∧ ∀ a > 0, a ≠ scale x k p →
      logLik x k p a < logLik x k p (scale x k p) := by
  have hs : scale x k p ∈ Ioi 0 := exp_pos _
  have hr := scale_rate hn x hk hp
  refine ⟨hs, fun a ha hne ↦ ?_⟩
  rw [logLik_power hn, logLik_power hn, hr]
  apply add_lt_add_left
  exact (Gamma.heldShape hn (x := powerSample x p) (fun _ ↦ exp_pos _) hk).2 _ (exp_pos _) (fun h ↦
    hne ((rate_bijOn hp).injOn ha hs (h.trans hr.symm)))

private theorem strict_winner_unique {f : ℝ → ℝ} {a : ℝ} (ha : 0 < a)
    (h : ∀ b > 0, b ≠ a → f b < f a) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) b := by
  refine ⟨a, ⟨ha, ?_⟩, fun b hb ↦ ?_⟩
  · intro b hb
    by_cases he : b = a
    · simp [he]
    · exact (h b hb he).le
  · by_contra he
    exact (h b hb.1 he).not_ge (hb.2 ha)

/-- Holding both shapes gives a unique finite scale on every positive sample. -/
theorem heldShapes (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {k p : ℝ} (hk : 0 < k) (hp : 0 < p) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k p) (Ioi 0) a :=
  let h := heldShapes_scale hn x hk hp
  strict_winner_unique h.1 h.2

/-- Holding scale and power gives a unique finite Gamma shape. -/
theorem heldScalePower (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {a p : ℝ} (_ha : 0 < a) (_hp : 0 < p) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p a) (Ioi 0) k :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi 0))
    (fun k _ ↦ logLik_power hn x k p a)).mpr
    (Gamma.heldRate (powerSample x p) (exp_pos _))

private theorem log_sample_nonconstant {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) : ∃ i j, log (x i) ≠ log (x j) := by
  obtain ⟨i, j, hij⟩ := hne
  exact ⟨i, j, fun h ↦ hij (log_injOn_pos (hx i) (hx j) h)⟩

private theorem rate_prod_bijOn {p : ℝ} (hp : 0 < p) :
    BijOn (fun q : ℝ × ℝ ↦ (q.1, powerRate p q.2)) Gamma.domain Gamma.domain := by
  have h := rate_bijOn hp
  refine ⟨fun q hq ↦ ⟨hq.1, h.mapsTo hq.2⟩, ?_, ?_⟩
  · intro q hq r hr he
    have h₁ := congrArg (fun q : ℝ × ℝ ↦ q.1) he
    exact Prod.ext h₁ (h.injOn hq.2 hr.2 (congrArg Prod.snd he))
  · intro r hr
    obtain ⟨a, ha, he⟩ := h.surjOn hr.2
    exact ⟨(r.1, a), ⟨hr.1, ha⟩, Prod.ext rfl he⟩

/-- Holding power alone gives a unique finite fit in shape–scale order for nonconstant data. -/
theorem heldPower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 p r.2) Gamma.domain q := by
  obtain ⟨i, j, hij⟩ := log_sample_nonconstant hx hne
  exact (Transfer.existsUnique_max_iff (rate_prod_bijOn hp)
    (fun q _ ↦ logLik_power hn x q.1 p q.2)).mpr
    (Gamma.jointMLE hn (fun _ ↦ exp_pos _) ⟨i, j, fun h ↦
      hij (mul_left_cancel₀ hp.ne' (exp_injective h))⟩)

/-- At a constant sample the held-power likelihood diverges along the Gamma fixed-mean path. -/
theorem heldPower_constant_unbounded (hn : 0 < n) (c : ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k p
      (scale (fun _ : Fin n ↦ c) k p)) atTop atTop := by
  let x : Fin n → ℝ := fun _ ↦ c
  let d := exp (p * log c)
  refine (tendsto_atTop_add_const_right atTop (log p + (p - 1) * Gamma.avgLog x)
    (Gamma.constant_unbounded hn (exp_pos (p * log c)))).congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
  change Gamma.logLik (fun _ : Fin n ↦ d) k (k / d) + _ = logLik x k p (scale x k p)
  rw [logLik_power hn, scale_rate hn x hk hp, Gamma.rate]
  change Gamma.logLik (fun _ : Fin n ↦ d) k (k / d) + _ =
    Gamma.logLik (fun _ : Fin n ↦ d) k (k / Gamma.avg (fun _ : Fin n ↦ d)) + _
  rw [avg_const hn]

private theorem loss_strictConvex (k : ℝ) : StrictConvexOn ℝ univ (loss k) := by
  refine ⟨convex_univ, ?_⟩
  intro s _ t _ hst u v hu hv huv
  have h := strictConvexOn_exp.2 (mem_univ s) (mem_univ t) hst hu hv huv
  simp only [smul_eq_mul] at h ⊢
  unfold loss
  linarith only [h, show k * (u * s + v * t) = u * (k * s) + v * (k * t) by ring]

private theorem exp_tangent {b : ℝ} (hb : 0 < b) (t : ℝ) :
    b * (t - log b + 1) ≤ exp t := by
  have h := mul_le_mul_of_nonneg_left (add_one_le_exp (t - log b)) hb.le
  rw [exp_sub, exp_log hb, mul_div_cancel₀ _ hb.ne'] at h
  exact h

private theorem loss_linearLowerBound {k : ℝ} (hk : 0 < k) :
    LocationScale.LinearLowerBound (loss k) := by
  let b := k / 2
  have hb : 0 < b := by dsimp [b]; positivity
  let d := max (b * (log b - 1)) ((3 * b) * (log (3 * b) - 1))
  refine ⟨b, hb, d, fun t ↦ ?_⟩
  rcases le_total 0 t with ht | ht
  · rw [abs_of_nonneg ht]
    have h := exp_tangent (show 0 < 3 * b by positivity) t
    have hd : 3 * b * (log (3 * b) - 1) ≤ d := le_max_right _ _
    unfold loss
    dsimp [b] at h hd ⊢
    linarith only [h, hd]
  · rw [abs_of_nonpos ht]
    have h := exp_tangent hb t
    have hd : b * (log b - 1) ≤ d := le_max_left _ _
    unfold loss
    dsimp [b] at h hd ⊢
    linarith only [h, hd]

private theorem location_bijOn :
    BijOn (fun q : ℝ × ℝ ↦ (log q.2, q.1⁻¹)) Gamma.domain LocationScale.domain := by
  refine ⟨fun q hq ↦ ⟨mem_univ _, inv_pos.mpr hq.1⟩, ?_, ?_⟩
  · intro q hq r hr he
    exact Prod.ext (inv_injective (congrArg Prod.snd he))
      (log_injOn_pos hq.2 hr.2 (congrArg Prod.fst he))
  · intro r hr
    have hp : 0 < r.2 := hr.2
    exact ⟨(r.2⁻¹, exp r.1), ⟨inv_pos.mpr hp, exp_pos _⟩, by simp⟩

private theorem location_transfer (hn : 0 < n) (x : Fin n → ℝ) (k : ℝ)
    (q : ℝ × ℝ) (_hq : q ∈ Gamma.domain) :
    (n : ℝ) * logLik x k q.1 q.2 =
      LocationScale.logLik (loss k) (fun i ↦ log (x i)) (log q.2, q.1⁻¹) +
        (-(n : ℝ) * (log (Real.Gamma k) + Gamma.avgLog x)) := by
  have hres (i : Fin n) : (log (x i) - log q.2) * q.1 =
      q.1 * (log (x i) - log q.2) := mul_comm _ _
  simp only [logLik, LocationScale.logLik, log_inv, neg_neg, div_inv_eq_mul,
    hres, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, Gamma.avg]
  field_simp [(nat_pos hn).ne']
  ring

/-- Holding Gamma shape alone gives a unique finite fit in power–scale order on nonconstant data. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {k : ℝ} (hk : 0 < k) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x k r.1 r.2) Gamma.domain q := by
  have h := (Transfer.existsUnique_max_iff location_bijOn (location_transfer hn x k)).mpr
    (LocationScale.existsUnique_mle (loss_strictConvex k) (loss_linearLowerBound hk)
      (log_sample_nonconstant hx hne))
  apply (existsUnique_congr (fun q ↦ and_congr_right fun _ ↦ ?_)).mp h
  exact ⟨fun h r hr ↦ le_of_mul_le_mul_left (h hr) (nat_pos hn),
    fun h r hr ↦ mul_le_mul_of_nonneg_left (h hr) (nat_pos hn).le⟩

/-- At a constant sample, power can diverge while scale remains at the common value. -/
theorem heldShape_constant_unbounded (hn : 0 < n) (c k : ℝ) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ c) k p c) atTop atTop := by
  simpa only [logLik, Gamma.avgLog, sub_self, mul_zero, loss, exp_zero, sub_zero,
    avg_const hn, sub_eq_add_neg, neg_zero, add_zero, add_assoc] using
    tendsto_atTop_add_const_right atTop
      (-log (Real.Gamma k) + (-log c + -1)) tendsto_log_atTop

private theorem powerLoss_convex (z : Fin n → ℝ) (k : ℝ) :
    ConvexOn ℝ (Ioi 0) (fun p ↦ Gamma.avg (fun i ↦ loss k (p * z i))) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro p _ q _ u v hu hv huv
  simp only [smul_eq_mul, Gamma.avg, ← mul_div_assoc, ← add_div, Finset.mul_sum,
    ← Finset.sum_add_distrib]
  apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg _)
  apply Finset.sum_le_sum
  intro i _
  have h := (loss_strictConvex k).convexOn.2 (mem_univ (p * z i))
    (mem_univ (q * z i)) hu hv huv
  simpa only [smul_eq_mul, add_mul, mul_assoc] using h

/-- The held-scale, held-shape objective is strictly concave in positive power. -/
theorem heldScaleShape_strictConcave (x : Fin n → ℝ) (k a : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun p ↦ logLik x k p a) := by
  exact (((strictConcaveOn_log_Ioi.sub_convexOn (convexOn_const _ (convex_Ioi 0))).sub_convexOn
    (convexOn_const _ (convex_Ioi 0))).sub_convexOn
      (powerLoss_convex (fun i ↦ log (x i) - log a) k))

/-- Holding scale and Gamma shape gives a unique power if any observation differs from scale. -/
theorem heldScaleShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) (hne : ∃ i, x i ≠ a) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p := by
  let z := fun i ↦ log (x i) - log a
  obtain ⟨j, hj⟩ := hne
  obtain ⟨b, hb, d, hloss⟩ := loss_linearLowerBound hk
  let v := b * Gamma.avg (fun i ↦ |z i|)
  have hv : 0 < v := by
    apply mul_pos hb
    apply div_pos _ (nat_pos hn)
    exact (abs_pos.mpr (sub_ne_zero.mpr (fun h ↦ hj (log_injOn_pos (hx j) ha h)))).trans_le
      (Finset.single_le_sum (f := fun i ↦ |z i|) (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ j))
  let C := d - log (Real.Gamma k) - Gamma.avgLog x
  have hupper (p : ℝ) (hp : 0 < p) : logLik x k p a ≤ log p + C - v * p := by
    have H := avg_mono _ _ (fun i ↦ hloss (p * z i))
    have he : Gamma.avg (fun i ↦ b * |p * z i| - d) =
        b * p * Gamma.avg (fun i ↦ |z i|) - d := by
      simp only [Gamma.avg, abs_mul, abs_of_pos hp, ← mul_assoc, Finset.sum_sub_distrib,
        ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      field_simp [(nat_pos hn).ne']
    rw [he] at H
    change log p - log (Real.Gamma k) - Gamma.avgLog x -
      Gamma.avg (fun i ↦ loss k (p * z i)) ≤ _
    dsimp [C, v, Gamma.avg] at H ⊢
    linarith only [H, show b * p * ((∑ i, |z i|) / n) = b * ((∑ i, |z i|) / n) * p by ring]
  apply Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    (nonempty_Ioi (a := (0 : ℝ))) isOpen_Ioi (heldScaleShape_strictConcave x k a)
  apply tendsto_atBot.mpr
  intro level
  let lo := exp (level - C)
  let hi := (C - 1 - log (v / 2) - level) / (v / 2)
  have hsub : Icc lo hi ⊆ Ioi (0 : ℝ) := fun _ h ↦ (exp_pos _).trans_le h.1
  have hcompact : IsCompact (Subtype.val ⁻¹' Icc lo hi : Set (Ioi (0 : ℝ))) :=
    Topology.IsInducing.subtypeVal.isCompact_preimage' isCompact_Icc
      (fun t ht ↦ ⟨⟨t, hsub ht⟩, rfl⟩)
  refine mem_cocompact.mpr ⟨_, hcompact, ?_⟩
  intro p hp
  change logLik x k (p : ℝ) a ≤ level
  by_contra! hlevel
  have hp' : 0 < (p : ℝ) := p.property
  have H := hupper p hp'
  have hd : 0 < v / 2 := by positivity
  have ht := log_le_sub_one_of_pos (mul_pos hd hp')
  rw [log_mul hd.ne' hp'.ne'] at ht
  apply hp
  change (p : ℝ) ∈ Icc lo hi
  constructor
  · dsimp [lo]
    rw [← exp_log hp', exp_le_exp]
    linarith only [H, hlevel, mul_pos hv hp']
  · dsimp [hi]
    apply (le_div_iff₀ hd).mpr
    linarith only [H, hlevel, ht]

/-- At the held endpoint the log likelihood grows as log power. -/
theorem heldScaleShape_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k a : ℝ}
    (he : ∀ i, x i = a) :
    Tendsto (fun p ↦ logLik x k p a) atTop atTop := by
  have h : x = fun _ ↦ a := funext he
  rw [h]
  exact heldShape_constant_unbounded hn a k

private theorem noMax_of_path {α : Type*} {D : Set α} {f : α → ℝ} {path : ℝ → α}
    (hD : ∀ᶠ t in atTop, path t ∈ D) (h : Tendsto (f ∘ path) atTop atTop) :
    ¬ ∃ a ∈ D, IsMaxOn f D a := by
  rintro ⟨a, _, ha⟩
  obtain ⟨t, ht, hlt⟩ := (hD.and (h.eventually (eventually_gt_atTop (f a)))).exists
  exact hlt.not_ge (ha ht)

/-- The sole exception to a finite held-scale, held-shape power maximum is the endpoint sample. -/
theorem heldScaleShape_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p) ↔
      ∃ i, x i ≠ a := by
  constructor
  · intro hm
    by_contra! he
    exact noMax_of_path (f := fun p ↦ logLik x k p a) (D := Ioi 0) (path := id)
      (eventually_gt_atTop 0) (heldScaleShape_unbounded hn he) hm
  · intro he
    exact (heldScaleShape hn hx hk ha he).exists

/-- A singleton off the endpoint already disproves concavity in log-power. -/
theorem logPower_not_concave :
    ¬ ConcaveOn ℝ univ
      (fun t ↦ logLik (fun _ : Fin 1 ↦ 2) (20 / log 2) (exp t) 1) := by
  intro h
  have hd : 0 < log (2 : ℝ) := log_pos (by norm_num)
  have hm := h.2 (mem_univ (0 : ℝ)) (mem_univ (2 * log 2))
    (show (0 : ℝ) ≤ 1 / 2 by norm_num) (show (0 : ℝ) ≤ 1 / 2 by norm_num)
    (show (1 / 2 : ℝ) + 1 / 2 = 1 by norm_num)
  have htwo (m : ℕ) : exp ((m : ℝ) * log 2) = 2 ^ m := by
    rw [exp_nat_mul, exp_log (by norm_num)]
  simp only [smul_eq_mul, mul_zero, zero_add] at hm
  rw [show (1 / 2 : ℝ) * (2 * log 2) = log 2 by ring] at hm
  simp only [logLik, Gamma.avgLog, avg_const (by decide : 0 < 1), log_exp, log_one, sub_zero,
    loss, exp_zero, exp_log (by norm_num : 0 < (2 : ℝ)), one_mul] at hm
  rw [show exp (2 * log 2) = 4 by have h := htwo 2; norm_num at h; exact h] at hm
  rw [show exp (4 * log 2) = 16 by have h := htwo 4; norm_num at h; exact h] at hm
  have he (r : ℝ) : 20 / log 2 * (r * log 2) = 20 * r := by
    rw [mul_comm r, ← mul_assoc, div_mul_cancel₀ _ hd.ne']
  have he₁ : 20 / log 2 * log 2 = 20 := div_mul_cancel₀ _ hd.ne'
  simp only [he, he₁] at hm
  linarith only [hm]

end

end UnivariateML.TransformedGamma

namespace UnivariateML.InverseTransformedGamma

noncomputable section

open TransformedGamma (loss)

variable {n : ℕ}

def reciprocal (x : Fin n → ℝ) : Fin n → ℝ := fun i ↦ (x i)⁻¹

def logLik (x : Fin n → ℝ) (k p a : ℝ) : ℝ :=
  log p - log (Real.Gamma k) - Gamma.avgLog x -
    Gamma.avg (fun i ↦ loss k (p * (log a - log (x i))))

/-- Reciprocal transfer includes the data-only mean log Jacobian. -/
theorem logLik_reciprocal (x : Fin n → ℝ) (k p a : ℝ) :
    logLik x k p a =
      TransformedGamma.logLik (reciprocal x) k p a⁻¹ + (-2 * Gamma.avgLog x) := by
  have hlog : Gamma.avgLog (reciprocal x) = -Gamma.avgLog x := by
    simp [Gamma.avgLog, Gamma.avg, reciprocal, log_inv, Finset.sum_neg_distrib, neg_div]
  have hres (i : Fin n) : log (reciprocal x i) - log a⁻¹ = log a - log (x i) := by
    simp only [reciprocal, log_inv]
    ring
  simp only [logLik, TransformedGamma.logLik, hlog, hres]
  ring

private theorem inv_bijOn : BijOn (fun a : ℝ ↦ a⁻¹) (Ioi 0) (Ioi 0) :=
  ⟨fun _ ha ↦ inv_pos.mpr ha, fun _ _ _ _ h ↦ inv_injective h,
    fun a ha ↦ ⟨a⁻¹, inv_pos.mpr ha, inv_inv a⟩⟩

private theorem inv_prod_bijOn :
    BijOn (fun q : ℝ × ℝ ↦ (q.1, q.2⁻¹)) Gamma.domain Gamma.domain := by
  refine ⟨fun q hq ↦ ⟨hq.1, inv_pos.mpr hq.2⟩, ?_, ?_⟩
  · intro q _ r _ he
    have h₁ := congrArg (fun q : ℝ × ℝ ↦ q.1) he
    exact Prod.ext h₁ (inv_injective (congrArg Prod.snd he))
  · intro r hr
    exact ⟨(r.1, r.2⁻¹), ⟨hr.1, inv_pos.mpr hr.2⟩, Prod.ext rfl (inv_inv _)⟩

/-- Holding both shapes gives a unique finite inverse transformed-gamma scale. -/
theorem heldShapes (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k p : ℝ} (hk : 0 < k) (hp : 0 < p) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k p) (Ioi 0) a :=
  (Transfer.existsUnique_max_iff inv_bijOn
    (fun a _ ↦ logLik_reciprocal x k p a)).mpr
    (TransformedGamma.heldShapes hn (fun i ↦ inv_pos.mpr (hx i)) hk hp)

/-- Holding power alone transfers the unique fit on nonconstant data in shape–scale order. -/
theorem heldPower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 p r.2) Gamma.domain q := by
  obtain ⟨i, j, hij⟩ := hne
  exact (Transfer.existsUnique_max_iff inv_prod_bijOn
    (fun q _ ↦ logLik_reciprocal x q.1 p q.2)).mpr
    (TransformedGamma.heldPower hn (fun l ↦ inv_pos.mpr (hx l))
      ⟨i, j, fun h ↦ hij (inv_injective h)⟩ hp)

/-- Holding Gamma shape alone transfers the unique fit on nonconstant data in power–scale order. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {k : ℝ} (hk : 0 < k) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x k r.1 r.2) Gamma.domain q := by
  obtain ⟨i, j, hij⟩ := hne
  exact (Transfer.existsUnique_max_iff inv_prod_bijOn
    (fun q _ ↦ logLik_reciprocal x k q.1 q.2)).mpr
    (TransformedGamma.heldShape hn (fun l ↦ inv_pos.mpr (hx l))
      ⟨i, j, fun h ↦ hij (inv_injective h)⟩ hk)

/-- Constant data remain unbounded on the reciprocal held-power path. -/
theorem heldPower_constant_unbounded (hn : 0 < n) (c : ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k p
      (TransformedGamma.scale (fun _ : Fin n ↦ c⁻¹) k p)⁻¹) atTop atTop := by
  simp_rw [logLik_reciprocal, inv_inv]
  exact tendsto_atTop_add_const_right atTop _
    (TransformedGamma.heldPower_constant_unbounded hn c⁻¹ hp)

/-- The reciprocal held-shape likelihood diverges as power grows at the common scale. -/
theorem heldShape_constant_unbounded (hn : 0 < n) (c k : ℝ) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ c) k p c) atTop atTop := by
  simp_rw [logLik_reciprocal]
  exact tendsto_atTop_add_const_right atTop _
    (TransformedGamma.heldShape_constant_unbounded hn c⁻¹ k)

/-- Holding scale and power transfers the unique Gamma shape, including on constants. -/
theorem heldScalePower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a p : ℝ} (ha : 0 < a) (hp : 0 < p) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p a) (Ioi 0) k :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi 0))
    (fun k _ ↦ logLik_reciprocal x k p a)).mpr
    (TransformedGamma.heldScalePower hn (fun i ↦ inv_pos.mpr (hx i)) (inv_pos.mpr ha) hp)

/-- The reciprocal held-scale, held-shape objective is strictly concave in power. -/
theorem heldScaleShape_strictConcave (x : Fin n → ℝ) (k a : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun p ↦ logLik x k p a) := by
  simp_rw [logLik_reciprocal]
  exact (TransformedGamma.heldScaleShape_strictConcave (reciprocal x) k a⁻¹).add_concaveOn
    (concaveOn_const _ (convex_Ioi 0))

/-- Any observation different from the held scale ensures a unique finite power. -/
theorem heldScaleShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) (hne : ∃ i, x i ≠ a) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p := by
  obtain ⟨i, hi⟩ := hne
  exact (Transfer.existsUnique_max_iff (bijOn_id (Ioi 0))
    (fun p _ ↦ logLik_reciprocal x k p a)).mpr
    (TransformedGamma.heldScaleShape hn (fun j ↦ inv_pos.mpr (hx j)) hk (inv_pos.mpr ha)
      ⟨i, fun h ↦ hi (inv_injective h)⟩)

/-- The reciprocal endpoint case is also unbounded along increasing power. -/
theorem heldScaleShape_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k a : ℝ}
    (he : ∀ i, x i = a) :
    Tendsto (fun p ↦ logLik x k p a) atTop atTop := by
  simp_rw [logLik_reciprocal]
  exact tendsto_atTop_add_const_right atTop _
    (TransformedGamma.heldScaleShape_unbounded (k := k) hn
      (fun i ↦ congrArg Inv.inv (he i)))

/-- The held endpoint is the only finite-power exception in the reciprocal family. -/
theorem heldScaleShape_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p) ↔
      ∃ i, x i ≠ a := by
  rw [Transfer.exists_max_iff
    (g := fun p ↦ TransformedGamma.logLik (reciprocal x) k p a⁻¹) (bijOn_id (Ioi 0))
    (fun p _ ↦ logLik_reciprocal x k p a),
    TransformedGamma.heldScaleShape_exists_iff hn (x := reciprocal x)
      (fun i ↦ inv_pos.mpr (hx i)) hk (inv_pos.mpr ha)]
  simp [reciprocal]

end

end UnivariateML.InverseTransformedGamma

namespace UnivariateML.LogGamma

noncomputable section

variable {n : ℕ}

def logLik (x : Fin n → ℝ) (k β : ℝ) : ℝ :=
  Gamma.avg (fun i ↦ Gamma.logDensity k β (log (x i)) - log (x i))

/-- Taking logs changes the Gamma objective only by the mean log Jacobian. -/
theorem logLik_transfer (hn : 0 < n) (x : Fin n → ℝ) (k β : ℝ) :
    logLik x k β = Gamma.logLik (fun i ↦ log (x i)) k β + (-Gamma.avgLog x) := by
  rw [Gamma.logLik_eq_avg hn]
  rw [← sub_eq_add_neg]
  simp only [logLik, Gamma.avgLog, Gamma.avg, Finset.sum_sub_distrib, sub_div]

/-- Holding the rate gives a unique shape even on constant data. -/
theorem heldRate (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 1 < x i)
    {β : ℝ} (hβ : 0 < β) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t β) (Ioi 0) k :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi 0))
    (fun k _ ↦ logLik_transfer hn x k β)).mpr
    (Gamma.heldRate (fun i ↦ log (x i)) hβ)

/-- Holding the shape gives a unique rate even on constant data. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! β, β ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k) (Ioi 0) β := by
  apply (Transfer.existsUnique_max_iff (bijOn_id (Ioi 0))
    (fun β _ ↦ logLik_transfer hn x k β)).mpr
  have h := Gamma.heldShape hn (fun i ↦ log_pos (hx i)) hk
  exact TransformedGamma.strict_winner_unique h.1 h.2

/-- Nonconstant observations above one give a unique shape–rate pair. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2) Gamma.domain q :=
  (Transfer.existsUnique_max_iff (bijOn_id Gamma.domain)
    (fun q _ ↦ logLik_transfer hn x q.1 q.2)).mpr
    (Gamma.jointMLE hn (fun i ↦ log_pos (hx i))
      (TransformedGamma.log_sample_nonconstant (fun i ↦ zero_lt_one.trans (hx i)) hne))

/-- The dossier's fixed shape/rate-ratio path diverges on a constant sample. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 1 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k (k / log c)) atTop atTop := by
  simp_rw [logLik_transfer hn]
  exact tendsto_atTop_add_const_right atTop _ (Gamma.constant_unbounded hn (log_pos hc))

/-- A log-gamma joint maximum exists exactly on nonconstant samples above one. -/
theorem joint_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i) :
    (∃ q ∈ Gamma.domain, IsMaxOn (fun r ↦ logLik x r.1 r.2) Gamma.domain q) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · intro hm
    by_contra! he
    let i : Fin n := ⟨0, hn⟩
    have h : x = fun _ ↦ x i := funext (fun j ↦ he j i)
    rw [h] at hm
    exact TransformedGamma.noMax_of_path (D := Gamma.domain)
      (f := fun q ↦ logLik (fun _ : Fin n ↦ x i) q.1 q.2)
      (path := fun k ↦ (k, k / log (x i)))
      ((eventually_gt_atTop (0 : ℝ)).mono (fun _ hk ↦ ⟨hk, div_pos hk (log_pos (hx i))⟩))
      (constant_unbounded hn (hx i)) hm
  · intro h
    exact (jointMLE hn hx h).exists

end

end UnivariateML.LogGamma
