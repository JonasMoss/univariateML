module

public import UnivariateML.Family.Shape2Burr
public import UnivariateML.Family.ShapeFacesInverseBurr
public import UnivariateML.Family.Weibull

set_option linter.style.header false

/-! # Two-level Burr and inverse Burr at held power

Powering positive observations transports the fitted shapes and scales to
Lomax. Reciprocation transports the inverse problem to the forward problem.
These are pointwise two-level conclusions. The almost-everywhere bridge for
higher counts and the all-free tie comparisons are separate.
-/

@[expose] public section

open Set Real Filter Topology
namespace UnivariateML.Shape3Burr
open Transfer
noncomputable section

private theorem pair_bij {f g : ℝ → ℝ} (hf : ∀ s > 0, 0 < f s)
    (hg : ∀ s > 0, 0 < g s) (hgf : ∀ s > 0, g (f s) = s)
    (hfg : ∀ s > 0, f (g s) = s) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, f p.2)) Lomax.domain Lomax.domain := by
  refine ⟨fun p hp ↦ ⟨hp.1, hf p.2 hp.2⟩, ?_, ?_⟩
  · intro p hp q hq hpq
    change (p.1, f p.2) = (q.1, f q.2) at hpq
    obtain ⟨h₁, h₂⟩ := Prod.mk.inj hpq
    exact Prod.ext h₁ (by rw [← hgf p.2 hp.2, ← hgf q.2 hq.2, h₂])
  · intro q hq
    exact ⟨(q.1, g q.2), ⟨hq.1, hg q.2 hq.2⟩, Prod.ext rfl (hfg q.2 hq.2)⟩

private theorem rate_bij {c : ℝ} (hc : 0 < c) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, Burr.rate c p.2)) Lomax.domain Lomax.domain := by
  have hp (s : ℝ) (hs : 0 < s) : 0 < Burr.rate c s := by
    exact inv_pos.mpr (rpow_pos_of_pos hs _)
  refine pair_bij (g := fun s ↦ s⁻¹ ^ c⁻¹) hp (fun s hs ↦ rpow_pos_of_pos (inv_pos.mpr hs) _) ?_ ?_
  · intro s hs
    rw [Burr.rate, inv_inv, ← rpow_mul hs.le, mul_inv_cancel₀ hc.ne', rpow_one]
  · intro s hs
    rw [Burr.rate, ← rpow_mul (inv_pos.mpr hs).le, inv_mul_cancel₀ hc.ne', rpow_one, inv_inv]

/-- Attained Burr fits at fixed positive power are unique whenever there
are at most two observation levels. -/
theorem held_power_two_level_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c)
    (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b)
    (he : ∃ p ∈ Lomax.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ Burr.logLik x c q.1 q.2) Lomax.domain p) :
    ∃! p, p ∈ Lomax.domain ∧ IsMaxOn
      (fun q : ℝ × ℝ ↦ Burr.logLik x c q.1 q.2) Lomax.domain p := by
  have hf (p : ℝ × ℝ) (hp : p ∈ Lomax.domain) :=
    Burr.logLik_transfer hx c p.1 hp.2
  let g : ℝ × ℝ → ℝ := fun q ↦ Lomax.logLik (Burr.powered x c) q.1 q.2
  have he' := (Transfer.exists_max_iff (g := g) (rate_bij hc) hf).1 he
  apply (Transfer.existsUnique_max_iff (g := g) (rate_bij hc) hf).2
  apply LomaxTwoPoint.joint_unique hn (fun i ↦ rpow_pos_of_pos (hx i) c) _ he'
  obtain ⟨a, b, h⟩ := h2
  exact ⟨a ^ c, b ^ c, fun i ↦ (h i).imp (congrArg (fun s : ℝ ↦ s ^ c))
    (congrArg (fun s : ℝ ↦ s ^ c))⟩

/-- The same two-level singleton conclusion holds for inverse Burr. -/
theorem inverse_held_power_two_level_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c)
    (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b)
    (he : ∃ p ∈ Lomax.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x q.1 c q.2) Lomax.domain p) :
    ∃! p, p ∈ Lomax.domain ∧ IsMaxOn
      (fun q : ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x q.1 c q.2) Lomax.domain p := by
  have hφ := pair_bij (fun _ h ↦ inv_pos.mpr h) (fun _ h ↦ inv_pos.mpr h)
    (fun s _ ↦ inv_inv s) (fun s _ ↦ inv_inv s)
  have hf (p : ℝ × ℝ) (hp : p ∈ Lomax.domain) :=
    ShapeFacesInverseBurr.logLik_reciprocal hx p.1 c hp.2
  let g : ℝ × ℝ → ℝ := fun q ↦ Burr.logLik (reciprocal x) c q.1 q.2
  have he' := (Transfer.exists_max_iff (g := g) hφ hf).1 he
  apply (Transfer.existsUnique_max_iff (g := g) hφ hf).2
  apply held_power_two_level_unique hn (fun i ↦ inv_pos.mpr (hx i)) hc _ he'
  obtain ⟨a, b, h⟩ := h2
  exact ⟨a⁻¹, b⁻¹, fun i ↦ (h i).imp (congrArg Inv.inv) (congrArg Inv.inv)⟩

private theorem burr_log_factor_limit {ι : Type*} {l : Filter ι} {c k : ι → ℝ}
    {z d : ℝ} (hz : 0 ≤ z) (hd : 0 < d)
    (hc : Tendsto c l atTop) (hkc : Tendsto (fun j ↦ k j * c j) l (𝓝 d))
    (hk : ∀ᶠ j in l, 0 < k j) :
    Tendsto (fun j ↦ log (k j) + log (c j) + c j * z -
      (k j + 1) * log (1 + exp (c j * z))) l
      (𝓝 (log d - d * z - (if z = 0 then log 2 else 0))) := by
  have hk0 : Tendsto k l (𝓝 0) := by
    have h := hkc.mul (tendsto_inv_atTop_zero.comp hc)
    simp only [mul_zero] at h
    refine h.congr' ?_
    filter_upwards [hc.eventually (eventually_gt_atTop (0 : ℝ))] with j hj
    change k j * c j * (c j)⁻¹ = k j
    rw [mul_assoc, mul_inv_cancel₀ hj.ne', mul_one]
  have htail : Tendsto (fun j ↦ log (1 + exp (-(c j * z)))) l
      (𝓝 (if z = 0 then log 2 else 0)) := by
    by_cases h : z = 0
    · simp only [h, mul_zero, neg_zero, exp_zero, ite_true]
      convert (tendsto_const_nhds :
        Tendsto (fun _ : ι ↦ log 2) l (𝓝 (log 2))) using 1 <;> norm_num
    · have hz' : 0 < z := lt_of_le_of_ne hz (Ne.symm h)
      have harg : Tendsto (fun j ↦ -(c j * z)) l atBot :=
        (hc.const_mul_atTop_of_neg (neg_lt_zero.mpr hz')).congr fun j ↦ by ring
      have he := tendsto_exp_atBot.comp harg
      have hd := (he.const_add 1).log (by norm_num)
      simpa only [Function.comp_def, add_zero, log_one, ite_eq_right h] using hd
  have hlim := ((hkc.log hd.ne').sub (hkc.mul_const z)).sub ((hk0.add_const 1).mul htail)
  simp only [zero_add, one_mul] at hlim
  refine hlim.congr' ?_
  filter_upwards [hc.eventually (eventually_gt_atTop (0 : ℝ)), hk] with j hcj hkj
  have he : 1 + exp (c j * z) = exp (c j * z) * (1 + exp (-(c j * z))) := by
    rw [mul_add, ← exp_add, add_neg_cancel, exp_zero, mul_one, add_comm]
  rw [he, log_mul (exp_ne_zero _) (by positivity), log_exp, log_mul hkj.ne' hcj.ne']
  ring

/-- Burr has a Pareto trace for arbitrary moving positive shape products
on the fixed-endpoint path. Ties at scale contribute a factor one half. -/
theorem held_endpoint_pareto_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {s d : ℝ} (hs : 0 < s) (hd : 0 < d)
    (hle : ∀ i, s ≤ x i) {c k : ι → ℝ} (hc : Tendsto c l atTop)
    (hkc : Tendsto (fun j ↦ k j * c j) l (𝓝 d)) (hk : ∀ᶠ j in l, 0 < k j) :
    Tendsto (fun j ↦ Burr.logLik x (c j) (k j) s) l
      (𝓝 (∑ i : Fin n, (log d - d * log (x i / s) - log (x i) -
        (if x i = s then log 2 else 0)))) := by
  unfold Burr.logLik
  apply tendsto_finsetSum
  intro i _
  let z := log (x i / s)
  have hz : 0 ≤ z := log_nonneg ((one_le_div hs).2 (hle i))
  have he : z = 0 ↔ x i = s := by
    rw [show z = log (x i) - log s by exact log_div (hx i).ne' hs.ne', sub_eq_zero]
    exact ⟨log_injOn_pos (hx i) hs, congrArg log⟩
  have ht := (burr_log_factor_limit hz hd hc hkc hk).sub_const (log (x i))
  simp only [he, sub_right_comm _ (if x i = s then log 2 else 0) (log (x i))] at ht
  apply ht.congr
  intro j
  rw [Burr.logDensity, log_div hs.ne' (hx i).ne', rpow_def_of_pos (div_pos (hx i) hs)]
  simp only [show z = log (x i) - log s from log_div (hx i).ne' hs.ne',
    log_div (hx i).ne' hs.ne']
  ring

/-- The inverse Burr upper-endpoint power trace has the same one-half
contact factor, with the orientation reversed. -/
theorem inverse_held_endpoint_power_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {s d : ℝ} (hs : 0 < s) (hd : 0 < d)
    (hle : ∀ i, x i ≤ s) {c k : ι → ℝ} (hc : Tendsto c l atTop)
    (hkc : Tendsto (fun j ↦ k j * c j) l (𝓝 d)) (hk : ∀ᶠ j in l, 0 < k j) :
    Tendsto (fun j ↦ ShapeFacesInverseBurr.logLik x (k j) (c j) s) l
      (𝓝 (∑ i : Fin n, (log d - d * log (s / x i) - log (x i) -
        (if x i = s then log 2 else 0)))) := by
  have hl := (held_endpoint_pareto_path (x := reciprocal x) (fun i ↦ inv_pos.mpr (hx i))
    (inv_pos.mpr hs) hd (fun i ↦ inv_anti₀ (hx i) (hle i)) hc hkc hk).add_const (reciprocalJac x)
  have hsum :
      (∑ i : Fin n, (log d - d * log (reciprocal x i / s⁻¹) - log (reciprocal x i) -
        (if reciprocal x i = s⁻¹ then log 2 else 0))) + reciprocalJac x =
      ∑ i : Fin n, (log d - d * log (s / x i) - log (x i) -
        (if x i = s then log 2 else 0)) := by
    simp only [reciprocal, reciprocalJac, inv_div_inv, log_inv, inv_inj, ← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    ring
  rw [hsum] at hl
  exact hl.congr fun j ↦ (ShapeFacesInverseBurr.logLik_reciprocal hx (k j) (c j) hs).symm

/-- Held positive Burr power has its Weibull limit along every
diverging first-shape path with a positive limiting rescaled scale. -/
theorem held_power_weibull_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {c a : ℝ} (hc : 0 < c) (ha : 0 < a)
    {k s : ι → ℝ} (hk : Tendsto k l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (h : Tendsto (fun j ↦ s j * (k j) ^ (-c⁻¹)) l (𝓝 a)) :
    Tendsto (fun j ↦ Burr.logLik x c (k j) (s j)) l (𝓝 (Weibull.logLik x c a)) := by
  let R := (a ^ c)⁻¹
  have hR : 0 < R := inv_pos.mpr (rpow_pos_of_pos ha c)
  have hrate : Tendsto (fun j ↦ k j * Burr.rate c (s j)) l (𝓝 R) := by
    have ht := (continuousAt_rpow_const a c (Or.inl ha.ne')).tendsto.comp h
    refine (ht.inv₀ (rpow_pos_of_pos ha c).ne').congr' ?_
    filter_upwards [hk.eventually (eventually_gt_atTop (0 : ℝ)), hs] with j hkj hsj
    change ((s j * k j ^ (-c⁻¹)) ^ c)⁻¹ = k j * Burr.rate c (s j)
    rw [mul_rpow hsj.le (rpow_nonneg hkj.le _), ← rpow_mul hkj.le,
      show -c⁻¹ * c = -(1 : ℝ) by field_simp [hc.ne'],
      rpow_neg_one, mul_inv_rev, inv_inv, Burr.rate]
  have hlim : Tendsto (fun j ↦ Lomax.logLik (Burr.powered x c) (k j) (Burr.rate c (s j))) l
      (𝓝 (∑ i : Fin n, Lomax.expLogDensity R ((x i) ^ c))) := by
    unfold Lomax.logLik
    exact tendsto_finsetSum Finset.univ (fun i _ ↦
      Lomax.logDensity_tendsto_of_mul hR (rpow_nonneg (hx i).le c) hk hrate)
  have hface : (∑ i : Fin n, Lomax.expLogDensity R ((x i) ^ c)) + Burr.powerJac x c =
      Weibull.logLik x c a := by
    unfold Burr.powerJac Weibull.logLik
    rw [← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro i _
    rw [Weibull.logDensity, Lomax.expLogDensity, log_div (hx i).ne' ha.ne',
      div_rpow (hx i).le ha.le]
    simp only [R, log_inv, log_rpow ha, div_eq_mul_inv]
    ring
  simpa only [hface] using (hlim.add_const (Burr.powerJac x c)).congr'
    (hs.mono fun j hj ↦ (Burr.logLik_transfer hx c (k j) hj).symm)

/-- The reciprocal Weibull path accepts arbitrary moving rescaled
inverse Burr scales with a strictly positive limit. -/
theorem inverse_held_power_weibull_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {c a : ℝ} (hc : 0 < c) (ha : 0 < a)
    {k s : ι → ℝ} (hk : Tendsto k l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (h : Tendsto (fun j ↦ s j * (k j) ^ c⁻¹) l (𝓝 a)) :
    Tendsto (fun j ↦ ShapeFacesInverseBurr.logLik x (k j) c (s j)) l
      (𝓝 (Weibull.logLik (reciprocal x) c a⁻¹ + reciprocalJac x)) := by
  have hh : Tendsto (fun j ↦ (s j)⁻¹ * (k j) ^ (-c⁻¹)) l (𝓝 (a⁻¹)) := by
    refine (h.inv₀ ha.ne').congr' ?_
    filter_upwards [hk.eventually (eventually_gt_atTop (0 : ℝ))] with j hj
    rw [mul_inv_rev, rpow_neg hj.le, mul_comm]
  have ht := held_power_weibull_path (fun i ↦ inv_pos.mpr (hx i)) hc (inv_pos.mpr ha)
    hk (hs.mono fun j hj ↦ inv_pos.mpr hj) hh
  exact (ht.add_const (reciprocalJac x)).congr'
    (hs.mono fun j hj ↦ (ShapeFacesInverseBurr.logLik_reciprocal hx (k j) c hj).symm)


end
end UnivariateML.Shape3Burr
end
