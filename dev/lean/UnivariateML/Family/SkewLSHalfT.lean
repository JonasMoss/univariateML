module

public import UnivariateML.Family.TDist
public import UnivariateML.Family.SkewLSGaussian
public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Topology.Order.LeftRight
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Translated half-Student likelihood

## Deviations

Support is a constraint on the feasible set. When shape is held, any
positive value is allowed. The fitted all-free domain uses the dossier
shape floor of one.
-/

@[expose] public section

open Set Filter Topology Real UnivariateML.SkewLSGaussian

namespace UnivariateML.SkewLSHalfT

noncomputable section

/-- On its compatible support, the half-Student density doubles the
ordinary Student density observation by observation. -/
def likelihood {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) : ℝ :=
  (2 : ℝ)^n * TDist.likelihood x μ ν s

def logLik {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) : ℝ :=
  TDist.logLik x μ ν s + n * log 2

def collapseValue {n : ℕ} (x : Fin n → ℝ) (μ ν : ℝ) : ℝ :=
  (2 : ℝ)^n * exp (TDist.collapseLogValue x μ ν)

private theorem scale_max_transfer {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) :
    IsMaxOn (logLik x μ ν) (Ioi 0) s ↔ IsMaxOn (TDist.logLik x μ ν) (Ioi 0) s := by
  simp only [isMaxOn_iff, logLik, add_le_add_iff_right]

private theorem endpoint_mono {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {μ m ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) (hμ : μ < m)
    (hm : ∀ i, m ≤ x i) :
    logLik x μ ν s < logLik x m ν s := by
  unfold logLik
  apply add_lt_add_left
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  unfold TDist.logLik
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  have hsq : (x i - m)^2 < (x i - μ)^2 := by
    have hle := hm i
    nlinarith [mul_pos (sub_pos.mpr hμ) (show 0 < (x i - μ) + (x i - m) by linarith)]
  have hden : 0 < ν * s^2 := by positivity
  have hlog := Real.log_lt_log (show 0 < 1 + (x i - m)^2 / (ν * s^2) by positivity)
    (add_lt_add_right ((div_lt_div_iff_of_pos_right hden).mpr hsq) 1)
  have hmul := mul_lt_mul_of_pos_left hlog (show 0 < (ν + 1) / 2 by positivity)
  linarith

/-- Every feasible location below the sample minimum is strictly worse
at the same positive scale and held positive shape. -/
theorem endpoint_profile {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν s : ℝ}
    (hν : 0 < ν) (hs : 0 < s) (hm : ∀ j, x i ≤ x j) :
    ∃! μ, μ ∈ {m | ∀ j, m ≤ x j} ∧
      IsMaxOn (fun m ↦ logLik x m ν s) {m | ∀ j, m ≤ x j} μ := by
  have h (μ : ℝ) (hμ : μ < x i) :
      logLik x μ ν s < logLik x (x i) ν s :=
    endpoint_mono x (Nat.zero_lt_of_lt i.isLt) hν hs hμ hm
  refine ⟨x i, ⟨hm, ?_⟩, ?_⟩
  · intro μ hμ
    rcases lt_or_eq_of_le (hμ i) with hlt | rfl
    · exact (h μ hlt).le
    · exact le_rfl
  · rintro μ ⟨hμ, hmax⟩
    rcases lt_or_eq_of_le (hμ i) with hlt | he
    · exact ((h μ hlt).not_ge (hmax hm)).elim
    · exact he

private theorem held_endpoint_exists {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (∃ s ∈ Ioi 0, IsMaxOn (logLik x μ ν) (Ioi 0) s) ↔
      0 < TDist.exponent x μ ν := by
  by_cases hx : ∃ i, x i ≠ μ
  · simpa only [scale_max_transfer] using (TDist.heldLocation_maximum x μ hν hx).1
  · push Not at hx
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    have he : TDist.exponent x μ ν < 0 := by
      rw [TDist.exponent_all_tied x μ ν hx]
      linarith
    constructor
    · rintro ⟨s, hs, hm⟩
      have ht := TDist.collapse_unbounded x μ hν he
      have he' : ∀ᶠ a : ℝ in 𝓝[>] 0, a ∈ Ioi 0 := self_mem_nhdsWithin
      obtain ⟨a, ha, hval⟩ := (he'.and
        (ht.eventually_gt_atTop (TDist.likelihood x μ ν s))).exists
      have hh := (scale_max_transfer x μ ν s).mp hm ha
      exact (hval.not_ge (exp_le_exp.mpr hh)).elim
    · exact fun h ↦ (he.not_gt h).elim

/-- The held-endpoint scale maximum exists exactly at positive collapse
exponent and is then unique. Support compatibility is imposed separately. -/
theorem held_endpoint {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) :
    (∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x μ ν) (Ioi 0) s) ↔
      0 < TDist.exponent x μ ν := by
  constructor
  · rintro ⟨s, ⟨hs, hmax⟩, _⟩
    exact (held_endpoint_exists hn x μ hν).mp ⟨s, hs, hmax⟩
  · intro he
    have hx : ∃ i, x i ≠ μ := by
      by_contra! hall
      rw [TDist.exponent_all_tied x μ ν hall] at he
      have : (0 : ℝ) < n := by exact_mod_cast hn
      linarith
    have hroot := (TDist.heldLocation_maximum x μ hν hx).2 he
    obtain ⟨s, hs, huniq⟩ := hroot.1
    refine ⟨s, ⟨hs.1, (scale_max_transfer x μ ν s).mpr ((hroot.2 s hs.1).mpr hs.2)⟩, ?_⟩
    rintro t ⟨ht, hmax⟩
    exact huniq t ⟨ht, (hroot.2 t ht).mp ((scale_max_transfer x μ ν t).mp hmax)⟩

/-- With free endpoint and held shape, the exact existence criterion uses
the multiplicity at the sample minimum. Every attained fit is unique. -/
theorem held_shape {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν : ℝ}
    (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) :
    (∃! p : ℝ × ℝ, p ∈ {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} ∧
      IsMaxOn (fun p ↦ logLik x p.1 ν p.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} p) ↔
      0 < TDist.exponent x (x i) ν := by
  have hn := Nat.zero_lt_of_lt i.isLt
  have hcenter (p : ℝ × ℝ) (hp : (∀ j, p.1 ≤ x j) ∧ 0 < p.2)
      (hmax : IsMaxOn (fun p : ℝ × ℝ ↦ logLik x p.1 ν p.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} p) : p.1 = x i := by
    apply le_antisymm (hp.1 i)
    by_contra! hlt
    exact (endpoint_mono x hn hν hp.2 hlt hm).not_ge
      (hmax (show (x i, p.2) ∈ {q : ℝ × ℝ | (∀ j, q.1 ≤ x j) ∧ 0 < q.2} from ⟨hm, hp.2⟩))
  have he := held_endpoint_exists hn x (x i) hν
  constructor
  · rintro ⟨p, ⟨hp, hmax⟩, _⟩
    have hc := hcenter p hp hmax
    apply he.mp
    refine ⟨p.2, hp.2, ?_⟩
    intro s hs
    have h := hmax (show (x i, s) ∈ {q : ℝ × ℝ | (∀ j, q.1 ≤ x j) ∧ 0 < q.2} from ⟨hm, hs⟩)
    simpa [hc] using h
  · intro h
    obtain ⟨s, ⟨hs, hmax⟩, hu⟩ := (held_endpoint hn x (x i) hν).mpr h
    refine ⟨(x i, s), ⟨⟨hm, hs⟩, ?_⟩, ?_⟩
    · intro p hp
      change (∀ j, p.1 ≤ x j) ∧ 0 < p.2 at hp
      apply le_trans _ (hmax hp.2)
      rcases lt_or_eq_of_le (hp.1 i) with hl | hl
      · exact (endpoint_mono x hn hν hp.2 hl hm).le
      · exact le_of_eq (congrArg (fun m ↦ logLik x m ν p.2) hl)
    · rintro p ⟨hp, hmax'⟩
      have hc := hcenter p hp hmax'
      apply Prod.ext hc
      apply hu p.2
      refine ⟨hp.2, ?_⟩
      intro t ht
      have hh := hmax' (show (x i, t) ∈ {q : ℝ × ℝ | (∀ j, q.1 ≤ x j) ∧ 0 < q.2} from ⟨hm, ht⟩)
      simpa [hc] using hh

private theorem equality_path {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (collapseValue x μ ν)) ∧
    ∀ a > 0, likelihood x μ ν a < collapseValue x μ ν := by
  have hx : ∃ i, x i ≠ μ := by
    by_contra! hall
    rw [TDist.exponent_all_tied x μ ν hall] at he
    have : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  obtain ⟨ht, _, hlt⟩ := TDist.heldLocation_likelihood_equality x μ hν hx he
  exact ⟨ht.const_mul _, fun a ha ↦ mul_lt_mul_of_pos_left (hlt a ha) (by positivity)⟩

/-- At critical equality, the half-Student likelihood has a finite
unattained supremum along the positive-scale slice. -/
theorem held_endpoint_equality {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (collapseValue x μ ν)) ∧
      IsLUB (likelihood x μ ν '' Ioi 0) (collapseValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < collapseValue x μ ν := by
  obtain ⟨ht, hlt⟩ := equality_path hn x μ hν he
  refine ⟨ht, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨a, ha, rfl⟩
    exact (hlt a ha).le
  · intro z hz
    have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    exact le_of_tendsto ht (hs.mono (fun s hs ↦ hz ⟨s, hs, rfl⟩))

/-- At critical equality, the same finite unattained supremum holds
with the endpoint free on its compatible domain. -/
theorem held_shape_equality {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν : ℝ}
    (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) (he : TDist.exponent x (x i) ν = 0) :
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) (collapseValue x (x i) ν) ∧
      ∀ μ s : ℝ, (∀ j, μ ≤ x j) → 0 < s →
        likelihood x μ ν s < collapseValue x (x i) ν := by
  have hn := Nat.zero_lt_of_lt i.isLt
  obtain ⟨ht, hlt⟩ := equality_path hn x (x i) hν he
  have hstrict (μ s : ℝ) (hμ : ∀ j, μ ≤ x j) (hs : 0 < s) :
      likelihood x μ ν s < collapseValue x (x i) ν := by
    rcases lt_or_eq_of_le (hμ i) with h | rfl
    · have hl := endpoint_mono x hn hν hs h hm
      simp only [logLik, add_lt_add_iff_right] at hl
      exact (mul_lt_mul_of_pos_left (exp_lt_exp.mpr hl) (by positivity)).trans (hlt s hs)
    · exact hlt s hs
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hstrict p.1 p.2 hp.1 hp.2).le
  · intro b hb
    apply le_of_tendsto ht
    have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    exact hs.mono fun s hs ↦ hb ⟨(x i, s), ⟨hm, hs⟩, rfl⟩

/-- At negative exponent, the normalized likelihood diverges as scale
tends to zero. -/
theorem held_endpoint_collapse {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop :=
  (TDist.collapse_unbounded x μ hν he).const_mul_atTop (by positivity)

/-- With a compatible held endpoint and any endpoint tie, there is a
positive shape at which the scale-collapse path diverges. -/
theorem held_endpoint_free_shape_unbounded {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (htie : 0 < TDist.multiplicity x μ) :
    ∃ ν > 0, Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop := by
  let k : ℝ := TDist.multiplicity x μ
  have hk : 0 < k := by dsimp [k]; exact_mod_cast htie
  let ν := k/(2*(n+1))
  have hν : 0 < ν := by dsimp [ν]; positivity
  refine ⟨ν, hν, held_endpoint_collapse x μ hν ?_⟩
  change ν*((n : ℝ)-k)-k < 0
  have hb : 0 < 2*((n : ℝ)+1) := by positivity
  have he : ν*(2*((n : ℝ)+1)) = k := div_mul_cancel₀ _ hb.ne'
  have hsmall : ν*((n : ℝ)-k) < ν*(2*((n : ℝ)+1)) := by
    apply mul_lt_mul_of_pos_left _ hν
    have hn : 0 ≤ (n : ℝ) := by positivity
    linarith
  linarith

/-- At the fitted shape floor one, a strict majority at the sample
minimum makes the all-free likelihood unbounded. -/
theorem majority_at_floor {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (h : n < 2 * TDist.multiplicity x (x i)) :
    Tendsto (likelihood x (x i) 1) (𝓝[>] 0) atTop := by
  apply held_endpoint_collapse x (x i) zero_lt_one
  have h' : (n : ℝ) < 2*(TDist.multiplicity x (x i) : ℝ) := by exact_mod_cast h
  unfold TDist.exponent
  linarith

/-- In the fitted all-free shape range, a sub-half endpoint tie has an
attained scale profile at the floor and at every larger shape. An exact
half tie has an attained profile at every shape strictly above the floor. -/
theorem eligible_shape_profile {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 1 ≤ ν)
    (hcount : 2 * TDist.multiplicity x (x i) ≤ n)
    (hstrict : 1 < ν ∨ 2 * TDist.multiplicity x (x i) < n) :
    ∃! p : ℝ × ℝ, p ∈ {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} ∧
      IsMaxOn (fun p ↦ logLik x p.1 ν p.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} p := by
  refine (held_shape x (zero_lt_one.trans_le hν) hm).mpr ?_
  let k : ℝ := TDist.multiplicity x (x i)
  have hc : 2*k ≤ n := by dsimp [k]; exact_mod_cast hcount
  have hn : 0 < (n : ℝ) := by exact_mod_cast Nat.zero_lt_of_lt i.isLt
  have hp : 0 < (n : ℝ)-k := by linarith
  change 0 < ν*((n : ℝ)-k)-k
  rcases hstrict with h | h
  · have hmul := mul_lt_mul_of_pos_right h hp
    linarith
  · have h' : 2*k < n := by dsimp [k]; exact_mod_cast h
    have hmul := mul_le_mul_of_nonneg_right hν hp.le
    linarith

private theorem gamma_log_step (z : ℝ) (hz : 0 < z) :
    log (Gamma (z+1)) = log z + log (Gamma z) := by
  rw [Gamma_add_one hz.ne', log_mul hz.ne' (Gamma_pos_of_pos hz).ne']

private theorem gamma_half_log_bounds {z : ℝ} (hz : 0 < z) :
    -(log (1+1/(2*z)))/2 ≤ log (Gamma (z+1/2))-log (Gamma z)-log z/2 ∧
    log (Gamma (z+1/2))-log (Gamma z)-log z/2 ≤ 0 := by
  have hmid {a : ℝ} (ha : 0 < a) :
      log (Gamma (a+1/2)) ≤ (log (Gamma a)+log (Gamma (a+1)))/2 := by
    have h := convexOn_log_Gamma.2 (show a ∈ Ioi 0 from ha)
      (show a+1 ∈ Ioi 0 from (by positivity : 0 < a+1))
      (by norm_num : (0 : ℝ) ≤ 1/2) (by norm_num : (0 : ℝ) ≤ 1/2)
      (by norm_num : (1/2 : ℝ)+1/2=1)
    simp only [Function.comp_apply, smul_eq_mul] at h
    rw [show (1/2 : ℝ)*a + 1/2*(a+1) = a+1/2 by ring] at h
    linarith
  have hupper := hmid hz
  rw [gamma_log_step z hz] at hupper
  have hlower := hmid (show 0 < z+1/2 by positivity)
  rw [show z+1/2+1/2 = z+1 by ring, gamma_log_step z hz,
    gamma_log_step (z+1/2) (by positivity)] at hlower
  have hlog : log (1+1/(2*z)) = log (z+1/2)-log z := by
    rw [show 1+1/(2*z) = (z+1/2)/z by field_simp,
      log_div (by positivity : z+1/2 ≠ 0) hz.ne']
  rw [hlog]
  constructor <;> linarith

private theorem student_log_normalizer_limit :
    Tendsto TDist.logNormalizer atTop (𝓝 (-(log (2*π))/2)) := by
  let D (z : ℝ) := log (Gamma (z+1/2))-log (Gamma z)-log z/2
  have hD : Tendsto D atTop (𝓝 0) := by
    have h1 : Tendsto (fun z : ℝ ↦ 1+1/(2*z)) atTop (𝓝 1) := by
      have h : Tendsto (fun z : ℝ ↦ z*2) atTop atTop :=
        tendsto_id.atTop_mul_const (by norm_num : (0 : ℝ) < 2)
      simpa [one_div, mul_comm, Function.comp_def] using
        (tendsto_inv_atTop_zero.comp h).const_add (1 : ℝ)
    have hl : Tendsto (fun z : ℝ ↦ -(log (1+1/(2*z)))/2) atTop (𝓝 0) := by
      simpa [Function.comp_def] using
        (((continuousAt_log one_ne_zero).tendsto.comp h1).neg.div_const 2)
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hl tendsto_const_nhds ?_ ?_
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun z hz ↦ (gamma_half_log_bounds hz).1
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun z hz ↦ (gamma_half_log_bounds hz).2
  have h := (hD.comp (tendsto_id.atTop_mul_const (show (0 : ℝ) < 1/2 by norm_num))).sub_const
    (log (2*π)/2)
  simp only [zero_sub, ← neg_div] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with ν hν
  change D (ν*(1/2)) - log (2*π)/2 = TDist.logNormalizer ν
  unfold TDist.logNormalizer
  have he : ν*π = (ν*(1/2))*(2*π) := by ring
  rw [he, log_mul (by positivity : ν*(1/2) ≠ 0) (by positivity : 2*π ≠ 0)]
  dsimp [D]
  rw [show ν*(1/2) = ν/2 by ring, show ν/2+1/2 = (ν+1)/2 by ring]
  ring

private theorem student_kernel_limit {y : ℝ → ℝ} {y₀ : ℝ}
    (hy : Tendsto y atTop (𝓝 y₀)) (hnonneg : ∀ᶠ ν in atTop, 0 ≤ y ν) :
    Tendsto (fun ν : ℝ ↦ (ν+1)*log (1+y ν/ν)) atTop (𝓝 y₀) := by
  have h0 : Tendsto (fun ν : ℝ ↦ 1+y ν/ν) atTop (𝓝 1) := by
    simpa [div_eq_mul_inv] using (hy.mul tendsto_inv_atTop_zero).const_add (1 : ℝ)
  have h1 : Tendsto (fun ν : ℝ ↦ y ν*(1+ν⁻¹)) atTop (𝓝 y₀) := by
    simpa using hy.mul ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_add 1)
  have hl := h1.div h0 one_ne_zero
  simp only [div_one] at hl
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hl h1 ?_ ?_
  · filter_upwards [eventually_gt_atTop (0 : ℝ), hnonneg] with ν hν hyν
    have hpos : 0 < 1+y ν/ν := by positivity
    have hlog := log_le_sub_one_of_pos (inv_pos.mpr hpos)
    rw [log_inv] at hlog
    have hbound : (y ν/ν)/(1+y ν/ν) ≤ log (1+y ν/ν) := by
      have he : (y ν/ν)/(1+y ν/ν) = 1 - (1+y ν/ν)⁻¹ := by field_simp; ring
      linarith
    have he : y ν*(1+ν⁻¹)/(1+y ν/ν) = (ν+1)*((y ν/ν)/(1+y ν/ν)) := by
      field_simp [hν.ne', hpos.ne']
    change y ν*(1+ν⁻¹)/(1+y ν/ν) ≤ (ν+1)*log (1+y ν/ν)
    rw [he]
    exact mul_le_mul_of_nonneg_left hbound (by positivity)
  · filter_upwards [eventually_gt_atTop (0 : ℝ), hnonneg] with ν hν hyν
    have hpos : 0 < 1+y ν/ν := by positivity
    have hlog : log (1+y ν/ν) ≤ y ν/ν := by linarith [log_le_sub_one_of_pos hpos]
    have he : (ν+1)*(y ν/ν) = y ν*(1+ν⁻¹) := by field_simp [hν.ne']
    exact (mul_le_mul_of_nonneg_left hlog (by positivity : 0 ≤ ν+1)).trans_eq he

private theorem half_normal_path_along {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {s : ℝ → ℝ} {s₀ : ℝ} (hs₀ : 0 < s₀) (hs : Tendsto s atTop (𝓝 s₀)) :
    Tendsto (fun ν ↦ likelihood x μ ν (s ν)) atTop
      (𝓝 (∏ i, 2*φ ((x i-μ)/s₀)/s₀)) := by
  unfold likelihood TDist.likelihood TDist.logLik
  have hpow (ν : ℝ) : (2 : ℝ)^n * exp (∑ i, (TDist.logNormalizer ν - log (s ν) -
      (ν+1)/2*log (1+(x i-μ)^2/(ν*(s ν)^2)))) =
      ∏ i, 2*exp (TDist.logNormalizer ν-log (s ν)-
        (ν+1)/2*log (1+(x i-μ)^2/(ν*(s ν)^2))) := by
    rw [exp_sum, Finset.prod_mul_distrib]
    simp
  simp_rw [hpow]
  refine tendsto_finsetProd Finset.univ fun i _ ↦ ?_
  let y (ν : ℝ) := (x i-μ)^2/(s ν)^2
  let y₀ := (x i-μ)^2/s₀^2
  have hy : Tendsto y atTop (𝓝 y₀) :=
    tendsto_const_nhds.div (hs.pow 2) (pow_ne_zero _ hs₀.ne')
  have hy' : ∀ᶠ ν in atTop, 0 ≤ y ν := Eventually.of_forall fun ν ↦ by dsimp [y]; positivity
  have hslog : Tendsto (fun ν ↦ log (s ν)) atTop (𝓝 (log s₀)) :=
    (continuousAt_log hs₀.ne').tendsto.comp hs
  have h := (student_log_normalizer_limit.sub hslog).sub
    ((student_kernel_limit hy hy').div_const 2)
  have he := (continuous_exp.continuousAt.tendsto.comp h).const_mul (2 : ℝ)
  have heng : -log (2*π)/2 - log s₀ - y₀/2 =
      log (1/sqrt (2*π)) - log s₀ - y₀/2 := by
    rw [log_div one_ne_zero (by positivity), log_one, log_sqrt (by positivity)]
    ring
  rw [heng, exp_sub, exp_sub, exp_log (by positivity), exp_log hs₀] at he
  rw [phi_formula]
  convert he using 1
  · ext ν
    have hj : (x i-μ)^2/(ν*(s ν)^2) = y ν/ν := by dsimp [y]; ring
    simp only [Function.comp_apply, hj]
    rw [show (ν+1)/2*log (1+y ν/ν) = (ν+1)*log (1+y ν/ν)/2 by ring]
  · dsimp [y₀]
    rw [div_pow, exp_neg]
    congr 1
    ring

/-- At fixed compatible endpoint and positive scale, the half-Student
likelihood approaches the translated half-normal likelihood as shape
tends to infinity. -/
theorem half_normal_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {s : ℝ} (hs : 0 < s) :
    Tendsto (fun ν ↦ likelihood x μ ν s) atTop
      (𝓝 (∏ i, 2*φ ((x i-μ)/s)/s)) :=
  half_normal_path_along x μ hs tendsto_const_nhds

/-- RMS of the residuals about a fixed endpoint. -/
def rmsScale {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  sqrt ((∑ i, (x i-μ)^2) / n)

private theorem score_scale_bounds {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hn : 0 < n) {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s)
    (hroot : TDist.scaleScore x μ ν s = 0) :
    let q := ∑ i, (x i-μ)^2
    let M := (1+ν⁻¹)*(q/n)
    M-q/ν ≤ s^2 ∧ s^2 ≤ M := by
  intro q M
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hq : 0 ≤ q := Finset.sum_nonneg (fun _ _ ↦ sq_nonneg _)
  have hbase : 0 < ν*s^2 := by positivity
  have hsum : (∑ i, (ν+1)*(x i-μ)^2/(ν*s^2+(x i-μ)^2)) = (n : ℝ) :=
    sub_eq_zero.mp hroot
  have hlo : (ν+1)*q/(ν*s^2+q) ≤ n := by
    rw [← hsum]
    rw [show (ν+1)*q/(ν*s^2+q) =
      ∑ i, (ν+1)*(x i-μ)^2/(ν*s^2+q) by rw [← Finset.sum_div, ← Finset.mul_sum]]
    exact Finset.sum_le_sum fun i _ ↦ div_le_div_of_nonneg_left (by positivity)
      (by positivity) (add_le_add_right (Finset.single_le_sum
        (f := fun j ↦ (x j-μ)^2) (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i)) _)
  have hhi : (n : ℝ) ≤ (ν+1)*q/(ν*s^2) := by
    rw [← hsum]
    rw [show (ν+1)*q/(ν*s^2) =
      ∑ i, (ν+1)*(x i-μ)^2/(ν*s^2) by rw [← Finset.sum_div, ← Finset.mul_sum]]
    exact Finset.sum_le_sum fun i _ ↦ div_le_div_of_nonneg_left (by positivity)
      hbase (le_add_of_nonneg_right (sq_nonneg _))
  have hlo' := (div_le_iff₀ (add_pos_of_pos_of_nonneg hbase hq)).mp hlo
  have hhi' := (le_div_iff₀ hbase).mp hhi
  have hprod : M*(n*ν) = (ν+1)*q := by
    dsimp [M]
    field_simp [hn'.ne', hν.ne']
  have hprod' : (q/ν)*(n*ν) = n*q := by field_simp [hν.ne']
  constructor
  · apply le_of_mul_le_mul_right _ (show 0 < (n : ℝ)*ν by positivity)
    rw [sub_mul, hprod, hprod']
    nlinarith only [hlo']
  · apply le_of_mul_le_mul_right _ (show 0 < (n : ℝ)*ν by positivity)
    rw [hprod]
    nlinarith only [hhi']

private theorem rmsScale_pos {n : ℕ} {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) : 0 < rmsScale x μ := by
  obtain ⟨i, hi⟩ := hx
  have hn : (0 : ℝ) < n := by exact_mod_cast Nat.zero_lt_of_lt i.isLt
  unfold rmsScale
  apply sqrt_pos.mpr
  exact div_pos ((sq_pos_of_ne_zero (sub_ne_zero.mpr hi)).trans_le
    (Finset.single_le_sum (f := fun j ↦ (x j-μ)^2) (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i))) hn

/-- Any eventually positive sequence of exact held-shape scale roots tends
to the Gaussian RMS scale as the degrees of freedom tend to infinity. -/
theorem scale_root_normal_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) {s : ℝ → ℝ}
    (hroot : ∀ᶠ ν in atTop, 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0) :
    Tendsto s atTop (𝓝 (rmsScale x μ)) := by
  let q := ∑ i, (x i-μ)^2
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact Nat.zero_lt_of_lt i.isLt
  have hu : Tendsto (fun ν : ℝ ↦ (1+ν⁻¹)*(q/n)) atTop (𝓝 (q/n)) := by
    simpa only [add_zero, one_mul] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_add 1).mul_const (q/n)
  have hq : Tendsto (fun ν : ℝ ↦ q/ν) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using tendsto_inv_atTop_zero.const_mul q
  have hl : Tendsto (fun ν : ℝ ↦ (1+ν⁻¹)*(q/n)-q/ν) atTop (𝓝 (q/n)) := by
    simpa only [sub_zero] using hu.sub hq
  have hboth : ∀ᶠ ν in atTop, (1+ν⁻¹)*(q/n)-q/ν ≤ (s ν)^2 ∧
      (s ν)^2 ≤ (1+ν⁻¹)*(q/n) := by
    filter_upwards [hroot, eventually_gt_atTop (0 : ℝ)] with ν hν hpos
    exact score_scale_bounds x μ hn hpos hν.1 hν.2
  have hs2 : Tendsto (fun ν ↦ (s ν)^2) atTop (𝓝 (q/n)) :=
    tendsto_of_tendsto_of_tendsto_of_le_of_le' hl hu
      (hboth.mono fun _ h ↦ h.1) (hboth.mono fun _ h ↦ h.2)
  refine (continuous_sqrt.continuousAt.tendsto.comp hs2).congr' ?_
  filter_upwards [hroot] with ν hν
  exact sqrt_sq hν.1.le

/-- The actual half-Student profile value at a fixed compatible endpoint,
optimized over positive scales. -/
def scaleProfile {n : ℕ} (x : Fin n → ℝ) (μ ν : ℝ) : ℝ :=
  sSup (likelihood x μ ν '' Ioi 0)

private theorem likelihood_eq_exp_logLik {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) :
    likelihood x μ ν s = exp (logLik x μ ν s) := by
  unfold likelihood logLik TDist.likelihood
  rw [exp_add, exp_nat_mul, exp_log (by norm_num : (0 : ℝ) < 2), mul_comm]

/-- At a compatible endpoint with a nonzero residual, the optimized
scale profile reaches the half-normal value at the endpoint RMS scale. -/
theorem profiled_half_normal_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) :
    Tendsto (scaleProfile x μ) atTop
      (𝓝 (∏ i, 2*φ ((x i-μ)/rmsScale x μ)/rmsScale x μ)) := by
  classical
  let k : ℝ := TDist.multiplicity x μ
  have hp : 0 < (n : ℝ)-k := by
    have ht : TDist.multiplicity x μ < n := by
      unfold TDist.multiplicity
      by_contra! h
      have h' : (Finset.univ.filter fun i ↦ x i=μ).card = (Finset.univ : Finset (Fin n)).card := by
        have ht := Finset.card_le_card (Finset.filter_subset (fun i ↦ x i=μ) Finset.univ)
        simp only [Finset.card_univ, Fintype.card_fin] at ht ⊢
        exact le_antisymm ht h
      have hall := Finset.card_filter_eq_iff.mp h'
      obtain ⟨i, hi⟩ := hx
      exact hi (hall i (Finset.mem_univ _))
    have ht' : k < (n : ℝ) := by dsimp [k]; exact_mod_cast ht
    linarith
  let B := max 0 (k/((n : ℝ)-k))
  have hchoose (ν : ℝ) : ∃ s : ℝ, B < ν →
      0 < s ∧ TDist.scaleScore x μ ν s = 0 ∧ IsMaxOn (logLik x μ ν) (Ioi 0) s := by
    by_cases h : B < ν
    · have hν : 0 < ν := (le_max_left _ _).trans_lt h
      have he : 0 < TDist.exponent x μ ν := by
        change 0 < ν*((n : ℝ)-k)-k
        have hh := (div_lt_iff₀ hp).mp ((le_max_right _ _).trans_lt h)
        linarith
      have hr := (TDist.heldLocation_maximum x μ hν hx).2 he
      obtain ⟨s, hs, _⟩ := hr.1
      exact ⟨s, fun _ ↦ ⟨hs.1, hs.2, (scale_max_transfer x μ ν s).mpr ((hr.2 s hs.1).mpr hs.2)⟩⟩
    · exact ⟨1, fun hh ↦ (h hh).elim⟩
  choose s hs using hchoose
  have hr : ∀ᶠ ν in atTop, 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0 :=
    (eventually_gt_atTop B).mono fun ν h ↦ ⟨(hs ν h).1, (hs ν h).2.1⟩
  have hlim := half_normal_path_along x μ (rmsScale_pos hx) (scale_root_normal_limit x μ hx hr)
  refine hlim.congr' ?_
  filter_upwards [eventually_gt_atTop B] with ν hν
  obtain ⟨hpos, _, hm⟩ := hs ν hν
  have hmax : IsMaxOn (likelihood x μ ν) (Ioi 0) (s ν) := by
    intro a ha
    change likelihood x μ ν a ≤ likelihood x μ ν (s ν)
    rw [likelihood_eq_exp_logLik, likelihood_eq_exp_logLik]
    exact exp_le_exp.mpr (hm ha)
  exact ((hmax.isLUB hpos).csSup_eq (Nonempty.image _ ⟨s ν,hpos⟩)).symm

private def off {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : Finset (Fin n) :=
  Finset.univ.filter (fun i ↦ x i ≠ μ)

private theorem mem_off {n : ℕ} {x : Fin n → ℝ} {μ : ℝ} {i : Fin n} :
    i ∈ off x μ ↔ x i ≠ μ := by simp [off]

private theorem off_count {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) :
    ((off x μ).card : ℝ) = (n : ℝ)-TDist.multiplicity x μ := by
  have h : TDist.multiplicity x μ + (off x μ).card = n := by
    unfold TDist.multiplicity off
    rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
  have hr : (TDist.multiplicity x μ : ℝ) + ((off x μ).card : ℝ) = n := by exact_mod_cast h
  linarith

private theorem sum_off {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) (f : Fin n → ℝ)
    (hf : ∀ i, x i = μ → f i = 0) : (∑ i, f i) = ∑ i ∈ off x μ, f i := by
  unfold off
  exact (Finset.sum_filter_of_ne (s := Finset.univ) (p := fun i ↦ x i ≠ μ)
    (fun i _ hi he ↦ hi (hf i he))).symm

private theorem exponent_at_root {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) (hroot : TDist.scaleScore x μ ν s = 0) :
    TDist.exponent x μ ν =
      (ν+1) * ∑ i ∈ off x μ, ν*s^2/(ν*s^2+(x i-μ)^2) := by
  have hnEq : (∑ i, (ν+1)*(x i-μ)^2/(ν*s^2+(x i-μ)^2)) = (n : ℝ) :=
    sub_eq_zero.mp hroot
  rw [sum_off x μ _ (fun i hi ↦ by simp [hi])] at hnEq
  rw [Finset.mul_sum]
  have hterm (i : Fin n) :
      (ν+1)*(ν*s^2/(ν*s^2+(x i-μ)^2)) =
        (ν+1)-(ν+1)*(x i-μ)^2/(ν*s^2+(x i-μ)^2) := by
    field_simp [show ν*s^2+(x i-μ)^2 ≠ 0 by positivity]
    ring
  simp_rw [hterm]
  rw [Finset.sum_sub_distrib, hnEq, Finset.sum_const, nsmul_eq_mul, off_count]
  unfold TDist.exponent
  ring

private def remainder {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) : ℝ :=
  n * TDist.logNormalizer ν + (ν+1)/2 *
    (((n : ℝ)-TDist.multiplicity x μ)*log ν -
      ∑ i ∈ off x μ, log (ν*s^2+(x i-μ)^2))

private theorem log_decompose {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    TDist.logLik x μ ν s = TDist.exponent x μ ν*log s + remainder x μ ν s := by
  have hterm (d : ℝ) : log (1+d^2/(ν*s^2)) = log (ν*s^2+d^2)-log ν-2*log s := by
    rw [show 1+d^2/(ν*s^2) = (ν*s^2+d^2)/(ν*s^2) by field_simp,
      log_div (by positivity) (by positivity), log_mul hν.ne' (by positivity), log_pow]
    ring
  unfold TDist.logLik TDist.exponent remainder
  rw [Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, sum_off x μ _ (fun i hi ↦ by simp [hi])]
  simp_rw [hterm]
  rw [Finset.sum_sub_distrib, Finset.sum_sub_distrib, Finset.sum_const, Finset.sum_const,
    nsmul_eq_mul, nsmul_eq_mul, off_count]
  ring

private theorem scaleScore_continuous_shape {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    ContinuousAt (fun z ↦ TDist.scaleScore x μ z s) ν := by
  unfold TDist.scaleScore
  apply ContinuousAt.sub _ continuousAt_const
  refine tendsto_finsetSum Finset.univ (fun i _ ↦ ?_)
  exact ((by fun_prop : Continuous (fun z : ℝ ↦ (z+1)*(x i-μ)^2)).continuousAt.div
    (by fun_prop) (by positivity)).tendsto

private theorem scale_root_at_collapse {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) (he : TDist.exponent x μ 1 = 0) {s : ℝ → ℝ}
    (hroot : ∀ᶠ ν in 𝓝[>] (1 : ℝ), 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0) :
    Tendsto s (𝓝[>] (1 : ℝ)) (𝓝[>] 0) := by
  apply tendsto_inf.mpr
  refine ⟨Metric.tendsto_nhds.mpr ?_, tendsto_principal.mpr (hroot.mono fun _ h ↦ h.1)⟩
  intro ε hε
  have ha := TDist.scaleScore_strictAntiOn x μ zero_lt_one hx
  have hs₀ : TDist.scaleScore x μ 1 (ε/2) ≤ 0 := by
    have hlim := (TDist.scaleScore_limits x μ zero_lt_one).1
    rw [he] at hlim
    apply ge_of_tendsto hlim
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (half_pos hε))] with t ht ht'
    exact ha.antitoneOn ht (half_pos hε) ht'.le
  have hneg := (ha (half_pos hε) hε (by linarith)).trans_le hs₀
  have hnear := ((scaleScore_continuous_shape x μ zero_lt_one hε).continuousWithinAt
    (s := Ioi (1 : ℝ))).tendsto.eventually (eventually_lt_nhds hneg)
  filter_upwards [hnear, hroot, self_mem_nhdsWithin (s := Ioi (1 : ℝ))] with ν hn hr hν
  have hνpos : 0 < ν := zero_lt_one.trans hν
  rw [Real.dist_eq, sub_zero, abs_of_pos hr.1]
  by_contra! hh
  have hcmp := (TDist.scaleScore_strictAntiOn x μ hνpos hx).antitoneOn hε hr.1 hh
  linarith [hr.2]

private theorem exponent_log_scale_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {s : ℝ → ℝ} (hs : Tendsto s (𝓝[>] (1 : ℝ)) (𝓝[>] 0))
    (hroot : ∀ᶠ ν in 𝓝[>] (1 : ℝ), 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0) :
    Tendsto (fun ν ↦ TDist.exponent x μ ν * log (s ν)) (𝓝[>] (1 : ℝ)) (𝓝 0) := by
  let C := 6 * ∑ i ∈ off x μ, 1/(x i-μ)^2
  have hbound : ∀ᶠ ν in 𝓝[>] (1 : ℝ),
      0 ≤ TDist.exponent x μ ν ∧ TDist.exponent x μ ν ≤ C*(s ν)^2 := by
    filter_upwards [hroot, self_mem_nhdsWithin (s := Ioi (1 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (by norm_num : (1 : ℝ) < 2))]
      with ν hν hν' hν''
    have hpos : 0 < ν := zero_lt_one.trans hν'
    have hlt : ν < 2 := hν''
    rw [exponent_at_root x μ hpos hν.1 hν.2]
    constructor
    · exact mul_nonneg (by positivity) (Finset.sum_nonneg fun _ _ ↦ by positivity)
    · have hsum : (∑ i ∈ off x μ, ν*(s ν)^2/(ν*(s ν)^2+(x i-μ)^2)) ≤
          ν*(s ν)^2*∑ i ∈ off x μ, 1/(x i-μ)^2 := by
        rw [Finset.mul_sum]
        refine Finset.sum_le_sum fun i hi ↦ ?_
        have hd := sq_pos_of_ne_zero (sub_ne_zero.mpr (mem_off.mp hi))
        calc
          _ ≤ ν*(s ν)^2/(x i-μ)^2 :=
            div_le_div_of_nonneg_left (by positivity) hd (le_add_of_nonneg_left (by positivity))
          _ = _ := by rw [div_eq_mul_inv, one_div]
      apply (mul_le_mul_of_nonneg_left hsum (by positivity : 0 ≤ ν+1)).trans
      have hfac : (ν+1)*ν ≤ 6 := by
        nlinarith only [mul_le_mul (show ν+1 ≤ 3 by linarith) hlt.le
          (by positivity : 0 ≤ ν) (by norm_num : (0 : ℝ) ≤ 3)]
      have hn : 0 ≤ (s ν)^2 * ∑ i ∈ off x μ, 1/(x i-μ)^2 := by positivity
      dsimp [C]
      nlinarith only [mul_le_mul_of_nonneg_right hfac hn]
  have hsmall : Tendsto (fun a : ℝ ↦ log a * a^2) (𝓝[>] 0) (𝓝 0) := by
    simpa only [Real.rpow_natCast] using tendsto_log_mul_rpow_nhdsGT_zero
      (show (0 : ℝ) < (2 : ℕ) by positivity)
  have hlim : Tendsto (fun ν ↦ C*(s ν)^2*log (s ν)) (𝓝[>] (1 : ℝ)) (𝓝 0) := by
    have h := (hsmall.comp hs).const_mul C
    simp only [mul_zero] at h
    exact h.congr fun ν ↦ by simp only [Function.comp_apply]; ring
  have hscale : ∀ᶠ ν in 𝓝[>] (1 : ℝ), s ν < 1 :=
    (hs.mono_right nhdsWithin_le_nhds).eventually (eventually_lt_nhds zero_lt_one)
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hlim tendsto_const_nhds ?_ ?_
  · filter_upwards [hroot, hbound, hscale] with ν hn hb hh
    exact mul_le_mul_of_nonpos_right hb.2 (log_neg hn.1 hh).le
  · filter_upwards [hroot, hbound, hscale] with ν hn hb hh
    exact mul_nonpos_of_nonneg_of_nonpos hb.1 (log_neg hn.1 hh).le

private theorem logGamma_continuous_pos {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun t : ℝ ↦ log (Gamma t)) z :=
  (differentiableAt_Gamma (fun m ↦ ne_of_gt
    ((neg_nonpos.mpr (Nat.cast_nonneg m)).trans_lt hz))).continuousAt.log
      (Gamma_pos_of_pos hz).ne'

private theorem logNormalizer_continuous_pos {ν : ℝ} (hν : 0 < ν) :
    ContinuousAt TDist.logNormalizer ν := by
  unfold TDist.logNormalizer
  refine ContinuousAt.sub (ContinuousAt.sub ?_ ?_) ?_
  · exact (logGamma_continuous_pos (show 0 < (ν+1)/2 by positivity)).comp'
      (f := fun t : ℝ ↦ (t+1)/2) (x := ν) (by fun_prop)
  · exact (logGamma_continuous_pos (show 0 < ν/2 by positivity)).comp'
      (f := fun t : ℝ ↦ t/2) (x := ν) (by fun_prop)
  · exact ((by fun_prop : Continuous (fun t : ℝ ↦ t*π)).continuousAt.log
      (by positivity)).div_const 2

private theorem remainder_at_floor {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {s : ℝ → ℝ} (hs : Tendsto s (𝓝[>] (1 : ℝ)) (𝓝 0)) :
    Tendsto (fun ν ↦ remainder x μ ν (s ν)) (𝓝[>] (1 : ℝ))
      (𝓝 (TDist.collapseLogValue x μ 1)) := by
  have hv : Tendsto (fun ν : ℝ ↦ ν) (𝓝[>] (1 : ℝ)) (𝓝 1) := nhdsWithin_le_nhds
  have hN : ContinuousAt TDist.logNormalizer 1 := logNormalizer_continuous_pos zero_lt_one
  have hsum (i : Fin n) (hi : i ∈ off x μ) :
      Tendsto (fun ν ↦ log (ν*(s ν)^2+(x i-μ)^2)) (𝓝[>] (1 : ℝ))
        (𝓝 (log ((x i-μ)^2))) := by
    have h := (hv.mul (hs.pow 2)).add_const ((x i-μ)^2)
    have ht : Tendsto (fun ν ↦ ν*(s ν)^2+(x i-μ)^2) (𝓝[>] (1 : ℝ))
        (𝓝 ((x i-μ)^2)) := by simpa using h
    have hn := (sq_pos_of_ne_zero (sub_ne_zero.mpr (mem_off.mp hi))).ne'
    exact (continuousAt_log hn).tendsto.comp ht
  have hlog : Tendsto (fun ν : ℝ ↦ log ν) (𝓝[>] (1 : ℝ)) (𝓝 (log 1)) :=
    (continuousAt_log one_ne_zero).tendsto.comp hv
  unfold remainder TDist.collapseLogValue
  exact ((hN.tendsto.comp hv).const_mul _).add (((hv.add_const 1).div_const 2).mul
    ((hlog.const_mul _).sub (tendsto_finsetSum _ hsum)))

/-- At an exact half tie, the optimized scale profile approaches the
Cauchy-floor scale-zero value as shape decreases to one from above. -/
theorem profiled_half_tie_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) (he : TDist.exponent x μ 1 = 0) :
    Tendsto (scaleProfile x μ) (𝓝[>] (1 : ℝ)) (𝓝 (collapseValue x μ 1)) := by
  classical
  have hn : 0 < (n : ℝ) := by
    obtain ⟨i, _⟩ := hx
    exact_mod_cast Nat.zero_lt_of_lt i.isLt
  have hk : 0 < (n : ℝ)-TDist.multiplicity x μ := by
    unfold TDist.exponent at he
    linarith
  have hchoose (ν : ℝ) : ∃ s : ℝ, 1 < ν →
      0 < s ∧ TDist.scaleScore x μ ν s = 0 ∧ IsMaxOn (logLik x μ ν) (Ioi 0) s := by
    by_cases h : 1 < ν
    · have hν := zero_lt_one.trans h
      have he' : 0 < TDist.exponent x μ ν := by
        unfold TDist.exponent at he ⊢
        linarith only [he, mul_lt_mul_of_pos_right h hk]
      have hr := (TDist.heldLocation_maximum x μ hν hx).2 he'
      obtain ⟨s, hs, _⟩ := hr.1
      exact ⟨s, fun _ ↦ ⟨hs.1, hs.2, (scale_max_transfer x μ ν s).mpr ((hr.2 s hs.1).mpr hs.2)⟩⟩
    · exact ⟨1, fun hh ↦ (h hh).elim⟩
  choose s hs using hchoose
  have hν : ∀ᶠ ν : ℝ in 𝓝[>] (1 : ℝ), 1 < ν := self_mem_nhdsWithin
  have hr : ∀ᶠ ν in 𝓝[>] (1 : ℝ), 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0 :=
    hν.mono fun ν h ↦ ⟨(hs ν h).1, (hs ν h).2.1⟩
  have hsl := scale_root_at_collapse x μ hx he hr
  have hR := remainder_at_floor x μ (hsl.mono_right nhdsWithin_le_nhds)
  have hlog : Tendsto (fun ν ↦ TDist.logLik x μ ν (s ν)) (𝓝[>] (1 : ℝ))
      (𝓝 (TDist.collapseLogValue x μ 1)) := by
    have h := (exponent_log_scale_limit x μ hsl hr).add hR
    simp only [zero_add] at h
    refine h.congr' ?_
    filter_upwards [hν, hr] with ν hν hr
    exact (log_decompose x μ (zero_lt_one.trans hν) hr.1).symm
  have hlim : Tendsto (fun ν ↦ likelihood x μ ν (s ν)) (𝓝[>] (1 : ℝ))
      (𝓝 (collapseValue x μ 1)) :=
    (continuous_exp.continuousAt.tendsto.comp hlog).const_mul _
  refine hlim.congr' ?_
  filter_upwards [hν] with ν hν
  obtain ⟨hpos, _, hm⟩ := hs ν hν
  have hmax : IsMaxOn (likelihood x μ ν) (Ioi 0) (s ν) := by
    intro a ha
    change likelihood x μ ν a ≤ likelihood x μ ν (s ν)
    rw [likelihood_eq_exp_logLik, likelihood_eq_exp_logLik]
    exact exp_le_exp.mpr (hm ha)
  exact ((hmax.isLUB hpos).csSup_eq (Nonempty.image _ ⟨s ν,hpos⟩)).symm

/-- At any positive shape the free-endpoint profile has the same upper
bounds as the scale profile at the sample minimum. -/
theorem free_endpoint_profile_upperBounds {n : ℕ} (x : Fin n → ℝ)
    {i : Fin n} {ν : ℝ} (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) :
    upperBounds ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) =
      upperBounds (likelihood x (x i) ν '' Ioi 0) := by
  ext z
  constructor
  · intro hz
    rintro _ ⟨s, hs, rfl⟩
    exact hz ⟨(x i,s), ⟨hm,hs⟩, rfl⟩
  · intro hz
    rintro _ ⟨p, hp, rfl⟩
    have ht : likelihood x (x i) ν p.2 ≤ z := hz ⟨p.2,hp.2,rfl⟩
    rcases lt_or_eq_of_le (hp.1 i) with h | h
    · have hl := endpoint_mono x (Nat.zero_lt_of_lt i.isLt) hν hp.2 h hm
      exact (show likelihood x p.1 ν p.2 ≤ likelihood x (x i) ν p.2 by
        rw [likelihood_eq_exp_logLik, likelihood_eq_exp_logLik]
        exact (exp_lt_exp.mpr hl).le).trans ht
    · simpa only [h] using ht

/-- In the fitted all-free domain, a strict majority at the sample
minimum gives an unbounded likelihood. -/
theorem joint_majority_unbounded {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) (h : n < 2 * TDist.multiplicity x (x i)) :
    ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2}) := by
  rintro ⟨z, hz⟩
  have hs : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
  obtain ⟨s, hs, hlt⟩ := (hs.and ((majority_at_floor x h).eventually_gt_atTop z)).exists
  exact hlt.not_ge (hz ⟨(x i,1,s), ⟨hm,le_rfl,hs⟩, rfl⟩)

private theorem scale_profile_eq_if_max {n : ℕ} (x : Fin n → ℝ) (μ ν : ℝ)
    {s : ℝ} (hs : 0 < s) (hm : IsMaxOn (logLik x μ ν) (Ioi 0) s) :
    scaleProfile x μ ν = likelihood x μ ν s := by
  have hp : IsMaxOn (likelihood x μ ν) (Ioi 0) s := by
    intro a ha
    change likelihood x μ ν a ≤ likelihood x μ ν s
    rw [likelihood_eq_exp_logLik, likelihood_eq_exp_logLik]
    exact exp_le_exp.mpr (hm ha)
  exact (hp.isLUB hs).csSup_eq (Nonempty.image _ ⟨s,hs⟩)

/-- The exact positive-scale profile is continuous at every positive
shape with positive collapse exponent. -/
theorem scale_profile_continuous {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) {ν₀ : ℝ}
    (hν₀ : 0 < ν₀) (he₀ : 0 < TDist.exponent x μ ν₀) :
    ContinuousAt (scaleProfile x μ) ν₀ := by
  classical
  have hchoose (ν : ℝ) : ∃ s : ℝ, 0 < ν → 0 < TDist.exponent x μ ν →
      0 < s ∧ TDist.scaleScore x μ ν s = 0 ∧ IsMaxOn (logLik x μ ν) (Ioi 0) s := by
    by_cases h : 0 < ν ∧ 0 < TDist.exponent x μ ν
    · have hr := (TDist.heldLocation_maximum x μ h.1 hx).2 h.2
      obtain ⟨s,hs,_⟩ := hr.1
      exact ⟨s,fun _ _ ↦ ⟨hs.1,hs.2,(scale_max_transfer x μ ν s).mpr ((hr.2 s hs.1).mpr hs.2)⟩⟩
    · exact ⟨1,fun hν he ↦ (h ⟨hν,he⟩).elim⟩
  choose s hs using hchoose
  have hs₀ := hs ν₀ hν₀ he₀
  have hnν : ∀ᶠ ν : ℝ in 𝓝 ν₀, 0 < ν := eventually_gt_nhds hν₀
  have hne : ∀ᶠ ν : ℝ in 𝓝 ν₀, 0 < TDist.exponent x μ ν := by
    have hc : Continuous (TDist.exponent x μ) := by unfold TDist.exponent; fun_prop
    exact hc.continuousAt.tendsto.eventually (eventually_gt_nhds he₀)
  have hsct : ContinuousAt s ν₀ := by
    apply Metric.tendsto_nhds.mpr
    intro ε hε
    let d := min (ε/2) (s ν₀/2)
    have hd : 0 < d := lt_min (half_pos hε) (half_pos hs₀.1)
    have hA : 0 < s ν₀-d := by
      have h := min_le_right (ε/2) (s ν₀/2)
      dsimp [d]
      linarith [hs₀.1]
    have hB : 0 < s ν₀+d := by linarith [hs₀.1]
    have ha := TDist.scaleScore_strictAntiOn x μ hν₀ hx
    have hlow : 0 < TDist.scaleScore x μ ν₀ (s ν₀-d) := by
      rw [← hs₀.2.1]
      exact ha hA hs₀.1 (sub_lt_self _ hd)
    have hhigh : TDist.scaleScore x μ ν₀ (s ν₀+d) < 0 := by
      rw [← hs₀.2.1]
      exact ha hs₀.1 hB (lt_add_of_pos_right _ hd)
    have h₁ := (scaleScore_continuous_shape x μ hν₀ hA).tendsto.eventually (eventually_gt_nhds hlow)
    have h₂ := (scaleScore_continuous_shape x μ hν₀ hB).tendsto.eventually
      (eventually_lt_nhds hhigh)
    filter_upwards [hnν,hne,h₁,h₂] with ν hν he hl hh
    have hsν := hs ν hν he
    have ham := (TDist.scaleScore_strictAntiOn x μ hν hx).antitoneOn
    have hleft : s ν₀-d < s ν := by
      by_contra! h
      have hc := ham hsν.1 hA h
      linarith [hsν.2.1]
    have hright : s ν < s ν₀+d := by
      by_contra! h
      have hc := ham hB hsν.1 h
      linarith [hsν.2.1]
    rw [Real.dist_eq]
    have hlt : d < ε := (min_le_left _ _).trans_lt (by linarith)
    exact abs_lt.mpr (by constructor <;> linarith)
  have hll : ContinuousAt (fun ν ↦ logLik x μ ν (s ν)) ν₀ := by
    unfold logLik TDist.logLik
    apply ContinuousAt.add _ continuousAt_const
    refine tendsto_finsetSum Finset.univ fun i _ ↦ ?_
    have h₁ := (logNormalizer_continuous_pos hν₀).sub (hsct.log hs₀.1.ne')
    have hn : ContinuousAt (fun ν ↦ ν*(s ν)^2) ν₀ := continuousAt_id.mul (hsct.pow 2)
    have hpos : 0 < ν₀*(s ν₀)^2 := by positivity [hs₀.1]
    have h₂ : ContinuousAt (fun ν ↦ 1+(x i-μ)^2/(ν*(s ν)^2)) ν₀ :=
      (continuousAt_const.div hn hpos.ne').const_add (1 : ℝ)
    have hlog : ContinuousAt (fun ν ↦ log (1+(x i-μ)^2/(ν*(s ν)^2))) ν₀ :=
      h₂.log (by positivity)
    have hmul : ContinuousAt (fun ν : ℝ ↦ (ν+1)/2) ν₀ := by fun_prop
    exact (h₁.sub (hmul.mul hlog)).tendsto
  have ht : Tendsto (fun ν ↦ likelihood x μ ν (s ν)) (𝓝 ν₀) (𝓝 (likelihood x μ ν₀ (s ν₀))) := by
    simp only [likelihood_eq_exp_logLik]
    exact (continuous_exp.continuousAt.tendsto.comp hll.tendsto)
  rw [ContinuousAt, scale_profile_eq_if_max x μ ν₀ hs₀.1 hs₀.2.2]
  refine ht.congr' ?_
  filter_upwards [hnν,hne] with ν hν he
  exact (scale_profile_eq_if_max x μ ν (hs ν hν he).1 (hs ν hν he).2.2).symm

/-- The excluded half-normal likelihood at the RMS scale about the
specified endpoint. -/
def normalFace {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  ∏ i, 2*φ ((x i-μ)/rmsScale x μ)/rmsScale x μ

/-- The scale-zero face is relevant at an exact half tie. A zero entry
represents its absence in a positive-likelihood comparison. -/
def collapseFace {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  if TDist.exponent x μ 1 = 0 then collapseValue x μ 1 else 0

private theorem profile_eq_at_collapse_floor {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (μ : ℝ) (he : TDist.exponent x μ 1 = 0) :
    scaleProfile x μ 1 = collapseValue x μ 1 :=
  ((held_endpoint_equality hn x μ zero_lt_one he).2.1).csSup_eq
    (Nonempty.image _ ⟨1,zero_lt_one⟩)

/-- Below or at a half tie, a finite fit in the all-free domain exists
exactly when an eligible finite shape reaches both excluded face values. -/
theorem joint_attainment_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) (hcount : 2 * TDist.multiplicity x (x i) ≤ n) :
    (∃ p ∈ {p : ℝ × (ℝ × ℝ) | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2},
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2} p) ↔
      ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ scaleProfile x (x i) ν := by
  let m := x i
  let P := scaleProfile x m
  let H := normalFace x m
  let C := collapseFace x m
  let k : ℝ := TDist.multiplicity x m
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hk : 2*k ≤ n := by dsimp [k,m]; exact_mod_cast hcount
  have hd : 0 < (n : ℝ)-k := by linarith
  have hx : ∃ j, x j ≠ m := by
    by_contra! hall
    have h : TDist.multiplicity x m = n := by simp [TDist.multiplicity,hall]
    have h' : k = n := by dsimp [k]; exact_mod_cast h
    linarith
  have hdeg {ν : ℝ} (hν : 1 ≤ ν) :
      0 < TDist.exponent x m ν ∨ ν = 1 ∧ TDist.exponent x m 1 = 0 := by
    rcases hν.eq_or_lt with h | h
    · subst ν
      have he : 0 ≤ TDist.exponent x m 1 := by
        change 0 ≤ 1*((n : ℝ)-k)-k
        linarith
      exact he.eq_or_lt.elim (fun h ↦ Or.inr ⟨rfl,h.symm⟩) Or.inl
    · left
      change 0 < ν*((n : ℝ)-k)-k
      linarith only [hk,mul_lt_mul_of_pos_right h hd]
  have hfloor {ν : ℝ} (h : ν = 1 ∧ TDist.exponent x m 1 = 0) : P ν = C := by
    rw [h.1, show C = collapseValue x m 1 by simp [C,collapseFace,h.2]]
    exact profile_eq_at_collapse_floor hn x m h.2
  have hc : ContinuousOn P (Ici 1) := by
    intro ν hν
    rcases hdeg hν with he | he
    · exact (scale_profile_continuous x m hx (zero_lt_one.trans_le hν) he).continuousWithinAt
    · obtain ⟨rfl,he⟩ := he
      apply continuousWithinAt_Ioi_iff_Ici.mp
      change Tendsto P (𝓝[>] (1 : ℝ)) (𝓝 (scaleProfile x m 1))
      rw [profile_eq_at_collapse_floor hn x m he]
      exact profiled_half_tie_limit x m hx he
  have hnormal : Tendsto P atTop (𝓝 H) := profiled_half_normal_limit x m hx
  have hupper (ν : ℝ) (hν : 1 ≤ ν) (s : ℝ) (hs : 0 < s) :
      likelihood x m ν s ≤ P ν := by
    change likelihood x m ν s ≤ scaleProfile x m ν
    rcases hdeg hν with he | ⟨rfl,h⟩
    · obtain ⟨t,⟨ht,hmax⟩,_⟩ := (held_endpoint hn x m (zero_lt_one.trans_le hν)).mpr he
      rw [scale_profile_eq_if_max x m ν ht hmax,likelihood_eq_exp_logLik,likelihood_eq_exp_logLik]
      exact exp_le_exp.mpr (hmax hs)
    · rw [profile_eq_at_collapse_floor hn x m h]
      exact ((held_endpoint_equality hn x m zero_lt_one h).2.2 s hs).le
  constructor
  · rintro ⟨⟨μ,ν,s⟩,⟨hμ,hν,hs⟩,hmax⟩
    have hm' : μ = m := by
      apply le_antisymm (hμ i)
      by_contra! he
      have hcmp := endpoint_mono x hn (zero_lt_one.trans_le hν) hs he hm
      have hbad : likelihood x m ν s ≤ likelihood x μ ν s :=
        isMaxOn_iff.mp hmax (m,ν,s) ⟨hm,hν,hs⟩
      rw [likelihood_eq_exp_logLik,likelihood_eq_exp_logLik] at hbad
      exact (exp_lt_exp.mpr hcmp).not_ge hbad
    subst μ
    have hsmax : IsMaxOn (logLik x m ν) (Ioi 0) s := by
      intro t ht
      have hh : likelihood x m ν t ≤ likelihood x m ν s :=
        isMaxOn_iff.mp hmax (m,ν,t) ⟨hm,hν,ht⟩
      rw [likelihood_eq_exp_logLik,likelihood_eq_exp_logLik] at hh
      exact exp_le_exp.mp hh
    have he := (held_endpoint_exists hn x m (zero_lt_one.trans_le hν)).mp ⟨s,hs,hsmax⟩
    have hp : P ν = likelihood x m ν s := scale_profile_eq_if_max x m ν hs hsmax
    have htrace (u : ℝ) (hu : 1 ≤ u) : P u ≤ likelihood x m ν s := by
      apply csSup_le (Nonempty.image _ ⟨1,zero_lt_one⟩)
      rintro _ ⟨t,ht,rfl⟩
      exact isMaxOn_iff.mp hmax (m,u,t) ⟨hm,hu,ht⟩
    refine ⟨ν,hν,he,?_⟩
    change max H C ≤ P ν
    rw [hp]
    apply max_le
    · exact le_of_tendsto hnormal ((eventually_ge_atTop 1).mono (fun u hu ↦ htrace u hu))
    · by_cases h : TDist.exponent x m 1 = 0
      · rw [← hfloor ⟨rfl,h⟩]
        exact htrace 1 le_rfl
      · have hp' : 0 < likelihood x m ν s := by unfold likelihood TDist.likelihood; positivity
        simpa [C,collapseFace,h] using hp'.le
  · rintro ⟨ν₀,hν₀,he₀,h₀⟩
    have hchoose : ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x m ν ∧
        IsMaxOn P (Ici 1) ν := by
      by_cases h : ∃ ν ≥ (1 : ℝ), max H C < P ν
      · obtain ⟨ν₁,hν₁,hbetter⟩ := h
        obtain ⟨R,hR⟩ := eventually_atTop.mp (hnormal.eventually
          (eventually_lt_nhds ((le_max_left H C).trans_lt hbetter)))
        let K := Icc 1 (max R ν₁)
        have h₁ : ν₁ ∈ K := ⟨hν₁,le_max_right _ _⟩
        obtain ⟨v,hv,hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨ν₁,h₁⟩
          (hc.mono fun _ h ↦ h.1)
        have hgood : max H C < P v := hbetter.trans_le (isMaxOn_iff.mp hmax ν₁ h₁)
        have he : 0 < TDist.exponent x m v := (hdeg hv.1).resolve_right
          (fun hh ↦ ((le_max_right H C).trans_lt hgood).ne (hfloor hh).symm)
        refine ⟨v,hv.1,he,fun u hu ↦ ?_⟩
        rcases le_or_gt u (max R ν₁) with hu' | hu'
        · exact hmax ⟨hu,hu'⟩
        · exact (hR u ((le_max_left _ _).trans hu'.le)).le.trans (hmax h₁)
      · exact ⟨ν₀,hν₀,he₀,fun u hu ↦ (le_of_not_gt (fun hu' ↦ h ⟨u,hu,hu'⟩)).trans h₀⟩
    obtain ⟨ν,hν,he,hmax⟩ := hchoose
    obtain ⟨s,⟨hs,hsmax⟩,_⟩ := (held_endpoint hn x m (zero_lt_one.trans_le hν)).mpr he
    refine ⟨(m,ν,s),⟨hm,hν,hs⟩,?_⟩
    rintro ⟨μ,u,t⟩ ⟨hμ,hu,ht⟩
    change likelihood x μ u t ≤ likelihood x m ν s
    rw [← scale_profile_eq_if_max x m ν hs hsmax]
    refine le_trans ?_ ((hupper u hu t ht).trans (isMaxOn_iff.mp hmax u hu))
    rcases lt_or_eq_of_le (hμ i) with hl | hl
    · rw [likelihood_eq_exp_logLik,likelihood_eq_exp_logLik]
      exact (exp_lt_exp.mpr (endpoint_mono x hn (zero_lt_one.trans_le hu) ht hl hm)).le
    · rw [show μ = m from hl]

end

end UnivariateML.SkewLSHalfT
