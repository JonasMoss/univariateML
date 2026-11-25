module

public import Mathlib.Analysis.Convex.Continuous
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp
public import Mathlib.Topology.Order.Compact
public import UnivariateML.Generic.Profile

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Kumaraswamy likelihood

Both shape parameters are positive. We use the log-transformed observations
`t i = -log (x i)` in the likelihood.

## Deviations

The sample is explicitly nonempty. The logarithmic curvature bound uses a
finite Cauchy–Schwarz inequality and `q ≤ -log (1 - q)`, bypassing the
infinite-series differentiation in the dossier.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Kumaraswamy

noncomputable section

variable {n : ℕ}

def t (x : Fin n → ℝ) (i : Fin n) : ℝ := -log (x i)

def G (x : Fin n → ℝ) (a : ℝ) : ℝ :=
  ∑ i, -log (1 - exp (-(a * t x i)))

def logLik (x : Fin n → ℝ) (a b : ℝ) : ℝ :=
  n * (log a + log b) - (a - 1) * (∑ i, t x i) - (b - 1) * G x a

def shape (x : Fin n → ℝ) (a : ℝ) : ℝ := n / G x a

def profile (x : Fin n → ℝ) (a : ℝ) : ℝ := logLik x a (shape x a)

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem t_pos {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) (i : Fin n) :
    0 < t x i := neg_pos.mpr (log_neg (hx i).1 (hx i).2)

private theorem q_lt_one {a s : ℝ} (ha : 0 < a) (hs : 0 < s) :
    exp (-(a * s)) < 1 := exp_lt_one_iff.mpr (neg_neg_of_pos (mul_pos ha hs))

private theorem q_le_neg_log {q : ℝ} (hq : q < 1) : q ≤ -log (1 - q) := by
  have h := log_le_sub_one_of_pos (sub_pos.mpr hq)
  linarith

private theorem G_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) : 0 < G x a := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact Finset.sum_pos (fun i _ ↦ (exp_pos _).trans_le
    (q_le_neg_log (q_lt_one ha (t_pos hx i)))) Finset.univ_nonempty

/-- The transformed objective sums the log densities in the original observations. -/
theorem logLik_eq_sum (x : Fin n → ℝ) (hx : ∀ i, x i ∈ Ioo 0 1) (a b : ℝ) :
    logLik x a b = ∑ i, (log a + log b + (a - 1) * log (x i) +
      (b - 1) * log (1 - (x i) ^ a)) := by
  have h (i : Fin n) : (x i) ^ a = exp (-(a * t x i)) := by
    rw [rpow_def_of_pos (hx i).1, t, mul_neg, neg_neg, mul_comm]
  simp only [logLik, G, h, Finset.sum_add_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum, Finset.sum_neg_distrib, t]
  ring

/-- For each held positive first shape, the closed-form second shape is feasible
and strictly beats every other positive second shape. -/
theorem held_a (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) :
    0 < shape x a ∧ ∀ b > 0, b ≠ shape x a → logLik x a b < profile x a := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hG := G_pos hn hx ha
  have hs : 0 < shape x a := div_pos hN hG
  refine ⟨hs, fun b hb hne ↦ ?_⟩
  have he : shape x a * G x a = n := div_mul_cancel₀ _ hG.ne'
  have h := log_lt_sub_one_of_pos (div_pos hb hs)
    (show b / shape x a ≠ 1 by simpa [div_eq_iff hs.ne'] using hne)
  rw [log_div hb.ne' hs.ne'] at h
  have h' : (n : ℝ) * (log b - log (shape x a)) < b * G x a - n := by
    calc
      _ < n * (b / shape x a - 1) := mul_lt_mul_of_pos_left h hN
      _ = _ := by rw [← he, (show shape x a * G x a * (b / shape x a - 1) =
           b * G x a - shape x a * G x a by field_simp)]
  unfold profile logLik
  linarith only [h', he]

private def g₁ (s a : ℝ) : ℝ := -s * exp (-(a * s)) / (1 - exp (-(a * s)))

private def g₂ (s a : ℝ) : ℝ := s ^ 2 * exp (-(a * s)) / (1 - exp (-(a * s))) ^ 2

private theorem q_deriv (s a : ℝ) :
    HasDerivAt (fun v ↦ exp (-(v * s))) (-s * exp (-(a * s))) a := by
  simpa [mul_comm] using (((hasDerivAt_id a).mul_const s).neg.exp)

private theorem g_deriv {s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    HasDerivAt (fun v ↦ -log (1 - exp (-(v * s)))) (g₁ s a) a := by
  have h := ((q_deriv s a).const_sub 1).log (sub_pos.mpr (q_lt_one ha hs)).ne'
  convert h.neg using 1
  unfold g₁
  ring

private theorem g₁_deriv {s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    HasDerivAt (g₁ s) (g₂ s a) a := by
  have h := ((q_deriv s a).const_mul (-s)).div ((q_deriv s a).const_sub 1)
    (sub_pos.mpr (q_lt_one ha hs)).ne'
  convert h using 1
  · rfl
  · unfold g₂
    ring

private def G₁ (x : Fin n → ℝ) (a : ℝ) : ℝ := ∑ i, g₁ (t x i) a

private def G₂ (x : Fin n → ℝ) (a : ℝ) : ℝ := ∑ i, g₂ (t x i) a

private theorem G_deriv {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) {a : ℝ} (ha : 0 < a) :
    HasDerivAt (G x) (G₁ x a) a :=
  HasDerivAt.fun_sum fun i _ ↦ g_deriv (t_pos hx i) ha

private theorem G₁_deriv {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) {a : ℝ} (ha : 0 < a) :
    HasDerivAt (G₁ x) (G₂ x a) a :=
  HasDerivAt.fun_sum fun i _ ↦ g₁_deriv (t_pos hx i) ha

private theorem g₂_pos {s a : ℝ} (hs : 0 < s) (ha : 0 < a) : 0 < g₂ s a := by
  unfold g₂
  exact div_pos (mul_pos (sq_pos_of_pos hs) (exp_pos _))
    (sq_pos_of_pos (sub_pos.mpr (q_lt_one ha hs)))

private theorem g₂_lt {s a : ℝ} (hs : 0 < s) (ha : 0 < a) : g₂ s a < 1 / a ^ 2 := by
  let u := a * s / 2
  have hu : 0 < u := by positivity
  have h := self_lt_sinh_iff.mpr hu
  rw [sinh_eq] at h
  have hq : exp (-u) < 1 := exp_lt_one_iff.mpr (by linarith)
  have hp := exp_pos (-u)
  have he : exp (-u) * exp u = 1 := by rw [← exp_add]; simp
  have hq₂ : exp (-(a * s)) = exp (-u) ^ 2 := by
    rw [pow_two, ← exp_add]
    congr 1
    dsimp [u]
    ring
  have hb : a * s * exp (-u) < 1 - exp (-u) ^ 2 := by
    have hh := mul_lt_mul_of_pos_left h hp
    have heq : exp (-u) * (exp u - exp (-u)) = 1 - exp (-u) ^ 2 := by
      rw [mul_sub, he, pow_two]
    dsimp [u] at hh
    nlinarith only [hh, heq]
  have hsq := sq_lt_sq' (show -(1 - exp (-u) ^ 2) < a * s * exp (-u) by
    have := mul_pos (mul_pos ha hs) hp
    nlinarith only [this, hb]) hb
  unfold g₂
  rw [div_lt_div_iff₀ (sq_pos_of_pos (sub_pos.mpr (q_lt_one ha hs))) (sq_pos_of_pos ha), hq₂]
  convert hsq using 1 <;> ring

private theorem G₂_bounds (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) : 0 < G₂ x a ∧ G₂ x a < n / a ^ 2 := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  refine ⟨Finset.sum_pos (fun i _ ↦ g₂_pos (t_pos hx i) ha) Finset.univ_nonempty, ?_⟩
  calc
    G₂ x a < ∑ _i : Fin n, (1 / a ^ 2) :=
      Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty (fun i _ ↦ g₂_lt (t_pos hx i) ha)
    _ = _ := by simp [div_eq_mul_inv]

private theorem G_log_curvature (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) : 0 ≤ G₂ x a * G x a - G₁ x a ^ 2 := by
  have h := Finset.sum_sq_le_sum_mul_sum_of_sq_le_mul Finset.univ
    (fun i _ ↦ (g₂_pos (t_pos hx i) ha).le)
    (g := fun i ↦ exp (-(a * t x i))) (fun i _ ↦ (exp_pos _).le)
    (r := fun i ↦ g₁ (t x i) a) (fun i _ ↦ by
      unfold g₁ g₂
      rw [div_pow, mul_pow, neg_sq]
      ring_nf
      exact le_rfl)
  have hsum : ∑ i, exp (-(a * t x i)) ≤ G x a :=
    Finset.sum_le_sum fun i _ ↦ q_le_neg_log (q_lt_one ha (t_pos hx i))
  exact sub_nonneg.mpr (h.trans (mul_le_mul_of_nonneg_left hsum (G₂_bounds hn hx ha).1.le))

private theorem profile_eq (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) :
    profile x a = n * log a - n * log (G x a) - a * (∑ i, t x i) + G x a +
      (n * log n + (∑ i, t x i) - n) := by
  have hG := G_pos hn hx ha
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold profile shape logLik
  rw [log_div hN hG.ne']
  field_simp
  ring

private theorem deriv2_neg_of_calculation {f score : ℝ → ℝ}
    (hf : ∀ a > 0, HasDerivAt f (score a) a)
    (hs : ∀ a > 0, ∃ v < 0, HasDerivAt score v a) :
    StrictConcaveOn ℝ (Ioi 0) f := by
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioi 0)
    (fun a ha ↦ (hf a ha).continuousAt.continuousWithinAt)
  intro a ha
  obtain ⟨v, hv, hd⟩ := hs a ha
  have he : deriv f =ᶠ[𝓝 a] score :=
    (show ∀ᶠ y in 𝓝 a, y ∈ Ioi (0 : ℝ) from Ioi_mem_nhds ha).mono fun y hy ↦ (hf y hy).deriv
  change deriv (deriv f) a < 0
  rwa [he.deriv_eq, hd.deriv]

/-- The profile is strictly concave on the positive shape axis,
including for constant samples. -/
theorem profile_strictConcave (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo 0 1) : StrictConcaveOn ℝ (Ioi 0) (profile x) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  apply deriv2_neg_of_calculation (score := fun a ↦
    n * a⁻¹ - n * (G₁ x a / G x a) - (∑ i, t x i) + G₁ x a)
  · intro a ha
    have hG := G_pos hn hx ha
    have hd := ((((hasDerivAt_log ha.ne').const_mul (n : ℝ)).sub
      (((G_deriv hx ha).log hG.ne').const_mul (n : ℝ))).sub
      ((hasDerivAt_id a).mul_const (∑ i, t x i))).add (G_deriv hx ha)
    have he : (fun y ↦ n * log y - n * log (G x y) - y * (∑ i, t x i) + G x y +
      (n * log n + (∑ i, t x i) - n)) =ᶠ[𝓝 a] profile x :=
      (show ∀ᶠ y in 𝓝 a, y ∈ Ioi (0 : ℝ) from Ioi_mem_nhds ha).mono fun y hy ↦ (profile_eq hn hx hy).symm
    simpa using (hd.add_const _).congr_of_eventuallyEq he.symm
  · intro a ha
    have hG := G_pos hn hx ha
    have hd := ((((hasDerivAt_inv ha.ne').const_mul (n : ℝ)).sub
      (((G₁_deriv hx ha).div (G_deriv hx ha) hG.ne').const_mul (n : ℝ))).sub_const
      (∑ i, t x i)).add (G₁_deriv hx ha)
    refine ⟨_, ?_, hd⟩
    have hc := div_nonneg (G_log_curvature hn hx ha) (sq_nonneg (G x a))
    have hb := (G₂_bounds hn hx ha).2
    change n * (-(a ^ 2)⁻¹) - n * ((G₂ x a * G x a - G₁ x a * G₁ x a) / G x a ^ 2) +
      G₂ x a < 0
    rw [← sq, mul_neg, ← div_eq_mul_inv]
    linarith only [hb, mul_nonneg hN.le hc]

private def R (x : Fin n → ℝ) (a : ℝ) : ℝ :=
  ∑ i, log ((1 - exp (-(a * t x i))) / a)

private theorem R_eq {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) {a : ℝ} (ha : 0 < a) :
    R x a = -G x a - n * log a := by
  unfold R G
  simp only [log_div (sub_pos.mpr (q_lt_one ha (t_pos hx _))).ne' ha.ne',
    Finset.sum_sub_distrib, Finset.sum_neg_distrib, neg_neg, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

private theorem R_tendsto {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) :
    Tendsto (R x) (𝓝[>] 0) (𝓝 (∑ i, log (t x i))) := by
  refine tendsto_finsetSum _ fun i _ ↦ (continuousAt_log (t_pos hx i).ne').tendsto.comp ?_
  have hd := (q_deriv (t x i) 0).const_sub 1
  simpa [div_eq_mul_inv, mul_comm] using hd.tendsto_slope_zero_right

private theorem G_tendsto_zero {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) :
    Tendsto (G x) atTop (𝓝 0) := by
  have h (i : Fin n) : Tendsto (fun a ↦ -log (1 - exp (-(a * t x i)))) atTop (𝓝 0) := by
    have hq := tendsto_exp_atBot.comp
      (tendsto_neg_atTop_atBot.comp (tendsto_id.atTop_mul_const (t_pos hx i)))
    have hsub := tendsto_const_nhds (x := (1 : ℝ)) |>.sub hq
    simpa using ((continuousAt_log one_ne_zero).tendsto.comp (by simpa using hsub)).neg
  change Tendsto (fun a ↦ ∑ i, -log (1 - exp (-(a * t x i)))) atTop (𝓝 0)
  simpa only [Finset.sum_const_zero] using tendsto_finsetSum Finset.univ (fun i _ ↦ h i)

private theorem linear_tendsto_zero (c : ℝ) :
    Tendsto (fun a : ℝ ↦ a * c) (𝓝[>] 0) (𝓝 0) := by
  have h : Tendsto id (𝓝[>] (0 : ℝ)) (𝓝 0) := nhdsWithin_le_nhds
  simpa using h.mul_const c

/-- The joint profile goes to minus infinity at zero for every interior sample. -/
theorem profile_tendsto_zero (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo 0 1) : Tendsto (profile x) (𝓝[>] 0) atBot := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hG : Tendsto (G x) (𝓝[>] 0) atTop := by
    refine (tendsto_neg_atBot_atTop.comp (tendsto_log_nhdsGT_zero.const_mul_atBot hN)).atTop_add
      (R_tendsto hx).neg |>.congr' ?_
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)) (a := 0)] with a ha
    have h := R_eq hx ha
    dsimp
    linarith
  have h := (tendsto_neg_atTop_atBot.comp ((tendsto_log_atTop.comp hG).const_mul_atTop hN)).atBot_add
    ((((linear_tendsto_zero (∑ i, t x i)).neg.sub (R_tendsto hx))).const_add
      ((n : ℝ) * log n + (∑ i, t x i) - n))
  refine h.congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)) (a := 0)] with a ha
  rw [profile_eq hn hx ha]
  dsimp
  linarith only [R_eq hx ha]

private theorem t_ne {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {i j : Fin n} (h : x i ≠ x j) : t x i ≠ t x j := by
  intro he
  exact h (strictMonoOn_log.injOn (hx i).1 (hx j).1 (neg_injective he))

/-- The profile goes to minus infinity at infinity when two observations differ. -/
theorem profile_tendsto_atTop (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) : Tendsto (profile x) atTop atBot := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  obtain ⟨m, hm⟩ : ∃ m, ∀ i, t x m ≤ t x i := by
    obtain ⟨m, _, hm⟩ := Finset.exists_min_image Finset.univ (t x) Finset.univ_nonempty
    exact ⟨m, fun i ↦ hm i (Finset.mem_univ _)⟩
  let δ := (∑ i, t x i) - n * t x m
  have hδ : 0 < δ := by
    obtain ⟨i, j, hij⟩ := hne
    have hh : ∃ k, t x m < t x k := by
      by_cases hi : t x m = t x i
      · exact ⟨j, (hm j).lt_of_ne (by simpa [hi] using t_ne hx hij)⟩
      · exact ⟨i, (hm i).lt_of_ne hi⟩
    obtain ⟨k, hk⟩ := hh
    have h := Finset.sum_lt_sum (s := (Finset.univ : Finset (Fin n)))
      (fun i _ ↦ hm i) ⟨k, Finset.mem_univ _, hk⟩
    simpa [δ, sub_pos] using h
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hlog := ((isLittleO_log_id_atTop.const_mul_left (n : ℝ)).bound
    (show 0 < δ / 2 by positivity))
  have hlim := (tendsto_id.const_mul_atTop_of_neg (show -δ / 2 < 0 by linarith)).atBot_add
    ((G_tendsto_zero hx).const_add ((n : ℝ) * log n + (∑ i, t x i) - n))
  refine tendsto_atBot_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ), hlog] with a ha hlog
  have hqG : exp (-(a * t x m)) ≤ G x a :=
    (q_le_neg_log (q_lt_one ha (t_pos hx m))).trans
      (Finset.single_le_sum (fun i _ ↦ (exp_pos _).le.trans
        (q_le_neg_log (q_lt_one ha (t_pos hx i)))) (Finset.mem_univ m))
  have hl := log_le_log (exp_pos _) hqG
  rw [log_exp] at hl
  have hla : n * log a ≤ δ / 2 * a := by
    rw [Real.norm_eq_abs, Real.norm_eq_abs, id_eq, abs_of_pos ha] at hlog
    exact (le_abs_self _).trans hlog
  rw [profile_eq hn hx ha]
  have hh := mul_le_mul_of_nonneg_left hl hN.le
  dsimp [δ] at *
  nlinarith only [hla, hh]

private theorem held_b_strictConcave (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo 0 1) {b : ℝ} (hb : 0 < b) :
    StrictConcaveOn ℝ (Ioi 0) (fun a ↦ logLik x a b) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  apply deriv2_neg_of_calculation (score := fun a ↦
    n * a⁻¹ - (∑ i, t x i) - (b - 1) * G₁ x a)
  · intro a ha
    simpa [logLik, Pi.sub_def] using ((((hasDerivAt_log ha.ne').add_const (log b)).const_mul
      (n : ℝ)).sub (((hasDerivAt_id a).sub_const 1).mul_const (∑ i, t x i))).sub
      ((G_deriv hx ha).const_mul (b - 1))
  · intro a ha
    have hd := (((hasDerivAt_inv ha.ne').const_mul (n : ℝ)).sub_const
      (∑ i, t x i)).sub ((G₁_deriv hx ha).const_mul (b - 1))
    refine ⟨_, ?_, hd⟩
    have ⟨hpos, hlt⟩ := G₂_bounds hn hx ha
    have hbase : 0 < (n : ℝ) / a ^ 2 := by positivity
    change n * (-(a ^ 2)⁻¹) - (b - 1) * G₂ x a < 0
    rw [mul_neg, ← div_eq_mul_inv]
    nlinarith only [hlt, mul_pos hb hpos]

private theorem existsUnique_of_endpoints {f : ℝ → ℝ}
    (hc : StrictConcaveOn ℝ (Ioi 0) f)
    (hzero : Tendsto f (𝓝[>] 0) atBot) (htop : Tendsto f atTop atBot) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn f (Ioi 0) a := by
  have hf : ContinuousOn f (Ioi 0) := hc.concaveOn.continuousOn isOpen_Ioi
  obtain ⟨l, hl, hz⟩ := (nhdsGT_basis (0 : ℝ)).eventually_iff.mp (hzero.eventually_le_atBot (f 1))
  obtain ⟨r, hr⟩ := eventually_atTop.mp (htop.eventually_le_atBot (f 1))
  let lower := min (l / 2) 1
  let upper := max r 1
  have hlow : 0 < lower := lt_min (by positivity) zero_lt_one
  have h1 : (1 : ℝ) ∈ Icc lower upper := ⟨min_le_right .., le_max_right ..⟩
  have hsub : Icc lower upper ⊆ Ioi 0 := fun _ hi ↦ hlow.trans_le hi.1
  obtain ⟨a, ha, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩ (hf.mono hsub)
  have hmax : IsMaxOn f (Ioi 0) a := by
    intro y hy
    by_cases hy' : y ∈ Icc lower upper
    · exact hm hy'
    · apply le_trans _ (hm h1)
      rw [mem_Icc, not_and_or, not_le, not_le] at hy'
      rcases hy' with h | h
      · exact hz ⟨hy, h.trans_le ((min_le_left ..).trans (by linarith))⟩
      · exact hr y ((le_max_left ..).trans h.le)
  exact ⟨a, ⟨hsub ha, hmax⟩, fun y hy ↦ hc.eq_of_isMaxOn hy.2 hmax hy.1 (hsub ha)⟩

/-- Every positive held second shape gives a unique first-shape maximum,
including for constant data. -/
theorem held_b (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {b : ℝ} (hb : 0 < b) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a b) (Ioi 0) a := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hT : 0 < ∑ i, t x i := by
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    exact Finset.sum_pos (fun i _ ↦ t_pos hx i) Finset.univ_nonempty
  apply existsUnique_of_endpoints (held_b_strictConcave hn hx hb)
  · refine (tendsto_log_nhdsGT_zero.const_mul_atBot (mul_pos hN hb)).atBot_add
      ((((R_tendsto hx).const_mul (b - 1)).sub
        (linear_tendsto_zero (∑ i, t x i))).const_add
        ((n : ℝ) * log b + (∑ i, t x i))) |>.congr' ?_
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)) (a := 0)] with a ha
    dsimp [logLik]
    rw [R_eq hx ha]
    ring
  · have hlog := ((isLittleO_log_id_atTop.const_mul_left (n : ℝ)).bound
      (show 0 < (∑ i, t x i) / 2 by positivity))
    have hlim := (tendsto_id.const_mul_atTop_of_neg
      (show -(∑ i, t x i) / 2 < 0 by linarith)).atBot_add
      (((G_tendsto_zero hx).const_mul (1 - b)).const_add
        ((n : ℝ) * log b + (∑ i, t x i)))
    refine tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ), hlog] with a ha hlog
    have hla : n * log a ≤ (∑ i, t x i) / 2 * a := by
      rw [Real.norm_eq_abs, Real.norm_eq_abs, id_eq, abs_of_pos ha] at hlog
      exact (le_abs_self _).trans hlog
    dsimp [logLik]
    linarith only [hla]

/-- A nonconstant interior sample has exactly one feasible joint maximizer. -/
theorem existsUnique_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  have hf : ∀ a ∈ Ioi 0, shape x a ∈ Ioi 0 ∧ IsMaxOn (logLik x a) (Ioi 0) (shape x a) := by
    intro a ha
    obtain ⟨hp, hs⟩ := held_a hn hx ha
    refine ⟨hp, fun b hb ↦ ?_⟩
    by_cases he : b = shape x a
    · simp [he]
    · exact (hs b hb he).le
  have hu : ∀ a ∈ Ioi 0, ∀ b ∈ Ioi 0, IsMaxOn (logLik x a) (Ioi 0) b → b = shape x a := by
    intro a ha b hb hm
    obtain ⟨hp, hs⟩ := held_a hn hx ha
    by_contra he
    exact (hs b hb he).not_ge (hm hp)
  obtain ⟨a, ⟨ha, hm⟩, huniq⟩ := existsUnique_of_endpoints (profile_strictConcave hn hx)
    (profile_tendsto_zero hn hx) (profile_tendsto_atTop hn hx hne)
  have hg := Generic.joint_maximizers_eq_profile_graph hf hu
  refine ⟨(a, shape x a), Generic.isMaxOn_prod_of_profile hf ha hm, ?_⟩
  intro p hp
  have hp' : p ∈ {p ∈ Ioi 0 ×ˢ Ioi 0 | IsMaxOn (fun q ↦ logLik x q.1 q.2)
      (Ioi 0 ×ˢ Ioi 0) p} := hp
  rw [hg] at hp'
  obtain ⟨y, hy, rfl⟩ := hp'
  rw [huniq y hy]

/-- The log-likelihood for a constant sample diverges along the dossier path
`b = c ^ (-a)`. -/
theorem constant_tendsto (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo 0 1) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ c) a (c ^ (-a))) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let s := -log c
  have hs : 0 < s := neg_pos.mpr (log_neg hc.1 hc.2)
  have hq := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_id.atTop_mul_const hs))
  have hlim := tendsto_atTop_add_const_right _ ((n : ℝ) * s - 2 * n)
    (tendsto_log_atTop.const_mul_atTop hN)
  refine tendsto_atTop_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ), hq.eventually (Iio_mem_nhds
    (show (0 : ℝ) < 1 / 2 by norm_num))] with a ha hq'
  let q := exp (-(a * s))
  have hq0 : 0 < q := exp_pos _
  have hq1 : q < 1 := q_lt_one ha hs
  have hqhalf : q < 1 / 2 := hq'
  have hlog := log_le_sub_one_of_pos (inv_pos.mpr (sub_pos.mpr hq1))
  rw [log_inv] at hlog
  have hbound : -log (1 - q) ≤ 2 * q := by
    calc
      _ ≤ (1 - q)⁻¹ - 1 := hlog
      _ = q / (1 - q) := by field_simp; ring
      _ ≤ _ := (div_le_iff₀ (sub_pos.mpr hq1)).2 (by nlinarith only [hqhalf, hq0])
  have hb : c ^ (-a) = exp (a * s) := by
    rw [rpow_def_of_pos hc.1]
    congr 1
    dsimp [s]
    ring
  have he : exp (a * s) * q = 1 := by
    dsimp [q]
    rw [← exp_add, add_neg_cancel, exp_zero]
  have hG : G (fun _ : Fin n ↦ c) a = n * -log (1 - q) := by
    simp [G, t, q, s]
  have hProd : (exp (a * s) - 1) * G (fun _ : Fin n ↦ c) a ≤ 2 * n := by
    have hGpos : 0 < G (fun _ : Fin n ↦ c) a := G_pos hn (x := fun _ ↦ c) (fun _ ↦ hc) ha
    calc
      _ ≤ exp (a * s) * G (fun _ : Fin n ↦ c) a := by linarith
      _ ≤ exp (a * s) * (n * (2 * q)) := by
        rw [hG]
        exact mul_le_mul_of_nonneg_left (mul_le_mul_of_nonneg_left hbound hN.le) (exp_pos _).le
      _ = 2 * n := by linear_combination (2 * (n : ℝ)) * he
  rw [hb, logLik, log_exp]
  have ht : (∑ i : Fin n, t (fun _ : Fin n ↦ c) i) = n * s := by simp [t, s]
  rw [ht]
  linarith only [hProd]

/-- Constant interior data give a likelihood unbounded above. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo 0 1) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ c) p.1 p.2) '' domain) := by
  rintro ⟨M, hM⟩
  obtain ⟨a, ha, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((constant_tendsto hn hc).eventually_gt_atTop M)).exists
  exact hgt.not_ge (hM ⟨(a, c ^ (-a)), ⟨ha, rpow_pos_of_pos hc.1 _⟩, rfl⟩)

end
end UnivariateML.Kumaraswamy
