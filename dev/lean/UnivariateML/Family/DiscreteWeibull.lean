module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.ExpDeriv
public import Mathlib.Topology.Order.Compact

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Type I discrete Weibull likelihood

The coordinates are `α = log (-log q)` and positive shape `β`.
Sample criteria concern nonempty vectors of natural numbers.

## Deviations

The range criterion is expressed by two sample indices with counts separated
by at least two. The compact-box proof uses the weaker derivative bound
`1` for the endpoint survival function, which still gives a positive lower
bound on shape. The module concerns exact-real likelihoods.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.DiscreteWeibull

noncomputable section

variable {n : ℕ}

def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

def endpoint (k : ℕ) (p : ℝ × ℝ) : ℝ :=
  p.1 + p.2 * log (k : ℝ)

def survival (k : ℕ) (p : ℝ × ℝ) : ℝ :=
  if k = 0 then 1 else exp (-exp (endpoint k p))

def mass (k : ℕ) (p : ℝ × ℝ) : ℝ := survival k p - survival (k + 1) p

def logLik (x : Fin n → ℕ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, log (mass (x i) p)

/-- In the public probability coordinate the mass is the stated difference of powers. -/
theorem mass_eq_rpow {q β : ℝ} (hq : 0 < q) (hq1 : q < 1) (hβ : 0 < β)
    (k : ℕ) :
    mass k (log (-log q), β) = q ^ ((k : ℝ) ^ β) - q ^ (((k + 1 : ℕ) : ℝ) ^ β) := by
  have hlog : 0 < -log q := neg_pos.mpr (log_neg hq hq1)
  have hs (j : ℕ) : survival j (log (-log q), β) = q ^ ((j : ℝ) ^ β) := by
    by_cases hj : j = 0
    · subst j
      simp [survival, zero_rpow hβ.ne']
    · have hj' : 0 < (j : ℝ) := by exact_mod_cast Nat.pos_of_ne_zero hj
      simp only [survival, hj, ite_false, endpoint, exp_add, exp_log hlog, rpow_def_of_pos hq,
        rpow_def_of_pos hj']
      congr 1
      ring_nf
  simp only [mass, hs]

private theorem endpoint_gap_pos {k : ℕ} (hk : 0 < k) :
    0 < log ((k + 1 : ℕ) : ℝ) - log (k : ℝ) := by
  apply sub_pos.mpr
  apply log_lt_log (by exact_mod_cast hk)
  exact_mod_cast Nat.lt_succ_self k

private theorem mass_pos {p : ℝ × ℝ} (hp : p ∈ domain) (k : ℕ) :
    0 < mass k p := by
  have hb : 0 < p.2 := hp.2
  rcases k with _ | k
  · simp [mass, survival, sub_pos, exp_lt_one_iff, exp_pos]
  · simp only [mass, survival, Nat.succ_ne_zero, ite_false, sub_pos, exp_lt_exp,
      neg_lt_neg_iff]
    unfold endpoint
    have := mul_pos hb (endpoint_gap_pos (Nat.succ_pos k))
    linarith [mul_sub p.2 (log (↑(k.succ + 1) : ℝ)) (log (k.succ : ℝ))]

private theorem survival_pos (k : ℕ) (p : ℝ × ℝ) : 0 < survival k p := by
  unfold survival
  split <;> positivity

private theorem survival_le_one (k : ℕ) (p : ℝ × ℝ) : survival k p ≤ 1 := by
  unfold survival
  split
  · rfl
  · exact exp_le_one_iff.mpr (neg_nonpos.mpr (exp_pos _).le)

private theorem mass_lt_one (k : ℕ) (p : ℝ × ℝ) : mass k p < 1 :=
  (sub_lt_self _ (survival_pos (k + 1) p)).trans_le (survival_le_one k p)

private def h (s : ℝ) : ℝ := log (1 - exp (-s))
private def g (v : ℝ) : ℝ := h (exp v)

private theorem hasDeriv_g (v : ℝ) :
    HasDerivAt g (exp v / (exp (exp v) - 1)) v := by
  have he : 0 < exp (exp v) - 1 := sub_pos.mpr (one_lt_exp_iff.mpr (exp_pos _))
  have hn : 0 < 1 - exp (-exp v) :=
    sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (exp_pos _)))
  have hd := (((hasDerivAt_exp v).neg.exp).const_sub 1).log hn.ne'
  convert hd using 1
  · rfl
  change _ = -(exp (-exp v) * -exp v) / (1 - exp (-exp v))
  rw [exp_neg]
  field_simp [he.ne', (exp_pos (exp v)).ne']

private theorem g_strict : StrictConcaveOn ℝ univ g := by
  apply StrictAntiOn.strictConcaveOn_of_deriv convex_univ
  · exact (continuous_iff_continuousAt.mpr fun x ↦ (hasDeriv_g x).continuousAt).continuousOn
  · simp only [interior_univ]
    intro x _ y _ hxy
    rw [(hasDeriv_g x).deriv, (hasDeriv_g y).deriv]
    have hx := exp_pos x
    have hy := exp_pos y
    have hlt := exp_lt_exp.mpr hxy
    let b := exp x / exp y
    have hb : 0 < b := div_pos hx hy
    have ha : 0 < 1 - b := sub_pos.mpr ((div_lt_one hy).mpr hlt)
    have hby : b * exp y = exp x := div_mul_cancel₀ _ hy.ne'
    have hs := strictConvexOn_exp.2 (mem_univ (0 : ℝ)) (mem_univ (exp y))
      (ne_of_lt hy) ha hb (sub_add_cancel 1 b)
    simp only [smul_eq_mul, mul_zero, zero_add, exp_zero, mul_one, hby] at hs
    apply (div_lt_div_iff₀ (sub_pos.mpr (one_lt_exp_iff.mpr hy))
      (sub_pos.mpr (one_lt_exp_iff.mpr hx))).mpr
    have hh : exp (exp x) - 1 < b * (exp (exp y) - 1) := by linarith
    have hm := mul_lt_mul_of_pos_right hh hy
    rwa [mul_right_comm, hby, mul_comm] at hm

private theorem g_mono : StrictMono g := by
  intro x y hxy
  apply log_lt_log
  · exact sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (exp_pos _)))
  · exact sub_lt_sub_left (exp_lt_exp.mpr (neg_lt_neg (exp_lt_exp.mpr hxy))) _

private theorem log_mass_zero (p : ℝ × ℝ) : log (mass 0 p) = g p.1 := by
  simp [mass, survival, endpoint, g, h]

private theorem domain_convex : Convex ℝ domain := convex_univ.prod (convex_Ioi _)

/-- Log mass between endpoints with log cumulative hazards `u` (upper) and `v` (lower). -/
def endpointLogMass (u v : ℝ) : ℝ := log (exp (-exp v) - exp (-exp u))

private theorem endpoint_dir_second (u v du dv z : ℝ) (huv : v + z * dv < u + z * du) :
    let a := exp (v + z * dv)
    let b := exp (u + z * du)
    let E := exp (a - b)
    let D := 1 - E
    deriv (deriv (fun s ↦ endpointLogMass (u + s * du) (v + s * dv))) z =
      (b * E * (D - b) / D ^ 2) * du ^ 2 +
      2 * (a * b * E / D ^ 2) * du * dv +
      (-a / D - a ^ 2 * E / D ^ 2) * dv ^ 2 := by
  intro a b E D
  let A := fun s : ℝ ↦ exp (v + s * dv)
  let B := fun s : ℝ ↦ exp (u + s * du)
  let N := fun s : ℝ ↦ exp (A s - B s)
  let F := fun s : ℝ ↦ 1 - N s
  let G := fun s : ℝ ↦ A s * dv - B s * du
  have ha (s : ℝ) : HasDerivAt A (A s * dv) s :=
    by simpa only [id_eq, one_mul, A] using ((hasDerivAt_id s).mul_const dv |>.const_add v).exp
  have hb (s : ℝ) : HasDerivAt B (B s * du) s :=
    by simpa only [id_eq, one_mul, B] using ((hasDerivAt_id s).mul_const du |>.const_add u).exp
  have hN (s : ℝ) : HasDerivAt N (N s * G s) s := ((ha s).sub (hb s)).exp
  have hF (s : ℝ) : HasDerivAt F (-(N s * G s)) s := (hN s).const_sub 1
  have hG (s : ℝ) : HasDerivAt G (A s * dv * dv - B s * du * du) s :=
    ((ha s).mul_const dv).sub ((hb s).mul_const du)
  have hAB : A z < B z := exp_lt_exp.mpr huv
  have hne : F z ≠ 0 := (sub_pos.mpr (exp_lt_one_iff.mpr (sub_neg.mpr hAB))).ne'
  let score := fun s : ℝ ↦ -(A s * dv) - N s * G s / F s
  have hscore : deriv (fun s ↦ endpointLogMass (u + s * du) (v + s * dv)) =ᶠ[𝓝 z] score := by
    have hf : ∀ᶠ s in 𝓝 z, F s ≠ 0 :=
      (hF z).continuousAt.eventually_ne hne
    filter_upwards [hf] with s hs
    have hfact : (fun t ↦ endpointLogMass (u + t * du) (v + t * dv)) =
        fun t ↦ log (exp (-A t) * F t) := by
      funext t
      simp only [endpointLogMass, A, B, F, N, mul_sub, mul_one, ← exp_add]
      congr 2; ring_nf
    rw [hfact]
    have hprod : HasDerivAt (fun t ↦ exp (-A t) * F t)
        (exp (-A s) * -(A s * dv) * F s + exp (-A s) * -(N s * G s)) s :=
      (ha s).neg.exp.mul (hF s)
    rw [(hprod.log (mul_ne_zero (exp_pos _).ne' hs)).deriv]
    dsimp [score]
    field_simp [hs, (exp_pos (-A s)).ne']
    ring
  rw [Filter.EventuallyEq.deriv_eq hscore]
  have hder := ((ha z).mul_const dv).neg.sub (((hN z).mul (hG z)).div (hF z) hne)
  have hder' : HasDerivAt score _ z := hder
  rw [hder'.deriv]
  have hD : D ≠ 0 := by simpa [F, N, A, B, D, E, a, b] using hne
  have hA : A z = a := rfl
  have hB : B z = b := rfl
  have hN0 : N z = E := by simp only [N, hA, hB, E]
  have hF0 : F z = D := by simp only [F, hN0, D]
  have hG0 : G z = a * dv - b * du := by simp only [G, hA, hB]
  simp only [Pi.mul_apply, hA, hB, hN0, hF0, hG0]
  field_simp [hD]
  have he : E + D = 1 := by dsimp [D]; ring
  linear_combination (a * dv ^ 2 * D) * he

/-- The endpoint Hessian has strictly negative quadratic form in every nonzero direction. -/
theorem endpoint_hessian_neg {u v du dv z : ℝ}
    (huv : v + z * dv < u + z * du) (hd : du ≠ 0 ∨ dv ≠ 0) :
    deriv (deriv (fun s ↦ endpointLogMass (u + s * du) (v + s * dv))) z < 0 := by
  rw [endpoint_dir_second u v du dv z huv]
  let a := exp (v + z * dv)
  let b := exp (u + z * du)
  let E := exp (a - b)
  let D := 1 - E
  let H := b * E * (D - b) / D ^ 2
  let J := a * b * E / D ^ 2
  let K := -a / D - a ^ 2 * E / D ^ 2
  change H * du ^ 2 + 2 * J * du * dv + K * dv ^ 2 < 0
  have ha : 0 < a := exp_pos _
  have hb : 0 < b := exp_pos _
  have hab : a < b := exp_lt_exp.mpr huv
  have hE : 0 < E := exp_pos _
  have hD : 0 < D := sub_pos.mpr (exp_lt_one_iff.mpr (sub_neg.mpr hab))
  have hDb : D < b - a := by
    have hh := one_sub_lt_exp_neg (sub_ne_zero.mpr hab.ne')
    rw [neg_sub] at hh
    dsimp [D, E]
    linarith
  have hH : H < 0 :=
    div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg (mul_pos hb hE)
      (sub_neg.mpr (by linarith))) (sq_pos_of_pos hD)
  have hdet_eq : H * K - J ^ 2 = a * b * E / D ^ 3 * (b - D - a * E) := by
    dsimp only [H, J, K]
    field_simp [hD.ne']
    ring
  have hdet : 0 < H * K - J ^ 2 := by
    rw [hdet_eq]
    apply mul_pos (div_pos (mul_pos (mul_pos ha hb) hE) (pow_pos hD _))
    have hh : b - D - a * E = (b - a - D) + a * D := by dsimp [D]; ring
    rw [hh]
    exact add_pos (by linarith) (mul_pos ha hD)
  have he : H * du ^ 2 + 2 * J * du * dv + K * dv ^ 2 =
      H * (du + J / H * dv) ^ 2 + ((H * K - J ^ 2) / H) * dv ^ 2 := by
    field_simp [hH.ne]
    ring
  rw [he]
  rcases eq_or_ne dv 0 with rfl | hdv
  · simp only [mul_zero, add_zero, ne_eq, not_true_eq_false, or_false] at hd ⊢
    simpa using mul_neg_of_neg_of_pos hH (sq_pos_of_ne_zero hd)
  · exact add_neg_of_nonpos_of_neg (mul_nonpos_of_nonpos_of_nonneg hH.le (sq_nonneg _))
      (mul_neg_of_neg_of_pos (div_neg_of_pos_of_neg hdet hH) (sq_pos_of_ne_zero hdv))

private theorem continuous_survival (k : ℕ) : Continuous (survival k) := by
  by_cases hk : k = 0
  · subst k
    exact continuous_const
  · unfold survival
    simp only [hk, ite_false, endpoint]
    fun_prop

private theorem continuous_mass (k : ℕ) : Continuous (mass k) :=
  (continuous_survival k).sub (continuous_survival (k + 1))

/-- Each positive cell contributes a strictly concave log mass. -/
theorem strictConcave_log_mass {k : ℕ} (hk : 0 < k) :
    StrictConcaveOn ℝ domain (fun p ↦ log (mass k p)) := by
  refine ⟨domain_convex, ?_⟩
  intro p hp r hr hpr a b ha hb hab
  let u := endpoint (k + 1) p
  let v := endpoint k p
  let du := endpoint (k + 1) r - u
  let dv := endpoint k r - v
  have hgap := endpoint_gap_pos hk
  have hne : du ≠ 0 ∨ dv ≠ 0 := by
    by_contra! hn
    have hβ : p.2 = r.2 := by
      apply mul_left_cancel₀ hgap.ne'
      dsimp [du, dv, u, v, endpoint] at hn
      nlinarith
    have hα : p.1 = r.1 := by
      have hh := hn.2
      dsimp [dv, v, endpoint] at hh
      rw [hβ] at hh
      linarith
    exact hpr (Prod.ext hα hβ)
  let f := fun z : ℝ ↦ endpointLogMass (u + z * du) (v + z * dv)
  have hf (z : ℝ) :
      f z = log (mass k ((1 - z) • p + z • r)) := by
    have he (j : ℕ) : endpoint j ((1 - z) • p + z • r) =
        endpoint j p + z * (endpoint j r - endpoint j p) := by
      simp [endpoint, Prod.smul_fst, Prod.smul_snd, smul_eq_mul]
      ring
    simp only [f, u, v, du, dv, endpointLogMass, mass, survival, hk.ne',
      Nat.add_one_ne_zero, ite_false, he]
  have hseg (z : ℝ) (hz : z ∈ Icc (0 : ℝ) 1) : (1 - z) • p + z • r ∈ domain :=
    domain_convex hp hr (sub_nonneg.mpr hz.2) hz.1 (sub_add_cancel 1 z)
  have hc : ContinuousOn f (Icc 0 1) := by
    have heal : Continuous (fun z : ℝ ↦ mass k ((1 - z) • p + z • r)) :=
      (continuous_mass k).comp (by fun_prop)
    exact (heal.continuousOn.log (fun z hz ↦ (mass_pos (hseg z hz) k).ne')).congr
      (fun z _ ↦ hf z)
  have hs : StrictConcaveOn ℝ (Icc 0 1) f := by
    apply strictConcaveOn_of_deriv2_neg (convex_Icc _ _) hc
    intro z hz
    have hz' := (hseg z (interior_subset hz)).2
    have hb' : 0 < ((1 - z) * p.2 + z * r.2) * (log ((k + 1 : ℕ) : ℝ) - log (k : ℝ)) :=
      mul_pos hz' hgap
    have hlt : v + z * dv < u + z * du := by
      dsimp [u, v, du, dv, endpoint]
      nlinarith
    exact endpoint_hessian_neg hlt hne
  have hh := hs.2 (show (0 : ℝ) ∈ Icc 0 1 from ⟨le_rfl, zero_le_one⟩)
    (show (1 : ℝ) ∈ Icc 0 1 from ⟨zero_le_one, le_rfl⟩) zero_ne_one ha hb hab
  simp only [smul_eq_mul, mul_zero, mul_one, zero_add, hf, sub_zero, one_smul,
    zero_smul, add_zero, zero_add, sub_self] at hh
  rwa [show 1 - b = a by linarith] at hh

private theorem concave_log_mass (k : ℕ) :
    ConcaveOn ℝ domain (fun p ↦ log (mass k p)) := by
  by_cases hk : 0 < k
  · exact (strictConcave_log_mass hk).concaveOn
  · have : k = 0 := by omega
    subst k
    refine ⟨domain_convex, ?_⟩
    intro p _ r _ a b ha hb hab
    simpa [log_mass_zero, Prod.smul_fst, smul_eq_mul] using
      g_strict.concaveOn.2 (mem_univ p.1) (mem_univ r.1) ha hb hab

/-- A sample containing a positive count has strictly concave joint log likelihood. -/
theorem strictConcave_logLik {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    StrictConcaveOn ℝ domain (logLik x) := by
  obtain ⟨j, hj⟩ := hx
  refine ⟨domain_convex, ?_⟩
  intro p hp r hr hpr a b ha hb hab
  simp only [logLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  apply Finset.sum_lt_sum
  · intro i _
    exact (concave_log_mass (x i)).2 hp hr ha.le hb.le hab
  · exact ⟨j, Finset.mem_univ _, (strictConcave_log_mass hj).2 hp hr hpr ha hb hab⟩


private theorem continuousOn_logLik (x : Fin n → ℕ) : ContinuousOn (logLik x) domain := by
  unfold logLik
  apply continuousOn_finsetSum
  intro i _
  exact (continuous_mass (x i)).continuousOn.log (fun p hp ↦ (mass_pos hp _).ne')

private theorem mass_le_exp_endpoint (k : ℕ) (p : ℝ × ℝ) :
    mass k p ≤ exp (endpoint (k + 1) p) := by
  have hs := survival_le_one k p
  have he := one_sub_le_exp_neg (exp (endpoint (k + 1) p))
  unfold mass
  rw [show survival (k + 1) p = exp (-exp (endpoint (k + 1) p)) by
    simp only [survival, Nat.add_one_ne_zero, ite_false]]
  linarith

private theorem mass_le_survival (k : ℕ) (p : ℝ × ℝ) :
    mass k p ≤ survival k p :=
  sub_le_self _ (survival_pos (k + 1) p).le

private theorem mass_le_shape_gap {k : ℕ} (hk : 0 < k) {p : ℝ × ℝ} (hp : p ∈ domain) :
    mass k p ≤ p.2 * (log ((k + 1 : ℕ) : ℝ) - log (k : ℝ)) := by
  have hmono : Monotone (fun y : ℝ ↦ y + exp (-exp y)) := by
    apply monotone_of_deriv_nonneg
    · fun_prop
    · intro y
      have hd : HasDerivAt (fun y : ℝ ↦ y + exp (-exp y))
          (1 + exp (-exp y) * -exp y) y :=
        (hasDerivAt_id y).add ((hasDerivAt_exp y).neg.exp)
      rw [hd.deriv]
      have he := add_one_le_exp (exp y)
      have hmul := mul_le_mul_of_nonneg_right (show exp y ≤ exp (exp y) by linarith)
        (exp_pos (-exp y)).le
      rw [← exp_add (exp y) (-exp y), add_neg_cancel, exp_zero] at hmul
      nlinarith
  have hd := mul_pos hp.2 (endpoint_gap_pos hk)
  have hdiff : endpoint (k + 1) p - endpoint k p =
      p.2 * (log ((k + 1 : ℕ) : ℝ) - log (k : ℝ)) := by
    unfold endpoint
    ring
  have hh := hmono (show endpoint k p ≤ endpoint (k + 1) p by linarith)
  simp only [mass, survival, hk.ne', Nat.add_one_ne_zero, ite_false]
  linarith

private theorem mass_ge_reference {x : Fin n → ℕ} {p r : ℝ × ℝ}
    (hp : p ∈ domain) (hle : logLik x r ≤ logLik x p) (i : Fin n) :
    exp (logLik x r) ≤ mass (x i) p := by
  have hlog : logLik x p ≤ log (mass (x i) p) := by
    unfold logLik
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    exact add_le_of_nonpos_left (Finset.sum_nonpos fun j _ ↦
      log_nonpos (mass_pos hp (x j)).le (mass_lt_one _ _).le)
  rw [← exp_log (mass_pos hp (x i))]
  exact exp_le_exp.mpr (hle.trans hlog)

private theorem endpoint_lower {k : ℕ} {p : ℝ × ℝ} {c : ℝ} (hc : 0 < c)
    (hm : c ≤ mass k p) : log c ≤ endpoint (k + 1) p :=
  (log_le_iff_le_exp hc).mpr (hm.trans (mass_le_exp_endpoint k p))

private theorem endpoint_upper {k : ℕ} {p : ℝ × ℝ} {c : ℝ}
    (hk : 0 < k) (hc : 0 < c) (hc1 : c < 1) (hm : c ≤ mass k p) :
    endpoint k p ≤ log (-log c) := by
  have hh := hm.trans (mass_le_survival k p)
  simp only [survival, hk.ne', ite_false] at hh
  rw [← log_le_iff_le_exp hc] at hh
  exact (le_log_iff_exp_le (neg_pos.mpr (log_neg hc hc1))).mpr (by linarith)

private theorem logLik_neg {x : Fin n → ℕ} {p : ℝ × ℝ} (hp : p ∈ domain) (i : Fin n) :
    logLik x p < 0 := by
  have hm : logLik x p ≤ log (mass (x i) p) := by
    unfold logLik
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    exact add_le_of_nonpos_left (Finset.sum_nonpos fun j _ ↦
      log_nonpos (mass_pos hp (x j)).le (mass_lt_one _ _).le)
  exact hm.trans_lt (log_neg (mass_pos hp _) (mass_lt_one _ _))

private theorem exists_max_of_box {x : Fin n → ℕ} {C : Set (ℝ × ℝ)}
    (hC : IsClosed C) {r : ℝ × ℝ} (hr : r ∈ domain ∩ C)
    {a A b B : ℝ} (hb : 0 < b)
    (hbox : ∀ p ∈ domain ∩ C, logLik x r ≤ logLik x p →
      p ∈ Icc (a, b) (A, B)) :
    ∃ p ∈ domain ∩ C, IsMaxOn (logLik x) (domain ∩ C) p := by
  let K : Set (ℝ × ℝ) := Icc (a, b) (A, B) ∩ C
  have hK : IsCompact K := isCompact_Icc.inter_right hC
  have hsub : K ⊆ domain ∩ C := fun _ hp ↦
    ⟨⟨mem_univ _, hb.trans_le hp.1.1.2⟩, hp.2⟩
  have hrK : r ∈ K := ⟨hbox r hr le_rfl, hr.2⟩
  obtain ⟨p, hp, hm⟩ := hK.exists_isMaxOn ⟨r, hrK⟩
    ((continuousOn_logLik x).mono (fun _ h ↦ (hsub h).1))
  refine ⟨p, hsub hp, ?_⟩
  intro z hz
  by_cases h : logLik x r ≤ logLik x z
  · exact hm ⟨hbox z hz h, hz.2⟩
  · exact (le_of_not_ge h).trans (hm hrK)

private theorem exists_max_of_shape_bound {x : Fin n → ℕ} {C : Set (ℝ × ℝ)}
    (hC : IsClosed C) {r : ℝ × ℝ} (hr : r ∈ domain ∩ C)
    {i : Fin n} (hi : 0 < x i) {B : ℝ}
    (hshape : ∀ p ∈ domain ∩ C, logLik x r ≤ logLik x p → p.2 ≤ B) :
    ∃ p ∈ domain ∩ C, IsMaxOn (logLik x) (domain ∩ C) p := by
  let c := exp (logLik x r)
  have hc : 0 < c := exp_pos _
  have hc1 : c < 1 := exp_lt_one_iff.mpr (logLik_neg hr.1 i)
  let d := log ((x i + 1 : ℕ) : ℝ) - log (x i : ℝ)
  have hd : 0 < d := endpoint_gap_pos hi
  apply exists_max_of_box hC hr (a := log c - B * log ((x i + 1 : ℕ) : ℝ))
    (A := log (-log c)) (B := B) (div_pos hc hd)
  intro p hp hl
  have hm := mass_ge_reference hp.1 hl i
  have hlo := endpoint_lower hc hm
  have hhi := endpoint_upper hi hc hc1 hm
  have hgap := hm.trans (mass_le_shape_gap hi hp.1)
  have hβ := hshape p hp hl
  have hk : 0 ≤ log ((x i + 1 : ℕ) : ℝ) := log_nonneg (by exact_mod_cast Nat.succ_pos (x i))
  have hk' : 0 ≤ log (x i : ℝ) := log_nonneg (by exact_mod_cast hi)
  refine ⟨⟨?_, (div_le_iff₀ hd).mpr hgap⟩, ⟨?_, hβ⟩⟩
  · unfold endpoint at hlo
    linarith [mul_le_mul_of_nonneg_right hβ hk]
  · unfold endpoint at hhi
    linarith [mul_nonneg hp.1.2.le hk']

private theorem unique_on {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {C : Set (ℝ × ℝ)} (hC : Convex ℝ C)
    (hm : ∃ p ∈ domain ∩ C, IsMaxOn (logLik x) (domain ∩ C) p) :
    ∃! p, p ∈ domain ∩ C ∧ IsMaxOn (logLik x) (domain ∩ C) p := by
  obtain ⟨p, hp, hm⟩ := hm
  refine ⟨p, ⟨hp, hm⟩, fun z hz ↦ ?_⟩
  exact ((strictConcave_logLik hx).subset inter_subset_left
    (domain_convex.inter hC)).eq_of_isMaxOn hz.2 hm hz.1 hp

/-- The presence of observations separated by at least two ensures attainment and uniqueness. -/
theorem existsUnique_joint_of_gap {x : Fin n → ℕ} {i j : Fin n}
    (hgap : x i + 2 ≤ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p := by
  have hj : 0 < x j := by omega
  have hr : (0, 1) ∈ domain ∩ (univ : Set (ℝ × ℝ)) := ⟨⟨mem_univ _, zero_lt_one⟩, mem_univ _⟩
  let c := exp (logLik x (0, 1))
  have hc : 0 < c := exp_pos _
  have hc1 : c < 1 := exp_lt_one_iff.mpr (logLik_neg hr.1 j)
  let d := log (x j : ℝ) - log ((x i + 1 : ℕ) : ℝ)
  have hd : 0 < d := sub_pos.mpr (log_lt_log (by positivity) (by exact_mod_cast
    (show x i + 1 < x j by omega)))
  have hb : ∀ p ∈ domain ∩ (univ : Set (ℝ × ℝ)), logLik x (0, 1) ≤ logLik x p →
      p.2 ≤ max 0 ((log (-log c) - log c) / d) := by
    intro p hp hl
    apply le_trans _ (le_max_right _ _)
    apply (le_div_iff₀ hd).mpr
    have hlo := endpoint_lower hc (mass_ge_reference hp.1 hl i)
    have hhi := endpoint_upper hj hc hc1 (mass_ge_reference hp.1 hl j)
    unfold endpoint at hlo hhi
    dsimp [d]
    linarith [mul_sub p.2 (log (x j : ℝ)) (log ((x i + 1 : ℕ) : ℝ))]
  simpa only [inter_univ] using unique_on ⟨j, hj⟩ convex_univ
    (exists_max_of_shape_bound isClosed_univ hr hj hb)


def fixedShape (β : ℝ) : Set (ℝ × ℝ) := domain ∩ (univ ×ˢ {β})

def fixedProbability (α : ℝ) : Set (ℝ × ℝ) := domain ∩ ({α} ×ˢ univ)

private theorem fixedShape_attains {x : Fin n → ℕ} {β : ℝ} (hβ : 0 < β)
    (hx : ∃ i, 0 < x i) :
    ∃! p, p ∈ fixedShape β ∧ IsMaxOn (logLik x) (fixedShape β) p := by
  obtain ⟨i, hi⟩ := hx
  have hr : (0, β) ∈ fixedShape β := ⟨⟨mem_univ _, hβ⟩, mem_univ _, rfl⟩
  exact unique_on ⟨i, hi⟩ (convex_univ.prod (convex_singleton _))
    (exists_max_of_shape_bound (isClosed_univ.prod isClosed_singleton) hr hi
      (fun p hp _ ↦ (show p.2 = β from hp.2.2).le))

private theorem fixedProbability_attains {x : Fin n → ℕ} (α : ℝ)
    (hx : ∃ i, 2 ≤ x i) :
    ∃! p, p ∈ fixedProbability α ∧ IsMaxOn (logLik x) (fixedProbability α) p := by
  obtain ⟨i, hi⟩ := hx
  have hi' : 0 < x i := by omega
  have hr : (α, 1) ∈ fixedProbability α := ⟨⟨mem_univ _, zero_lt_one⟩, rfl, mem_univ _⟩
  let c := exp (logLik x (α, 1))
  have hc : 0 < c := exp_pos _
  have hc1 : c < 1 := exp_lt_one_iff.mpr (logLik_neg hr.1 i)
  have hd : 0 < log (x i : ℝ) := log_pos (by exact_mod_cast (show 1 < x i by omega))
  apply unique_on ⟨i, hi'⟩ ((convex_singleton _).prod convex_univ)
  apply exists_max_of_shape_bound (isClosed_singleton.prod isClosed_univ) hr hi'
    (B := (log (-log c) - α) / log (x i : ℝ))
  intro p hp hl
  have hh := endpoint_upper hi' hc hc1 (mass_ge_reference hp.1 hl i)
  unfold endpoint at hh
  rw [show p.1 = α from hp.2.1] at hh
  exact (le_div_iff₀ hd).mpr (by linarith)

/-- With held probability and all zeros, the likelihood is flat on the whole shape fiber. -/
theorem fixedProbability_all_zero {x : Fin n → ℕ} (hx : ∀ i, x i = 0)
    (α : ℝ) {p : ℝ × ℝ} (hp : p ∈ fixedProbability α) :
    logLik x p = n * log (1 - exp (-exp α)) ∧
      IsMaxOn (logLik x) (fixedProbability α) p := by
  have hh (r : ℝ × ℝ) (hr : r ∈ fixedProbability α) :
      logLik x r = n * log (1 - exp (-exp α)) := by
    simp [logLik, hx, log_mass_zero, (show r.1 = α from hr.2.1), g, h]
  exact ⟨hh p hp, fun r hr ↦ (hh r hr).le.trans (hh p hp).ge⟩

private theorem fixedShape_all_zero_no_max {x : Fin n → ℕ} (hn : 0 < n)
    (hx : ∀ i, x i = 0) {β : ℝ} :
    ¬ ∃ p ∈ fixedShape β, IsMaxOn (logLik x) (fixedShape β) p := by
  rintro ⟨p, hp, hm⟩
  have hq : (p.1 + 1, p.2) ∈ fixedShape β := ⟨⟨mem_univ _, hp.1.2⟩, hp.2⟩
  have hh := hm hq
  simp only [logLik, hx, log_mass_zero, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul] at hh
  exact ((mul_lt_mul_of_pos_left (g_mono (show p.1 < p.1 + 1 by linarith))
    (by exact_mod_cast hn : (0 : ℝ) < n)).not_ge hh)

/-- A held shape yields a unique finite fit exactly when an observation is positive. -/
theorem fixedShape_existsUnique_iff {x : Fin n → ℕ} (hn : 0 < n) {β : ℝ} (hβ : 0 < β) :
    (∃! p, p ∈ fixedShape β ∧ IsMaxOn (logLik x) (fixedShape β) p) ↔
      ∃ i, 0 < x i := by
  refine ⟨?_, fixedShape_attains hβ⟩
  rintro ⟨p, hp, _⟩
  by_contra! hx
  exact fixedShape_all_zero_no_max hn (fun i ↦ by have := hx i; omega) ⟨p, hp⟩

private theorem fixedProbability_zero_one_no_max {x : Fin n → ℕ} (α : ℝ)
    (hx : ∀ i, x i ≤ 1) (hex : ∃ i, 0 < x i) :
    ¬ ∃ p ∈ fixedProbability α, IsMaxOn (logLik x) (fixedProbability α) p := by
  rintro ⟨p, hp, hm⟩
  let r : ℝ × ℝ := (p.1, p.2 + 1)
  have hr : r ∈ fixedProbability α := ⟨⟨mem_univ _, by exact add_pos hp.1.2 zero_lt_one⟩, hp.2⟩
  have h₁ : log (mass 1 p) < log (mass 1 r) := by
    apply log_lt_log (mass_pos hp.1 1)
    have he : endpoint 2 p < endpoint 2 r := by
      have hlog : 0 < log (2 : ℝ) := log_pos (by norm_num)
      simp [endpoint, r]
      linarith
    have hs : survival 2 r < survival 2 p := by
      simpa [survival] using (exp_lt_exp.mpr (neg_lt_neg (exp_lt_exp.mpr he)))
    have hs' : survival 1 p = survival 1 r := by simp [survival, endpoint, r]
    simp only [mass, hs']
    exact sub_lt_sub_left hs _
  have h₀ : log (mass 0 p) = log (mass 0 r) := by simp [log_mass_zero, r]
  obtain ⟨j, hj⟩ := hex
  apply (hm hr).not_gt
  apply Finset.sum_lt_sum
  · intro i _
    have hi := hx i
    rcases (show x i = 0 ∨ x i = 1 by omega) with hh | hh
    · simp [hh, h₀]
    · simpa only [hh] using h₁.le
  · exact ⟨j, Finset.mem_univ _, by simpa only [show x j = 1 by have := hx j; omega] using h₁⟩

/-- With held probability and a positive count, a unique fit exists exactly when
a count is at least two. -/
theorem fixedProbability_existsUnique_iff {x : Fin n → ℕ} (α : ℝ)
    (hx : ∃ i, 0 < x i) :
    (∃! p, p ∈ fixedProbability α ∧ IsMaxOn (logLik x) (fixedProbability α) p) ↔
      ∃ i, 2 ≤ x i := by
  refine ⟨?_, fixedProbability_attains α⟩
  rintro ⟨p, hp, _⟩
  by_contra! hh
  exact fixedProbability_zero_one_no_max α (fun i ↦ by have := hh i; omega) hx ⟨p, hp⟩


private def path (a v β : ℝ) : ℝ × ℝ := (a - β * v, β)

private theorem survival_path_left (a v : ℝ) (k : ℕ)
    (hk : k = 0 ∨ log (k : ℝ) < v) :
    Tendsto (fun β ↦ survival k (path a v β)) atTop (𝓝 1) := by
  by_cases hz : k = 0
  · subst k
    exact tendsto_const_nhds
  have hd : log (k : ℝ) - v < 0 := sub_neg.mpr (hk.resolve_left hz)
  have ht : Tendsto (fun β : ℝ ↦ a + (log (k : ℝ) - v) * β) atTop atBot :=
    tendsto_atBot_add_const_left _ _ (tendsto_const_mul_atBot_of_neg hd |>.mpr tendsto_id)
  have he := tendsto_exp_atBot.comp ht
  have hh := continuous_exp.continuousAt.tendsto.comp he.neg
  simpa only [Function.comp_def, neg_zero, exp_zero, survival, hz, ite_false,
    endpoint, path, show ∀ β : ℝ, a - β * v + β * log (k : ℝ) =
      a + (log (k : ℝ) - v) * β by intro; ring] using hh

private theorem survival_path_right (a v : ℝ) (k : ℕ) (hk : k ≠ 0)
    (hvk : v < log (k : ℝ)) :
    Tendsto (fun β ↦ survival k (path a v β)) atTop (𝓝 0) := by
  have hd : 0 < log (k : ℝ) - v := sub_pos.mpr hvk
  have ht : Tendsto (fun β : ℝ ↦ a + (log (k : ℝ) - v) * β) atTop atTop :=
    tendsto_atTop_add_const_left _ _ (tendsto_const_mul_atTop_of_pos hd |>.mpr tendsto_id)
  have hh := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_exp_atTop.comp ht))
  simpa only [Function.comp_def, survival, hk, ite_false, endpoint, path,
    show ∀ β : ℝ, a - β * v + β * log (k : ℝ) =
      a + (log (k : ℝ) - v) * β by intro; ring] using hh

private theorem limit_of_cell_limits {x : Fin n → ℕ} {param : ℝ → ℝ × ℝ} {w : Fin n → ℝ}
    (hw : ∀ i, 0 < w i)
    (ht : ∀ i, Tendsto (fun β ↦ mass (x i) (param β)) atTop (𝓝 (w i))) :
    Tendsto (fun β ↦ logLik x (param β)) atTop (𝓝 (∑ i, log (w i))) :=
  tendsto_finsetSum _ fun i _ ↦ (ht i).log (hw i).ne'

private theorem sup_eq_of_path {x : Fin n → ℕ} {v : ℝ} {param : ℝ → ℝ × ℝ}
    (hp : ∀ᶠ β in atTop, param β ∈ domain)
    (ht : Tendsto (fun β ↦ logLik x (param β)) atTop (𝓝 v))
    (hlt : ∀ p ∈ domain, logLik x p < v) :
    sSup (logLik x '' domain) = v ∧ ¬ ∃ p ∈ domain, IsMaxOn (logLik x) domain p := by
  have hu : v ∈ upperBounds (logLik x '' domain) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (hlt p hp).le
  have hB : BddAbove (logLik x '' domain) := ⟨v, hu⟩
  obtain ⟨p, hpd⟩ := hp.exists
  refine ⟨le_antisymm (csSup_le ⟨_, mem_image_of_mem _ hpd⟩ hu) ?_, ?_⟩
  · exact le_of_tendsto ht (hp.mono fun β hb ↦ le_csSup hB (mem_image_of_mem _ hb))
  · rintro ⟨p, hp', hm⟩
    exact (hlt p hp').not_ge (le_of_tendsto ht (hp.mono fun _ hb ↦ hm hb))

private theorem log_nat_succ_lt {m : ℕ} (hm : 0 < m) :
    log (m : ℝ) < log ((m + 1 : ℕ) : ℝ) :=
  sub_pos.mp (endpoint_gap_pos hm)

/-- Constant samples have supremum zero, approached along a point-mass path. -/
theorem constant_sup {x : Fin n → ℕ} (hn : 0 < n) {m : ℕ} (hx : ∀ i, x i = m) :
    sSup (logLik x '' domain) = 0 ∧ ¬ ∃ p ∈ domain, IsMaxOn (logLik x) domain p := by
  let v := if m = 0 then -1 else (log (m : ℝ) + log ((m + 1 : ℕ) : ℝ)) / 2
  have hl : m = 0 ∨ log (m : ℝ) < v := by
    by_cases hm : m = 0
    · exact Or.inl hm
    · right
      simp only [v, hm, ite_false]
      linarith [log_nat_succ_lt (Nat.pos_of_ne_zero hm)]
  have hr : v < log ((m + 1 : ℕ) : ℝ) := by
    by_cases hm : m = 0
    · subst m
      simp [v]
    · simp only [v, hm, ite_false]
      linarith [log_nat_succ_lt (Nat.pos_of_ne_zero hm)]
  apply sup_eq_of_path (param := path 0 v)
  · exact (eventually_gt_atTop (0 : ℝ)).mono (fun β hβ ↦ ⟨mem_univ _, hβ⟩)
  · have hm := (survival_path_left 0 v m hl).sub
      (survival_path_right 0 v (m + 1) (Nat.add_one_ne_zero m) hr)
    have ht := limit_of_cell_limits (x := x) (w := fun _ ↦ 1) (fun _ ↦ zero_lt_one)
      (fun i ↦ by simpa only [hx i, mass, sub_zero] using hm)
    simpa using ht
  · exact fun p hp ↦ logLik_neg hp ⟨0, hn⟩

private theorem adjacent_path_mass {m : ℕ} {w : ℝ} (hw : 0 < w) (hw1 : w < 1) :
    let param := path (log (-log w)) (log ((m + 1 : ℕ) : ℝ))
    Tendsto (fun β ↦ mass m (param β)) atTop (𝓝 (1 - w)) ∧
      Tendsto (fun β ↦ mass (m + 1) (param β)) atTop (𝓝 w) := by
  intro param
  have hh : ∀ β : ℝ, survival (m + 1) (param β) = w := by
    intro β
    simp only [param, path, survival, Nat.add_one_ne_zero, ite_false, endpoint]
    rw [sub_add_cancel, exp_log (neg_pos.mpr (log_neg hw hw1)), neg_neg, exp_log hw]
  have hmid : Tendsto (fun β ↦ survival (m + 1) (param β)) atTop (𝓝 w) := by
    simpa only [hh] using (tendsto_const_nhds (x := w) (f := atTop (α := ℝ)))
  have hl : m = 0 ∨ log (m : ℝ) < log ((m + 1 : ℕ) : ℝ) := by
    by_cases hm : m = 0
    · exact Or.inl hm
    · exact Or.inr (log_nat_succ_lt (Nat.pos_of_ne_zero hm))
  have hr := log_nat_succ_lt (Nat.succ_pos m)
  exact ⟨(survival_path_left _ _ _ hl).sub hmid,
    by simpa only [mass, sub_zero] using
      hmid.sub (survival_path_right _ _ _ (Nat.add_one_ne_zero _) hr)⟩


/-- Empirical fraction in the upper cell of an adjacent pair. -/
def upperFraction (x : Fin n → ℕ) (m : ℕ) : ℝ :=
  ((Finset.univ.filter fun i ↦ x i = m + 1).card : ℝ) / n

/-- The adjacent empirical law gives the unattained supremum for two occupied adjacent cells. -/
theorem adjacent_sup {x : Fin n → ℕ} {m : ℕ}
    (hx : ∀ i, x i = m ∨ x i = m + 1)
    (hl : ∃ i, x i = m) (hr : ∃ i, x i = m + 1) :
    let w := upperFraction x m
    sSup (logLik x '' domain) = ∑ i, log (if x i = m + 1 then w else 1 - w) ∧
      ¬ ∃ p ∈ domain, IsMaxOn (logLik x) domain p := by
  intro w
  let U := Finset.univ.filter fun i ↦ x i = m + 1
  let L := Finset.univ.filter fun i ↦ x i ≠ m + 1
  have hcard : L.card + U.card = n := by
    simpa [U, L, add_comm] using
      (Finset.card_filter_add_card_filter_not (s := Finset.univ) (fun i ↦ x i = m + 1))
  obtain ⟨i, hi⟩ := hl
  obtain ⟨j, hj⟩ := hr
  have hL : 0 < L.card := Finset.card_pos.mpr ⟨i, by simp [L, hi]⟩
  have hU : 0 < U.card := Finset.card_pos.mpr ⟨j, by simp [U, hj]⟩
  have hn : (0 : ℝ) < n := by exact_mod_cast (Nat.zero_le j.val).trans_lt j.isLt
  have hL' : (0 : ℝ) < L.card := by exact_mod_cast hL
  have hU' : (0 : ℝ) < U.card := by exact_mod_cast hU
  have hcard' : (L.card : ℝ) + U.card = n := by exact_mod_cast hcard
  have hwn : w * n = U.card := div_mul_cancel₀ _ hn.ne'
  have hln : (1 - w) * n = L.card := by linarith [sub_mul 1 w (n : ℝ)]
  have hw : 0 < w := div_pos hU' hn
  have hw1 : w < 1 := (div_lt_one hn).mpr (by linarith)
  let cell : Fin n → ℝ := fun i ↦ if x i = m + 1 then w else 1 - w
  have hcell (i : Fin n) : 0 < cell i := by
    unfold cell
    split
    · exact hw
    · exact sub_pos.mpr hw1
  apply sup_eq_of_path (param := path (log (-log w)) (log ((m + 1 : ℕ) : ℝ)))
  · exact (eventually_gt_atTop (0 : ℝ)).mono (fun β hβ ↦ ⟨mem_univ _, hβ⟩)
  · apply limit_of_cell_limits hcell
    intro i
    rcases hx i with hh | hh
    · simpa [cell, hh] using (adjacent_path_mass (m := m) hw hw1).1
    · simpa [cell, hh] using (adjacent_path_mass (m := m) hw hw1).2
  · intro p hp
    have hmass : mass m p + mass (m + 1) p < 1 := by
      unfold mass
      linarith [survival_le_one m p, survival_pos (m + 1 + 1) p]
    have hsum : ∑ i, mass (x i) p / cell i = n * (mass m p + mass (m + 1) p) := by
      rw [← Finset.sum_filter_add_sum_filter_not _ (fun i ↦ x i = m + 1)]
      change (∑ i ∈ U, mass (x i) p / cell i) + (∑ i ∈ L, mass (x i) p / cell i) = _
      have hu : ∑ i ∈ U, mass (x i) p / cell i = U.card * (mass (m + 1) p / w) := by
        rw [← nsmul_eq_mul, ← Finset.sum_const]
        apply Finset.sum_congr rfl
        intro i hi
        have hh := (Finset.mem_filter.mp hi).2
        simp [cell, hh]
      have hl : ∑ i ∈ L, mass (x i) p / cell i = L.card * (mass m p / (1 - w)) := by
        rw [← nsmul_eq_mul, ← Finset.sum_const]
        apply Finset.sum_congr rfl
        intro i hi
        have hne := (Finset.mem_filter.mp hi).2
        have hh := (hx i).resolve_right hne
        simp [cell, hh]
      rw [hu, hl, ← hwn, ← hln]
      field_simp [hw.ne', (sub_pos.mpr hw1).ne']
      ring
    change logLik x p < ∑ i, log (cell i)
    apply sub_neg.mp
    rw [logLik, ← Finset.sum_sub_distrib]
    calc
      ∑ i, (log (mass (x i) p) - log (cell i)) ≤ ∑ i, (mass (x i) p / cell i - 1) := by
        apply Finset.sum_le_sum
        intro i _
        rw [← log_div (mass_pos hp _).ne' (hcell i).ne']
        exact log_le_sub_one_of_pos (div_pos (mass_pos hp _) (hcell i))
      _ < 0 := by
        rw [Finset.sum_sub_distrib, hsum]
        simpa using (mul_lt_mul_of_pos_left hmass hn)

/-- A nonempty sample has a unique joint MLE exactly when its range is at least two. -/
theorem joint_existsUnique_iff {x : Fin n → ℕ} (hn : 0 < n) :
    (∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ↔
      ∃ i j, x i + 2 ≤ x j := by
  constructor
  · rintro ⟨p, hp, _⟩
    by_contra! hgap
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    have hh (j : Fin n) : x j = x i ∨ x j = x i + 1 := by
      have hlo := hi j (Finset.mem_univ j)
      have hup := hgap i j
      omega
    by_cases hc : ∀ j, x j = x i
    · exact (constant_sup hn hc).2 ⟨p, hp⟩
    · obtain ⟨j, hj⟩ := not_forall.mp hc
      exact (adjacent_sup hh ⟨i, rfl⟩ ⟨j, (hh j).resolve_left hj⟩).2 ⟨p, hp⟩
  · rintro ⟨i, j, hij⟩
    exact existsUnique_joint_of_gap hij



end

end UnivariateML.DiscreteWeibull
