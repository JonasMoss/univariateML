module

public import UnivariateML.Family.BurrHeld
public import UnivariateML.Family.Weibull
public import UnivariateML.Family.ShapeFacesInverseBurr
public import UnivariateML.Family.ShapeFacesParetoIV
public import UnivariateML.Family.Paralogistic
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

set_option linter.style.header false

/-!
# Burr XII competitive escape values and tied-shape restrictions

The log-data chart gives the uniform escape bound and carries both
optimized boundary values and their likelihood paths. The reciprocal
and excess transformations preserve the criterion in physical coordinates.

## Deviations

The dossier lists Weibull and Pareto as the all-free competitors. The
log-logistic family is the interior slice at first shape one. Lomax is
the interior slice at power one. The escape result concerns sample
likelihood and does not classify weak limits of probability measures.
The paralogistic tie construction gives two distinct maximizers at a
crossing in the report's sample interval. It does not prove exactly
two maximizers or uniqueness of the crossing.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.BurrFree

noncomputable section

variable {n : ℕ}

/-- Mean Burr likelihood of log observations, in shape, power and intercept coordinates. -/
def likelihood (y : Fin n → ℝ) (p : ℝ × ℝ × ℝ) : ℝ :=
  BurrHeld.firstLik y p.1 p.2

/-- Feasible shape and power are positive and the intercept is unrestricted. -/
def domain : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ univ)

/-- Optimized Pareto mean log likelihood when the least log observation is zero. -/
def paretoValue (y : Fin n → ℝ) : ℝ := -log (BurrHeld.avg y) - 1

/-- Weibull mean log likelihood of log observations, in power and intercept coordinates. -/
def weibullLik (y : Fin n → ℝ) (c b : ℝ) : ℝ :=
  log c + BurrHeld.avg (fun i ↦ c * y i + b - exp (c * y i + b))

local notation "avg" => BurrHeld.avg
local notation "sp" => BurrHeld.softplus

private theorem npos (hn : 0 < n) : (0 : ℝ) < n := by exact_mod_cast hn

private theorem avg_const (hn : 0 < n) (r : ℝ) : avg (fun _ : Fin n ↦ r) = r := by
  simp [BurrHeld.avg, mul_comm (n : ℝ), (npos hn).ne']

private theorem avg_add (f g : Fin n → ℝ) :
    avg (fun i ↦ f i + g i) = avg f + avg g := by
  simp [BurrHeld.avg, Finset.sum_add_distrib, add_div]

private theorem avg_sub (f g : Fin n → ℝ) :
    avg (fun i ↦ f i - g i) = avg f - avg g := by
  simp [BurrHeld.avg, Finset.sum_sub_distrib, sub_div]

private theorem avg_mul (a : ℝ) (f : Fin n → ℝ) :
    avg (fun i ↦ a * f i) = a * avg f := by
  simp only [BurrHeld.avg, ← Finset.mul_sum, mul_div_assoc]

private theorem avg_mono {f g : Fin n → ℝ} (h : ∀ i, f i ≤ g i) :
    avg f ≤ avg g :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

private theorem avg_one_le {f : Fin n → ℝ} (i : Fin n) (hf : ∀ j, 0 ≤ f j) :
    f i / n ≤ avg f :=
  div_le_div_of_nonneg_right
    (Finset.single_le_sum (fun j _ ↦ hf j) (Finset.mem_univ i)) (Nat.cast_nonneg _)

private theorem sp_pos (z : ℝ) : 0 < sp z := Real.log_pos (lt_add_of_pos_right 1 (exp_pos z))

private theorem sp_shift (z : ℝ) : sp z = z + sp (-z) := by
  unfold BurrHeld.softplus
  calc
    _ = log (exp z * (1 + exp (-z))) := by
      congr 1
      rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
    _ = _ := by rw [log_mul (exp_pos z).ne' (by positivity), log_exp]

private theorem sp_ge (z : ℝ) : z ≤ sp z := by
  linarith [sp_shift z, sp_pos (-z)]

private theorem sp_le_exp (z : ℝ) : sp z ≤ exp z := by
  unfold BurrHeld.softplus
  linarith [log_le_sub_one_of_pos (show 0 < 1 + exp z by positivity)]

private theorem avg_tendsto {ι : Type*} {l : Filter ι} {f : ι → Fin n → ℝ}
    {v : Fin n → ℝ} (h : ∀ i, Tendsto (fun t ↦ f t i) l (𝓝 (v i))) :
    Tendsto (fun t ↦ avg (f t)) l (𝓝 (avg v)) :=
  (tendsto_finsetSum _ fun i _ ↦ h i).div_const _

private theorem likelihood_pareto_form (hn : 0 < n) (y : Fin n → ℝ)
    {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) :
    likelihood y (k, c, b) =
      log (k * c) - k * c * avg y - k * b -
        (k + 1) * avg (fun i ↦ sp (-(c * y i + b))) := by
  unfold likelihood BurrHeld.firstLik
  dsimp
  rw [log_mul hk.ne' hc.ne']
  simp_rw [show ∀ i, sp (c * y i + b) = c * y i + b + sp (-(c * y i + b))
    from fun i ↦ sp_shift (c * y i + b)]
  rw [avg_add, avg_mul, avg_add, avg_add, avg_mul, avg_const hn]
  ring

/-- The Pareto likelihood at the minimum is optimized at the reciprocal mean log excess. -/
theorem pareto_optimized {y : Fin n → ℝ} (hy : 0 < BurrHeld.avg y) {a : ℝ}
    (ha : 0 < a) :
    log a - a * BurrHeld.avg y ≤ paretoValue y ∧
      log (BurrHeld.avg y)⁻¹ - (BurrHeld.avg y)⁻¹ * BurrHeld.avg y = paretoValue y := by
  have h := log_le_sub_one_of_pos (mul_pos ha hy)
  rw [log_mul ha.ne' hy.ne'] at h
  exact ⟨by unfold paretoValue; linarith, by
    rw [paretoValue, log_inv, inv_mul_cancel₀ hy.ne']⟩

/-- Sufficiently small first shapes lose to the optimized Pareto face uniformly
in power and scale. -/
theorem small_shape_lt_pareto (hn : 0 < n) {y : Fin n → ℝ}
    (hy : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) (hsmall : (n : ℝ) * k ≤ k + 1) :
    likelihood y (k, c, b) < paretoValue y := by
  rw [likelihood_pareto_form hn y hk hc]
  have hav := avg_one_le j (fun i ↦ (sp_pos (-(c * y i + b))).le)
  rw [hj, mul_zero, zero_add] at hav
  have h1 := mul_le_mul_of_nonneg_left hav (show 0 ≤ k + 1 by positivity)
  have h2 : k * sp (-b) ≤ (k + 1) * (sp (-b) / n) := by
    calc
      k * sp (-b) = ((n : ℝ) * k) * (sp (-b) / n) := by
        field_simp [(npos hn).ne']
      _ ≤ _ := mul_le_mul_of_nonneg_right hsmall (by exact div_nonneg (sp_pos _).le (npos hn).le)
  have h3 : 0 < k * (b + sp (-b)) := by rw [← sp_shift]; exact mul_pos hk (sp_pos b)
  linarith [(pareto_optimized hy (mul_pos hk hc)).1]

/-- A path to the upper Pareto trace with a moving endpoint below the sample minimum. -/
theorem pareto_path (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < BurrHeld.avg y) :
    Tendsto (fun t : ℝ ↦ likelihood y ((avg y)⁻¹ / t^2, t^2, t))
      atTop (𝓝 (paretoValue y)) := by
  let a := (avg y)⁻¹
  have ha : 0 < a := inv_pos.mpr hmean
  have hT : ∀ᶠ t : ℝ in atTop, 1 ≤ t := eventually_ge_atTop 1
  have ht2 : Tendsto (fun t : ℝ ↦ t^2) atTop atTop :=
    tendsto_pow_atTop (by decide)
  have hk : Tendsto (fun t : ℝ ↦ a / t^2) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using (tendsto_const_nhds (x := a)).mul
      (tendsto_inv_atTop_zero.comp ht2)
  have hb : Tendsto (fun t : ℝ ↦ a / t) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using (tendsto_const_nhds (x := a)).mul tendsto_inv_atTop_zero
  have hs (i : Fin n) :
      Tendsto (fun t : ℝ ↦ sp (-(t^2 * y i + t))) atTop (𝓝 0) := by
    apply squeeze_zero (fun _ ↦ (sp_pos _).le) (fun t ↦ (sp_le_exp _).trans ?_)
      (tendsto_exp_atBot.comp tendsto_neg_atTop_atBot)
    exact exp_le_exp.mpr (by nlinarith [mul_nonneg (sq_nonneg t) (hy i)])
  have hav : Tendsto (fun t : ℝ ↦ avg (fun i ↦ sp (-(t^2 * y i + t))))
      atTop (𝓝 0) := by simpa [avg_const hn] using avg_tendsto hs
  have hlim := (tendsto_const_nhds (x := log a - a * avg y)).sub hb |>.sub
    ((hk.add_const 1).mul hav)
  simp only [sub_zero, zero_add, one_mul] at hlim
  rw [(pareto_optimized hmean ha).2] at hlim
  refine hlim.congr' (hT.mono fun t ht ↦ ?_)
  have ht' : t ≠ 0 := by linarith
  have hsq : 0 < t^2 := sq_pos_of_ne_zero ht'
  change _ = likelihood y (a / t^2, t^2, t)
  rw [likelihood_pareto_form hn y (div_pos ha hsq) hsq,
    div_mul_cancel₀ a hsq.ne', show a / t^2 * t = a / t by field_simp,
    (pareto_optimized hmean ha).2]

private theorem likelihood_shifted (hn : 0 < n) (y : Fin n → ℝ)
    {k : ℝ} (hk : 0 < k) (c b : ℝ) :
    likelihood y (k, c, b - log k) =
      log c + avg (fun i ↦ c * y i + b -
        (k + 1) * log (1 + exp (c * y i + b) / k)) := by
  unfold likelihood BurrHeld.firstLik
  dsimp
  have he (i : Fin n) :
      exp (c * y i + (b - log k)) = exp (c * y i + b) / k := by
    rw [show c * y i + (b - log k) = c * y i + b - log k by ring,
      exp_sub, exp_log hk]
  let r : Fin n → ℝ := fun i ↦ c * y i + b
  let f : Fin n → ℝ := fun i ↦ r i - (k + 1) * log (1 + exp (r i) / k)
  have h (i : Fin n) :
      sp (-(c * y i + (b - log k))) + k * sp (c * y i + (b - log k)) =
        log k - f i := by
    have hs := sp_shift (c * y i + (b - log k))
    unfold BurrHeld.softplus at hs ⊢
    rw [he] at hs ⊢
    dsimp [f, r]
    linarith
  simp_rw [h]
  rw [avg_sub, avg_const hn]
  change log k + log c - (log k - avg f) = log c + avg f
  ring

private theorem kernel_lower {k v : ℝ} (hk : 0 < k) (hv : 0 ≤ v) :
    v - v^2 / (2*k) ≤ (k+1) * log (1 + v/k) := by
  have h := le_log_one_add_of_nonneg (div_nonneg hv hk.le)
  have hden : 0 < v/k + 2 := by positivity
  have h1 : v - v^2 / (2*k) ≤ k * (2 * (v/k) / (v/k + 2)) := by
    rw [← mul_div_assoc, le_div_iff₀ hden]
    field_simp
    ring_nf
    nlinarith [mul_nonneg (sq_nonneg v) hv]
  have hp : 0 ≤ log (1 + v/k) := log_nonneg (by linarith [div_nonneg hv hk.le])
  linarith [h1.trans (mul_le_mul_of_nonneg_left h hk.le)]

private theorem kernel_limit {v : ℝ} (hv : 0 ≤ v) :
    Tendsto (fun k : ℝ ↦ (k+1) * log (1 + v/k)) atTop (𝓝 v) := by
  have hinv : Tendsto (fun k : ℝ ↦ k⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have hf (a : ℝ) : Tendsto (fun k : ℝ ↦ a/k) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using (tendsto_const_nhds (x := a)).mul hinv
  have hlo : Tendsto (fun k ↦ v - v^2/2/k) atTop (𝓝 v) := by
    simpa using (hf (v^2/2)).const_sub v
  have hhi : Tendsto (fun k ↦ v + v/k) atTop (𝓝 v) := by
    simpa using (hf v).const_add v
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hlo hhi
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
    convert kernel_lower hk hv using 1
    ring
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
    have h := log_le_sub_one_of_pos (show 0 < 1+v/k by positivity)
    calc
      _ ≤ (k+1) * (v/k) := by gcongr; linarith
      _ = _ := by field_simp

/-- First shape tending to infinity with the adjusted intercept fixed gives the Weibull face. -/
theorem weibull_path (hn : 0 < n) (y : Fin n → ℝ) (c b : ℝ) :
    Tendsto (fun k : ℝ ↦ likelihood y (k, c, b - log k)) atTop (𝓝 (weibullLik y c b)) := by
  have hlim := (avg_tendsto fun i ↦ (kernel_limit (exp_pos (c*y i+b)).le).const_sub
    (c*y i+b)).const_add (log c)
  refine hlim.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
  exact (likelihood_shifted hn y hk c b).symm

private theorem weibull_eq (hn : 0 < n) (y : Fin n → ℝ) (c : ℝ) {s : ℝ}
    (hs : 0 < s) :
    weibullLik y c (-(c * log s)) =
      Weibull.logLik (fun i ↦ exp (y i)) c s / n + avg y := by
  have h (i : Fin n) : Weibull.logDensity c s (exp (y i)) =
      log c - y i + (c * y i - c * log s) - exp (c * y i - c * log s) := by
    unfold Weibull.logDensity
    rw [rpow_def_of_pos (div_pos (exp_pos _) hs), log_div (exp_pos _).ne' hs.ne', log_exp]
    ring_nf
  change _ = avg (fun i ↦ Weibull.logDensity c s (exp (y i))) + avg y
  let z : Fin n → ℝ := fun i ↦ c * y i - c * log s
  have h' (i : Fin n) : Weibull.logDensity c s (exp (y i)) =
      log c + (z i - exp (z i)) - y i := by rw [h]; dsimp [z]; ring
  simp_rw [h']
  rw [avg_sub, avg_add, avg_const hn]
  unfold weibullLik
  have hz (i : Fin n) : c * y i + -(c * log s) = z i := by rfl
  simp_rw [hz]
  ring

/-- The Weibull scale optimization at fixed power has a closed-form value. -/
theorem weibull_profile_value (hn : 0 < n) (y : Fin n → ℝ) (c : ℝ) :
    let A := BurrHeld.avg (fun i ↦ exp (c*y i))
    let b := -log A
    weibullLik y c b = log c+c*BurrHeld.avg y-log A-1 ∧
      ∀ a, weibullLik y c a ≤ weibullLik y c b := by
  let A := avg (fun i ↦ exp (c*y i))
  have hA : 0 < A := (div_pos (exp_pos (c*y ⟨0,hn⟩)) (npos hn)).trans_le
    (avg_one_le (f := fun i ↦ exp (c*y i)) ⟨0,hn⟩ (fun _ ↦ (exp_pos _).le))
  have hform (a : ℝ) : weibullLik y c a = log c+c*avg y+a-exp a*A := by
    unfold weibullLik
    rw [avg_sub,avg_add,avg_mul,avg_const hn]
    have he (i : Fin n) : exp (c*y i+a) = exp a*exp (c*y i) := by rw [add_comm, exp_add]
    simp_rw [he]
    rw [avg_mul]
    ring
  have hb : weibullLik y c (-log A) = log c+c*avg y-log A-1 := by
    rw [hform,exp_neg,exp_log hA,inv_mul_cancel₀ hA.ne']
    ring
  refine ⟨hb,fun a ↦ ?_⟩
  rw [hform,hb]
  have hl := log_le_sub_one_of_pos (mul_pos (exp_pos a) hA)
  rw [log_mul (exp_pos a).ne' hA.ne',log_exp] at hl
  linarith

/-- The maximum over the Weibull face is attained at positive power and finite intercept. -/
theorem weibull_optimized (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃ c > 0, ∃ b, ∀ d > 0, ∀ a, weibullLik y d a ≤ weibullLik y c b := by
  obtain ⟨i, j, hij⟩ := hne
  obtain ⟨p, ⟨hp, hm⟩, _⟩ := Weibull.jointMLE hn (fun i ↦ exp_pos (y i))
      ⟨i, j, fun h ↦ hij (exp_injective h)⟩
  have hc : 0 < p.1 := hp.1
  have hs : 0 < p.2 := hp.2
  refine ⟨p.1, hc, -(p.1 * log p.2), fun d hd a ↦ ?_⟩
  let s := exp (-a/d)
  have hsa : -(d * log s) = a := by dsimp [s]; rw [log_exp]; field_simp
  rw [← hsa, weibull_eq hn y d (exp_pos _), weibull_eq hn y p.1 hs]
  gcongr
  exact hm (show (d, s) ∈ Weibull.domain from ⟨hd, exp_pos _⟩)


private theorem intercept_stationary (hn : 0 < n) (y : Fin n → ℝ) {k c b : ℝ}
    (hc : 0 < c) (hm : IsMaxOn (BurrHeld.firstLik y k) BurrHeld.firstDomain (c, b)) :
    avg (fun i ↦ exp (c*y i+b) / (1 + exp (c*y i+b))) * (k+1) = 1 := by
  let q : Fin n → ℝ := fun i ↦ exp (c*y i+b) / (1 + exp (c*y i+b))
  let score := avg (fun i ↦ 1 - (k+1) * q i)
  let f : ℝ → ℝ := fun a ↦ log k + log c +
    avg (fun i ↦ c*y i+a - (k+1)*sp (c*y i+a))
  have hder : HasDerivAt f score b := by
    apply HasDerivAt.const_add
    unfold score BurrHeld.avg
    apply HasDerivAt.div_const
    apply HasDerivAt.fun_sum
    intro i _
    have h := (hasDerivAt_id b).const_add (c*y i)
    have he := h.exp.const_add 1
    have hlog := he.log (show 1 + exp (c*y i+b) ≠ 0 by positivity)
    simpa [q, BurrHeld.softplus, Pi.sub_def] using h.sub (hlog.const_mul (k+1))
  have hmax : IsMaxOn f univ b := by
    intro a _
    have he (t : ℝ) : f t = BurrHeld.firstLik y k (c,t) := by
      unfold f BurrHeld.firstLik
      dsimp
      have h (i : Fin n) : c*y i+t - (k+1)*sp (c*y i+t) =
          -(sp (-(c*y i+t)) + k*sp (c*y i+t)) := by
        linarith [sp_shift (c*y i+t)]
      simp_rw [h]
      rw [show avg (fun i ↦ -(sp (-(c*y i+t)) + k*sp (c*y i+t))) =
        -(avg (fun i ↦ sp (-(c*y i+t)) + k*sp (c*y i+t))) by
          simp only [← zero_sub, avg_sub, avg_const hn]]
      ring
    change f a ≤ f b
    rw [he, he]
    exact hm (show (c,a) ∈ BurrHeld.firstDomain from ⟨hc, mem_univ _⟩)
  have hz : score = 0 := by
    rw [← hder.deriv]
    have hlocal : IsLocalMax f b := Eventually.of_forall fun a ↦ hmax (mem_univ a)
    exact hlocal.deriv_eq_zero
  dsimp [score] at hz
  rw [avg_sub, avg_const hn, avg_mul] at hz
  linarith

/-- At large first shape every finite Burr value is at most the optimized
Weibull value plus an explicit error tending to zero. -/
theorem large_shape_le_weibull (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {W : ℝ}
    (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) (hlarge : 2 * (n : ℝ) ≤ k) :
    likelihood y (k,c,b) ≤ W + 2 * (n : ℝ)^2/k := by
  obtain ⟨p, ⟨hp, hm⟩, _⟩ := BurrHeld.first_existsUnique hn hk hne
  have hp' : 0 < p.1 := hp.1
  have hb : ∀ i, exp (p.1*y i + (p.2 + log k)) ≤ 2 * n := by
    intro i
    let e := exp (p.1*y i+p.2)
    have he : 0 < e := exp_pos _
    let q : Fin n → ℝ := fun j ↦ exp (p.1*y j+p.2) / (1 + exp (p.1*y j+p.2))
    have hq := avg_one_le (f := q) i (fun j ↦ by dsimp [q]; positivity)
    have hstat : avg q * (k+1) = 1 := intercept_stationary hn y hp' hm
    have hq' : (k+1) * (e / (1+e)) ≤ n := by
      have h := (div_le_iff₀ (npos hn)).mp hq
      have h' := mul_le_mul_of_nonneg_right h (show 0 ≤ k+1 by positivity)
      change q i * (k+1) ≤ avg q * n * (k+1) at h'
      rw [show avg q * n * (k+1) = avg q * (k+1) * n by ring, hstat, one_mul] at h'
      simpa [q, e, mul_comm] using h'
    have hq'' : (k+1)*e ≤ n*(1+e) := by
      rw [← mul_div_assoc] at hq'
      exact (div_le_iff₀ (show 0 < 1+e by positivity)).mp hq'
    have hr : k*e ≤ 2*n := by
      have h := mul_le_mul_of_nonneg_right hlarge he.le
      nlinarith
    rw [show p.1*y i + (p.2 + log k) = (p.1*y i+p.2) + log k by ring,
      exp_add, exp_log hk]
    simpa [e, mul_comm] using hr
  let a := p.2 + log k
  have hl := likelihood_shifted hn y hk p.1 a
  dsimp [a] at hl
  rw [add_sub_cancel_right] at hl
  have ht : ∀ i, p.1*y i+a - (k+1)*log (1+exp (p.1*y i+a)/k) ≤
      (p.1*y i+a - exp (p.1*y i+a)) + 2*(n : ℝ)^2/k := by
    intro i
    let v := exp (p.1*y i+a)
    have hv : 0 ≤ v := (exp_pos _).le
    have h := kernel_lower hk hv
    have hh : v^2 / (2*k) ≤ 2*(n : ℝ)^2/k := by
      have hv' : v ≤ 2*n := hb i
      rw [← div_div]
      apply div_le_div_of_nonneg_right _ hk.le
      have hs : v^2 ≤ (2*(n : ℝ))^2 := by gcongr
      linarith
    dsimp [v] at h hh
    linarith
  have hcomp := avg_mono ht
  rw [avg_add, avg_const hn] at hcomp
  have hmax := hm (show (c,b) ∈ BurrHeld.firstDomain from ⟨hc, mem_univ _⟩)
  change likelihood y (k,c,b) ≤ likelihood y (k,p.1,p.2) at hmax
  have hw := hW p.1 hp' a
  unfold weibullLik at hw
  change likelihood y (k,p.1,p.2) = log p.1 +
    avg (fun i ↦ p.1*y i+a - (k+1)*log (1+exp (p.1*y i+a)/k)) at hl
  linarith


private theorem point_penalty {k m z : ℝ} (hm : 0 ≤ m) (hm1 : m ≤ 1) (hmk : m ≤ k) :
    m * |z| ≤ sp (-z) + k * sp z := by
  by_cases hz : 0 ≤ z
  · rw [abs_of_nonneg hz]
    nlinarith [mul_le_mul_of_nonneg_right hmk hz,
      mul_le_mul_of_nonneg_left (sp_ge z) (hm.trans hmk), sp_pos (-z)]
  · rw [abs_of_neg (lt_of_not_ge hz)]
    nlinarith [mul_le_mul_of_nonneg_right hm1 (neg_nonneg.mpr (le_of_not_ge hz)),
      sp_ge (-z), mul_nonneg (hm.trans hmk) (sp_pos z).le]

private theorem bounded_shape_superlevel (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {l u : ℝ} (hl : 0 < l) (hlu : l ≤ u) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ domain ∧
      ∀ p ∈ domain, p.1 ∈ Icc l u → M ≤ likelihood y p → p ∈ K := by
  obtain ⟨i,j,hij⟩ := hne
  let m := min 1 l
  have hm : 0 < m := lt_min zero_lt_one hl
  have hu : 0 < u := hl.trans_le hlu
  let d := m * |y i - y j| / (2*n)
  have hd : 0 < d := div_pos (mul_pos hm (abs_pos.mpr (sub_ne_zero.mpr hij))) (by positivity)
  let C := max (2/d) (2*(log u - log (d/2) - 1 - M)/d)
  have hC : 0 < C := (div_pos zero_lt_two hd).trans_le (le_max_left _ _)
  let A := exp (M-log u)
  let B := (log u + log C - M)*n/m + C*|y i|
  refine ⟨Icc l u ×ˢ (Icc A C ×ˢ Icc (-B) B),
    isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc), ?_, ?_⟩
  · rintro ⟨k,c,b⟩ ⟨hk,hc,_⟩
    exact ⟨hl.trans_le hk.1, (exp_pos _).trans_le hc.1, mem_univ _⟩
  rintro ⟨k,c,b⟩ ⟨hk,hc,_⟩ hk' hM
  change 0 < k at hk
  change 0 < c at hc
  let z : Fin n → ℝ := fun t ↦ c*y t+b
  have hlog := log_le_log hk hk'.2
  have hpk (t : Fin n) : m*|z t| ≤ sp (-(z t))+k*sp (z t) :=
    point_penalty hm.le (min_le_left _ _) ((min_le_right _ _).trans hk'.1)
  have hpen := avg_mono hpk
  rw [avg_mul] at hpen
  have hnon : 0 ≤ avg (fun t ↦ |z t|) := by
    unfold BurrHeld.avg
    exact div_nonneg (Finset.sum_nonneg fun _ _ ↦ abs_nonneg _) (npos hn).le
  change M ≤ log k+log c-avg (fun t ↦ sp (-(z t))+k*sp (z t)) at hM
  have hmain : m*avg (fun t ↦ |z t|) ≤ log u+log c-M := by linarith
  have hA : A ≤ c := by
    dsimp [A]
    rw [← exp_log hc, exp_le_exp]
    linarith [mul_nonneg hm.le hnon]
  have hz (t : Fin n) : |z t| ≤ avg (fun t ↦ |z t|)*n :=
    (div_le_iff₀ (npos hn)).mp (avg_one_le t (fun _ ↦ abs_nonneg _))
  have hdiff := abs_sub (z i) (z j)
  rw [show z i-z j = c*(y i-y j) by dsimp [z]; ring, abs_mul, abs_of_pos hc] at hdiff
  have hspread : d*c ≤ m*avg (fun t ↦ |z t|) := by
    have h : c*|y i-y j| ≤ 2*(avg (fun t ↦ |z t|)*n) := by linarith [hz i,hz j]
    dsimp [d]
    rw [div_mul_eq_mul_div, div_le_iff₀ (show 0 < 2*(n : ℝ) by positivity)]
    linear_combination m*h
  have hd2 : 0 < d/2 := by positivity
  have hlin := log_le_sub_one_of_pos (mul_pos hd2 hc)
  rw [log_mul hd2.ne' hc.ne'] at hlin
  have hCb : c ≤ C := by
    apply le_trans _ (le_max_right _ _)
    apply (le_div_iff₀ hd).mpr
    linarith
  have hlogC := log_le_log hc hCb
  have hzi : |z i| ≤ (log u+log C-M)*n/m := by
    apply (le_div_iff₀ hm).mpr
    have h := mul_le_mul_of_nonneg_right hmain (npos hn).le
    have h' := mul_le_mul_of_nonneg_right (hz i) hm.le
    have h'' := mul_le_mul_of_nonneg_right hlogC (npos hn).le
    nlinarith
  have hb : |b| ≤ B := by
    have h := abs_sub (z i) (c*y i)
    rw [show z i-c*y i = b by dsimp [z]; ring, abs_mul, abs_of_pos hc] at h
    have h' := mul_le_mul_of_nonneg_right hCb (abs_nonneg (y i))
    dsimp [B]
    linarith
  exact ⟨hk', ⟨hA,hCb⟩, abs_le.mp hb⟩

/-- If the first shape stays in a fixed positive compact interval,
any simultaneous escape of the other coefficients has log likelihood tending to minus infinity. -/
theorem bounded_shape_escape (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {a b : ℝ} (ha : 0 < a) (hab : a ≤ b)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ × ℝ}
    (hp : ∀ᶠ t in l, p t ∈ domain ∧ (p t).1 ∈ Icc a b)
    (hescape : ∀ K : Set (ℝ × ℝ × ℝ), IsCompact K → K ⊆ domain →
      ∀ᶠ t in l, p t ∉ K) :
    Tendsto (fun t ↦ likelihood y (p t)) l atBot := by
  apply tendsto_atBot.mpr
  intro M
  obtain ⟨K,hK,hKD,hbox⟩ := bounded_shape_superlevel hn hne ha hab M
  filter_upwards [hp,hescape K hK hKD] with t ht htK
  exact (lt_of_not_ge fun h ↦ htK (hbox _ ht.1 ht.2 h)).le

/-- Every strict superlevel above both face values is contained in a compact
subset of the feasible domain. This bounds all escapes, including simultaneous paths. -/
theorem competitive_superlevel_compact (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {W M : ℝ} (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    (hP : paretoValue y < M) (hWM : W < M) :
    ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ domain ∧
      ∀ p ∈ domain, M ≤ likelihood y p → p ∈ K := by
  let l := (n : ℝ)⁻¹
  let u := max l (max (2*n) (2*(n : ℝ)^2/(M-W)))
  have hl : 0 < l := inv_pos.mpr (npos hn)
  obtain ⟨K,hK,hKD,hbox⟩ := bounded_shape_superlevel hn hne hl
    (le_max_left l (max (2*n) (2*(n : ℝ)^2/(M-W)))) M
  refine ⟨K,hK,hKD,?_⟩
  rintro ⟨k,c,b⟩ hp hM
  have hk : 0 < k := hp.1
  have hc : 0 < c := hp.2.1
  apply hbox _ hp _ hM
  constructor
  · by_contra! h
    have hs : (n : ℝ)*k ≤ k+1 := by
      have h' := (lt_inv_comm₀ hk (npos hn)).mp h
      have h'' := (lt_div_iff₀ hk).mp (show (n : ℝ) < 1/k by simpa using h')
      nlinarith
    linarith [small_shape_lt_pareto hn hmean hj (b := b) hk hc hs]
  · by_contra! h
    have h1 : 2*(n : ℝ) ≤ k := (le_max_left _ _).trans ((le_max_right _ _).trans h.le)
    have h2 : 2*(n : ℝ)^2/(M-W) < k := (le_max_right _ _).trans_lt
      ((le_max_right _ _).trans_lt h)
    have h3 := (div_lt_iff₀ (sub_pos.mpr hWM)).mp h2
    have h4 := (div_lt_iff₀ hk).mpr (show 2*(n : ℝ)^2 < (M-W)*k by linarith)
    linarith [large_shape_le_weibull hn hne hW (b := b) hk hc h1]


/-- Along any net escaping every compact subset of the domain, the likelihood
is eventually below every number strictly above both optimized faces. -/
theorem escape_eventually_lt (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {W : ℝ} (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ × ℝ}
    (hp : ∀ᶠ t in l, p t ∈ domain)
    (hescape : ∀ K : Set (ℝ × ℝ × ℝ), IsCompact K → K ⊆ domain →
      ∀ᶠ t in l, p t ∉ K) {M : ℝ} (hM : max (paretoValue y) W < M) :
    ∀ᶠ t in l, likelihood y (p t) < M := by
  obtain ⟨K, hK, hKD, hb⟩ := competitive_superlevel_compact hn hne hmean hj hW
    ((le_max_left _ _).trans_lt hM) ((le_max_right _ _).trans_lt hM)
  filter_upwards [hp, hescape K hK hKD] with t ht htK
  exact lt_of_not_ge fun hm ↦ htK (hb _ ht hm)

private theorem likelihood_continuousOn (y : Fin n → ℝ) :
    ContinuousOn (likelihood y) domain := by
  have hs : Continuous sp := by
    unfold BurrHeld.softplus
    exact (continuous_const.add continuous_exp).log (fun t ↦
      show 1 + exp t ≠ 0 from (add_pos zero_lt_one (exp_pos t)).ne')
  have hz (i : Fin n) : Continuous (fun p : ℝ × ℝ × ℝ ↦ p.2.1*y i+p.2.2) :=
    (continuous_fst.comp continuous_snd |>.mul continuous_const).add
      (continuous_snd.comp continuous_snd)
  have h1 : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ log p.1) domain :=
    continuous_fst.continuousOn.log fun p hp ↦ (show 0 < p.1 from hp.1).ne'
  have h2 : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ log p.2.1) domain :=
    (continuous_fst.comp continuous_snd).continuousOn.log
      fun p hp ↦ (show 0 < p.2.1 from hp.2.1).ne'
  apply (h1.add h2).sub
  unfold BurrHeld.avg
  exact ((continuous_finsetSum _ fun i _ ↦
    (hs.comp (hz i).neg).add (continuous_fst.mul (hs.comp (hz i)))).div_const _).continuousOn

/-- A finite Burr maximizer exists exactly when a finite point reaches or
exceeds the better of the two optimized escape values. -/
theorem jointMLE_iff (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hne : ∃ i j, y i ≠ y j)
    (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {c b : ℝ} (hc : 0 < c) (hW : ∀ d > 0, ∀ a, weibullLik y d a ≤ weibullLik y c b) :
    (∃ p ∈ domain, IsMaxOn (likelihood y) domain p) ↔
      ∃ p ∈ domain, max (paretoValue y) (weibullLik y c b) ≤ likelihood y p := by
  constructor
  · rintro ⟨p, hp, hm⟩
    refine ⟨p,hp,max_le ?_ ?_⟩
    · apply le_of_tendsto (pareto_path hn hy hmean)
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      exact hm (show ( (avg y)⁻¹/t^2,t^2,t) ∈ domain from
        ⟨div_pos (inv_pos.mpr hmean) (by positivity), show 0 < t^2 by positivity, mem_univ _⟩)
    · apply le_of_tendsto (weibull_path hn y c b)
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
      exact hm (show (k,c,b-log k) ∈ domain from ⟨hk,hc,mem_univ _⟩)
  · rintro ⟨p,hp,hpB⟩
    by_cases hm : IsMaxOn (likelihood y) domain p
    · exact ⟨p,hp,hm⟩
    have hex : ∃ q ∈ domain, likelihood y p < likelihood y q := by
      by_contra! h
      exact hm (fun _ hq ↦ h _ hq)
    obtain ⟨q,hq,hqp⟩ := hex
    have hB := hpB.trans_lt hqp
    obtain ⟨K,hK,hKD,hbox⟩ := competitive_superlevel_compact hn hne hmean hj hW
      ((le_max_left _ _).trans_lt hB) ((le_max_right _ _).trans_lt hB)
    have hqK := hbox q hq le_rfl
    obtain ⟨r,hr,hrm⟩ := hK.exists_isMaxOn ⟨q,hqK⟩ ((likelihood_continuousOn y).mono hKD)
    refine ⟨r,hKD hr,fun s hs ↦ ?_⟩
    by_cases h : likelihood y q ≤ likelihood y s
    · exact hrm (hbox s hs h)
    · exact (le_of_not_ge h).trans (hrm hqK)


/-- The tied-shape submodel has compact containment at every finite
likelihood level for nonconstant data. -/
theorem diagonal_superlevel_compact (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ BurrHeld.firstDomain ∧
      ∀ p ∈ BurrHeld.firstDomain, M ≤ likelihood y (p.1,p.1,p.2) → p ∈ K := by
  obtain ⟨i,j,hij⟩ := hne
  let l := min 1 (exp (M/2))
  have hl : 0 < l := lt_min zero_lt_one (exp_pos _)
  let d := |y i-y j|/(2*n)
  have hd : 0 < d := div_pos (abs_pos.mpr (sub_ne_zero.mpr hij)) (by positivity)
  let u := max 1 (4*(-log (d/4)-1-M/2)/d)
  have hlu : l ≤ u := (min_le_left _ _).trans (le_max_left _ _)
  obtain ⟨K,hK,hKD,hbox⟩ := bounded_shape_superlevel hn ⟨i,j,hij⟩ hl hlu M
  let diagonal : ℝ × ℝ × ℝ → ℝ := fun p ↦ p.1-p.2.1
  have hclosed : IsClosed (diagonal ⁻¹' {0}) :=
    isClosed_singleton.preimage (continuous_fst.sub (continuous_fst.comp continuous_snd))
  refine ⟨Prod.snd '' (K ∩ diagonal ⁻¹' {0}),
    (hK.inter_right hclosed).image continuous_snd, ?_, ?_⟩
  · rintro _ ⟨q,hq,rfl⟩
    exact (hKD hq.1).2
  rintro ⟨k,b⟩ ⟨hk,_⟩ hM
  change 0 < k at hk
  let z : Fin n → ℝ := fun t ↦ k*y t+b
  change M ≤ log k+log k-avg (fun t ↦ sp (-(z t))+k*sp (z t)) at hM
  have hnon : 0 ≤ avg (fun t ↦ sp (-(z t))+k*sp (z t)) := by
    unfold BurrHeld.avg
    exact div_nonneg (Finset.sum_nonneg fun t _ ↦
      add_nonneg (sp_pos _).le (mul_nonneg hk.le (sp_pos _).le)) (npos hn).le
  have hlk : l ≤ k := by
    apply (min_le_right _ _).trans
    rw [← exp_log hk, exp_le_exp]
    linarith
  have hku : k ≤ u := by
    by_cases h1 : k ≤ 1
    · exact h1.trans (le_max_left _ _)
    have h1' : 1 ≤ k := le_of_not_ge h1
    have hpen := avg_mono (fun t ↦ point_penalty (z := z t) zero_le_one le_rfl h1')
    simp only [one_mul] at hpen
    have hzi (t : Fin n) : |z t| ≤ avg (fun t ↦ |z t|)*n :=
      (div_le_iff₀ (npos hn)).mp (avg_one_le t (fun _ ↦ abs_nonneg _))
    have hdiff := abs_sub (z i) (z j)
    rw [show z i-z j = k*(y i-y j) by dsimp [z]; ring, abs_mul, abs_of_pos hk] at hdiff
    have hspread : d*k ≤ avg (fun t ↦ |z t|) := by
      dsimp [d]
      rw [div_mul_eq_mul_div, div_le_iff₀ (show 0 < 2*(n : ℝ) by positivity)]
      nlinarith [hzi i,hzi j]
    have hd4 : 0 < d/4 := by positivity
    have hlog := log_le_sub_one_of_pos (mul_pos hd4 hk)
    rw [log_mul hd4.ne' hk.ne'] at hlog
    apply le_trans _ (le_max_right _ _)
    apply (le_div_iff₀ hd).mpr
    linarith
  exact ⟨(k,k,b),⟨hbox _ ⟨hk,hk,mem_univ _⟩ ⟨hlk,hku⟩ hM, by simp [diagonal]⟩,rfl⟩

/-- Every nonconstant sample has a finite maximum on the tied-shape submodel. -/
theorem diagonal_MLE (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃ p ∈ BurrHeld.firstDomain,
      IsMaxOn (fun q ↦ likelihood y (q.1,q.1,q.2)) BurrHeld.firstDomain p := by
  let L : ℝ × ℝ → ℝ := fun q ↦ likelihood y (q.1,q.1,q.2)
  have hp : ((1,0) : ℝ × ℝ) ∈ BurrHeld.firstDomain := ⟨zero_lt_one, mem_univ _⟩
  obtain ⟨K,hK,hKD,hbox⟩ := diagonal_superlevel_compact hn hne (L (1,0))
  have hpK : (1,0) ∈ K := hbox _ hp le_rfl
  have hL : ContinuousOn L BurrHeld.firstDomain :=
    (likelihood_continuousOn y).comp
      (continuous_fst.prodMk continuous_id).continuousOn
      (fun _ h ↦ ⟨h.1,h.1,h.2⟩)
  obtain ⟨q,hq,hm⟩ := hK.exists_isMaxOn ⟨_,hpK⟩ (hL.mono hKD)
  refine ⟨q,hKD hq,fun t ht ↦ ?_⟩
  by_cases h : L (1,0) ≤ L t
  · exact hm (hbox _ ht h)
  · exact (le_of_not_ge h).trans (hm hpK)


/-- Log sample relative to a nominated endpoint. -/
def logExcess (x : Fin n → ℝ) (j : Fin n) : Fin n → ℝ :=
  fun i ↦ log (x i) - log (x j)

/-- Positive physical scale domain, with first shape and power ordered as in the dossier. -/
def physicalDomain : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ Ioi 0)

private theorem logExcess_spec (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) {j : Fin n}
    (hj : ∀ i, x j ≤ x i) :
    (∀ i, 0 ≤ logExcess x j i) ∧ (∃ i k, logExcess x j i ≠ logExcess x j k) ∧
      0 < avg (logExcess x j) := by
  have hy (i : Fin n) : 0 ≤ logExcess x j i := sub_nonneg.mpr (log_le_log (hx j) (hj i))
  obtain ⟨a,b,hab⟩ := hne
  have hne' : ∃ i k, logExcess x j i ≠ logExcess x j k := by
    refine ⟨a,b,fun h ↦ hab ?_⟩
    have h' : log (x a) = log (x b) := by unfold logExcess at h; linarith
    rw [← exp_log (hx a), ← exp_log (hx b), h']
  have hp : ∃ i, 0 < logExcess x j i := by
    by_contra! h
    obtain ⟨i,k,hik⟩ := hne'
    exact hik ((le_antisymm (h i) (hy i)).trans (le_antisymm (h k) (hy k)).symm)
  obtain ⟨i,hi⟩ := hp
  exact ⟨hy,hne', (div_pos hi (npos hn)).trans_le (avg_one_le i hy)⟩

private def scaleChart (r : ℝ) (p : ℝ × ℝ × ℝ) : ℝ × ℝ × ℝ :=
  (p.1,p.2.1,p.2.1*(r-log p.2.2))

private def fromChart (r : ℝ) (p : ℝ × ℝ × ℝ) : ℝ × ℝ × ℝ :=
  (p.1,p.2.1,exp (r-p.2.2/p.2.1))

private theorem fromChart_mem (r : ℝ) {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
    fromChart r p ∈ physicalDomain := ⟨hp.1,hp.2.1,exp_pos _⟩

private theorem scaleChart_bij (r : ℝ) : BijOn (scaleChart r) physicalDomain domain := by
  refine ⟨fun _ hp ↦ ⟨hp.1,hp.2.1,mem_univ _⟩, ?_, ?_⟩
  · rintro ⟨k,c,s⟩ hp ⟨k',c',s'⟩ hp' h
    simp only [scaleChart, Prod.mk.injEq] at h ⊢
    obtain ⟨rfl,rfl,h⟩ := h
    have hc : 0 < c := hp.2.1
    have hs : 0 < s := hp.2.2
    have hs' : 0 < s' := hp'.2.2
    have he := mul_left_cancel₀ hc.ne' h
    have hlog : log s = log s' := by linarith
    exact ⟨rfl,rfl,by rw [← exp_log hs, ← exp_log hs', hlog]⟩
  · rintro ⟨k,c,b⟩ hp
    have hc : 0 < c := hp.2.1
    refine ⟨fromChart r (k,c,b), fromChart_mem r hp, ?_⟩
    simp only [fromChart, scaleChart, log_exp, Prod.mk.injEq, true_and]
    field_simp
    ring

private theorem fromChart_left (r : ℝ) {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain) :
    fromChart r (scaleChart r p) = p := by
  rcases p with ⟨k,c,s⟩
  have hc : 0 < c := hp.2.1
  have hs : 0 < s := hp.2.2
  simp only [fromChart, scaleChart, Prod.mk.injEq, true_and, mul_div_cancel_left₀ _ hc.ne']
  rw [sub_sub_cancel, exp_log hs]

private theorem fromChart_continuousOn (r : ℝ) : ContinuousOn (fromChart r) domain := by
  have hc : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ p.2.1) domain :=
    (continuous_fst.comp continuous_snd).continuousOn
  have hb : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ p.2.2) domain :=
    (continuous_snd.comp continuous_snd).continuousOn
  have h : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ r-p.2.2/p.2.1) domain :=
    continuous_const.continuousOn.sub
      (hb.div hc (fun p hp ↦ (show 0 < p.2.1 from hp.2.1).ne'))
  exact continuous_fst.continuousOn.prodMk (hc.prodMk (continuous_exp.comp_continuousOn h))

private theorem physical_likelihood (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain) :
    BurrHeld.logLik x p.1 p.2.1 p.2.2 =
      likelihood (logExcess x j) (scaleChart (log (x j)) p) -
        avg (fun i ↦ log (x i)) := by
  rw [BurrHeld.logLik_eq_first hn hx hp.2.2]
  unfold likelihood BurrHeld.firstLik
  have h (i : Fin n) :
      p.2.1 * logExcess x j i + (scaleChart (log (x j)) p).2.2 =
        p.2.1 * log (x i) + -(p.2.1 * log p.2.2) := by
    unfold scaleChart logExcess
    dsimp
    ring
  change _ = log p.1 + log p.2.1 - avg (fun i ↦
    sp (-(p.2.1 * logExcess x j i + (scaleChart (log (x j)) p).2.2)) +
    p.1 * sp (p.2.1 * logExcess x j i + (scaleChart (log (x j)) p).2.2)) -
    avg (fun i ↦ log (x i))
  simp_rw [h]

private theorem transported_criterion {D : Set (ℝ × ℝ × ℝ)}
    {f : (ℝ × ℝ × ℝ) → ℝ} {y : Fin n → ℝ} {φ : (ℝ × ℝ × ℝ) → (ℝ × ℝ × ℝ)}
    (hφ : BijOn φ D domain) {a C : ℝ} (ha : 0 < a)
    (hf : ∀ p ∈ D, f p = a * likelihood y (φ p) + C)
    {B : ℝ} (h : (∃ p ∈ domain, IsMaxOn (likelihood y) domain p) ↔
      ∃ p ∈ domain, B ≤ likelihood y p) :
    (∃ p ∈ D, IsMaxOn f D p) ↔ ∃ p ∈ D, a*B+C ≤ f p := by
  have hm : (∃ p ∈ D, IsMaxOn f D p) ↔
      ∃ p ∈ domain, IsMaxOn (likelihood y) domain p := by
    have h1 {p} (hp : p ∈ D) :
        IsMaxOn f D p ↔ IsMaxOn (likelihood y) domain (φ p) := by
      constructor
      · intro hq q hq'
        obtain ⟨s,hs,rfl⟩ := hφ.surjOn hq'
        have hs' : f s ≤ f p := hq hs
        rw [hf s hs,hf p hp] at hs'
        exact le_of_mul_le_mul_left (le_of_add_le_add_right hs') ha
      · intro hq s hs
        change f s ≤ f p
        rw [hf s hs,hf p hp]
        exact add_le_add_left (mul_le_mul_of_nonneg_left (hq (hφ.mapsTo hs)) ha.le) C
    constructor
    · rintro ⟨p,hp,hm⟩
      exact ⟨φ p,hφ.mapsTo hp,(h1 hp).mp hm⟩
    · rintro ⟨q,hq,hm⟩
      obtain ⟨p,hp,rfl⟩ := hφ.surjOn hq
      exact ⟨p,hp,(h1 hp).mpr hm⟩
  rw [hm,h]
  constructor
  · rintro ⟨q,hq,hB⟩
    obtain ⟨p,hp,rfl⟩ := hφ.surjOn hq
    exact ⟨p,hp,by rw [hf p hp]; gcongr⟩
  · rintro ⟨p,hp,hB⟩
    rw [hf p hp] at hB
    exact ⟨φ p,hφ.mapsTo hp,le_of_mul_le_mul_left (le_of_add_le_add_right hB) ha⟩

private theorem transported_compact {D : Set (ℝ × ℝ × ℝ)}
    {f : (ℝ × ℝ × ℝ) → ℝ} {y : Fin n → ℝ} {φ ψ : (ℝ × ℝ × ℝ) → (ℝ × ℝ × ℝ)}
    (hφ : MapsTo φ D domain) (hψ : MapsTo ψ domain D)
    (hleft : ∀ p ∈ D, ψ (φ p) = p) (hcont : ContinuousOn ψ domain)
    {a C : ℝ} (ha : 0 < a) (hf : ∀ p ∈ D, f p = a * likelihood y (φ p) + C)
    {M : ℝ} (h : ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ domain ∧
      ∀ p ∈ domain, M ≤ likelihood y p → p ∈ K) :
    ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ D ∧
      ∀ p ∈ D, a*M+C ≤ f p → p ∈ K := by
  obtain ⟨K,hK,hKD,hbox⟩ := h
  refine ⟨ψ '' K,hK.image_of_continuousOn (hcont.mono hKD),
    fun _ ⟨q,hq,he⟩ ↦ he ▸ hψ (hKD hq),fun p hp hM ↦ ?_⟩
  rw [hf p hp] at hM
  exact ⟨φ p,hbox _ (hφ hp) (le_of_mul_le_mul_left (le_of_add_le_add_right hM) ha),
    hleft p hp⟩

private theorem burr_total_mean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    Transfer.Burr.logLik x c k s = n * BurrHeld.logLik x k c s := by
  have h (i : Fin n) : Transfer.Burr.logDensity c k s (x i) = BurrHeld.logDensity k c s (x i) := by
    unfold Transfer.Burr.logDensity BurrHeld.logDensity BurrHeld.softplus
    rw [rpow_def_of_pos (div_pos (hx i) hs), log_div (hx i).ne' hs.ne',
      log_div hs.ne' (hx i).ne']
    ring_nf
  unfold Transfer.Burr.logLik BurrHeld.logLik BurrHeld.avg
  simp_rw [h]
  field_simp [(npos hn).ne']


private theorem physical_conclusions (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i)
    {f : ℝ × ℝ × ℝ → ℝ} {φ ψ : (ℝ × ℝ × ℝ) → (ℝ × ℝ × ℝ)}
    (hφ : BijOn φ physicalDomain domain) (hψ : MapsTo ψ domain physicalDomain)
    (hleft : ∀ p ∈ physicalDomain, ψ (φ p) = p) (hcont : ContinuousOn ψ domain)
    {a C : ℝ} (ha : 0 < a) (hf : ∀ p ∈ physicalDomain,
      f p = a * likelihood (logExcess x j) (φ p) + C) :
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik (logExcess x j) d t ≤
      weibullLik (logExcess x j) c b) ∧
    let B := max (paretoValue (logExcess x j)) (weibullLik (logExcess x j) c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, a*B+C ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, a*M+C ≤ f p → p ∈ K := by
  obtain ⟨hy,hne',hmean⟩ := logExcess_spec hn hx hne hj
  have hj0 : logExcess x j j = 0 := sub_self _
  obtain ⟨c,hc,b,hW⟩ := weibull_optimized hn hne'
  refine ⟨c,hc,b,hW,transported_criterion hφ ha hf
    (jointMLE_iff hn hy hne' hmean hj0 hc hW),fun M hM ↦ ?_⟩
  exact transported_compact hφ.mapsTo hψ hleft hcont ha hf
    (competitive_superlevel_compact hn hne' hmean hj0 hW
      ((le_max_left _ _).trans_lt hM) ((le_max_right _ _).trans_lt hM))

/-- The escape bound and exact finite-witness criterion in physical Burr coefficients. -/
theorem physical_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) :
    let y := logExcess x j
    let f := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik x p.1 p.2.1 p.2.2
    let J := BurrHeld.avg (fun i ↦ log (x i))
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, M-J ≤ f p → p ∈ K := by
  simpa only [one_mul, sub_eq_add_neg] using
    physical_conclusions hn hx hne hj (scaleChart_bij (log (x j)))
      (fun _ hp ↦ fromChart_mem _ hp) (fun _ hp ↦ fromChart_left _ hp)
      (fromChart_continuousOn _) (a := 1) (C := -avg (fun i ↦ log (x i))) zero_lt_one
      (fun _ hp ↦ by simpa only [one_mul, ← sub_eq_add_neg] using physical_likelihood hn hx j hp)

private theorem paretoIV_physical_likelihood (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (j : Fin n) {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain) :
    let e := ShapeFacesParetoIV.excess x μ
    ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2 =
      n*likelihood (logExcess e j) (scaleChart (log (e j)) p) - ∑ i, log (e i) := by
  dsimp only
  let e := ShapeFacesParetoIV.excess x μ
  have he (i : Fin n) : 0 < e i := sub_pos.mpr (hx i)
  have hJ : (n : ℝ) * avg (fun i ↦ log (e i)) = ∑ i, log (e i) := by
    unfold BurrHeld.avg
    field_simp [(npos hn).ne']
  rw [ShapeFacesParetoIV.logLik_excess, burr_total_mean hn he _ _ hp.2.2,
    physical_likelihood hn he j hp, mul_sub, hJ]

/-- The exact criterion and all-escape bound for Pareto IV with location held. -/
theorem paretoIV_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) :
    let e := ShapeFacesParetoIV.excess x μ
    let y := logExcess e j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let J := ∑ i, log (e i)
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, n*B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, n*M-J ≤ f p → p ∈ K := by
  dsimp only
  let e := ShapeFacesParetoIV.excess x μ
  have he (i : Fin n) : 0 < e i := sub_pos.mpr (hx i)
  obtain ⟨i,k,hik⟩ := hne
  have hne' : ∃ i k, e i ≠ e k := ⟨i,k,fun h ↦ hik (sub_left_inj.mp h)⟩
  have hmin (i : Fin n) : e j ≤ e i := sub_le_sub_right (hj i) μ
  simpa only [sub_eq_add_neg] using physical_conclusions hn he hne' hmin
    (scaleChart_bij (log (e j))) (fun _ hp ↦ fromChart_mem _ hp)
    (fun _ hp ↦ fromChart_left _ hp) (fromChart_continuousOn _) (npos hn)
    (fun _ hp ↦ (paretoIV_physical_likelihood hn hx j hp).trans (sub_eq_add_neg _ _))

private theorem reciprocate_mem {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain) :
    ShapeFacesInverseBurr.reciprocateScale p ∈ physicalDomain :=
  ⟨hp.1,hp.2.1,inv_pos.mpr hp.2.2⟩

private theorem reciprocate_twice (p : ℝ × ℝ × ℝ) :
    ShapeFacesInverseBurr.reciprocateScale (ShapeFacesInverseBurr.reciprocateScale p) = p := by
  rcases p with ⟨k,c,s⟩
  simp [ShapeFacesInverseBurr.reciprocateScale]

private theorem reciprocate_bij :
    BijOn ShapeFacesInverseBurr.reciprocateScale physicalDomain physicalDomain := by
  refine ⟨fun _ hp ↦ reciprocate_mem hp,fun p _ q _ h ↦ ?_,fun q hq ↦ ?_⟩
  · simpa only [reciprocate_twice] using congrArg ShapeFacesInverseBurr.reciprocateScale h
  · exact ⟨_,reciprocate_mem hq,reciprocate_twice q⟩

private theorem inverse_chart_continuous (r : ℝ) :
    ContinuousOn (ShapeFacesInverseBurr.reciprocateScale ∘ fromChart r) domain := by
  have h1 : ContinuousOn (fun p : ℝ × ℝ × ℝ ↦ (fromChart r p).2.2) domain :=
    (continuous_snd.comp continuous_snd).comp_continuousOn (fromChart_continuousOn r)
  exact continuous_fst.continuousOn.prodMk
    ((continuous_fst.comp continuous_snd).continuousOn.prodMk
    (h1.inv₀ fun p hp ↦ (show 0 < (fromChart r p).2.2 from (fromChart_mem _ hp).2.2).ne'))

private theorem inverse_physical_likelihood (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) {p : ℝ × ℝ × ℝ} (hp : p ∈ physicalDomain) :
    let e := Transfer.reciprocal x
    ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2 =
      n*likelihood (logExcess e j) (scaleChart (log (e j))
        (ShapeFacesInverseBurr.reciprocateScale p)) - ∑ i, log (x i) := by
  dsimp only
  let e := Transfer.reciprocal x
  have he (i : Fin n) : 0 < e i := inv_pos.mpr (hx i)
  let φ := scaleChart (log (e j)) ∘ ShapeFacesInverseBurr.reciprocateScale
  rw [ShapeFacesInverseBurr.logLik_reciprocal hx _ _ hp.2.2,
    burr_total_mean hn he _ _ (inv_pos.mpr hp.2.2)]
  change n*BurrHeld.logLik e p.1 p.2.1 p.2.2⁻¹ + Transfer.reciprocalJac x = _
  have h := physical_likelihood hn he j (reciprocate_mem hp)
  change BurrHeld.logLik e p.1 p.2.1 p.2.2⁻¹ =
    likelihood (logExcess e j) (φ p) - avg (fun i ↦ log (e i)) at h
  rw [h,mul_sub]
  unfold BurrHeld.avg Transfer.reciprocalJac
  dsimp only [e, Transfer.reciprocal]
  simp only [log_inv, Finset.sum_neg_distrib, ← Finset.mul_sum]
  change n*likelihood (logExcess e j) (φ p) - n*(-(∑ t, log (x t))/n) +
    -2*(∑ t, log (x t)) = _
  rw [mul_div_cancel₀ _ (npos hn).ne']
  simp [φ, e, Transfer.reciprocal, Function.comp_def, log_inv]
  ring

/-- The exact criterion and all-escape bound for inverse Burr. Its power
face is supported below the maximum and its Weibull face is reflected. -/
theorem inverse_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x i ≤ x j) :
    let y := logExcess (Transfer.reciprocal x) j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let J := ∑ i, log (x i)
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, n*B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, n*M-J ≤ f p → p ∈ K := by
  dsimp only
  let e := Transfer.reciprocal x
  have he (i : Fin n) : 0 < e i := inv_pos.mpr (hx i)
  obtain ⟨i,k,hik⟩ := hne
  have hne' : ∃ i k, e i ≠ e k := ⟨i,k,fun h ↦ hik (inv_injective h)⟩
  have hmin (i : Fin n) : e j ≤ e i := inv_anti₀ (hx i) (hj i)
  let r := log (e j)
  simpa only [sub_eq_add_neg] using physical_conclusions hn he hne' hmin
    ((scaleChart_bij r).comp reciprocate_bij)
    (fun _ hp ↦ reciprocate_mem (fromChart_mem r hp))
    (fun p hp ↦ by
      change ShapeFacesInverseBurr.reciprocateScale
        (fromChart r (scaleChart r (ShapeFacesInverseBurr.reciprocateScale p))) = p
      rw [fromChart_left r (reciprocate_mem hp),reciprocate_twice])
    (inverse_chart_continuous r) (npos hn)
    (fun _ hp ↦ (inverse_physical_likelihood hn hx j hp).trans (sub_eq_add_neg _ _))


private def pairChart (v : ℝ) (p : ℝ × ℝ) : ℝ × ℝ := (p.1,v*p.1*log p.2)

private def fromPair (v : ℝ) (p : ℝ × ℝ) : ℝ × ℝ := (p.1, exp (v*p.2/p.1))

private theorem fromPair_mem (v : ℝ) {p : ℝ × ℝ} (hp : p ∈ BurrHeld.firstDomain) :
    fromPair v p ∈ Weibull.domain := ⟨hp.1,exp_pos _⟩

private theorem pair_bij {v : ℝ} (hv : v * v = 1) :
    BijOn (pairChart v) Weibull.domain BurrHeld.firstDomain := by
  have hv' : v ≠ 0 := by intro h; simp [h] at hv
  refine ⟨fun _ hp ↦ ⟨hp.1,mem_univ _⟩,?_ ,?_⟩
  · rintro ⟨a,s⟩ hp ⟨a',s'⟩ hp' h
    simp only [pairChart, Prod.mk.injEq] at h ⊢
    obtain ⟨rfl,he⟩ := h
    have ha : 0 < a := hp.1
    have hs : 0 < s := hp.2
    have hs' : 0 < s' := hp'.2
    have hlog := mul_left_cancel₀ (mul_ne_zero hv' ha.ne') he
    exact ⟨rfl,by rw [← exp_log hs, ← exp_log hs', hlog]⟩
  · rintro ⟨a,b⟩ hp
    have ha : 0 < a := hp.1
    refine ⟨fromPair v (a,b), fromPair_mem v hp,?_⟩
    simp only [pairChart,fromPair,Prod.mk.injEq,true_and,log_exp]
    have h : v*a*(v*b/a) = (v*v)*b := by field_simp
    rw [h,hv,one_mul]

private theorem fromPair_left {v : ℝ} (hv : v * v = 1) {p : ℝ × ℝ}
    (hp : p ∈ Weibull.domain) : fromPair v (pairChart v p) = p := by
  rcases p with ⟨a,s⟩
  have ha : 0 < a := hp.1
  have hs : 0 < s := hp.2
  simp only [pairChart,fromPair,Prod.mk.injEq,true_and]
  rw [show v*(v*a*log s)/a = (v*v)*log s by field_simp, hv, one_mul, exp_log hs]

private theorem fromPair_continuousOn (v : ℝ) : ContinuousOn (fromPair v) BurrHeld.firstDomain :=
  continuous_fst.continuousOn.prodMk (continuous_exp.comp_continuousOn
    ((continuous_const.mul continuous_snd).continuousOn.div continuous_fst.continuousOn
      (fun p hp ↦ (show 0 < p.1 from hp.1).ne')))

private theorem diagonal_transport (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {v J : ℝ} (hv : v * v = 1) {f : ℝ × ℝ → ℝ}
    (hf : ∀ p ∈ Weibull.domain,
      f p = likelihood y (p.1, p.1, (pairChart v p).2) - J) :
    (∃ p ∈ Weibull.domain, IsMaxOn f Weibull.domain p) ∧
    ∀ M : ℝ, ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Weibull.domain ∧
      ∀ p ∈ Weibull.domain, M ≤ f p → p ∈ K := by
  have H := pair_bij hv
  refine ⟨(Transfer.exists_max_iff H (c := -J) ?_).mpr (diagonal_MLE hn hne),?_⟩
  · exact fun p hp ↦ (hf p hp).trans (sub_eq_add_neg _ _)
  intro M
  obtain ⟨K,hK,hKD,hbox⟩ := diagonal_superlevel_compact hn hne (M+J)
  refine ⟨fromPair v '' K,hK.image_of_continuousOn ((fromPair_continuousOn v).mono hKD),
    fun _ ⟨q,hq,he⟩ ↦ he ▸ fromPair_mem v (hKD hq),fun p hp hM ↦ ?_⟩
  have ht := hf p hp
  have hbound : M + J ≤ likelihood y (p.1, p.1, (pairChart v p).2) := by linarith
  exact ⟨pairChart v p, hbox _ (H.mapsTo hp) hbound, fromPair_left hv hp⟩

private theorem log_nonconstant {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) : ∃ i j, log (x i) ≠ log (x j) := by
  obtain ⟨i,j,hij⟩ := hne
  refine ⟨i,j,fun h ↦ hij ?_⟩
  rw [← exp_log (hx i),← exp_log (hx j),h]

/-- Paralogistic has a finite maximum for nonconstant data and its likelihood
tends to minus infinity along every escape in the physical coefficients. -/
theorem paralogistic_MLE_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    let f := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
    (∃ p ∈ Weibull.domain, IsMaxOn f Weibull.domain p) ∧
    ∀ M : ℝ, ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Weibull.domain ∧
      ∀ p ∈ Weibull.domain, M ≤ f p → p ∈ K := by
  apply diagonal_transport hn (log_nonconstant hx hne) (v := -1)
    (J := avg (fun i ↦ log (x i))) (by norm_num)
  intro p hp
  unfold Paralogistic.logLik
  rw [BurrHeld.logLik_eq_first hn hx hp.2]
  simp [likelihood,pairChart]

/-- Inverse paralogistic has the same compact containment and finite
attainment for nonconstant data. -/
theorem inverse_paralogistic_MLE_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    let f := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
    (∃ p ∈ Weibull.domain, IsMaxOn f Weibull.domain p) ∧
    ∀ M : ℝ, ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Weibull.domain ∧
      ∀ p ∈ Weibull.domain, M ≤ f p → p ∈ K := by
  let e := Transfer.reciprocal x
  have he (i : Fin n) : 0 < e i := inv_pos.mpr (hx i)
  obtain ⟨i,j,hij⟩ := hne
  have hne' : ∃ i j, e i ≠ e j := ⟨i,j,fun h ↦ hij (inv_injective h)⟩
  apply diagonal_transport hn (log_nonconstant he hne') (v := 1)
    (J := avg (fun i ↦ log (x i))) (by norm_num)
  intro p hp
  rw [InvParalogistic.logLik_reciprocal hx]
  unfold Paralogistic.logLik
  rw [BurrHeld.logLik_eq_first hn he (inv_pos.mpr hp.2)]
  simp only [pairChart,one_mul,likelihood,log_inv,mul_neg,neg_neg]
  have h : avg (fun i ↦ log (e i)) = -avg (fun i ↦ log (x i)) := by
    simp only [e,Transfer.reciprocal,log_inv,BurrHeld.avg,Finset.sum_neg_distrib,neg_div]
  rw [h,avg_mul]
  ring


/-- Burr likelihood on a nonempty constant sample is unbounded above. -/
theorem constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦
      BurrHeld.logLik (fun _ : Fin n ↦ v) p.1 p.2.1 p.2.2) '' physicalDomain) := by
  rintro ⟨M,hM⟩
  apply BurrHeld.first_constant_unbounded hn 1 (log v)
  refine ⟨M+log v,?_⟩
  rintro _ ⟨⟨c,b⟩,hp,rfl⟩
  have hc : 0 < c := hp.1
  let s := exp (-b/c)
  have hs : 0 < s := exp_pos _
  have hb : -(c*log s) = b := by dsimp [s]; rw [log_exp]; field_simp
  have h := hM (mem_image_of_mem _ (show (1,c,s) ∈ physicalDomain from ⟨zero_lt_one,hc,hs⟩))
  change BurrHeld.logLik (fun _ : Fin n ↦ v) 1 c s ≤ M at h
  rw [BurrHeld.logLik_eq_first hn (fun _ ↦ hv) hs, hb,avg_const hn] at h
  linarith

private theorem diag_constant_limit (hn : 0 < n) (y₀ : ℝ) :
    Tendsto (fun t : ℝ ↦ likelihood (fun _ : Fin n ↦ y₀) (t,t,-(t*y₀)-log t))
      atTop atTop := by
  apply tendsto_atTop.mpr
  intro M
  filter_upwards [eventually_ge_atTop (1 : ℝ), tendsto_atTop.mp tendsto_log_atTop (M+2)]
    with t ht hlog
  have ht' : 0 < t := zero_lt_one.trans_le ht
  change M ≤ likelihood (fun _ : Fin n ↦ y₀) (t,t,-(t*y₀)-log t)
  rw [likelihood_shifted hn _ ht', avg_const hn, add_neg_cancel, exp_zero, zero_sub]
  have h := log_le_sub_one_of_pos (show 0 < 1+1/t by positivity)
  have h' : (t+1)*log (1+1/t) ≤ 2 := by
    calc
      _ ≤ (t+1)*(1/t) := mul_le_mul_of_nonneg_left (by linarith) (by positivity)
      _ ≤ _ := by rw [← mul_div_assoc, div_le_iff₀ ht']; linarith
  linarith

private theorem diagonal_constant_unbounded (hn : 0 < n) (y₀ : ℝ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood (fun _ : Fin n ↦ y₀) (p.1,p.1,p.2)) ''
      BurrHeld.firstDomain) := by
  rintro ⟨M,hM⟩
  have h1 := (tendsto_atTop.1 (diag_constant_limit hn y₀)) (M+1)
  have h2 : ∀ᶠ t : ℝ in atTop,
      likelihood (fun _ : Fin n ↦ y₀) (t,t,-(t*y₀)-log t) ≤ M := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact hM (mem_image_of_mem _ (show (t,-(t*y₀)-log t) ∈ BurrHeld.firstDomain from
      ⟨ht,mem_univ _⟩))
  obtain ⟨t,ht₁,ht₂⟩ := (h1.and h2).exists
  linarith

private theorem diagonal_constant_transfer (hn : 0 < n) (y₀ : ℝ) {v J : ℝ}
    (hv : v * v = 1) {f : ℝ × ℝ → ℝ}
    (hf : ∀ p ∈ Weibull.domain, f p =
      likelihood (fun _ : Fin n ↦ y₀) (p.1,p.1,(pairChart v p).2)-J) :
    ¬ BddAbove (f '' Weibull.domain) := by
  rintro ⟨M,hM⟩
  apply diagonal_constant_unbounded hn y₀
  refine ⟨M+J,?_⟩
  rintro _ ⟨q,hq,rfl⟩
  obtain ⟨p,hp,rfl⟩ := (pair_bij hv).surjOn hq
  have h := hM (mem_image_of_mem f hp)
  rw [hf p hp] at h
  change likelihood (fun _ : Fin n ↦ y₀) (p.1,p.1,(pairChart v p).2) ≤ M+J
  linarith

/-- Paralogistic likelihood diverges on every nonempty positive constant sample. -/
theorem paralogistic_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ Paralogistic.logLik (fun _ : Fin n ↦ v) p.1 p.2) ''
      Weibull.domain) := by
  apply diagonal_constant_transfer hn (log v) (v := -1) (J := log v) (by norm_num)
  intro p hp
  unfold Paralogistic.logLik
  rw [BurrHeld.logLik_eq_first hn (fun _ ↦ hv) hp.2,avg_const hn]
  simp [likelihood,pairChart]

/-- Inverse paralogistic inherits the constant-sample divergence by reciprocation. -/
theorem inverse_paralogistic_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ InvParalogistic.logLik (fun _ : Fin n ↦ v) p.1 p.2) ''
      Weibull.domain) := by
  rintro ⟨M,hM⟩
  apply paralogistic_constant_unbounded hn (inv_pos.mpr hv)
  refine ⟨M+2*log v,?_⟩
  rintro _ ⟨⟨a,s⟩,hp,rfl⟩
  have hs : 0 < s := hp.2
  have h := hM (mem_image_of_mem _ (show (a,s⁻¹) ∈ Weibull.domain from
    ⟨hp.1,inv_pos.mpr hs⟩))
  change InvParalogistic.logLik (fun _ : Fin n ↦ v) a s⁻¹ ≤ M at h
  rw [InvParalogistic.logLik_reciprocal (fun _ ↦ hv),inv_inv,avg_const hn] at h
  change Paralogistic.logLik (fun _ : Fin n ↦ v⁻¹) a s + -2*log v ≤ M at h
  linarith


private theorem sp_mono : Monotone sp := by
  intro a b hab
  unfold BurrHeld.softplus
  exact log_le_log (by positivity) (add_le_add_right (exp_le_exp.mpr hab) 1)

private theorem sp_deriv (a : ℝ) : HasDerivAt sp (exp (-sp (-a))) a := by
  have hd := ((hasDerivAt_id a).exp.const_add 1).log (by dsimp; positivity)
  have he : exp a / (1+exp a) = exp (-sp (-a)) := by
    unfold BurrHeld.softplus
    rw [exp_neg, exp_log (show 0 < 1+exp (-a) by positivity), exp_neg]
    field_simp
    ring
  rw [← he]
  change HasDerivAt (fun t ↦ log (1+exp t)) _ a
  simpa using hd

private theorem sp_symmetric (m t : ℝ) :
    sp (-(m+t)) + sp (-(m-t)) = log (1+2*exp (-m)*cosh t+exp (-m)^2) := by
  unfold BurrHeld.softplus
  rw [← log_mul (by positivity) (by positivity)]
  congr 1
  have hprod : exp (-t)*exp t = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
  rw [show -(m+t) = -m + -t by ring, show -(m-t) = -m+t by ring, exp_add, exp_add,
    cosh_eq]
  linear_combination exp (-m)^2 * hprod

private theorem sp_chord (b D : ℝ) (hD : 0 < D) :
    D * exp (-(sp (-b)+sp (-(b+D)))/2) ≤ sp (b+D)-sp b := by
  let m := b+D/2
  let d := D/2
  let f : ℝ → ℝ := fun t ↦ sp (m+t)-sp (m-t)
  let g : ℝ → ℝ := fun t ↦ exp (-sp (-(m+t))) + exp (-sp (-(m-t)))
  have hd : 0 < d := by dsimp [d]; positivity
  have hder (t : ℝ) : HasDerivAt f (g t) t := by
    have h1 := (sp_deriv (m+t)).comp t ((hasDerivAt_id t).const_add m)
    have h2 := (sp_deriv (m-t)).comp t ((hasDerivAt_id t).const_sub m)
    simpa [f,g,Function.comp_def,Pi.sub_def] using h1.sub h2
  have hc : Continuous f := continuous_iff_continuousAt.mpr fun t ↦ (hder t).continuousAt
  obtain ⟨t,ht,hval⟩ := exists_hasDerivAt_eq_slope f g hd hc.continuousOn (fun t _ ↦ hder t)
  have ht0 : 0 < t := ht.1
  have htd : t < d := ht.2
  have hcosh : cosh t ≤ cosh d :=
    cosh_le_cosh.mpr (by rw [abs_of_pos ht0, abs_of_pos hd]; exact htd.le)
  have hsum : sp (-(m+t))+sp (-(m-t)) ≤ sp (-(m+d))+sp (-(m-d)) := by
    rw [sp_symmetric m t,sp_symmetric m d]
    apply log_le_log
    · have h : 0 < cosh t := cosh_pos _
      positivity
    · gcongr
  have hgeom : exp (-(sp (-(m+d))+sp (-(m-d)))/2) ≤ g t/2 := by
    have he : exp (-(sp (-(m+d))+sp (-(m-d)))/2) ≤
        exp (-(sp (-(m+t))+sp (-(m-t)))/2) := exp_le_exp.mpr (by linarith)
    apply he.trans
    have h := convexOn_exp.2 (mem_univ (-sp (-(m+t)))) (mem_univ (-sp (-(m-t))))
      (show (0 : ℝ) ≤ 1/2 by norm_num) (show (0 : ℝ) ≤ 1/2 by norm_num)
      (show (1/2 : ℝ)+1/2=1 by norm_num)
    simp only [smul_eq_mul] at h
    convert h using 1 <;> (congr 1; ring)
  have h0 : f 0 = 0 := by simp [f]
  have hleft : m-d = b := by dsimp [m,d]; ring
  have hright : m+d = b+D := by dsimp [m,d]; ring
  rw [hleft,hright] at hgeom
  rw [h0,sub_zero,sub_zero] at hval
  have hval' := (eq_div_iff hd.ne').mp hval
  have hfd : f d = sp (b+D)-sp b := by unfold f; rw [hleft,hright]
  rw [hfd] at hval'
  rw [add_comm (sp (-(b+D))) (sp (-b))] at hgeom
  have hge := mul_le_mul_of_nonneg_right hgeom hD.le
  dsimp [d] at hval'
  linarith

open Classical in
private theorem avg_two_levels (hn : 0 < n) {y : Fin n → ℝ} {r : ℝ}
    (hy : ∀ i, y i = 0 ∨ y i = r) (f : ℝ → ℝ) :
    let w := ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ)/n
    avg (fun i ↦ f (y i)) = w*f 0+(1-w)*f r := by
  let U := (Finset.univ.filter (fun i ↦ y i = 0) : Finset (Fin n))
  let V := (Finset.univ.filter (fun i ↦ ¬ y i = 0) : Finset (Fin n))
  have hU : ∑ i ∈ U, f (y i) = U.card * f 0 := by
    rw [← nsmul_eq_mul, ← Finset.sum_const]
    exact Finset.sum_congr rfl fun i hi ↦ by rw [(Finset.mem_filter.mp hi).2]
  have hV : ∑ i ∈ V, f (y i) = V.card * f r := by
    rw [← nsmul_eq_mul, ← Finset.sum_const]
    exact Finset.sum_congr rfl fun i hi ↦ by
      rw [(hy i).resolve_left (Finset.mem_filter.mp hi).2]
  have hcard : (U.card : ℝ)+V.card = n := by
    simpa [U,V] using Finset.sum_filter_add_sum_filter_not Finset.univ
      (fun i ↦ y i = 0) (fun _ ↦ (1 : ℝ))
  have hs : ∑ i ∈ U, f (y i) + ∑ i ∈ V, f (y i) = ∑ i, f (y i) :=
    Finset.sum_filter_add_sum_filter_not _ _ _
  change (∑ i, f (y i))/n = _
  rw [← hs,hU,hV]
  change ((U.card : ℝ)*f 0+V.card*f r)/n =
    (U.card : ℝ)/n*f 0+(1-(U.card : ℝ)/n)*f r
  rw [show (V.card : ℝ) = n-U.card by linarith]
  field_simp [(npos hn).ne']

private theorem sum_antitone_cov {ι : Type*} (V : Finset ι) (x f : ι → ℝ)
    (h : ∀ i ∈ V, ∀ j ∈ V, (x i - x j) * (f i - f j) ≤ 0) :
    (V.card : ℝ)*(∑ i ∈ V, x i*f i) ≤ (∑ i ∈ V, x i)*(∑ i ∈ V, f i) := by
  have hn : ∑ i ∈ V, ∑ j ∈ V, (x i-x j)*(f i-f j) ≤ 0 :=
    Finset.sum_nonpos fun i hi ↦ Finset.sum_nonpos fun j hj ↦ h i hi j hj
  have he (i j : ι) : (x i-x j)*(f i-f j) = x i*f i-x i*f j-x j*f i+x j*f j := by ring
  simp_rw [he, Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum,
    ← Finset.sum_mul, Finset.sum_const, nsmul_eq_mul] at hn
  simp_rw [show ∀ i, x i * ((V.card : ℝ) * f i) = V.card * (x i*f i) by intro i; ring,
    ← Finset.mul_sum] at hn
  linarith

open Classical in
private theorem majority_comparison (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < avg y)
    (hw : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ))
    {b c : ℝ} (hc : 0 < c) :
    c*avg y*exp (-avg (fun i ↦ sp (-(c*y i+b)))) < avg (fun i ↦ sp (c*y i+b)) := by
  let U : Finset (Fin n) := Finset.univ.filter (fun i ↦ y i = 0)
  let V : Finset (Fin n) := Finset.univ.filter (fun i ↦ ¬ y i = 0)
  let N : ℝ := n
  let r : ℝ := V.card
  let Y := ∑ i ∈ V, y i
  let g : Fin n → ℝ := fun i ↦ sp (-(c*y i+b))
  let g₀ := sp (-b)
  let T := avg g
  let A := ∑ i ∈ V, g i
  let Q := ∑ i ∈ V, y i*g i
  have hN : 0 < N := npos hn
  have card_eq : (U.card : ℝ)+r=N := by
    simpa [U,V,r,N] using Finset.sum_filter_add_sum_filter_not Finset.univ
      (fun i ↦ y i = 0) (fun _ ↦ (1 : ℝ))
  have hr₂ : r ≤ N/2 := by
    change N/2 ≤ (U.card : ℝ) at hw
    linarith
  have hY : Y = N*avg y := by
    have hU : ∑ i ∈ U, y i = 0 := Finset.sum_eq_zero fun i hi ↦ (Finset.mem_filter.mp hi).2
    have hs : ∑ i ∈ U, y i + Y = ∑ i, y i :=
      Finset.sum_filter_add_sum_filter_not _ _ _
    unfold BurrHeld.avg
    change Y = N * ((∑ i, y i)/N)
    rw [mul_div_cancel₀ _ hN.ne']
    linarith
  have hYp : 0 < Y := hY ▸ mul_pos hN hmean
  have hV : V.Nonempty := by
    by_contra! h
    simp [Y, h] at hYp
  have hr : 0 < r := by dsimp [r]; exact_mod_cast hV.card_pos
  have hg (i j : Fin n) (h : y i ≤ y j) : g j ≤ g i := by
    apply sp_mono
    linarith [mul_le_mul_of_nonneg_left h hc.le]
  have hcov : r*Q ≤ Y*A := sum_antitone_cov V y g (fun i _ j _ ↦ by
    rcases le_total (y i) (y j) with he | he
    · exact mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr he) (sub_nonneg.mpr (hg i j he))
    · exact mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr he) (sub_nonpos.mpr (hg j i he)))
  have hQ : Q/Y ≤ A/r := by
    rw [div_le_div_iff₀ hYp hr]
    simpa only [mul_comm] using hcov
  have hA : A/r ≤ g₀ := by
    apply (div_le_iff₀ hr).mpr
    calc
      A ≤ ∑ i ∈ V, g₀ := Finset.sum_le_sum fun i _ ↦ by
        apply sp_mono
        linarith [mul_nonneg hc.le (hy i)]
      _ = g₀*r := by simp [r,mul_comm]
  have hT : N*T = (N-r)*g₀+A := by
    have hU : ∑ i ∈ U, g i = U.card*g₀ := by
      rw [← nsmul_eq_mul, ← Finset.sum_const]
      exact Finset.sum_congr rfl fun i hi ↦ by
        simp only [g,g₀,(Finset.mem_filter.mp hi).2,mul_zero,zero_add]
    have hs : ∑ i ∈ U, g i + A = ∑ i, g i :=
      Finset.sum_filter_add_sum_filter_not _ _ _
    change N*((∑ i, g i)/N) = _
    rw [mul_div_cancel₀ _ hN.ne', ← hs, hU, show (U.card : ℝ) = N-r by linarith]
  have hT' : (g₀+A/r)/2 ≤ T := by
    have hAr := (div_mul_cancel₀ A hr.ne')
    have h := mul_nonneg (show 0 ≤ N/2-r by linarith) (sub_nonneg.mpr hA)
    apply le_of_mul_le_mul_left _ hN
    nlinarith
  have hE : exp (-T) ≤ exp (-(g₀+Q/Y)/2) := exp_le_exp.mpr (by linarith)
  let v : Fin n → ℝ := fun i ↦ -(g₀+g i)/2
  let w : Fin n → ℝ := fun i ↦ y i/Y
  have hwSum : ∑ i ∈ V, w i = 1 := by
    simp only [w, ← Finset.sum_div, show ∑ i ∈ V, y i = Y by rfl, div_self hYp.ne']
  have hjens := convexOn_exp.map_sum_le (t := V) (w := w) (p := v)
    (fun i _ ↦ div_nonneg (hy i) hYp.le) hwSum (fun _ _ ↦ mem_univ _)
  have hwv : (∑ i ∈ V, w i • v i) = -(g₀+Q/Y)/2 := by
    simp only [smul_eq_mul, v, w]
    have he (i : Fin n) : y i/Y * (-(g₀+g i)/2) =
        -(g₀*(y i/Y)+(y i*g i)/Y)/2 := by ring
    simp_rw [he, ← Finset.sum_div, Finset.sum_neg_distrib,Finset.sum_add_distrib,
      ← Finset.mul_sum, ← Finset.sum_div]
    change -(g₀*(Y/Y)+Q/Y)/2 = _
    rw [div_self hYp.ne',mul_one]
  rw [hwv] at hjens
  simp only [smul_eq_mul] at hjens
  have hJ : Y*exp (-T) ≤ ∑ i ∈ V, y i*exp (v i) := by
    have h := mul_le_mul_of_nonneg_left (hE.trans hjens) hYp.le
    rw [Finset.mul_sum] at h
    have he (i : Fin n) : Y*(w i*exp (v i)) = y i*exp (v i) := by
      dsimp [w]
      field_simp
    simpa only [he] using h
  have hsum : c*(Y*exp (-T)) ≤ ∑ i ∈ V, (sp (c*y i+b)-sp b) := by
    apply (mul_le_mul_of_nonneg_left hJ hc.le).trans
    rw [Finset.mul_sum]
    apply Finset.sum_le_sum
    intro i hi
    have hpos : 0 < y i := lt_of_le_of_ne (hy i) (Ne.symm (Finset.mem_filter.mp hi).2)
    have hc' := sp_chord b (c*y i) (mul_pos hc hpos)
    simpa only [v,g,g₀,← mul_assoc,add_comm b (c*y i)] using hc'
  have hS : (n : ℝ)*avg (fun i ↦ sp (c*y i+b)) =
      n*sp b + ∑ i ∈ V, (sp (c*y i+b)-sp b) := by
    have hU : ∑ i ∈ U, (sp (c*y i+b)-sp b) = 0 :=
      Finset.sum_eq_zero fun i hi ↦ by simp [(Finset.mem_filter.mp hi).2]
    have hsplit : ∑ i ∈ U, (sp (c*y i+b)-sp b) +
        ∑ i ∈ V, (sp (c*y i+b)-sp b) = ∑ i, (sp (c*y i+b)-sp b) :=
      Finset.sum_filter_add_sum_filter_not _ _ _
    rw [hU,zero_add] at hsplit
    unfold BurrHeld.avg
    rw [hsplit,mul_div_cancel₀ _ (npos hn).ne']
    simp only [Finset.sum_sub_distrib,Finset.sum_const,Finset.card_univ,Fintype.card_fin,
      nsmul_eq_mul]
    ring
  apply lt_of_mul_lt_mul_left _ hN.le
  change N*(c*avg y*exp (-T)) < N*avg (fun i ↦ sp (c*y i+b))
  rw [hS, show N*(c*avg y*exp (-T)) = c*(Y*exp (-T)) by rw [hY]; ring]
  linarith [mul_pos hN (sp_pos b)]

open Classical in
/-- A majority at the minimum strictly excludes every finite Burr point,
even when the other observations are all distinct. -/
theorem majority_minimum_lt_pareto (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < BurrHeld.avg y)
    (hw : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ))
    {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
    likelihood y p < paretoValue y := by
  rcases p with ⟨k,c,b⟩
  have hk : 0 < k := hp.1
  have hc : 0 < c := hp.2.1
  let S := avg (fun i ↦ sp (c*y i+b))
  let T := avg (fun i ↦ sp (-(c*y i+b)))
  have hst : c*avg y*exp (-T) < S := majority_comparison hn hy hmean hw hc
  have hpos : 0 < S := (mul_pos (mul_pos hc hmean) (exp_pos _)).trans hst
  have hlog := log_lt_log (mul_pos (mul_pos hc hmean) (exp_pos (-T))) hst
  rw [log_mul (mul_pos hc hmean).ne' (exp_pos _).ne',log_exp,log_mul hc.ne' hmean.ne'] at hlog
  have hklog := log_le_sub_one_of_pos (mul_pos hk hpos)
  rw [log_mul hk.ne' hpos.ne'] at hklog
  unfold paretoValue
  change log k+log c - avg (fun i ↦ sp (-(c*y i+b))+k*sp (c*y i+b)) < _
  rw [avg_add,avg_mul]
  change log k+log c-(T+k*S) < _
  linarith


open Classical in
/-- On two distinct log levels, a majority (including equality) at the
minimum makes every finite Burr value strictly worse than Pareto. -/
theorem two_level_lt_pareto (hn : 0 < n) {y : Fin n → ℝ} {r : ℝ} (hr : 0 < r)
    (hy : ∀ i, y i = 0 ∨ y i = r) (hmean : 0 < BurrHeld.avg y)
    (hw : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ))
    {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
    likelihood y p < paretoValue y :=
  majority_minimum_lt_pareto hn
    (fun i ↦ (hy i).elim (fun h ↦ h ▸ le_rfl) (fun h ↦ h ▸ hr.le)) hmean hw hp

private theorem pareto_supremum_transport (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < avg y)
    (hstrict : ∀ p ∈ domain, likelihood y p < paretoValue y)
    {f : ℝ × ℝ × ℝ → ℝ} {φ : (ℝ × ℝ × ℝ) → (ℝ × ℝ × ℝ)}
    (hφ : BijOn φ physicalDomain domain) {a J : ℝ} (ha : 0 < a)
    (hf : ∀ p ∈ physicalDomain, f p = a * likelihood y (φ p) - J) :
    sSup (f '' physicalDomain) = a*paretoValue y-J ∧
      ∀ p ∈ physicalDomain, f p < a*paretoValue y-J := by
  have hb (p) (hp : p ∈ physicalDomain) : f p < a*paretoValue y-J := by
    rw [hf p hp]
    exact sub_lt_sub_right (mul_lt_mul_of_pos_left (hstrict _ (hφ.mapsTo hp)) ha) J
  have hB : BddAbove (f '' physicalDomain) := ⟨_, by
    rintro _ ⟨p,hp,rfl⟩
    exact (hb p hp).le⟩
  refine ⟨le_antisymm (csSup_le ((show physicalDomain.Nonempty from
    ⟨(1,1,1),zero_lt_one,zero_lt_one,zero_lt_one⟩).image f) ?_)
    ?_,hb⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hb p hp).le
  · apply le_of_tendsto (((pareto_path hn hy hmean).const_mul a).sub_const J)
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have ht' : 0 < t^2 := by positivity
    have hd : ((avg y)⁻¹/t^2,t^2,t) ∈ domain :=
      ⟨div_pos (inv_pos.mpr hmean) ht',ht',mem_univ _⟩
    obtain ⟨p,hp,he⟩ := hφ.surjOn hd
    have hv := le_csSup hB (mem_image_of_mem f hp)
    rw [hf p hp,he] at hv
    exact hv

private theorem logExcess_zero {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (i j : Fin n) :
    logExcess x j i = 0 ↔ x i = x j := by
  unfold logExcess
  rw [sub_eq_zero]
  constructor
  · intro he
    rw [← exp_log (hx i), ← exp_log (hx j), he]
  · exact congrArg log

open Classical in
/-- When at least half of a nonconstant positive sample equals its minimum,
the exact Burr supremum is the optimized Pareto face and is unattained. -/
theorem majority_minimum_physical (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let f := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik x p.1 p.2.1 p.2.2
    let P := paretoValue (logExcess x j)-BurrHeld.avg (fun i ↦ log (x i))
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P := by
  obtain ⟨hy,_,hmean⟩ := logExcess_spec hn hx hne hj
  have hw : (n : ℝ)/2 ≤ ((Finset.univ.filter (fun i ↦ logExcess x j i = 0)).card : ℝ) := by
    simpa only [logExcess_zero hx] using hm
  simpa only [one_mul] using pareto_supremum_transport hn hy hmean
    (fun _ hp ↦ majority_minimum_lt_pareto hn hy hmean hw hp) (scaleChart_bij _)
    (a := 1) zero_lt_one
    (fun _ hp ↦ by simpa only [one_mul] using physical_likelihood hn hx j hp)

open Classical in
/-- With a held location, a majority at the sample minimum gives the
unattained shifted-Pareto supremum. -/
theorem paretoIV_majority_minimum (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let e := ShapeFacesParetoIV.excess x μ
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let P := n*paretoValue (logExcess e j)-∑ i, log (e i)
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P := by
  let e := ShapeFacesParetoIV.excess x μ
  have he (i : Fin n) : 0 < e i := sub_pos.mpr (hx i)
  obtain ⟨i,k,hik⟩ := hne
  obtain ⟨hy,_,hmean⟩ := logExcess_spec hn he
    ⟨i,k,fun h ↦ hik (sub_left_inj.mp h)⟩ (fun i ↦ sub_le_sub_right (hj i) μ)
  have hw : (n : ℝ)/2 ≤ ((Finset.univ.filter (fun i ↦ logExcess e j i = 0)).card : ℝ) := by
    simpa only [logExcess_zero he,e,ShapeFacesParetoIV.excess,sub_left_inj] using hm
  exact pareto_supremum_transport hn hy hmean
    (fun _ hp ↦ majority_minimum_lt_pareto hn hy hmean hw hp) (scaleChart_bij _)
    (npos hn) (fun _ hp ↦ paretoIV_physical_likelihood hn hx j hp)

open Classical in
/-- For inverse Burr a majority at the sample maximum gives the unattained
upper-endpoint power supremum. -/
theorem inverse_majority_maximum (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x i ≤ x j)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let y := logExcess (Transfer.reciprocal x) j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let P := n*paretoValue y-∑ i, log (x i)
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P := by
  let e := Transfer.reciprocal x
  have he (i : Fin n) : 0 < e i := inv_pos.mpr (hx i)
  obtain ⟨i,k,hik⟩ := hne
  obtain ⟨hy,_,hmean⟩ := logExcess_spec hn he
    ⟨i,k,fun h ↦ hik (inv_injective h)⟩ (fun i ↦ inv_anti₀ (hx i) (hj i))
  have hw : (n : ℝ)/2 ≤ ((Finset.univ.filter (fun i ↦ logExcess e j i = 0)).card : ℝ) := by
    simpa only [logExcess_zero he,e,Transfer.reciprocal,inv_inj] using hm
  exact pareto_supremum_transport hn hy hmean
    (fun _ hp ↦ majority_minimum_lt_pareto hn hy hmean hw hp)
    ((scaleChart_bij _).comp reciprocate_bij)
    (npos hn) (fun _ hp ↦ inverse_physical_likelihood hn hx j hp)


private abbrev eps : ℝ := 1/10000
private abbrev bigWeight : ℝ := 1-eps

private def tieLogs (K : ℝ) (i : Fin 10000) : ℝ := if i = 0 then 0 else 100000000*K

private def tieObjective (K : ℝ) (p : ℝ × ℝ) : ℝ :=
  let a := eps*p.1
  let D := 10000*K*p.1
  let b := p.2
  2*log p.1 - (eps*(sp (-b)+a*sp b)+bigWeight*(sp (-(b+D))+a*sp (b+D)))

private theorem tieObjective_eq (K t b : ℝ) (ht : 0 < t) (hK : 0 < K) :
    likelihood (tieLogs K) (eps*t,eps*t,b) = tieObjective K (t,b) + 2*log eps := by
  have hlev (i : Fin 10000) : tieLogs K i = 0 ∨ tieLogs K i = 100000000*K := by
    by_cases hi : i = 0 <;> simp [tieLogs,hi]
  have hz : (Finset.univ.filter (fun i ↦ tieLogs K i = 0)) = {0} := by
    classical
    ext i
    have he : 100000000*K ≠ 0 := mul_ne_zero (by norm_num) hK.ne'
    simp [tieLogs,he]
  have hf := avg_two_levels (by decide : 0 < 10000) hlev
    (fun z ↦ sp (-(eps*t*z+b))+eps*t*sp (eps*t*z+b))
  classical
  dsimp only at hf
  rw [hz,Finset.card_singleton,Nat.cast_one] at hf
  unfold likelihood BurrHeld.firstLik tieObjective
  dsimp
  rw [hf,log_mul (by norm_num : eps ≠ 0) ht.ne']
  simp only [mul_zero,zero_add]
  have he : eps*t*(100000000*K)+b = b+10000*K*t := by dsimp [eps]; ring
  rw [he]
  ring

private theorem tie_hard_bound (K t b : ℝ) (ht : 0 < t) :
    tieObjective K (t,b) ≤ 2*log t-K*min (bigWeight*t^2) t := by
  let a := eps*t
  let D := 10000*K*t
  have ha : 0 < a := by dsimp [a,eps]; positivity
  have hw : 0 ≤ bigWeight := by norm_num [bigWeight,eps]
  have he : 0 ≤ eps := by norm_num [eps]
  let z := min eps (bigWeight*a)
  have hz : 0 ≤ z := le_min he (mul_nonneg hw ha.le)
  have h₁ : z*(-b) ≤ eps*sp (-b) :=
    (mul_le_mul_of_nonneg_left (sp_ge _) hz).trans
      (mul_le_mul_of_nonneg_right (min_le_left _ _) (sp_pos _).le)
  have h₂ : z*(b+D) ≤ bigWeight*a*sp (b+D) :=
    (mul_le_mul_of_nonneg_left (sp_ge _) hz).trans
      (mul_le_mul_of_nonneg_right (min_le_right _ _) (sp_pos _).le)
  have h₃ : z*D ≤ eps*(sp (-b)+a*sp b)+bigWeight*(sp (-(b+D))+a*sp (b+D)) := by
    nlinarith [mul_nonneg he (mul_nonneg ha.le (sp_pos b).le),
      mul_nonneg hw (sp_pos (-(b+D))).le]
  have hDz : z*D = K*min (bigWeight*t^2) t := by
    dsimp [z,a,D]
    by_cases h : eps ≤ bigWeight*(eps*t)
    · have h' : t ≤ bigWeight*t^2 := by
        have h'' := mul_le_mul_of_nonneg_right h ht.le
        norm_num [eps] at h''
        nlinarith
      rw [min_eq_left h,min_eq_right h']
      norm_num [eps]
      ring
    · have h' : bigWeight*t^2 ≤ t := by
        have h'' := mul_le_mul_of_nonneg_right (le_of_not_ge h) ht.le
        norm_num [eps] at h''
        nlinarith
      rw [min_eq_right (le_of_not_ge h),min_eq_left h']
      norm_num [eps]
      ring
  change 2*log t-(eps*(sp (-b)+a*sp b)+bigWeight*(sp (-(b+D))+a*sp (b+D))) ≤ _
  linarith

private theorem tie_low_witness {K : ℝ} (hK : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    2*log (4/5)-K*bigWeight*(4/5)^2-1/1000 < tieObjective K (4/5,0) := by
  let t : ℝ := 4/5
  let a := eps*t
  let D := 10000*K*t
  have hD : 10000 ≤ D := by
    have hb := hK.1
    dsimp [D,t]
    linarith
  have hs : sp (-D) ≤ 1/10001 := by
    have he := add_one_le_exp D
    rw [show (1/10001 : ℝ) = (10001 : ℝ)⁻¹ by norm_num]
    calc
      _ ≤ exp (-D) := sp_le_exp _
      _ = (exp D)⁻¹ := exp_neg _
      _ ≤ _ := inv_anti₀ (by norm_num) (by linarith)
  have h0 : sp 0 ≤ 1 := by simpa only [exp_zero] using sp_le_exp 0
  have h00 : 0 ≤ sp 0 := (sp_pos 0).le
  have hsd : 0 ≤ sp (-D) := (sp_pos _).le
  change 2*log t-K*bigWeight*t^2-1/1000 <
    2*log t-(eps*(sp (-0)+a*sp 0)+bigWeight*(sp (-(0+D))+a*sp (0+D)))
  rw [neg_zero,zero_add,sp_shift D]
  dsimp [D,a,t,eps,bigWeight] at ⊢ hs hsd
  nlinarith

private theorem tie_high_witness :
    2*log (10/7)-2-1/100 < tieObjective (7/5) (10/7,10-20000) := by
  let t : ℝ := 10/7
  let a := eps*t
  let b : ℝ := 10-20000
  have he : exp (10 : ℝ) ≥ 1024 := by
    rw [show (10 : ℝ) = (10 : ℕ) * 1 by norm_num,exp_nat_mul]
    calc
      _ ≥ (2 : ℝ)^10 := by gcongr; linarith [add_one_le_exp (1 : ℝ)]
      _ = _ := by norm_num
  have h10 : sp (-10) ≤ 1/1024 := by
    calc
      _ ≤ exp (-10) := sp_le_exp _
      _ = (exp 10)⁻¹ := exp_neg _
      _ ≤ _ := by rw [one_div]; exact inv_anti₀ (by norm_num) he
  have hb : sp b ≤ 1/19991 := by
    have he' := add_one_le_exp (19990 : ℝ)
    calc
      _ ≤ exp b := sp_le_exp _
      _ = (exp 19990)⁻¹ := by dsimp [b]; norm_num [exp_neg]
      _ ≤ _ := by rw [one_div]; exact inv_anti₀ (by norm_num) (by linarith)
  change 2*log t-2-1/100 < 2*log t-(eps*(sp (-b)+a*sp b)+
    bigWeight*(sp (-(b+10000*(7/5)*t))+a*sp (b+10000*(7/5)*t)))
  rw [show b+10000*(7/5)*t=10 by norm_num [b,t], sp_shift (-b), neg_neg, sp_shift (10 : ℝ)]
  dsimp [a,b,t,eps,bigWeight]
  change _ < 2*log (10/7)-((1/10000)*(-b+sp b+(1/10000*(10/7))*sp b)+
    (1-1/10000)*(sp (-10)+(1/10000*(10/7))*(10+sp (-10))))
  linarith [h10,hb,show b = -19990 by norm_num [b]]


private theorem log_four_fifths_lower : -(17/72 : ℝ) ≤ log (4/5) := by
  have h1 := log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 9/8)
  have h2 := log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 10/9)
  have he : log (4/5 : ℝ) = -(log (9/8)+log (10/9)) := by
    rw [← log_mul (by norm_num) (by norm_num), ← log_inv]
    norm_num
  rw [he]
  linarith

private theorem tie_low_gt_minus_two {K : ℝ} (hK : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) :
    -2 < tieObjective K (4/5,0) := by
  have h := tie_low_witness hK
  have hk := hK.2
  norm_num [bigWeight,eps] at h
  linarith [log_four_fifths_lower]

private theorem tie_seam_loses {K : ℝ} (hK : K ∈ Icc (7 / 5 : ℝ) (8 / 5)) (b : ℝ) :
    tieObjective K (1,b) < tieObjective K (4/5,0) := by
  have h := tie_hard_bound K 1 b zero_lt_one
  have h' := tie_low_witness hK
  have hk := hK.1
  norm_num [bigWeight,eps] at h h'
  linarith [log_four_fifths_lower]

private theorem tie_left_loses {t b : ℝ} (ht : 0 < t) (h₁ : t ≤ 1) :
    tieObjective (7/5) (t,b) < tieObjective (7/5) (10/7,10-20000) := by
  have hw : bigWeight ≤ 1 := by norm_num [bigWeight,eps]
  have hmin : bigWeight*t^2 ≤ t := by
    have h0 : bigWeight*t ≤ 1 := by nlinarith
    nlinarith [mul_le_mul_of_nonneg_right h0 ht.le]
  have h := tie_hard_bound (7/5) t b ht
  rw [min_eq_left hmin] at h
  have hprod : 0 < (7/5)*bigWeight := by norm_num [bigWeight,eps]
  have hh := log_le_sub_one_of_pos (mul_pos hprod (sq_pos_of_pos ht))
  rw [log_mul hprod.ne' (sq_pos_of_pos ht).ne', log_pow] at hh
  have hlog1 : 33/100 ≤ log ((7/5)*bigWeight) := by
    have hl := le_log_one_add_of_nonneg (by norm_num : 0 ≤ (19993/50000 : ℝ))
    norm_num at hl
    norm_num [bigWeight,eps]
    linarith
  have hlog2 : 6/17 ≤ log (10/7) := by
    have hl := le_log_one_add_of_nonneg (by norm_num : 0 ≤ (3/7 : ℝ))
    norm_num at hl ⊢
    exact hl
  have hh' := tie_high_witness
  linarith

private theorem tie_right_loses {t b : ℝ} (ht : 1 ≤ t) :
    tieObjective (8/5) (t,b) < tieObjective (8/5) (4/5,0) := by
  have hk : (8/5 : ℝ) ∈ Icc (7/5) (8/5) := ⟨by norm_num,le_rfl⟩
  have h' := tie_low_witness hk
  norm_num [bigWeight,eps] at h'
  have ht' : 0 < t := zero_lt_one.trans_le ht
  have h := tie_hard_bound (8/5) t b ht'
  have hlog := log_four_fifths_lower
  rcases le_total (bigWeight*t^2) t with hmin | hmin
  · rw [min_eq_left hmin] at h
    have hl := log_le_sub_one_of_pos ht'
    have hsq : 2*(t-1) ≤ (8/5)*bigWeight*(t^2-1) := by
      have hw : (1 : ℝ) ≤ (8/5)*bigWeight := by norm_num [bigWeight,eps]
      nlinarith [mul_le_mul_of_nonneg_right hw (show 0 ≤ t^2-1 by nlinarith)]
    norm_num [bigWeight,eps] at h hsq
    linarith
  · rw [min_eq_right hmin] at h
    have hh := log_le_sub_one_of_pos (mul_pos (by norm_num : (0 : ℝ) < 4/5) ht')
    rw [log_mul (by norm_num : (4/5 : ℝ) ≠ 0) ht'.ne'] at hh
    linarith

private def tieBox : Set (ℝ × ℝ) := Icc (1/10) 10 ×ˢ Icc (-1000000000000) 1000000000000
private def tieLeftBox : Set (ℝ × ℝ) := Icc (1/10) 1 ×ˢ Icc (-1000000000000) 1000000000000
private def tieRightBox : Set (ℝ × ℝ) := Icc 1 10 ×ˢ Icc (-1000000000000) 1000000000000

private theorem tie_superlevel {K t b : ℝ} (hK : K ∈ Icc (7 / 5 : ℝ) (8 / 5))
    (ht : 0 < t) (hval : -2 ≤ tieObjective K (t, b)) : (t, b) ∈ tieBox := by
  have ha : 0 < eps*t := by norm_num [eps, ht]
  have hp1 : 0 ≤ eps*(sp (-b)+eps*t*sp b) := by
    exact mul_nonneg (by norm_num [eps])
      (add_nonneg (sp_pos _).le (mul_nonneg ha.le (sp_pos _).le))
  have hp2 : 0 ≤ bigWeight*(sp (-(b+10000*K*t))+eps*t*sp (b+10000*K*t)) := by
    exact mul_nonneg (by norm_num [bigWeight,eps])
      (add_nonneg (sp_pos _).le (mul_nonneg ha.le (sp_pos _).le))
  change -2 ≤ 2*log t-(eps*(sp (-b)+eps*t*sp b)+
    bigWeight*(sp (-(b+10000*K*t))+eps*t*sp (b+10000*K*t))) at hval
  have hl : 1/10 ≤ t := by
    by_contra! h
    have hlog : log t ≤ log (1/4 : ℝ) := log_le_log ht (by linarith)
    have hb := le_log_one_add_of_nonneg (show (0 : ℝ) ≤ 3 by norm_num)
    norm_num at hb
    rw [one_div,log_inv] at hlog
    linarith
  have hu : t ≤ 10 := by
    by_contra! h
    have hk := hK.1
    have hbig : t ≤ bigWeight*t^2 := by
      have hw : 1 ≤ bigWeight*t := by norm_num [bigWeight,eps]; linarith
      nlinarith [mul_le_mul_of_nonneg_right hw ht.le]
    have hh := tie_hard_bound K t b ht
    rw [min_eq_right hbig] at hh
    have hlog := log_le_sub_one_of_pos (div_pos ht (by norm_num : (0 : ℝ) < 4))
    rw [log_div ht.ne' (by norm_num : (4 : ℝ) ≠ 0)] at hlog
    have h4 : log (4 : ℝ) ≤ 2 := by
      rw [show (4 : ℝ) = 2^2 by norm_num,log_pow]
      have h2 := log_le_sub_one_of_pos (by norm_num : (0 : ℝ) < 2)
      norm_num
      linarith
    have hk' := mul_le_mul_of_nonneg_right hk ht.le
    change -2 ≤ tieObjective K (t,b) at hval
    linarith
  have hb : |b| ≤ 1000000000000 := by
    have hx : (1/100000 : ℝ) ≤ eps*t := by norm_num [eps]; linarith
    have h := point_penalty (z := b) (by norm_num)
      (by norm_num : (1/100000 : ℝ) ≤ 1) hx
    have h' : |b|/1000000000 ≤ eps*(sp (-b)+eps*t*sp b) := by
      calc
        _ = eps*((1/100000)*|b|) := by norm_num [eps]; ring
        _ ≤ _ := mul_le_mul_of_nonneg_left h (by norm_num [eps])
    have hlog := (log_le_log ht hu).trans (log_le_sub_one_of_pos (by norm_num))
    have hbound : eps*(sp (-b)+eps*t*sp b) ≤ 20 := by linarith only [hval,hp2,hlog]
    have hc := h'.trans hbound
    have hp := (div_le_iff₀ (by norm_num : (0 : ℝ) < 1000000000)).mp hc
    linarith
  exact ⟨⟨hl,hu⟩,abs_le.mp hb⟩

private theorem tie_height_continuous {C : Set (ℝ × ℝ)} (hC : IsCompact C)
    (hlo : ∀ p ∈ C, 1 / 10 ≤ p.1) :
    Continuous (fun K ↦ sSup (tieObjective K '' C)) := by
  let F (K : ℝ) (p : ℝ × ℝ) := tieObjective K (max p.1 (1/10),p.2)
  have hf : Continuous (Function.uncurry F) := by
    let t : ℝ × (ℝ × ℝ) → ℝ := fun q ↦ max q.2.1 (1/10)
    have ht : Continuous t := (continuous_fst.comp continuous_snd).max continuous_const
    have hb : Continuous (fun q : ℝ × (ℝ × ℝ) ↦ q.2.2) :=
      continuous_snd.comp continuous_snd
    have hl := ht.log fun q ↦ (show 0 < t q from
      (by norm_num : (0 : ℝ) < 1/10).trans_le (le_max_right _ _)).ne'
    have hs : Continuous sp := by
      unfold BurrHeld.softplus
      exact (continuous_const.add continuous_exp).log fun q ↦
        (show 0 < 1+exp q by positivity).ne'
    have hz := hb.add ((continuous_fst.const_mul (10000 : ℝ)).mul ht)
    change Continuous (fun q : ℝ × (ℝ × ℝ) ↦
      2*log (t q) - (eps*(sp (-q.2.2)+eps*t q*sp q.2.2) +
        bigWeight*(sp (-(q.2.2+10000*q.1*t q))+eps*t q*sp (q.2.2+10000*q.1*t q))))
    exact (hl.const_mul _).sub ((((hs.comp hb.neg).add
      ((ht.const_mul _).mul (hs.comp hb))).const_mul _).add
      (((hs.comp hz.neg).add ((ht.const_mul _).mul (hs.comp hz))).const_mul _))
  have he : (fun K ↦ sSup (tieObjective K '' C)) = fun K ↦ sSup (F K '' C) := by
    funext K
    congr 1
    exact image_congr (fun p hp ↦ by unfold F; rw [max_eq_left (hlo p hp)])
  rw [he]
  exact hC.continuous_sSup hf

private theorem tie_box_max {C : Set (ℝ × ℝ)} (hC : IsCompact C)
    (hne : C.Nonempty) (hlo : ∀ p ∈ C, 1 / 10 ≤ p.1) (K : ℝ) :
    ∃ p ∈ C, IsMaxOn (tieObjective K) C p ∧
      sSup (tieObjective K '' C) = tieObjective K p := by
  have hc : ContinuousOn (tieObjective K) C := by
    have ht : ContinuousOn (fun p : ℝ × ℝ ↦ log p.1) C :=
      continuous_fst.continuousOn.log fun p hp ↦
        ((by norm_num : (0 : ℝ) < 1/10).trans_le (hlo p hp)).ne'
    have hs : Continuous sp := by
      unfold BurrHeld.softplus
      exact (continuous_const.add continuous_exp).log fun q ↦
        (show 0 < 1+exp q by positivity).ne'
    have h1 : Continuous (fun p : ℝ × ℝ ↦ p.1) := continuous_fst
    have h2 : Continuous (fun p : ℝ × ℝ ↦ p.2) := continuous_snd
    have hz := h2.add (h1.const_mul (10000*K))
    have ha := h1.const_mul eps
    exact (ht.const_mul _).sub ((((hs.comp h2.neg).add
      (ha.mul (hs.comp h2))).const_mul _).add
      (((hs.comp hz.neg).add (ha.mul (hs.comp hz))).const_mul _)).continuousOn
  obtain ⟨p,hp,hm⟩ := hC.exists_isMaxOn hne hc
  refine ⟨p,hp,hm,le_antisymm (csSup_le (hne.image _) ?_) (le_csSup ?_ (mem_image_of_mem _ hp))⟩
  · rintro _ ⟨q,hq,rfl⟩
    exact hm hq
  · refine ⟨tieObjective K p,?_⟩
    rintro _ ⟨q,hq,rfl⟩
    exact hm hq


private theorem tie_chart_global :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      ∃ p ∈ BurrHeld.firstDomain, ∃ q ∈ BurrHeld.firstDomain,
        p.1 < 1 ∧ 1 < q.1 ∧
          IsMaxOn (tieObjective K) BurrHeld.firstDomain p ∧
          IsMaxOn (tieObjective K) BurrHeld.firstDomain q := by
  let L (K : ℝ) := sSup (tieObjective K '' tieLeftBox)
  let R (K : ℝ) := sSup (tieObjective K '' tieRightBox)
  let wl : ℝ × ℝ := (4/5,0)
  let wr : ℝ × ℝ := (10/7,10-20000)
  have hwl : wl ∈ tieLeftBox := by norm_num [wl,tieLeftBox]
  have hwr : wr ∈ tieRightBox := by norm_num [wr,tieRightBox]
  have hLc : IsCompact tieLeftBox := isCompact_Icc.prod isCompact_Icc
  have hRc : IsCompact tieRightBox := isCompact_Icc.prod isCompact_Icc
  have hLl (p) (hp : p ∈ tieLeftBox) : 1/10 ≤ p.1 := hp.1.1
  have hRl (p) (hp : p ∈ tieRightBox) : 1/10 ≤ p.1 :=
    (by norm_num : (1/10 : ℝ) ≤ 1).trans hp.1.1
  have hposL (p) (hp : p ∈ tieLeftBox) : 0 < p.1 :=
    (by norm_num : (0 : ℝ) < 1/10).trans_le (hLl _ hp)
  have hposR (p) (hp : p ∈ tieRightBox) : 0 < p.1 := zero_lt_one.trans_le hp.1.1
  have left_max := tie_box_max hLc ⟨wl,hwl⟩ hLl
  have right_max := tie_box_max hRc ⟨wr,hwr⟩ hRl
  have hleft : L (7/5) < R (7/5) := by
    obtain ⟨l,hl,_,hval⟩ := left_max (7/5)
    obtain ⟨r,_,hr,hval'⟩ := right_max (7/5)
    change L (7/5) = _ at hval
    change R (7/5) = _ at hval'
    rw [hval,hval']
    exact (tie_left_loses (hposL _ hl) hl.1.2).trans_le (hr hwr)
  have hright : R (8/5) < L (8/5) := by
    obtain ⟨l,_,hl,hval⟩ := left_max (8/5)
    obtain ⟨r,hr,_,hval'⟩ := right_max (8/5)
    change L (8/5) = _ at hval
    change R (8/5) = _ at hval'
    rw [hval,hval']
    exact (tie_right_loses hr.1.1).trans_le (hl hwl)
  have hcont : Continuous (fun K ↦ L K - R K) :=
    (tie_height_continuous hLc hLl).sub (tie_height_continuous hRc hRl)
  obtain ⟨K,hK,hzero⟩ := intermediate_value_Icc (by norm_num : (7/5 : ℝ) ≤ 8/5)
    hcont.continuousOn
    (show 0 ∈ Icc (L (7/5)-R (7/5)) (L (8/5)-R (8/5)) from
      ⟨(sub_neg.mpr hleft).le,(sub_pos.mpr hright).le⟩)
  have hEq : L K = R K := sub_eq_zero.mp hzero
  have hKi : K ∈ Ioo (7/5 : ℝ) (8/5) := by
    refine ⟨hK.1.lt_of_ne ?_,hK.2.lt_of_ne ?_⟩
    · intro h
      exact hleft.ne (h ▸ hEq)
    · intro h
      exact hright.ne (h.symm ▸ hEq.symm)
  obtain ⟨p,hp,hpmax,hpval⟩ := left_max K
  obtain ⟨q,hq,hqmax,hqval⟩ := right_max K
  change L K = _ at hpval
  change R K = _ at hqval
  have he : tieObjective K p = tieObjective K q := hpval.symm.trans (hEq.trans hqval)
  have hw : tieObjective K wl ≤ tieObjective K p := hpmax hwl
  have hps : p.1 < 1 := hp.1.2.lt_of_ne fun ht ↦ by
    rcases p with ⟨t,b⟩
    change t = 1 at ht
    subst t
    exact (tie_seam_loses hK b).not_ge hw
  have hqs : 1 < q.1 := hq.1.1.lt_of_ne fun ht ↦ by
    rcases q with ⟨t,b⟩
    change 1 = t at ht
    subst t
    exact (tie_seam_loses hK b).not_ge (hw.trans he.le)
  have hpG : IsMaxOn (tieObjective K) BurrHeld.firstDomain p := by
    rintro ⟨t,b⟩ ht
    have ht' : 0 < t := ht.1
    by_cases hval : -2 ≤ tieObjective K (t,b)
    · have hmem := tie_superlevel hK ht' hval
      rcases le_total t 1 with h | h
      · exact hpmax (show (t,b) ∈ tieLeftBox from ⟨⟨hmem.1.1,h⟩,hmem.2⟩)
      · exact (hqmax (show (t,b) ∈ tieRightBox from ⟨⟨h,hmem.1.2⟩,hmem.2⟩)).trans he.ge
    · exact ((lt_of_not_ge hval).trans (tie_low_gt_minus_two hK)).le.trans hw
  exact ⟨K,hKi,p,⟨hposL _ hp,mem_univ _⟩,q,⟨hposR _ hq,mem_univ _⟩,
    hps,hqs,hpG,fun _ hx ↦ (hpG hx).trans he.le⟩


private theorem real_tie_scale_transport {v : ℝ} (hv : v * v = 1)
    {f : ℝ → ℝ × ℝ → ℝ} (J : ℝ → ℝ)
    (hf : ∀ K > 0, ∀ p ∈ Weibull.domain,
      f K p = likelihood (tieLogs K) (p.1, p.1, (pairChart v p).2) - J K) :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      ∃ p ∈ Weibull.domain, ∃ q ∈ Weibull.domain,
        p.1 < eps ∧ eps < q.1 ∧
        IsMaxOn (f K) Weibull.domain p ∧ IsMaxOn (f K) Weibull.domain q := by
  obtain ⟨K,hK,p,hp,q,hq,hp1,hq1,hpm,hqm⟩ := tie_chart_global
  have hK0 : 0 < K := (by norm_num : (0 : ℝ) < 7/5).trans hK.1
  have heps : 0 < eps := by norm_num [eps]
  let β (x : ℝ × ℝ) : ℝ × ℝ := (eps*x.1,x.2)
  have hm (x : ℝ × ℝ) (hx : x ∈ BurrHeld.firstDomain)
      (hmx : IsMaxOn (tieObjective K) BurrHeld.firstDomain x) :
      ∃ u ∈ Weibull.domain, u.1 = eps*x.1 ∧ IsMaxOn (f K) Weibull.domain u := by
    have hxb : β x ∈ BurrHeld.firstDomain := ⟨mul_pos heps hx.1,mem_univ _⟩
    obtain ⟨u,hu,hEq⟩ := (pair_bij hv).surjOn hxb
    have hu1 : u.1 = eps*x.1 := congrArg Prod.fst hEq
    have hu2 : (pairChart v u).2 = (β x).2 := congrArg Prod.snd hEq
    change (pairChart v u).2 = x.2 at hu2
    refine ⟨u,hu,hu1,fun w hw ↦ ?_⟩
    let t := w.1/eps
    have ht : 0 < t := div_pos hw.1 heps
    let a := (pairChart v w).2
    have ht1 : eps*t = w.1 := by dsimp [t]; field_simp
    have hwf := hf K hK0 w hw
    have huf := hf K hK0 u hu
    change f K w ≤ f K u
    rw [hwf,huf, ← ht1,hu1,hu2,tieObjective_eq K _ _ ht hK0,
      tieObjective_eq K _ _ (show 0 < x.1 from hx.1) hK0]
    exact sub_le_sub_right (add_le_add_left
      (hmx (show (t,a) ∈ BurrHeld.firstDomain from ⟨ht,mem_univ _⟩)) _) _
  obtain ⟨p',hp',hps,hpm'⟩ := hm p hp hpm
  obtain ⟨q',hq',hqs,hqm'⟩ := hm q hq hqm
  exact ⟨K,hK,p',hp',q',hq',by rw [hps]; nlinarith,by rw [hqs]; nlinarith,hpm',hqm'⟩

/-- The report's exact two-level paralogistic sample path contains a sample
with two distinct finite global maximizers on opposite sides of shape 1/10000. -/
theorem paralogistic_two_level_tie :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (100000000*K)
      let f := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
      ∃ p ∈ Weibull.domain, ∃ q ∈ Weibull.domain,
        p.1 < 1/10000 ∧ 1/10000 < q.1 ∧
        IsMaxOn f Weibull.domain p ∧ IsMaxOn f Weibull.domain q := by
  let x (K : ℝ) (i : Fin 10000) := exp (tieLogs K i)
  have hx (K : ℝ) (i : Fin 10000) : x K i = if i = 0 then 1 else exp (100000000*K) := by
    by_cases h : i = 0 <;> simp [x,tieLogs,h]
  have h := real_tie_scale_transport (v := -1) (by norm_num)
    (f := fun K p ↦ Paralogistic.logLik (x K) p.1 p.2) (fun K ↦ avg (tieLogs K)) ?_
  · obtain ⟨K,hK,p,hp,q,hq,h1,h2,hpm,hqm⟩ := h
    refine ⟨K,hK,p,hp,q,hq,h1,h2,?_⟩
    have he := funext (hx K)
    rw [← he]
    exact ⟨hpm,hqm⟩
  intro K _ p hp
  unfold Paralogistic.logLik
  rw [BurrHeld.logLik_eq_first (by decide) (fun i ↦ exp_pos (tieLogs K i)) hp.2]
  simp [log_exp, likelihood, pairChart]

/-- The corresponding reflected two-level sample has two distinct finite
global maximizers for inverse paralogistic. -/
theorem inverse_paralogistic_two_level_tie :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (-(100000000*K))
      let f := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
      ∃ p ∈ Weibull.domain, ∃ q ∈ Weibull.domain,
        p.1 < 1/10000 ∧ 1/10000 < q.1 ∧
        IsMaxOn f Weibull.domain p ∧ IsMaxOn f Weibull.domain q := by
  let x (K : ℝ) (i : Fin 10000) := exp (-(tieLogs K i))
  have hx (K : ℝ) (i : Fin 10000) : x K i = if i = 0 then 1 else exp (-(100000000*K)) := by
    by_cases h : i = 0 <;> simp [x,tieLogs,h]
  have h := real_tie_scale_transport (v := 1) (by norm_num)
    (f := fun K p ↦ InvParalogistic.logLik (x K) p.1 p.2) (fun K ↦ -avg (tieLogs K)) ?_
  · obtain ⟨K,hK,p,hp,q,hq,h1,h2,hpm,hqm⟩ := h
    refine ⟨K,hK,p,hp,q,hq,h1,h2,?_⟩
    have he := funext (hx K)
    rw [← he]
    exact ⟨hpm,hqm⟩
  intro K _ p hp
  have he : Transfer.reciprocal (x K) = fun i ↦ exp (tieLogs K i) := by
    funext i
    simp [x,Transfer.reciprocal,exp_neg]
  rw [InvParalogistic.logLik_reciprocal (fun i ↦ exp_pos (-(tieLogs K i))),he]
  unfold Paralogistic.logLik
  rw [BurrHeld.logLik_eq_first (by decide) (fun i ↦ exp_pos (tieLogs K i)) (inv_pos.mpr hp.2)]
  simp only [log_exp,log_inv,likelihood,pairChart,one_mul,mul_neg,neg_neg]
  rw [show (fun i ↦ -(-2 * tieLogs K i)) = (fun i ↦ 2 * tieLogs K i) by funext i; ring, avg_mul]
  ring

end

end UnivariateML.BurrFree
