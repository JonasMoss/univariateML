import UnivariateML.Family.Weibull
import Mathlib.Analysis.Calculus.Deriv.Slope
import UnivariateML.Generic.Escape
import UnivariateML.Family.Transfers

/-! Exponentiated Weibull likelihood and its Weibull subfamily.

## Deviations

The held-exponent and held-shape results cover nonconstant positive data.
The two common paths approach the optimized face values. Exhaustiveness
over arbitrary escaping sequences is a separate claim.
-/

open Real Set
namespace UnivariateML.ShapeFacesExpWeibull
noncomputable section

variable {n : ℕ}

def logLik (x : Fin n → ℝ) (b a c : ℝ) : ℝ :=
  Weibull.logLik x b a + (n : ℝ) * log c +
    (c - 1) * ∑ i, log (1 - exp (-((x i / a) ^ b)))

theorem exponent_one_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2 1) Weibull.domain p := by
  simpa [logLik] using Weibull.jointMLE hn hx hne

def exponentRate (x : Fin n → ℝ) (b a : ℝ) : ℝ :=
  ∑ i, -log (1 - exp (-((x i / a) ^ b)))

theorem held_shape_scale_mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) :
    ∃! c, c ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x b a) (Ioi 0) c := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hR : 0 < exponentRate x b a := by
    apply Finset.sum_pos
    · intro i _
      have h : 0 < (x i / a) ^ b := rpow_pos_of_pos (div_pos (hx i) ha) _
      have he : exp (-((x i / a) ^ b)) < 1 := exp_lt_one_iff.mpr (neg_neg_of_pos h)
      exact neg_pos.mpr (log_neg (sub_pos.mpr he) (by linarith [exp_pos (-((x i / a) ^ b))]))
    · exact ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let c₀ := (n : ℝ) / exponentRate x b a
  have hc₀ : 0 < c₀ := div_pos hn' hR
  have hprod : exponentRate x b a * c₀ = n := by
    dsimp [c₀]
    field_simp
  have hlt (c : ℝ) (hc : 0 < c) (hne : c ≠ c₀) : logLik x b a c < logLik x b a c₀ := by
    have hratio : c / c₀ ≠ 1 := fun h ↦ hne ((div_eq_one_iff_eq hc₀.ne').mp h)
    have h := Real.log_lt_sub_one_of_pos (div_pos hc hc₀) hratio
    rw [log_div hc.ne' hc₀.ne'] at h
    have h' := mul_lt_mul_of_pos_left h hn'
    have hmul : (n : ℝ) * (c / c₀) = exponentRate x b a * c := by
      rw [← hprod]
      field_simp
    have hright : (n : ℝ) * (c / c₀ - 1) = exponentRate x b a * c - n := by
      calc
        _ = (n : ℝ) * (c / c₀) - n := by ring
        _ = _ := by rw [hmul]
    rw [hright] at h'
    have hsum : exponentRate x b a = -(∑ i, log (1 - exp (-((x i / a) ^ b)))) :=
      Finset.sum_neg_distrib _
    unfold logLik
    rw [hsum] at hprod h'
    nlinarith only [hprod, h']
  refine ⟨c₀, ⟨hc₀, ?_⟩, ?_⟩
  · intro c hc
    obtain rfl | hne := eq_or_ne c c₀
    · exact show logLik x b a c₀ ≤ logLik x b a c₀ from le_rfl
    · exact (hlt c hc hne).le
  · intro c hc
    by_contra hne
    exact (hlt c hc.1 hne).not_ge (hc.2 hc₀)


/-- Parameter-dependent log density of the log observation, excluding
the two normalization terms. -/
def logKernel (c z : ℝ) : ℝ :=
  z - exp z + (c - 1) * log (1 - exp (-exp z))

private def R (u : ℝ) : ℝ := u / (exp u - 1)

private def R' (u : ℝ) : ℝ :=
  (exp u - 1 - u * exp u) / (exp u - 1) ^ 2

private theorem Q_pos {u : ℝ} (hu : 0 < u) : 0 < 1 - exp (-u) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hu))

private theorem R_deriv {u : ℝ} (hu : 0 < u) : HasDerivAt R (R' u) u := by
  have he : 0 < exp u - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hu)
  convert (hasDerivAt_id u).div ((hasDerivAt_exp u).sub_const 1) he.ne' using 1
  · rfl
  · simp [R', id_eq]

private theorem R'_bounds {u : ℝ} (hu : 0 < u) : -1 < R' u ∧ R' u < 0 := by
  have he : 0 < exp u - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hu)
  have hp : 0 < (exp u - 1) ^ 2 := sq_pos_of_pos he
  constructor
  · apply (lt_div_iff₀ hp).2
    nlinarith [mul_pos (exp_pos u) (sub_pos.mpr (add_one_lt_exp hu.ne'))]
  · apply div_neg_of_neg_of_pos _ hp
    have h := mul_lt_mul_of_pos_right (add_one_lt_exp (neg_ne_zero.mpr hu.ne')) (exp_pos u)
    rw [← exp_add, neg_add_cancel, exp_zero] at h
    nlinarith

private theorem logKernel_deriv (c z : ℝ) :
    HasDerivAt (logKernel c) (1 - exp z + (c - 1) * R (exp z)) z := by
  have hq := Q_pos (exp_pos z)
  have he : exp (-exp z) * exp (exp z) = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
  have hd : HasDerivAt (fun t ↦ log (1 - exp (-exp t))) (R (exp z)) z := by
    convert (((((hasDerivAt_exp z).neg).exp).const_sub 1).log hq.ne') using 1
    simp only [R, mul_neg, neg_neg, Pi.neg_apply]
    have hden : exp (exp z) - 1 ≠ 0 := (sub_pos.mpr (one_lt_exp_iff.mpr (exp_pos z))).ne'
    apply (div_eq_div_iff hden hq.ne').2
    nlinarith [congrArg (fun w ↦ w * exp z) he]
  exact ((hasDerivAt_id z).sub (hasDerivAt_exp z)).add (hd.const_mul (c - 1))

private theorem logKernel_strictConcave {c : ℝ} (hc : 0 < c) :
    StrictConcaveOn ℝ univ (logKernel c) := by
  apply strictConcaveOn_of_deriv2_neg' convex_univ
    (fun z _ ↦ (logKernel_deriv c z).continuousAt.continuousWithinAt)
  intro z _
  have he : deriv (logKernel c) = fun t ↦ 1 - exp t + (c - 1) * R (exp t) :=
    funext fun t ↦ (logKernel_deriv c t).deriv
  have hd := ((hasDerivAt_exp z).const_sub 1).add
    (((R_deriv (exp_pos z)).comp z (hasDerivAt_exp z)).const_mul (c - 1))
  change deriv (deriv (logKernel c)) z < 0
  rw [he]
  have hh : deriv (fun t ↦ 1 - exp t + (c - 1) * R (exp t)) z =
      -exp z + (c - 1) * (R' (exp z) * exp z) := hd.deriv
  rw [hh]
  obtain ⟨hlo, hhi⟩ := R'_bounds (exp_pos z)
  have h : (c - 1) * R' (exp z) < 1 := by
    by_cases h : 1 ≤ c
    · linarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hhi.le]
    · have h' := mul_lt_mul_of_neg_left hlo (sub_neg.mpr (lt_of_not_ge h))
      linarith
  change -exp z + (c - 1) * (R' (exp z) * exp z) < 0
  nlinarith [mul_lt_mul_of_pos_right h (exp_pos z)]

private theorem logKernel_bound {c : ℝ} (hc : 0 < c) (z : ℝ) :
    logKernel c z ≤ -(min 1 c * |z|) := by
  let u := exp z
  have hu : 0 < u := exp_pos z
  have hQ := Q_pos hu
  have hlow : z - u ≤ log (1 - exp (-u)) := by
    calc
      z - u = log (exp (z - u)) := (log_exp _).symm
      _ ≤ _ := by
        apply log_le_log (exp_pos _)
        rw [exp_sub]
        change u / exp u ≤ 1 - exp (-u)
        have h := add_one_le_exp u
        have hp : exp (-u) * exp u = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
        apply (div_le_iff₀ (exp_pos u)).2
        nlinarith
  have hupp : log (1 - exp (-u)) ≤ z := by
    rw [← log_exp z]
    apply log_le_log hQ
    exact (show 1 - exp (-u) ≤ u by linarith [add_one_le_exp (-u)])
  have hneg : log (1 - exp (-u)) ≤ 0 := log_nonpos hQ.le (by linarith [exp_pos (-u)])
  have h2 (hz : 0 ≤ z) : 2 * z ≤ exp z := by
    have he : exp 1 * exp (z - 1) = exp z := by
      rw [← exp_add, show 1 + (z - 1) = z by ring]
    have h := add_one_le_exp (z - 1)
    calc
      2 * z ≤ exp 1 * z := mul_le_mul_of_nonneg_right (show (2 : ℝ) ≤ exp 1 by linarith [add_one_le_exp (1 : ℝ)]) hz
      _ ≤ exp z := by rw [← he]; exact mul_le_mul_of_nonneg_left (by linarith) (exp_pos _).le
  change z - u + (c - 1) * log (1 - exp (-u)) ≤ -(min 1 c * |z|)
  by_cases h : 1 ≤ c
  · rw [min_eq_left h]
    rcases le_total 0 z with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [h2 hz, mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hneg]
    · rw [abs_of_nonpos hz]
      nlinarith [mul_le_mul_of_nonneg_left hupp (sub_nonneg.mpr h),
        mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hz]
  · rw [min_eq_right (le_of_not_ge h)]
    have hbound : z - u + (c - 1) * log (1 - exp (-u)) ≤ c * (z - u) := by
      nlinarith [mul_le_mul_of_nonpos_left hlow (sub_nonpos.mpr (le_of_not_ge h))]
    apply hbound.trans
    rcases le_total 0 z with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [mul_le_mul_of_nonneg_left (h2 hz) hc.le]
    · rw [abs_of_nonpos hz]
      nlinarith [mul_pos hc hu]


def affineDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

def affineLik (y : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) : ℝ :=
  (n : ℝ) * log p.1 + ∑ i, logKernel c (p.1 * y i + p.2)

private theorem blend (a b : ℝ) (p q : ℝ × ℝ) (y : ℝ) :
    (a • p + b • q).1 * y + (a • p + b • q).2 =
      a * (p.1 * y + p.2) + b * (q.1 * y + q.2) := by
  change (a * p.1 + b * q.1) * y + (a * p.2 + b * q.2) = _
  ring

theorem affine_strictConcave {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) : StrictConcaveOn ℝ affineDomain (affineLik y c) := by
  refine ⟨(convex_Ioi _).prod convex_univ, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  obtain ⟨i, j, hij⟩ := hne
  have hex : ∃ i, p.1 * y i + p.2 ≠ q.1 * y i + q.2 := by
    by_contra! h
    have hz : (p.1 - q.1) * (y i - y j) = 0 := by
      calc
        _ = (p.1 * y i + p.2 - (q.1 * y i + q.2)) -
            (p.1 * y j + p.2 - (q.1 * y j + q.2)) := by ring
        _ = 0 := by simp [h i, h j]
    have he := sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hij))
    exact hpq (Prod.ext he (by simpa [he] using h i))
  obtain ⟨i, hi⟩ := hex
  have hlog := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 ha.le hb.le hab
  change a * log p.1 + b * log q.1 ≤ log (a * p.1 + b * q.1) at hlog
  let K (r : ℝ × ℝ) (i : Fin n) := logKernel c (r.1 * y i + r.2)
  have hsum : (∑ i, (a * K p i + b * K q i)) < (∑ i, K (a • p + b • q) i) := by
    apply Finset.sum_lt_sum (f := fun i ↦ a * K p i + b * K q i)
      (g := K (a • p + b • q)) (fun j _ ↦ ?_) ⟨i, Finset.mem_univ _, ?_⟩
    · unfold K
      rw [blend]
      exact (logKernel_strictConcave hc).2 (mem_univ _) (mem_univ _) hi ha hb hab
    · unfold K
      rw [blend]
      exact (logKernel_strictConcave hc).concaveOn.2 (mem_univ _) (mem_univ _) ha.le hb.le hab
  simp only [Finset.sum_add_distrib, ← Finset.mul_sum] at hsum
  change a * ((n : ℝ) * log p.1 + ∑ i, K p i) +
      b * ((n : ℝ) * log q.1 + ∑ i, K q i) <
      (n : ℝ) * log (a * p.1 + b * q.1) + ∑ i, K (a • p + b • q) i
  nlinarith [mul_le_mul_of_nonneg_left hlog (Nat.cast_nonneg n : (0 : ℝ) ≤ n)]

private theorem affine_point_bound {c : ℝ} (hc : 0 < c) (y : Fin n → ℝ)
    (p : ℝ × ℝ) (i : Fin n) :
    min 1 c * |p.1 * y i + p.2| ≤ (n : ℝ) * log p.1 - affineLik y c p := by
  have hh : ∀ j : Fin n, 0 ≤ -logKernel c (p.1 * y j + p.2) := by
    intro j
    linarith [logKernel_bound hc (p.1 * y j + p.2),
      mul_nonneg (le_of_lt (lt_min zero_lt_one hc)) (abs_nonneg (p.1 * y j + p.2))]
  have hsingle := Finset.single_le_sum (fun j (_ : j ∈ Finset.univ) ↦ hh j)
    (Finset.mem_univ i)
  rw [Finset.sum_neg_distrib] at hsingle
  unfold affineLik
  linarith [logKernel_bound hc (p.1 * y i + p.2)]

private theorem affine_superlevel (hn : 0 < n) {c : ℝ} (hc : 0 < c)
    {y : Fin n → ℝ} (hne : ∃ i j, y i ≠ y j) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ affineDomain ∧
      ∀ p ∈ affineDomain, M ≤ affineLik y c p → p ∈ K := by
  obtain ⟨i, j, hij⟩ := hne
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let m := min 1 c
  have hm : 0 < m := lt_min zero_lt_one hc
  let d := m * |y i - y j| / (4 * n)
  have hd : 0 < d :=
    div_pos (mul_pos hm (abs_pos.mpr (sub_ne_zero.mpr hij))) (by positivity)
  let lo := exp (M / n)
  let hi := max ((-M / n - log d - 1) / d) lo
  let B := ((n : ℝ) * log hi - M) / m + hi * |y i|
  refine ⟨Icc lo hi ×ˢ Icc (-B) B, isCompact_Icc.prod isCompact_Icc, ?_, ?_⟩
  · intro p hp
    exact ⟨(exp_pos _).trans_le hp.1.1, mem_univ _⟩
  · intro p hp hM
    have hb : 0 < p.1 := hp.1
    have hpi := affine_point_bound hc y p i
    have hpj := affine_point_bound hc y p j
    change m * _ ≤ _ at hpi hpj
    have hnonneg := mul_nonneg hm.le (abs_nonneg (p.1 * y i + p.2))
    have hlo : lo ≤ p.1 := by
      rw [← exp_log hb]
      apply exp_le_exp.mpr
      apply (div_le_iff₀ hN).2
      nlinarith
    have htri := abs_sub (p.1 * y i + p.2) (p.1 * y j + p.2)
    rw [show p.1 * y i + p.2 - (p.1 * y j + p.2) = p.1 * (y i - y j) by ring,
      abs_mul, abs_of_pos hb] at htri
    have hlin := mul_le_mul_of_nonneg_left htri hm.le
    have hlog := log_le_sub_one_of_pos (mul_pos hd hb)
    rw [log_mul hd.ne' hb.ne'] at hlog
    have hnd : (n : ℝ) * d * p.1 = m * (p.1 * |y i - y j|) / 4 := by
      dsimp [d]
      field_simp
    have hupper : p.1 ≤ hi := by
      apply le_max_of_le_left
      apply (le_div_iff₀ hd).2
      have hlog' := mul_le_mul_of_nonneg_left hlog hN.le
      have hbound : (n : ℝ) * (d * p.1 + log d + 1) ≤ -M := by
        nlinarith only [hpi, hpj, hlin, hnd, hlog', hM]
      apply (le_of_mul_le_mul_left _ hN)
      have hM' : (n : ℝ) * (-M / n) = -M := by field_simp
      nlinarith only [hM', hbound]
    have hlogB := mul_le_mul_of_nonneg_left (log_le_log hb hupper) hN.le
    have hbi : |p.1 * y i + p.2| ≤ ((n : ℝ) * log hi - M) / m := by
      apply (le_div_iff₀ hm).2
      linarith only [hpi, hlogB, hM,
        show |p.1 * y i + p.2| * m = m * |p.1 * y i + p.2| by ring]
    have hB := abs_sub (p.1 * y i + p.2) (p.1 * y i)
    rw [add_sub_cancel_left, abs_mul, abs_of_pos hb] at hB
    exact ⟨⟨hlo, hupper⟩, abs_le.mp (hB.trans (add_le_add hbi
      (mul_le_mul_of_nonneg_right hupper (abs_nonneg _))))⟩

open Filter in
theorem affine_coercive (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    Tendsto (fun p : affineDomain ↦ affineLik y c p) (cocompact affineDomain) atBot := by
  apply tendsto_atBot.2
  intro M
  obtain ⟨K, hK, hKD, hlevel⟩ := affine_superlevel hn hc hne M
  have hK' : IsCompact ((Subtype.val : affineDomain → ℝ × ℝ) ⁻¹' K) :=
    Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage' hK
      (by simpa only [Subtype.range_coe] using hKD)
  have h : ∀ᶠ p : affineDomain in cocompact affineDomain,
      p ∈ ((Subtype.val : affineDomain → ℝ × ℝ) ⁻¹' K)ᶜ := hK'.compl_mem_cocompact
  exact h.mono
    (fun p hp ↦ (lt_of_not_ge fun hM ↦ hp (hlevel p p.property hM)).le)

theorem affine_held_exponent_mle (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃! p, p ∈ affineDomain ∧ IsMaxOn (affineLik y c) affineDomain p :=
  Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    ⟨(1, 0), by norm_num [affineDomain]⟩ (isOpen_Ioi.prod isOpen_univ)
    (affine_strictConcave hc hne) (affine_coercive hn hc hne)


private def toAffine (p : ℝ × ℝ) : ℝ × ℝ := (p.1, -(p.1 * log p.2))

private theorem toAffine_bij : BijOn toAffine Weibull.domain affineDomain := by
  refine ⟨fun p hp ↦ ⟨hp.1, mem_univ _⟩, ?_, ?_⟩
  · intro p hp q hq he
    change (p.1, -(p.1 * log p.2)) = (q.1, -(q.1 * log q.2)) at he
    have hb : p.1 = q.1 := (Prod.mk.inj he).1
    have hs : -(p.1 * log p.2) = -(q.1 * log q.2) :=
      congrArg (fun r : ℝ × ℝ ↦ r.2) he
    rw [← hb] at hs
    have hlog := mul_left_cancel₀ (show 0 < p.1 from hp.1).ne' (neg_injective hs)
    exact Prod.ext hb (log_injOn_pos hp.2 hq.2 hlog)
  · rintro ⟨b, d⟩ hp
    have hb : 0 < b := hp.1
    refine ⟨(b, exp (-d / b)), ⟨hp.1, exp_pos _⟩, Prod.ext rfl ?_⟩
    change -(b * log (exp (-d / b))) = d
    rw [log_exp]
    field_simp

private theorem physical_affine {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (c : ℝ) (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) :
    logLik x p.1 p.2 c =
      affineLik (fun i ↦ log (x i)) c (toAffine p) +
        ((n : ℝ) * log c - ∑ i, log (x i)) := by
  have hs : 0 < p.2 := hp.2
  have hr (i : Fin n) :
      (x i / p.2) ^ p.1 = exp (p.1 * log (x i) + -(p.1 * log p.2)) := by
    rw [rpow_def_of_pos (div_pos (hx i) hs), log_div (hx i).ne' hs.ne']
    congr 1
    ring
  have hd (i : Fin n) :
      Weibull.logDensity p.1 p.2 (x i) =
        log p.1 + (p.1 * log (x i) + -(p.1 * log p.2)) -
          exp (p.1 * log (x i) + -(p.1 * log p.2)) - log (x i) := by
    unfold Weibull.logDensity
    rw [hr, log_div (hx i).ne' hs.ne']
    ring
  simp only [logLik, Weibull.logLik, hd, affineLik, toAffine, logKernel, hr,
    Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum]
  ring

theorem held_exponent_mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p := by
  have hy : ∃ i j, log (x i) ≠ log (x j) := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij (log_injOn_pos (hx i) (hx j) h)⟩
  exact (Transfer.existsUnique_max_iff toAffine_bij (physical_affine hx c)).2
    (affine_held_exponent_mle hn hc hy)


private def logQ (u : ℝ) := log (1 - exp (-u))

private theorem logQ_bounds {u : ℝ} (hu : 0 < u) :
    log u - u ≤ logQ u ∧ logQ u ≤ log u ∧ logQ u ≤ -exp (-u) := by
  have hQ := Q_pos hu
  refine ⟨?_, ?_, ?_⟩
  · calc
      log u - u = log (exp (log u - u)) := (log_exp _).symm
      _ ≤ _ := by
        apply log_le_log (exp_pos _)
        rw [exp_sub, exp_log hu]
        apply (div_le_iff₀ (exp_pos u)).2
        have hp : exp (-u) * exp u = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
        nlinarith [add_one_le_exp u]
  · exact log_le_log hQ (by linarith [add_one_le_exp (-u)])
  · unfold logQ
    linarith [log_le_sub_one_of_pos hQ]

private def negLogQSum (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  ∑ i, -logQ (t * r i)

private def residualSum (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  ∑ i, (log (t * r i) - t * r i - logQ (t * r i))

private def heldShapeProfile (r : Fin n → ℝ) (t : ℝ) : ℝ :=
  (n : ℝ) * log (n / negLogQSum r t) - n + residualSum r t

private theorem one_le_negLogQSum {r : Fin n → ℝ} (hr : ∀ i, 0 < r i)
    {t : ℝ} (ht : 0 < t) (j : Fin n) : -logQ (t * r j) ≤ negLogQSum r t := by
  apply Finset.single_le_sum _ (Finset.mem_univ j)
  intro i _
  linarith [(logQ_bounds (mul_pos ht (hr i))).2.2, exp_pos (-(t * r i))]

private theorem negLogQSum_pos {r : Fin n → ℝ} (hr : ∀ i, 0 < r i)
    {t : ℝ} (ht : 0 < t) (j : Fin n) : 0 < negLogQSum r t :=
  (exp_pos (-(t * r j))).trans_le
    (by
      have h := (neg_le_neg (logQ_bounds (mul_pos ht (hr j))).2.2).trans
        (one_le_negLogQSum hr ht j)
      simpa only [neg_neg] using h)

private theorem heldShapeProfile_continuous {r : Fin n → ℝ} (hr : ∀ i, 0 < r i)
    (j : Fin n) : ContinuousOn (heldShapeProfile r) (Ioi 0) := by
  have hu (i : Fin n) : Continuous (fun t : ℝ ↦ t * r i) := continuous_id.mul_const _
  have hq (i : Fin n) : ContinuousOn (fun t ↦ logQ (t * r i)) (Ioi 0) := by
    unfold logQ
    exact (continuous_const.sub (continuous_exp.comp (hu i).neg)).continuousOn.log
      (fun t ht ↦ (Q_pos (mul_pos ht (hr i))).ne')
  have hh : ContinuousOn (negLogQSum r) (Ioi 0) :=
    continuousOn_finsetSum _ (fun i _ ↦ (hq i).neg)
  have hres : ContinuousOn (residualSum r) (Ioi 0) :=
    continuousOn_finsetSum _ (fun i _ ↦
      (((hu i).continuousOn.log (fun t ht ↦ (mul_pos ht (hr i)).ne')).sub
        (hu i).continuousOn).sub (hq i))
  have hc : ContinuousOn (fun t ↦ (n : ℝ) / negLogQSum r t) (Ioi 0) :=
    continuousOn_const.div hh (fun _ ht ↦ (negLogQSum_pos hr ht j).ne')
  have hn : (n : ℝ) ≠ 0 := by exact_mod_cast (Nat.ne_of_gt (Nat.zero_lt_of_lt j.isLt))
  have hl := hc.log (fun _ ht ↦ div_ne_zero hn (negLogQSum_pos hr ht j).ne')
  exact ((hl.const_mul (n : ℝ)).sub continuousOn_const).add hres

private theorem heldShapeProfile_max {r : Fin n → ℝ} (hr : ∀ i, 0 < r i)
    (hne : ∃ i j, r i ≠ r j) :
    ∃ t ∈ Ioi (0 : ℝ), IsMaxOn (heldShapeProfile r) (Ioi 0) t := by
  classical
  obtain ⟨i, j, hij⟩ := hne
  have hN : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt i.isLt)
  obtain ⟨k, _, hk⟩ :=
    Finset.exists_min_image Finset.univ r ⟨i, Finset.mem_univ _⟩
  have hmin (l : Fin n) : r k ≤ r l := hk l (Finset.mem_univ _)
  have hGap : 0 < (∑ l, r l) - (n : ℝ) * r k := by
    have hsome : ∃ l, r k < r l := by
      by_contra! H
      exact hij ((le_antisymm (H i) (hmin i)).trans (le_antisymm (H j) (hmin j)).symm)
    obtain ⟨l, hl⟩ := hsome
    have hs := Finset.sum_lt_sum (fun m (_ : m ∈ Finset.univ) ↦ hmin m)
      ⟨l, Finset.mem_univ _, hl⟩
    simpa [Finset.sum_const, nsmul_eq_mul, sub_pos] using hs
  let M := heldShapeProfile r 1
  let ε := min 1 (exp (-exp (log n - 1 - M / n)) / r k)
  have hε : 0 < ε := lt_min zero_lt_one (div_pos (exp_pos _) (hr k))
  have hε1 : ε ≤ 1 := min_le_left _ _
  have hlow (t : ℝ) (ht : 0 < t) (hM : M ≤ heldShapeProfile r t) : ε ≤ t := by
    let H := negLogQSum r t
    have hH : 0 < H := negLogQSum_pos hr ht k
    have hR : residualSum r t ≤ 0 :=
      Finset.sum_nonpos fun l _ ↦ sub_nonpos.mpr (logQ_bounds (mul_pos ht (hr l))).1
    have hlog : log H ≤ log n - 1 - M / n := by
      unfold heldShapeProfile at hM
      rw [log_div hN.ne' hH.ne'] at hM
      have hm : (n : ℝ) * (M / n) = M := by field_simp
      apply (le_of_mul_le_mul_left _ hN)
      nlinarith only [hM, hR, hm]
    have hHH : H ≤ exp (log n - 1 - M / n) := by
      rw [← exp_log hH]
      exact exp_le_exp.mpr hlog
    have hlogt : -(exp (log n - 1 - M / n)) ≤ log (t * r k) := by
      have hq := (logQ_bounds (mul_pos ht (hr k))).2.1
      have hh := one_le_negLogQSum hr ht k
      change -logQ (t * r k) ≤ H at hh
      linarith
    apply le_trans (min_le_right _ _)
    apply (div_le_iff₀ (hr k)).2
    rw [← exp_log (mul_pos ht (hr k))]
    apply exp_le_exp.mpr
    nlinarith
  let d := ((∑ l, r l) - (n : ℝ) * r k) / (2 * n)
  have hd : 0 < d := div_pos hGap (by positivity)
  let C := (n : ℝ) * (log n - 1 - log d - 1) +
    ∑ l, (log (r l) - logQ (r l))
  let B := max 1 ((C - M) / ((n : ℝ) * d))
  have hB1 : 1 ≤ B := le_max_left _ _
  have hupper (t : ℝ) (ht : 0 < t) (hM : M ≤ heldShapeProfile r t) : t ≤ B := by
    by_cases ht1 : t ≤ 1
    · exact ht1.trans hB1
    have ht1' : 1 ≤ t := (le_of_not_ge ht1)
    have hH : 0 < negLogQSum r t := negLogQSum_pos hr ht k
    have hh : -(t * r k) ≤ log (negLogQSum r t) := by
      rw [← log_exp (-(t * r k))]
      apply log_le_log (exp_pos _)
      simpa only [neg_neg] using (neg_le_neg (logQ_bounds (mul_pos ht (hr k))).2.2).trans
        (one_le_negLogQSum hr ht k)
    have hres : residualSum r t ≤ (n : ℝ) * log t -
        t * (∑ l, r l) + ∑ l, (log (r l) - logQ (r l)) := by
      unfold residualSum
      have he (l : Fin n) : logQ (r l) ≤ logQ (t * r l) := by
        apply log_le_log (Q_pos (hr l))
        apply sub_le_sub_left
        apply exp_le_exp.mpr
        linarith [mul_le_mul_of_nonneg_right ht1' (hr l).le]
      have hs := Finset.sum_le_sum (s := Finset.univ) (fun l _ ↦
        show log (t * r l) - t * r l - logQ (t * r l) ≤
            log t - t * r l + (log (r l) - logQ (r l)) by
          rw [log_mul ht.ne' (hr l).ne']
          linarith [he l])
      simpa only [Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
        Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum] using hs
    have hlog := log_le_sub_one_of_pos (mul_pos hd ht)
    rw [log_mul hd.ne' ht.ne'] at hlog
    unfold heldShapeProfile at hM
    rw [log_div hN.ne' hH.ne'] at hM
    apply le_trans _ (le_max_right _ _)
    apply (le_div_iff₀ (mul_pos hN hd)).2
    have hd' : 2 * (n : ℝ) * d = (∑ l, r l) - n * r k := by
      dsimp [d]
      field_simp
    dsimp [C]
    nlinarith only [hM, hres,
      mul_le_mul_of_nonneg_left hh hN.le, mul_le_mul_of_nonneg_left hlog hN.le,
      congrArg (fun v ↦ t * v) hd']
  have hsub : Icc ε B ⊆ Ioi (0 : ℝ) := fun _ h ↦ hε.trans_le h.1
  obtain ⟨t, ht, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨1, hε1, hB1⟩
    ((heldShapeProfile_continuous hr k).mono hsub)
  refine ⟨t, hsub ht, fun s hs ↦ ?_⟩
  by_cases h : M ≤ heldShapeProfile r s
  · exact hmax ⟨hlow s hs h, hupper s hs h⟩
  · exact (le_of_not_ge h).trans (hmax ⟨hε1, hB1⟩)


private theorem heldShape_lik {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (b c : ℝ) {a : ℝ} (ha : 0 < a) :
    let r : Fin n → ℝ := fun i ↦ x i ^ b
    let t := exp (-(b * log a))
    logLik x b a c = (n : ℝ) * log b - (∑ i, log (x i)) +
      ((n : ℝ) * log c - c * negLogQSum r t + residualSum r t) := by
  dsimp only
  have he (i : Fin n) : (x i / a) ^ b = exp (-(b * log a)) * x i ^ b := by
    rw [rpow_def_of_pos (div_pos (hx i) ha), rpow_def_of_pos (hx i), ← exp_add,
      log_div (hx i).ne' ha.ne']
    congr 1
    ring
  have hl (i : Fin n) : log (exp (-(b * log a)) * x i ^ b) =
      b * log (x i / a) := by
    rw [← he i, log_rpow (div_pos (hx i) ha)]
  unfold logLik Weibull.logLik negLogQSum residualSum
  simp only [Weibull.logDensity, he, hl, logQ, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    Finset.sum_neg_distrib, ← Finset.mul_sum]
  have heq : (∑ i, log (x i / a)) = (∑ i, log (x i)) - (n : ℝ) * log a := by
    simp [log_div (hx _).ne' ha.ne', Finset.sum_sub_distrib, nsmul_eq_mul]
  rw [heq]
  ring

private theorem heldShape_profile_le {r : Fin n → ℝ} {t c : ℝ} (hc : 0 < c)
    (hH : 0 < negLogQSum r t) (hn : 0 < (n : ℝ)) :
    (n : ℝ) * log c - c * negLogQSum r t + residualSum r t ≤ heldShapeProfile r t := by
  let c₀ := (n : ℝ) / negLogQSum r t
  have hc₀ : 0 < c₀ := div_pos hn hH
  have hprod : c₀ * negLogQSum r t = n := div_mul_cancel₀ _ hH.ne'
  have h := log_le_sub_one_of_pos (div_pos hc hc₀)
  rw [log_div hc.ne' hc₀.ne'] at h
  have hh := mul_le_mul_of_nonneg_left h hn.le
  have he : (n : ℝ) * (c / c₀ - 1) = c * negLogQSum r t - n := by
    rw [← hprod]
    field_simp
  rw [he] at hh
  unfold heldShapeProfile
  change (n : ℝ) * log c - c * negLogQSum r t + residualSum r t ≤
    (n : ℝ) * log c₀ - n + residualSum r t
  linarith

theorem held_shape_attains (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p := by
  let r : Fin n → ℝ := fun i ↦ x i ^ b
  have hr (i : Fin n) : 0 < r i := rpow_pos_of_pos (hx i) _
  have hvar : ∃ i j, r i ≠ r j := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij ((strictMonoOn_rpow_Ici_of_exponent_pos hb).injOn
      (show x i ∈ Ici 0 from (hx i).le) (show x j ∈ Ici 0 from (hx j).le) h)⟩
  obtain ⟨t, ht, hmax⟩ := heldShapeProfile_max hr hvar
  let a := exp (-log t / b)
  have ha : 0 < a := exp_pos _
  have ht' : exp (-(b * log a)) = t := by
    dsimp [a]
    rw [log_exp, show -(b * (-log t / b)) = log t by field_simp, exp_log ht]
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let i : Fin n := ⟨0, hn⟩
  have hH : 0 < negLogQSum r t := negLogQSum_pos hr ht i
  let c := (n : ℝ) / negLogQSum r t
  have hc : 0 < c := div_pos hN hH
  have hEq : logLik x b a c = (n : ℝ) * log b - (∑ i, log (x i)) +
      heldShapeProfile r t := by
    rw [heldShape_lik hx b c ha]
    rw [ht']
    change _ + ((n : ℝ) * log c - c * negLogQSum r t + residualSum r t) = _
    rw [show c * negLogQSum r t = n from div_mul_cancel₀ _ hH.ne']
    rfl
  refine ⟨(a, c), ⟨ha, hc⟩, fun p hp ↦ ?_⟩
  have hap : 0 < p.1 := hp.1
  have hcp : 0 < p.2 := hp.2
  change logLik x b p.1 p.2 ≤ logLik x b a c
  rw [hEq, heldShape_lik hx b p.2 hap]
  apply add_le_add_right
  exact (heldShape_profile_le hcp (negLogQSum_pos hr (exp_pos _) i) hN).trans
    (hmax (show exp (-(b * log p.1)) ∈ Ioi 0 from exp_pos _))


open Filter
open scoped Topology

private def logDataDensity (b c z : ℝ) : ℝ :=
  log b + log c + logKernel c z

private theorem logLik_logData {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (b c μ : ℝ) (hb : 0 < b) :
    logLik x b (exp μ) c =
      ∑ i, (logDataDensity b c (b * (log (x i) - μ)) - log (x i)) := by
  rw [physical_affine hx c (b, exp μ) ⟨hb, exp_pos _⟩]
  have hs (i : Fin n) : b * log (x i) + -(b * μ) = b * (log (x i) - μ) := by ring
  simp only [affineLik, toAffine, log_exp, hs, logDataDensity, Finset.sum_sub_distrib,
    Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

private theorem logQ_error_tendsto {z : ℝ → ℝ}
    (hz : Tendsto (fun t ↦ exp (z t)) atTop (𝓝 0)) :
    Tendsto (fun t ↦ logQ (exp (z t)) - z t) atTop (𝓝 0) := by
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le hz.neg tendsto_const_nhds
  · intro t
    have h := (logQ_bounds (exp_pos (z t))).1
    rw [log_exp] at h
    linarith
  · intro t
    have h := (logQ_bounds (exp_pos (z t))).2.1
    rw [log_exp] at h
    linarith

private theorem power_point_limit {k y m : ℝ} (hk : 0 < k) (hy : y ≤ m) :
    Tendsto (fun t : ℝ ↦ logDataDensity (t ^ 2) (k / t ^ 2)
      (t ^ 2 * (y - (m + t⁻¹)))) atTop (𝓝 (log k + k * (y - m))) := by
  let z := fun t : ℝ ↦ t ^ 2 * (y - (m + t⁻¹))
  have hz : Tendsto z atTop atBot := by
    apply tendsto_atBot_mono' atTop _ tendsto_neg_atTop_atBot
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have hzt : z t = t ^ 2 * (y - m) - t := by
      dsimp [z]
      field_simp; ring
    rw [hzt]
    linarith [mul_nonpos_of_nonneg_of_nonpos (sq_nonneg t) (sub_nonpos.mpr hy)]
  have hu := tendsto_exp_atBot.comp hz
  have he := logQ_error_tendsto hu
  have hi : Tendsto (fun t : ℝ ↦ t⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero
  have hc : Tendsto (fun t : ℝ ↦ k / t ^ 2 - 1) atTop (𝓝 (-1)) := by
    simpa only [div_eq_mul_inv, inv_pow, zero_pow (by decide : (2 : ℕ) ≠ 0),
      mul_zero, zero_sub] using ((hi.pow 2).const_mul k).sub_const 1
  have hlin : Tendsto (fun t : ℝ ↦ k * (y - m) - k * t⁻¹) atTop
      (𝓝 (k * (y - m))) := by
    simpa using (hi.const_mul k).const_sub (k * (y - m))
  have h := (((hlin.const_add (log k)).sub hu).add (hc.mul he))
  simp only [sub_zero, mul_zero, add_zero] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  have hpos : 0 < t ^ 2 := sq_pos_of_pos ht
  have hcz : (k / t ^ 2) * z t = k * (y - m) - k * t⁻¹ := by
    dsimp [z]
    field_simp; ring
  have hlogs : log (t ^ 2) + log (k / t ^ 2) = log k := by
    rw [log_div hk.ne' hpos.ne']
    ring
  unfold logDataDensity logKernel logQ
  change log k + (k * (y - m) - k * t⁻¹) - exp (z t) +
      (k / t ^ 2 - 1) * (log (1 - exp (-exp (z t))) - z t) =
    log (t ^ 2) + log (k / t ^ 2) + (z t - exp (z t) +
      (k / t ^ 2 - 1) * log (1 - exp (-exp (z t))))
  rw [hlogs, ← hcz]
  ring

theorem power_face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k m : ℝ} (hk : 0 < k) (hm : ∀ i, log (x i) ≤ m) :
    Tendsto (fun t : ℝ ↦ logLik x (t ^ 2) (exp (m + t⁻¹)) (k / t ^ 2)) atTop
      (𝓝 (∑ i, (log k + k * (log (x i) - m) - log (x i)))) := by
  have h := tendsto_finsetSum Finset.univ fun i _ ↦
    (power_point_limit hk (hm i)).sub_const (log (x i))
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  exact (logLik_logData hx _ _ _ (sq_pos_of_pos ht)).symm


private theorem inverse_u_correction (w : ℝ) :
    Tendsto (fun t : ℝ ↦ t * exp (w / t) - t) atTop (𝓝 w) := by
  have hi : Tendsto (fun t : ℝ ↦ t⁻¹) atTop (𝓝[≠] (0 : ℝ)) :=
    tendsto_nhdsWithin_iff.2 ⟨tendsto_inv_atTop_zero, by
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
      exact inv_ne_zero ht.ne'⟩
  have hd : HasDerivAt (fun v : ℝ ↦ exp (w * v)) w 0 := by
    have h := (hasDerivAt_exp (w * 0)).comp 0 ((hasDerivAt_id (0 : ℝ)).const_mul w)
    simpa only [Function.comp_def, id_eq, mul_zero, exp_zero, mul_one, one_mul] using h
  refine (hd.tendsto_slope_zero.comp hi).congr' ?_
  exact Eventually.of_forall fun t ↦ by
    simp only [Function.comp_def, zero_add, mul_zero, exp_zero, inv_inv, smul_eq_mul, div_eq_mul_inv]
    ring

private theorem log_sub_ratio {v : ℝ → ℝ} (hv : Tendsto v atTop (𝓝 0))
    (hpos : ∀ᶠ t in atTop, 0 < v t) :
    Tendsto (fun t ↦ log (1 - v t) / v t) atTop (𝓝 (-1)) := by
  have hnear : Tendsto v atTop (𝓝[≠] (0 : ℝ)) :=
    tendsto_nhdsWithin_iff.2 ⟨hv, hpos.mono fun _ ht ↦ ht.ne'⟩
  have hd : HasDerivAt (fun q : ℝ ↦ log (1 - q)) (-1) 0 := by
    simpa only [one_div, div_one, sub_zero, inv_one, neg_mul, one_mul, id_eq] using
      (((hasDerivAt_id (0 : ℝ)).const_sub 1).log (by norm_num : (1 : ℝ) - 0 ≠ 0))
  refine (hd.tendsto_slope_zero.comp hnear).congr' (Eventually.of_forall fun _ ↦ ?_)
  simp only [Function.comp_def, zero_add, sub_zero, log_one, smul_eq_mul, div_eq_mul_inv, mul_comm]

private theorem inverse_exponent_limit (w : ℝ) :
    Tendsto (fun t : ℝ ↦ (exp t - 1) * logQ (t * exp (w / t))) atTop
      (𝓝 (-exp (-w))) := by
  let u := fun t : ℝ ↦ t * exp (w / t)
  let v := fun t : ℝ ↦ exp (-u t)
  have hu : Tendsto (fun t ↦ u t - t) atTop (𝓝 w) := inverse_u_correction w
  have he : Tendsto (fun t : ℝ ↦ exp (-(u t - t))) atTop (𝓝 (exp (-w))) :=
    (continuous_exp.tendsto _).comp hu.neg
  have hzero : Tendsto (fun t : ℝ ↦ exp (-t)) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp tendsto_neg_atTop_atBot
  have hv : Tendsto v atTop (𝓝 0) := by
    simpa only [mul_zero] using (he.mul hzero).congr' (Eventually.of_forall fun t ↦ by
      change exp (-(u t - t)) * exp (-t) = exp (-u t)
      rw [← exp_add]
      congr 1
      ring)
  have hr := log_sub_ratio hv (Eventually.of_forall fun t ↦ exp_pos (-u t))
  have h := (he.sub hv).mul hr
  simp only [sub_zero, mul_neg, mul_one] at h
  refine h.congr' (Eventually.of_forall fun t ↦ ?_)
  have hv' : v t ≠ 0 := (exp_pos _).ne'
  have hprod : exp (-(u t - t)) = exp t * v t := by
    dsimp [v]
    rw [← exp_add]
    congr 1
    ring
  change (exp (-(u t - t)) - v t) * (log (1 - v t) / v t) =
    (exp t - 1) * log (1 - v t)
  rw [hprod]
  field_simp

private theorem inverse_point_limit {k y μ : ℝ} (hk : 0 < k) :
    Tendsto (fun t : ℝ ↦ logDataDensity (k / t) (exp t)
      ((k / t) * (y - (μ - log t / (k / t)))) ) atTop
      (𝓝 (log k - k * (y - μ) - exp (-(k * (y - μ))))) := by
  let w := k * (y - μ)
  have hw : Tendsto (fun t : ℝ ↦ w / t) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv, mul_zero] using tendsto_inv_atTop_zero.const_mul w
  have he := inverse_exponent_limit w
  have hd := inverse_u_correction w
  have h := ((hw.const_add (log k)).sub hd).add he
  simp only [add_zero, ← sub_eq_add_neg] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  have hz : k / t * (y - (μ - log t / (k / t))) = log t + w / t := by
    dsimp [w]
    field_simp
    ring
  have hu : exp (log t + w / t) = t * exp (w / t) := by rw [exp_add, exp_log ht]
  unfold logDataDensity logKernel logQ
  rw [hz, hu, log_div hk.ne' ht.ne', log_exp]
  ring

theorem inverse_weibull_face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) (μ : ℝ) :
    Tendsto (fun t : ℝ ↦ logLik x (k / t) (exp (μ - log t / (k / t))) (exp t)) atTop
      (𝓝 (Weibull.Inverse.logLik x k (exp μ))) := by
  have h := tendsto_finsetSum Finset.univ fun i _ ↦
    (inverse_point_limit (y := log (x i)) (μ := μ) hk).sub_const (log (x i))
  have hval : (∑ i, (log k - k * (log (x i) - μ) -
      exp (-(k * (log (x i) - μ))) - log (x i))) = Weibull.Inverse.logLik x k (exp μ) := by
    unfold Weibull.Inverse.logLik
    apply Finset.sum_congr rfl
    intro i _
    rw [Weibull.Inverse.logDensity, log_exp,
      rpow_def_of_pos (div_pos (hx i) (exp_pos μ)), log_div (hx i).ne' (exp_pos μ).ne',
      log_exp, mul_neg]
    ring_nf
  rw [hval] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  exact (logLik_logData hx _ _ _ (div_pos hk ht)).symm


/-- The best closed-support power value is attained within the face family
at the largest log observation and approached by one common EW path. -/
theorem optimized_power_face {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {m : ℝ} (hm : ∀ i, log (x i) ≤ m)
    (hcontact : ∃ i, log (x i) = m) :
    let D := ∑ i, (m - log (x i))
    let k := (n : ℝ) / D
    let P := (n : ℝ) * (log k - 1) - ∑ i, log (x i)
    0 < k ∧
      (∀ a l : ℝ, (∀ i, log (x i) ≤ a) → 0 < l →
        (∑ i, (log l + l * (log (x i) - a) - log (x i))) ≤ P) ∧
      Tendsto (fun t : ℝ ↦ logLik x (t ^ 2) (exp (m + t⁻¹)) (k / t ^ 2)) atTop (𝓝 P) := by
  dsimp only
  let D := ∑ i, (m - log (x i))
  obtain ⟨i, j, hij⟩ := hne
  have hN : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt i.isLt)
  have hD : 0 < D := by
    have h : ∃ j, 0 < m - log (x j) := by
      by_contra! h
      have hi := le_antisymm (hm i) (by linarith [h i] : m ≤ log (x i))
      have hj := le_antisymm (hm j) (by linarith [h j] : m ≤ log (x j))
      exact hij (log_injOn_pos (hx i) (hx j) (hi.trans hj.symm))
    obtain ⟨l, hl⟩ := h
    exact hl.trans_le (Finset.single_le_sum (fun q _ ↦ sub_nonneg.mpr (hm q)) (Finset.mem_univ l))
  let k := (n : ℝ) / D
  have hk : 0 < k := div_pos hN hD
  have hkD : k * D = n := div_mul_cancel₀ _ hD.ne'
  have hsum (l a : ℝ) :
      (∑ i, (log l + l * (log (x i) - a) - log (x i))) =
      (n : ℝ) * log l - l * (∑ i, (a - log (x i))) - ∑ i, log (x i) := by
    simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum, mul_sub]
    ring
  refine ⟨hk, ?_, ?_⟩
  · intro a l ha hl
    have hma : m ≤ a := by
      obtain ⟨z, hz⟩ := hcontact
      rw [← hz]
      exact ha z
    have hDa : D ≤ ∑ i, (a - log (x i)) :=
      Finset.sum_le_sum (fun q _ ↦ sub_le_sub_right hma _)
    have he := log_le_sub_one_of_pos (div_pos hl hk)
    rw [log_div hl.ne' hk.ne'] at he
    have he' := mul_le_mul_of_nonneg_left he hN.le
    have heq : (n : ℝ) * (l / k - 1) = l * D - n := by
      rw [← hkD]
      field_simp
    rw [heq] at he'
    rw [hsum]
    change _ ≤ (n : ℝ) * (log k - 1) - ∑ q, log (x q)
    nlinarith only [he', mul_le_mul_of_nonneg_left hDa hl.le]
  · have h := power_face_path hx hk hm
    rw [hsum, ← show D = ∑ i, (m - log (x i)) from rfl, hkD] at h
    simpa only [mul_sub, mul_one] using h

/-- A common EW path reaches the maximum over the inverse Weibull face. -/
theorem optimized_inverse_weibull_face (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      Tendsto (fun t : ℝ ↦ logLik x (p.1 / t)
        (exp (log p.2 - log t / (p.1 / t))) (exp t)) atTop
        (𝓝 (Weibull.Inverse.logLik x p.1 p.2)) := by
  obtain ⟨p, hp, _⟩ := Weibull.Inverse.jointMLE hn hx hne
  have hs : 0 < p.2 := hp.1.2
  refine ⟨p, hp.1, hp.2, ?_⟩
  simpa only [exp_log hs] using inverse_weibull_face_path hx hp.1.1 (log p.2)

end
end UnivariateML.ShapeFacesExpWeibull
