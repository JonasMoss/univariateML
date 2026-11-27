import UnivariateML.Family.Weibull
import UnivariateML.Generic.Score
import UnivariateML.Generic.Escape
import UnivariateML.Family.Transfers

/-! Power generalized Weibull.

## Deviations

The affine chart and the scalar results work for every positive exponent.
The two-face comparison for free exponent and scale is outside these results.
-/

open Real Set Filter
namespace UnivariateML.ShapeFacesPGW
noncomputable section

variable {n : ℕ}

def logDensity (b a c x : ℝ) : ℝ :=
  log b + log c - log x + b * log (x / a) +
    (c - 1) * log (1 + (x / a) ^ b) - ((1 + (x / a) ^ b) ^ c - 1)

def logLik (x : Fin n → ℝ) (b a c : ℝ) : ℝ := ∑ i, logDensity b a c (x i)

theorem exponent_one_logLik {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (b : ℝ) {a : ℝ} (ha : 0 < a) : logLik x b a 1 = Weibull.logLik x b a := by
  unfold logLik Weibull.logLik
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Weibull.logDensity, log_one, add_zero, sub_self, zero_mul, rpow_one]
  rw [log_div (hx i).ne' ha.ne']
  ring

theorem exponent_one_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2 1) Weibull.domain p := by
  obtain ⟨p, hp, hu⟩ := Weibull.jointMLE hn hx hne
  have he (q : ℝ × ℝ) (hq : q ∈ Weibull.domain) :=
    exponent_one_logLik hx q.1 hq.2
  refine ⟨p, ⟨hp.1, ?_⟩, ?_⟩
  · intro q hq
    change logLik x q.1 q.2 1 ≤ logLik x p.1 p.2 1
    rw [he q hq, he p hp.1]
    exact hp.2 hq
  · rintro q ⟨hq, hm⟩
    apply hu q
    refine ⟨hq, fun r hr ↦ ?_⟩
    change Weibull.logLik x r.1 r.2 ≤ Weibull.logLik x q.1 q.2
    rw [← he r hr, ← he q hq]
    exact hm hr

theorem held_exponent_constant_unbounded (hn : 0 < n) {t c : ℝ} (ht : 0 < t) (_hc : 0 < c) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) b t c) atTop atTop := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have he (b : ℝ) :
      logLik (fun _ : Fin n ↦ t) b t c = (n : ℝ) * log b +
        (n : ℝ) * (log c - log t + (c - 1) * log 2 - ((2 : ℝ) ^ c - 1)) := by
    simp [logLik, logDensity, ht.ne']
    ring_nf
  simp only [he]
  exact (tendsto_atTop_add_const_right _ _ (tendsto_log_atTop.const_mul_atTop hn'))

private def exponentScore (L : Fin n → ℝ) (c : ℝ) : ℝ :=
  (n : ℝ) / c + ∑ i, L i * (1 - exp (c * L i))

set_option maxHeartbeats 400000 in
theorem held_scale_shape_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b a : ℝ} (_hb : 0 < b) (ha : 0 < a) :
    ∃! c, c ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x b a) (Ioi 0) c := by
  let L : Fin n → ℝ := fun i ↦ log (1 + (x i / a) ^ b)
  have hL (i : Fin n) : 0 < L i :=
    log_pos (by linarith [rpow_pos_of_pos (div_pos (hx i) ha) b])
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hd (c : ℝ) (hc : c ∈ Ioi (0 : ℝ)) :
      HasDerivAt (logLik x b a) (exponentScore L c) c := by
    have hform : logLik x b a = fun t ↦ (n : ℝ) * log t +
        ∑ i, (log b - log (x i) + b * log (x i / a) +
          (t - 1) * L i - (exp (t * L i) - 1)) := by
      funext t
      unfold logLik
      have hsum : (∑ _ : Fin n, log t) = (n : ℝ) * log t := by simp [nsmul_eq_mul]
      rw [← hsum]
      rw [← Finset.sum_add_distrib]
      apply Finset.sum_congr rfl
      intro i _
      have hp : 0 < 1 + (x i / a) ^ b :=
        add_pos zero_lt_one (rpow_pos_of_pos (div_pos (hx i) ha) b)
      simp only [logDensity, rpow_def_of_pos hp, L]
      rw [mul_comm t]
      ring
    rw [hform, exponentScore]
    have ht (i : Fin n) :
        HasDerivAt (fun t ↦ log b - log (x i) + b * log (x i / a) +
          (t - 1) * L i - (exp (t * L i) - 1)) (L i * (1 - exp (c * L i))) c := by
      convert ((hasDerivAt_const c (log b - log (x i) + b * log (x i / a))).add
        (((hasDerivAt_id c).sub_const 1).mul_const (L i))).sub
          ((((hasDerivAt_id c).mul_const (L i)).exp).sub_const 1) using 1
      · funext t
        simp only [Pi.sub_apply, Pi.add_apply, id_eq]
      · simp only [id_eq]
        ring
    convert ((hasDerivAt_log hc.ne').const_mul (n : ℝ)).add
      (HasDerivAt.fun_sum (fun i _ ↦ ht i)) using 1
    simp [div_eq_mul_inv]
  have hanti : StrictAntiOn (exponentScore L) (Ioi 0) := by
    intro s hs t ht hst
    apply add_lt_add_of_lt_of_le (div_lt_div_of_pos_left hN hs hst)
    apply Finset.sum_le_sum
    intro i _
    apply mul_le_mul_of_nonneg_left _ (hL i).le
    exact sub_le_sub_left (exp_le_exp.mpr (mul_le_mul_of_nonneg_right hst.le (hL i).le)) _
  have hcont : ContinuousOn (exponentScore L) (Ioi 0) := by
    unfold exponentScore
    have hterm (i : Fin n) :
        Continuous (fun c : ℝ ↦ L i * (1 - exp (c * L i))) := by fun_prop
    have hsum : Continuous (fun c : ℝ ↦ ∑ i, L i * (1 - exp (c * L i))) :=
      continuous_finsetSum Finset.univ (fun i _ ↦ hterm i)
    exact (continuousOn_const.div continuousOn_id (fun _ ht ↦ ne_of_gt ht)).add hsum.continuousOn
  let A := ∑ i, L i * exp (L i)
  have hA : 0 < A := Finset.sum_pos (fun i _ ↦ mul_pos (hL i) (exp_pos _))
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let s := min 1 ((n : ℝ) / (2 * A))
  have hs : 0 < s := lt_min zero_lt_one (by positivity)
  have hsa : 0 < exponentScore L s := by
    have hfrac : 2 * A ≤ (n : ℝ) / s := by
      apply (le_div_iff₀ hs).2
      have h := (le_div_iff₀ (show 0 < 2 * A by positivity)).1 (min_le_right 1 ((n : ℝ) / (2 * A)))
      linarith
    have hsum : -A ≤ ∑ i, L i * (1 - exp (s * L i)) := by
      dsimp [A]
      rw [← Finset.sum_neg_distrib]
      apply Finset.sum_le_sum
      intro i _
      have he : exp (s * L i) ≤ exp (L i) :=
        exp_le_exp.mpr (mul_le_of_le_one_left (hL i).le (min_le_left _ _))
      nlinarith [mul_le_mul_of_nonneg_left he (hL i).le, hL i]
    unfold exponentScore
    linarith
  let B := ∑ i, L i ^ 2
  have hB : 0 < B := Finset.sum_pos (fun i _ ↦ pow_pos (hL i) _)
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let t := (n : ℝ) / B + 2
  have ht : 1 < t := by dsimp [t]; linarith [div_pos hN hB]
  have hsb : exponentScore L t < 0 := by
    have hfrac : (n : ℝ) / t ≤ n := (div_le_self hN.le ht.le)
    have hsum : (∑ i, L i * (1 - exp (t * L i))) ≤ -(t * B) := by
      dsimp [B]
      rw [Finset.mul_sum, ← Finset.sum_neg_distrib]
      apply Finset.sum_le_sum
      intro i _
      have he := add_one_le_exp (t * L i)
      nlinarith [mul_le_mul_of_nonneg_left he (hL i).le]
    have hmul : t * B = (n : ℝ) + 2 * B := by dsimp [t]; field_simp
    unfold exponentScore
    linarith
  obtain ⟨⟨r, hr, hu⟩, hm⟩ := Generic.existsUnique_score_zero_of_signs
    (convex_Ioi (0 : ℝ)) hd hcont hanti (show s ∈ Ioi 0 from hs)
    (show t ∈ Ioi 0 from zero_lt_one.trans ht) ((min_le_left _ _).trans ht.le) hsa hsb
  exact ⟨r, ⟨hr.1, (hm r hr.1).2 hr.2⟩, fun q hq ↦ hu q ⟨hq.1, (hm q hq.1).1 hq.2⟩⟩


private def softplus (z : ℝ) : ℝ := log (1 + exp z)

private def logistic (z : ℝ) : ℝ := exp z / (1 + exp z)

private theorem softplus_pos (z : ℝ) : 0 < softplus z :=
  log_pos (by linarith [exp_pos z])

private theorem le_softplus (z : ℝ) : z ≤ softplus z := by
  conv_lhs => rw [← log_exp z]
  exact log_le_log (exp_pos z) (by linarith)

private theorem softplus_deriv (z : ℝ) : HasDerivAt softplus (logistic z) z :=
  ((hasDerivAt_exp z).const_add 1).log (by positivity)

private theorem logistic_deriv (z : ℝ) :
    HasDerivAt logistic (exp z / (1 + exp z) ^ 2) z := by
  convert (hasDerivAt_exp z).div ((hasDerivAt_exp z).const_add 1) (by positivity) using 1
  · rfl
  · ring_nf

private def logKernel (c z : ℝ) : ℝ :=
  z + (c - 1) * softplus z - exp (c * softplus z)

private def kernelSlope (c z : ℝ) : ℝ :=
  1 + (c - 1 - c * exp (c * softplus z)) * logistic z

private theorem kernel_deriv (c z : ℝ) :
    HasDerivAt (logKernel c) (kernelSlope c z) z := by
  have h := ((hasDerivAt_id z).add ((softplus_deriv z).const_mul (c - 1))).sub
    (((softplus_deriv z).const_mul c).exp)
  convert h using 1
  · rfl
  · unfold kernelSlope
    ring

private theorem kernel_strictConcave {c : ℝ} (hc : 0 < c) :
    StrictConcaveOn ℝ univ (logKernel c) := by
  apply strictConcaveOn_of_deriv2_neg' convex_univ
    (fun z _ ↦ (kernel_deriv c z).continuousAt.continuousWithinAt)
  intro z _
  have h : deriv (logKernel c) = kernelSlope c := funext fun z ↦ (kernel_deriv c z).deriv
  have hA := ((((softplus_deriv z).const_mul c).exp).const_mul c).const_sub (c - 1)
  have hd := ((hA.mul (logistic_deriv z)).const_add 1)
  have hdd : HasDerivAt (kernelSlope c)
      (-(c * (exp (c * softplus z) * (c * logistic z))) * logistic z +
        (c - 1 - c * exp (c * softplus z)) * (exp z / (1 + exp z) ^ 2)) z := hd
  change deriv (deriv (logKernel c)) z < 0
  rw [h, hdd.deriv]
  have hAneg : c - 1 - c * exp (c * softplus z) < 0 := by
    have he : 1 ≤ exp (c * softplus z) :=
      one_le_exp_iff.mpr (mul_nonneg hc.le (softplus_pos _).le)
    linarith [mul_le_mul_of_nonneg_left he hc.le]
  have hq : 0 < logistic z := by unfold logistic; positivity
  exact add_neg_of_nonpos_of_neg
    (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr (by positivity)) hq.le)
    (mul_neg_of_neg_of_pos hAneg (by positivity))

private theorem logKernel_bound {c : ℝ} (hc : 0 < c) (z : ℝ) :
    logKernel c z ≤ -(min 1 c * |z|) := by
  have hL := le_softplus z
  have hLpos := softplus_pos z
  have hExp := add_one_le_exp (c * softplus z)
  rcases le_total 0 z with hz | hz
  · have ha : 2 * (c * softplus z) ≤ exp (c * softplus z) := by
      let u := c * softplus z
      have hu : 0 ≤ u := mul_nonneg hc.le hLpos.le
      have he : exp 1 * exp (u - 1) = exp u := by
        rw [← exp_add, show 1 + (u - 1) = u by ring]
      have h := add_one_le_exp (u - 1)
      calc
        2 * u ≤ exp 1 * u := mul_le_mul_of_nonneg_right
          (by linarith [add_one_le_exp (1 : ℝ)]) hu
        _ ≤ exp u := by rw [← he]; exact mul_le_mul_of_nonneg_left (by linarith) (exp_pos _).le
    rw [abs_of_nonneg hz]
    unfold logKernel
    nlinarith only [ha, hL, mul_le_mul_of_nonneg_left hL hc.le,
      mul_le_mul_of_nonneg_right (min_le_right 1 c) hz]
  · rw [abs_of_nonpos hz]
    unfold logKernel
    nlinarith only [hExp, hLpos, mul_le_mul_of_nonpos_right (min_le_left 1 c) hz]

def affineDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

def affineLik (y : Fin n → ℝ) (c : ℝ) (p : ℝ × ℝ) : ℝ :=
  (n : ℝ) * log p.1 + ∑ i, logKernel c (p.1 * y i + p.2)

private theorem blend (a b : ℝ) (p q : ℝ × ℝ) (y : ℝ) :
    (a • p + b • q).1 * y + (a • p + b • q).2 =
      a * (p.1 * y + p.2) + b * (q.1 * y + q.2) := by
  change (a * p.1 + b * q.1) * y + (a * p.2 + b * q.2) = _
  ring

theorem affine_strictConcave (hn : 0 < n) {c : ℝ} (hc : 0 < c) (y : Fin n → ℝ) :
    StrictConcaveOn ℝ affineDomain (affineLik y c) := by
  refine ⟨(convex_Ioi _).prod convex_univ, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  let K (r : ℝ × ℝ) (i : Fin n) := logKernel c (r.1 * y i + r.2)
  have hterm (i : Fin n) : a * K p i + b * K q i ≤ K (a • p + b • q) i := by
    unfold K
    rw [blend]
    exact (kernel_strictConcave hc).concaveOn.2 (mem_univ _) (mem_univ _) ha.le hb.le hab
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hweight : (∑ i, (a * K p i + b * K q i)) =
      a * (∑ i, K p i) + b * (∑ i, K q i) := by
    simp only [Finset.sum_add_distrib, ← Finset.mul_sum]
  change a * ((n : ℝ) * log p.1 + ∑ i, K p i) +
      b * ((n : ℝ) * log q.1 + ∑ i, K q i) <
      (n : ℝ) * log (a * p.1 + b * q.1) + ∑ i, K (a • p + b • q) i
  by_cases h : p.1 = q.1
  · let i : Fin n := ⟨0, hn⟩
    have hi : p.1 * y i + p.2 ≠ q.1 * y i + q.2 := by
      intro h'
      exact hpq (Prod.ext h (by rw [h] at h'; linarith))
    have hstrict : a * K p i + b * K q i < K (a • p + b • q) i := by
      unfold K
      rw [blend]
      exact (kernel_strictConcave hc).2 (mem_univ _) (mem_univ _) hi ha hb hab
    have hs := Finset.sum_lt_sum (fun j (_ : j ∈ Finset.univ) ↦ hterm j)
      ⟨i, Finset.mem_univ _, hstrict⟩
    rw [hweight] at hs
    have hlog := strictConcaveOn_log_Ioi.concaveOn.2 hp.1 hq.1 ha.le hb.le hab
    change a * log p.1 + b * log q.1 ≤ log (a * p.1 + b * q.1) at hlog
    nlinarith only [hs, mul_le_mul_of_nonneg_left hlog hN.le]
  · have hs := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ hterm i)
    rw [hweight] at hs
    have hlog := strictConcaveOn_log_Ioi.2 hp.1 hq.1 h ha hb hab
    change a * log p.1 + b * log q.1 < log (a * p.1 + b * q.1) at hlog
    nlinarith only [hs, mul_lt_mul_of_pos_left hlog hN]

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
    (affine_strictConcave hn hc y) (affine_coercive hn hc hne)


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
        ((n : ℝ) * (log c + 1) - ∑ i, log (x i)) := by
  have hs : 0 < p.2 := hp.2
  have hd (i : Fin n) :
      logDensity p.1 p.2 c (x i) = log p.1 +
        logKernel c (p.1 * log (x i) + -(p.1 * log p.2)) + (log c + 1) - log (x i) := by
    have hr : (x i / p.2) ^ p.1 = exp (p.1 * log (x i) + -(p.1 * log p.2)) := by
      rw [rpow_def_of_pos (div_pos (hx i) hs), log_div (hx i).ne' hs.ne']
      congr 1
      ring
    unfold logDensity logKernel softplus
    rw [hr, log_div (hx i).ne' hs.ne', rpow_def_of_pos (by positivity), mul_comm c]
    ring_nf
  simp only [logLik, hd, affineLik, toAffine, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
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


private theorem scalar_strictConcave (hn : 0 < n) {c : ℝ} (hc : 0 < c) (y : Fin n → ℝ)
    {S : Set ℝ} (hS : Convex ℝ S) (φ : ℝ → ℝ × ℝ) (hinj : Function.Injective φ)
    (hmem : MapsTo φ S affineDomain)
    (hblend : ∀ a b s t : ℝ, a + b = 1 → φ (a * s + b * t) = a • φ s + b • φ t) :
    StrictConcaveOn ℝ S (fun t ↦ affineLik y c (φ t)) := by
  refine ⟨hS, ?_⟩
  intro s hs t ht hst a b ha hb hab
  change a * affineLik y c (φ s) + b * affineLik y c (φ t) <
    affineLik y c (φ (a * s + b * t))
  rw [hblend a b s t hab]
  exact (affine_strictConcave hn hc y).2 (hmem hs) (hmem ht)
    (fun he ↦ hst (hinj he)) ha hb hab

private theorem scalar_max_on_interval {S : Set ℝ} (hopen : IsOpen S)
    {f : ℝ → ℝ} (hconc : StrictConcaveOn ℝ S f) {a b w : ℝ}
    (hw : w ∈ Icc a b) (hsub : Icc a b ⊆ S)
    (hlevel : ∀ t ∈ S, f w ≤ f t → t ∈ Icc a b) :
    ∃! t, t ∈ S ∧ IsMaxOn f S t := by
  obtain ⟨t, ht, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨w, hw⟩
    ((hconc.concaveOn.continuousOn hopen).mono hsub)
  have hm : IsMaxOn f S t := by
    intro r hr
    by_cases h : f w ≤ f r
    · exact hmax (hlevel r hr h)
    · exact (not_le.mp h).le.trans (hmax hw)
  exact ⟨t, ⟨hsub ht, hm⟩, fun r hr ↦ hconc.eq_of_isMaxOn hr.2 hm hr.1 (hsub ht)⟩

private theorem affine_held_shape_mle (hn : 0 < n) {b c : ℝ} (hb : 0 < b) (hc : 0 < c)
    (y : Fin n → ℝ) :
    ∃! v, v ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun t ↦ affineLik y c (b, t)) univ v := by
  let f := fun t : ℝ ↦ affineLik y c (b, t)
  let i : Fin n := ⟨0, hn⟩
  let m := min 1 c
  have hm : 0 < m := lt_min zero_lt_one hc
  let B := ((n : ℝ) * log b - f 0) / m + |b * y i|
  have hlevel (t : ℝ) (ht : f 0 ≤ f t) : t ∈ Icc (-B) B := by
    have hbound := affine_point_bound hc y (b, t) i
    change m * |b * y i + t| ≤ (n : ℝ) * log b - f t at hbound
    have hz : |b * y i + t| ≤ ((n : ℝ) * log b - f 0) / m := by
      apply (le_div_iff₀ hm).2
      rw [mul_comm]
      linarith
    have h := abs_sub (b * y i + t) (b * y i)
    rw [add_sub_cancel_left] at h
    exact abs_le.mp (h.trans (add_le_add_left hz _))
  have hconc : StrictConcaveOn ℝ univ f := by
    apply scalar_strictConcave hn hc y convex_univ (fun t ↦ (b, t))
    · intro s t h
      exact congrArg (fun p : ℝ × ℝ ↦ p.2) h
    · exact fun t _ ↦ ⟨hb, mem_univ _⟩
    · intro a d s t had
      apply Prod.ext
      · change b = a * b + d * b
        nlinarith [congrArg (fun v : ℝ ↦ v * b) had]
      · rfl
  exact scalar_max_on_interval isOpen_univ hconc (hlevel 0 le_rfl)
    (subset_univ _) (fun t _ ht ↦ hlevel t ht)

private theorem scale_intercept_bij {b : ℝ} (hb : 0 < b) :
    BijOn (fun a : ℝ ↦ -(b * log a)) (Ioi 0) univ := by
  refine ⟨fun _ _ ↦ mem_univ _, ?_, ?_⟩
  · intro a ha d hd he
    exact log_injOn_pos ha hd (mul_left_cancel₀ hb.ne' (neg_injective he))
  · intro t _
    refine ⟨exp (-t / b), exp_pos _, ?_⟩
    change -(b * log (exp (-t / b))) = t
    rw [log_exp]
    field_simp

theorem held_exponent_shape_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b c : ℝ} (hb : 0 < b) (hc : 0 < c) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x b t c) (Ioi 0) a := by
  have he (a : ℝ) (ha : a ∈ Ioi (0 : ℝ)) :
      logLik x b a c = affineLik (fun i ↦ log (x i)) c (b, -(b * log a)) +
        ((n : ℝ) * (log c + 1) - ∑ i, log (x i)) :=
    physical_affine hx c (b, a) ⟨hb, ha⟩
  exact (Transfer.existsUnique_max_iff (scale_intercept_bij hb) he).2
    (affine_held_shape_mle hn hb hc (fun i ↦ log (x i)))

private theorem affine_held_scale_mle (hn : 0 < n) {c l : ℝ} (hc : 0 < c)
    (y : Fin n → ℝ) {i : Fin n} (hi : y i ≠ l) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ affineLik y c (t, -(t * l))) (Ioi 0) b := by
  let f := fun b : ℝ ↦ affineLik y c (b, -(b * l))
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let d := min 1 c * |y i - l| / (2 * n)
  have hd : 0 < d := div_pos (mul_pos (lt_min zero_lt_one hc) (abs_pos.mpr (sub_ne_zero.mpr hi)))
    (by positivity)
  let A := exp (f 1 / n)
  let B := ((-f 1 / n - log d - 1) / d)
  have hlevel (b : ℝ) (hb : 0 < b) (hM : f 1 ≤ f b) : b ∈ Icc A B := by
    have hbound := affine_point_bound hc y (b, -(b * l)) i
    change min 1 c * |b * y i + -(b * l)| ≤ (n : ℝ) * log b - f b at hbound
    rw [show b * y i + -(b * l) = b * (y i - l) by ring, abs_mul, abs_of_pos hb] at hbound
    have hlo : A ≤ b := by
      rw [← exp_log hb]
      apply exp_le_exp.mpr
      apply (div_le_iff₀ hN).2
      linarith [mul_nonneg (le_of_lt (lt_min zero_lt_one hc))
        (mul_nonneg hb.le (abs_nonneg (y i - l)))]
    have hlog := log_le_sub_one_of_pos (mul_pos hd hb)
    rw [log_mul hd.ne' hb.ne'] at hlog
    have hlin : 2 * (n : ℝ) * d * b = min 1 c * (b * |y i - l|) := by
      dsimp [d]
      field_simp
    have hhi : b ≤ B := by
      apply (le_div_iff₀ hd).2
      apply (le_of_mul_le_mul_left _ hN)
      have hf : (n : ℝ) * (-f 1 / n) = -f 1 := by field_simp
      nlinarith only [hM, hbound, hlin, hf, mul_le_mul_of_nonneg_left hlog hN.le]
    exact ⟨hlo, hhi⟩
  have hconc : StrictConcaveOn ℝ (Ioi 0) f := by
    apply scalar_strictConcave hn hc y (convex_Ioi _) (fun b ↦ (b, -(b * l)))
    · intro s t h
      exact congrArg (fun p : ℝ × ℝ ↦ p.1) h
    · exact fun t ht ↦ ⟨ht, mem_univ _⟩
    · intro a d s t _
      apply Prod.ext
      · rfl
      · change -((a * s + d * t) * l) = a * -(s * l) + d * -(t * l)
        ring
  exact scalar_max_on_interval isOpen_Ioi hconc (hlevel 1 zero_lt_one le_rfl)
    (fun _ hp ↦ (exp_pos _).trans_le hp.1) (fun t ht hM ↦ hlevel t ht hM)

theorem held_exponent_scale_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a c : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hne : ∃ i, x i ≠ a) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t a c) (Ioi 0) b := by
  obtain ⟨i, hi⟩ := hne
  have hlog : log (x i) ≠ log a := fun h ↦ hi (log_injOn_pos (hx i) ha h)
  have he (t : ℝ) (ht : t ∈ Ioi (0 : ℝ)) :
      logLik x t a c = affineLik (fun i ↦ log (x i)) c (t, -(t * log a)) +
        ((n : ℝ) * (log c + 1) - ∑ i, log (x i)) :=
    physical_affine hx c (t, a) ⟨ht, ha⟩
  exact (Transfer.existsUnique_max_iff (bijOn_id _) he).2
    (affine_held_scale_mle hn hc (fun i ↦ log (x i)) hlog)
theorem held_exponent_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (he : ∀ i, x i = a) :
    Tendsto (fun b ↦ logLik x b a c) atTop atTop := by
  have hx : x = fun _ ↦ a := funext he
  rw [hx]
  exact held_exponent_constant_unbounded hn ha hc


end
end UnivariateML.ShapeFacesPGW
