module

public import UnivariateML.Family.Shape2ExpWeibull
public import UnivariateML.Generic.Compactification

set_option linter.style.header false

/-! # Exponentiated Weibull in the exponent-profiled chart

Use the transformed observation at the sample minimum as the positive nuisance
coordinate. This gives bounds that apply when both shape and scale vary.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.EVExpWeibull

noncomputable section

variable {n : ℕ}

/-- Negative log of the Weibull cdf at positive transformed time. -/
def loss (t : ℝ) : ℝ := -log (1 - exp (-t))

/-- The log-density term left after collecting the exponent dependence. -/
def residue (t : ℝ) : ℝ := log t - t + loss t

private theorem cdf_pos {t : ℝ} (ht : 0 < t) : 0 < 1 - exp (-t) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht))

private theorem loss_bounds {t : ℝ} (ht : 0 < t) :
    exp (-t) ≤ loss t ∧ -log t ≤ loss t ∧
    loss t ≤ t - log t ∧ loss t ≤ exp (-t) / (1 - exp (-t)) := by
  have hq := cdf_pos ht
  unfold loss
  refine ⟨?_, ?_, ?_, ?_⟩
  · linarith [log_le_sub_one_of_pos hq]
  · have h := log_le_log hq (show 1 - exp (-t) ≤ t by linarith [add_one_le_exp (-t)])
    linarith
  · have he : t * exp (-t) ≤ 1 - exp (-t) := by
      have h := mul_le_mul_of_nonneg_right (add_one_le_exp t) (exp_pos (-t)).le
      rw [add_mul, one_mul, ← exp_add, add_neg_cancel, exp_zero] at h
      linarith
    have h := log_le_log (mul_pos ht (exp_pos _)) he
    rw [log_mul ht.ne' (exp_pos _).ne', log_exp] at h
    linarith
  · have h := log_le_sub_one_of_pos (inv_pos.mpr hq)
    rw [log_inv] at h
    exact h.trans_eq (by field_simp; ring)

private theorem residue_nonpos {t : ℝ} (ht : 0 < t) : residue t ≤ 0 := by
  unfold residue
  linarith [(loss_bounds ht).2.2.1]

private theorem loss_anti {s t : ℝ} (hs : 0 < s) (hle : s ≤ t) : loss t ≤ loss s := by
  unfold loss
  apply neg_le_neg
  apply log_le_log (cdf_pos hs)
  exact sub_le_sub_left (exp_le_exp.mpr (neg_le_neg hle)) 1

/-- Transformed observation values for log gaps from a common reference. -/
def times (d : Fin n → ℝ) (b u : ℝ) : Fin n → ℝ := fun i ↦ u * exp (b * d i)

/-- The sum that gives the exact conditional exponent optimizer. -/
def H (d : Fin n → ℝ) (b u : ℝ) : ℝ := ∑ i, loss (times d b u i)

/-- Total log likelihood in the log-data coordinate after optimizing the
exponent. Subtract the log-observation sum to return to the original scale. -/
def jointProfile (d : Fin n → ℝ) (b u : ℝ) : ℝ :=
  (n : ℝ) * (log b + log (n / H d b u) - 1) + ∑ i, residue (times d b u i)

private theorem times_pos (d : Fin n → ℝ) (b : ℝ) {u : ℝ} (hu : 0 < u) (i : Fin n) :
    0 < times d b u i := mul_pos hu (exp_pos _)

private theorem H_ge {d : Fin n → ℝ} {b u : ℝ} (hu : 0 < u) (i : Fin n) :
    loss (times d b u i) ≤ H d b u :=
  Finset.single_le_sum (fun j _ ↦ (exp_pos _).le.trans
    (loss_bounds (times_pos d b hu j)).1) (Finset.mem_univ i)

private theorem H_pos {d : Fin n → ℝ} {b u : ℝ} (hu : 0 < u) (i : Fin n) :
    0 < H d b u :=
  (exp_pos _).trans_le ((loss_bounds (times_pos d b hu i)).1.trans (H_ge hu i))

private theorem times_at_zero {d : Fin n → ℝ} {b u : ℝ} {i : Fin n} (hi : d i = 0) :
    times d b u i = u := by simp [times, hi]

private theorem log_H_ge {d : Fin n → ℝ} {b u : ℝ} (hu : 0 < u)
    {j : Fin n} (hj : d j = 0) : -u ≤ log (H d b u) := by
  rw [← log_exp (-u)]
  apply log_le_log (exp_pos _)
  have h := H_ge (d := d) (b := b) hu j
  rw [times_at_zero hj] at h
  exact (loss_bounds hu).1.trans h

private theorem profile_residue_bound {d : Fin n → ℝ} {b u : ℝ}
    (_hb : 0 < b) (hu : 0 < u) {j : Fin n} (hj : d j = 0) :
    jointProfile d b u ≤ (n : ℝ) * (log b + log n - 1 + u) +
      ∑ i, residue (times d b u i) := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  unfold jointProfile
  rw [log_div hn.ne' (H_pos hu j).ne']
  apply add_le_add_left
  apply mul_le_mul_of_nonneg_left _ hn.le
  linarith only [log_H_ge (d := d) (b := b) hu hj]

private theorem gap_sum_pos {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hne : ∃ i, 0 < d i) : 0 < ∑ i, d i := by
  obtain ⟨i, hi⟩ := hne
  exact hi.trans_le (Finset.single_le_sum (fun j _ ↦ hd j) (Finset.mem_univ i))

private theorem profile_large_time {d : Fin n → ℝ} {b u : ℝ}
    (hb : 0 < b) (hu : 1 ≤ u) (hd : ∀ i, 0 ≤ d i)
    {j : Fin n} (hj : d j = 0) :
    jointProfile d b u ≤
      (n : ℝ) * (log (b * u) + log n - 1 + loss 1) + b * (∑ i, d i) -
        u * (∑ i, (exp (b * d i) - 1)) := by
  have hup : ∀ i, 1 ≤ times d b u i := by
    intro i
    exact hu.trans (le_mul_of_one_le_right (zero_le_one.trans hu)
      (one_le_exp_iff.mpr (mul_nonneg hb.le (hd i))))
  have hu' : 0 < u := zero_lt_one.trans_le hu
  have hres : (∑ i, residue (times d b u i)) ≤
      (n : ℝ) * (log u + loss 1 - u) + b * (∑ i, d i) -
        u * (∑ i, (exp (b * d i) - 1)) := by
    have hh := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
      show residue (times d b u i) ≤
        (log u + loss 1 - u) + b * d i - u * (exp (b * d i) - 1) by
        have h := loss_anti one_pos (hup i)
        unfold residue
        rw [show log (times d b u i) = log u + b * d i by
          simp [times, log_mul hu'.ne' (exp_pos _).ne']]
        dsimp [times] at *
        linarith only [h])
    simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum] at hh ⊢
    linarith only [hh]
  rw [log_mul hb.ne' hu'.ne']
  linarith only [profile_residue_bound hb hu' hj, hres]

private theorem physical_formula {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b a c m : ℝ} (ha : 0 < a) :
    let d : Fin n → ℝ := fun i ↦ log (x i) - m
    let u := exp (b * (m - log a))
    ShapeFacesExpWeibull.logLik x b a c =
      (n : ℝ) * (log b + log c) - ∑ i, log (x i) -
        c * H d b u + ∑ i, residue (times d b u i) := by
  dsimp only
  have ht (i : Fin n) : times (fun j ↦ log (x j) - m) b (exp (b * (m - log a))) i =
      (x i / a) ^ b := by
    unfold times
    rw [← exp_add, rpow_def_of_pos (div_pos (hx i) ha), log_div (hx i).ne' ha.ne']
    congr 1
    ring
  have hlog (i : Fin n) : log ((x i / a) ^ b) = b * (log (x i) - log a) := by
    rw [log_rpow (div_pos (hx i) ha), log_div (hx i).ne' ha.ne']
  unfold ShapeFacesExpWeibull.logLik Weibull.logLik H residue loss
  simp only [Weibull.logDensity, ht, hlog, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, Finset.sum_neg_distrib,
    ← Finset.mul_sum]
  have heq : (∑ i, log (x i / a)) = (∑ i, log (x i)) - (n : ℝ) * log a := by
    simp [log_div (hx _).ne' ha.ne', Finset.sum_sub_distrib, nsmul_eq_mul]
  rw [heq]
  ring

/-- Exact exponent profiling at fixed shape and scale, in the joint
transformed-minimum chart. -/
theorem exponent_profile {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {b a m : ℝ} (ha : 0 < a) :
    let d : Fin n → ℝ := fun i ↦ log (x i) - m
    let u := exp (b * (m - log a))
    let c₀ := (n : ℝ) / H d b u
    0 < c₀ ∧ ShapeFacesExpWeibull.logLik x b a c₀ =
      jointProfile d b u - ∑ i, log (x i) ∧
      ∀ c > 0, ShapeFacesExpWeibull.logLik x b a c ≤
        ShapeFacesExpWeibull.logLik x b a c₀ := by
  intro d u c₀
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hH : 0 < H d b u := H_pos (exp_pos _) ⟨0, hn⟩
  have hc₀ : 0 < c₀ := div_pos hn' hH
  have hprod : c₀ * H d b u = n := div_mul_cancel₀ _ hH.ne'
  refine ⟨hc₀, ?_, ?_⟩
  · rw [physical_formula (m := m) hx ha]
    change (n : ℝ) * (log b + log c₀) - _ - c₀ * H d b u + _ = _
    rw [hprod]
    unfold jointProfile
    change _ = (n : ℝ) * (log b + log c₀ - 1) + _ - _
    ring
  · intro c hc
    have hl := log_le_sub_one_of_pos (div_pos hc hc₀)
    rw [log_div hc.ne' hc₀.ne'] at hl
    have he : (n : ℝ) * (c / c₀ - 1) = c * H d b u - n := by
      rw [← hprod]
      field_simp
    have hh := mul_le_mul_of_nonneg_left hl hn'.le
    rw [he] at hh
    rw [physical_formula (m := m) hx ha, physical_formula (m := m) hx ha]
    change (n : ℝ) * (log b + log c) - _ - c * H d b u + _ ≤
      (n : ℝ) * (log b + log c₀) - _ - c₀ * H d b u + _
    rw [hprod]
    linarith only [hh]

private theorem joint_continuous {d : Fin n → ℝ} (j : Fin n) :
    ContinuousOn (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) (Ioi 0 ×ˢ Ioi 0) := by
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
  have ht (i : Fin n) : Continuous (fun p : ℝ × ℝ ↦ times d p.1 p.2 i) := by
    unfold times
    fun_prop
  have hl (i : Fin n) :
      ContinuousOn (fun p : ℝ × ℝ ↦ loss (times d p.1 p.2 i)) D := by
    unfold loss
    have hc : ContinuousOn (fun p : ℝ × ℝ ↦ 1 - exp (-(times d p.1 p.2 i))) D :=
      continuousOn_const.sub (continuous_exp.continuousOn.comp
      (ht i).continuousOn.neg (mapsTo_univ _ _))
    exact (hc.log (fun p hp ↦ (cdf_pos (times_pos d p.1 hp.2 i)).ne')).neg
  have hH : ContinuousOn (fun p : ℝ × ℝ ↦ H d p.1 p.2) D :=
    continuousOn_finsetSum _ (fun i _ ↦ hl i)
  have hn : (n : ℝ) ≠ 0 := by exact_mod_cast (Nat.ne_of_gt (Nat.zero_lt_of_lt j.isLt))
  have hg : ContinuousOn (fun p : ℝ × ℝ ↦ log ((n : ℝ) / H d p.1 p.2)) D :=
    (continuousOn_const.div hH (fun p hp ↦ (H_pos hp.2 j).ne')).log
      (fun p hp ↦ div_ne_zero hn (H_pos hp.2 j).ne')
  have hr (i : Fin n) :
      ContinuousOn (fun p : ℝ × ℝ ↦ residue (times d p.1 p.2 i)) D := by
    unfold residue
    have h := (ht i).continuousOn (s := D)
    exact ((h.log (fun p hp ↦ (times_pos d p.1 hp.2 i).ne')).sub h).add (hl i)
  unfold jointProfile
  have hb : ContinuousOn (fun p : ℝ × ℝ ↦ log p.1) D :=
    continuous_fst.continuousOn.log (fun _ hp ↦ (ne_of_gt hp.1))
  exact (((hb.add hg).sub
    continuousOn_const).const_mul _).add (continuousOn_finsetSum _ (fun i _ ↦ hr i))

private theorem log_H_upper_of_level {d : Fin n → ℝ} {b u B v : ℝ}
    (hu : 0 < u) (hb : 0 < b) (hB : b ≤ B) (hval : v ≤ jointProfile d b u)
    (j : Fin n) :
    log (H d b u) ≤ log B + log n - 1 - v / n := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  have hr : ∑ i, residue (times d b u i) ≤ 0 :=
    Finset.sum_nonpos fun i _ ↦ residue_nonpos (times_pos d b hu i)
  have hl := log_le_log hb hB
  unfold jointProfile at hval
  rw [log_div hn.ne' (H_pos hu j).ne'] at hval
  have h' : v ≤ (log B + log n - 1 - log (H d b u)) * n := by
    nlinarith only [hval, hr, mul_le_mul_of_nonneg_left hl hn.le]
  have h := (div_le_iff₀ hn).mpr h'
  linarith only [h]

/-- Uniform compactness in the nuisance coordinate for shapes in a fixed
positive compact interval. Shapes at either end of the positive half-line
are the only remaining candidates for likelihood-retaining escape. -/
theorem strip_compact {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hne : ∃ i, 0 < d i)
    {j : Fin n} (hj : d j = 0) {a B v : ℝ} (ha : 0 < a) :
    IsCompact ((Icc a B ×ˢ Ioi (0 : ℝ)) ∩
      (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) ⁻¹' Ici v) := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  let S := ∑ i, d i
  have hS : 0 < S := gap_sum_pos hd hne
  let C := log B + log n - 1 - v / n
  let lo := exp (-exp C)
  have hlo : 0 < lo := exp_pos _
  let κ := a * S / (2 * n)
  have hκ : 0 < κ := by positivity
  let T := (n : ℝ) * (log B - log κ + log n - 2 + loss 1) + B * S
  let hi := max 1 ((T - v) / (n * κ))
  let K := Icc a B ×ˢ Icc lo hi
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hsub : K ⊆ Icc a B ×ˢ Ioi (0 : ℝ) := fun _ h ↦ ⟨h.1, hlo.trans_le h.2.1⟩
  have hsub' : K ⊆ Ioi 0 ×ˢ Ioi 0 := fun _ h ↦ ⟨ha.trans_le h.1.1, hlo.trans_le h.2.1⟩
  suffices h : ((Icc a B ×ˢ Ioi (0 : ℝ)) ∩
      (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) ⁻¹' Ici v) =
      K ∩ (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) ⁻¹' Ici v by
    rw [h]
    exact ((joint_continuous j).mono hsub').upperSemicontinuousOn
      |>.isCompact_inter_preimage_Ici hK v
  refine Subset.antisymm ?_ (fun _ h ↦ ⟨hsub h.1, h.2⟩)
  rintro ⟨b,u⟩ ⟨⟨⟨hab,hbB⟩,hu⟩,hval⟩
  change 0 < u at hu
  change v ≤ jointProfile d b u at hval
  have hb := ha.trans_le hab
  have hlo' : lo ≤ u := by
    have hH := log_H_upper_of_level hu hb hbB hval j
    have hH' : H d b u ≤ exp C := by rw [← exp_log (H_pos hu j)]; exact exp_le_exp.mpr hH
    have hh := H_ge (d := d) (b := b) hu j
    rw [times_at_zero hj] at hh
    have ht := (loss_bounds hu).2.1.trans (hh.trans hH')
    rw [← exp_log hu]
    exact exp_le_exp.mpr (by linarith only [ht])
  have hhi : u ≤ hi := by
    by_cases h : u ≤ 1
    · exact h.trans (le_max_left _ _)
    have hsum : b * S ≤ ∑ i, (exp (b * d i) - 1) := by
      dsimp [S]
      rw [Finset.mul_sum]
      exact Finset.sum_le_sum fun i _ ↦ by linarith [add_one_le_exp (b * d i)]
    have hval' := hval.trans (profile_large_time hb (le_of_not_ge h) hd hj)
    rw [log_mul hb.ne' hu.ne'] at hval'
    have hlogu := log_le_sub_one_of_pos (mul_pos hκ hu)
    rw [log_mul hκ.ne' hu.ne'] at hlogu
    have hf : (2 * (n : ℝ) * κ) = a * S := by dsimp [κ]; field_simp
    apply le_trans _ (le_max_right _ _)
    apply (le_div_iff₀ (mul_pos hn hκ)).mpr
    dsimp [T]
    nlinarith only [hval', mul_le_mul_of_nonneg_left (log_le_log hb hbB) hn.le,
      mul_le_mul_of_nonneg_left hlogu hn.le, mul_le_mul_of_nonneg_right hbB hS.le,
      mul_le_mul_of_nonneg_left ((mul_le_mul_of_nonneg_right hab hS.le).trans hsum) hu.le,
      congrArg (fun z : ℝ ↦ u * z) hf]
  exact ⟨⟨⟨hab,hbB⟩, hlo', hhi⟩, hval⟩

/-- Optimized closed-endpoint power value on the log-data scale. -/
def powerValue (d : Fin n → ℝ) (D : ℝ) : ℝ :=
  (n : ℝ) * (log (n / (∑ i, (D - d i))) - 1)

private theorem max_time_bound {d : Fin n → ℝ} {D : ℝ} (hD : 0 < D)
    {j l : Fin n} (hj : d j = 0) (hl : d l = D) (v : ℝ) :
    ∃ C > 0, ∀ᶠ b : ℝ in atTop, ∀ u > 0,
      v ≤ jointProfile d b u → times d b u l ≤ C * b := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  let A := (n : ℝ) * (log n - 1) + log 2 - 1 + loss 1
  let C := max 1 (4 * ((n : ℝ) + max 0 (A - v)))
  have hC : 0 < C := one_pos.trans_le (le_max_left _ _)
  have ht : Tendsto (fun b : ℝ ↦ exp (-(b * D))) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp ((tendsto_id.const_mul_atTop_of_neg (neg_neg_of_pos hD)).congr
      (fun b ↦ by simp [id]; ring))
  refine ⟨C, hC, ?_⟩
  filter_upwards [ht.eventually (Iio_mem_nhds (show 0 < 1 / (4 * (n : ℝ)) by positivity)),
    eventually_ge_atTop (1 : ℝ)] with b hbe hb u hu hv
  have hb' := one_pos.trans_le hb
  let t := times d b u l
  by_cases h : t ≤ 1
  · exact h.trans ((le_max_left _ _).trans (le_mul_of_one_le_right hC.le hb))
  have ht' : 0 < t := times_pos d b hu l
  have hut : t * exp (-(b * D)) = u := by
    simp only [t, times, hl, mul_assoc, ← exp_add, add_neg_cancel, exp_zero, mul_one]
  have hmul : (n : ℝ) * u ≤ t / 4 := by
    rw [← hut]
    have he := (le_div_iff₀ (show 0 < 4 * (n : ℝ) by positivity)).mp hbe.le
    nlinarith only [mul_le_mul_of_nonneg_left he ht'.le]
  have hq := loss_anti one_pos (le_of_not_ge h)
  have hlog := log_le_sub_one_of_pos (div_pos ht' (show (0 : ℝ) < 2 by norm_num))
  rw [log_div ht'.ne' two_ne_zero] at hlog
  have hres : residue t ≤ -t / 2 + log 2 - 1 + loss 1 := by
    unfold residue
    linarith only [hlog, hq]
  have hsum : (∑ i, residue (times d b u i)) ≤ residue t := by
    have hs := Finset.sum_le_sum_of_subset_of_nonpos (Finset.singleton_subset_iff.mpr
      (Finset.mem_univ l)) (fun i _ _ ↦ residue_nonpos (times_pos d b hu i))
    simpa [t] using hs
  have hval := profile_residue_bound hb' hu hj
  have hlin : t ≤ 4 * ((n : ℝ) * b + max 0 (A - v)) := by
    dsimp [A]
    have hlb := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hb') hn.le
    have hmax := le_max_right 0 (A - v)
    dsimp only [A] at hmax
    nlinarith only [hval, hv, hsum, hres, hmul, hlb, hn, hmax]
  refine hlin.trans ?_
  have hmul := mul_le_mul_of_nonneg_right (le_max_right 1
    (4 * ((n : ℝ) + max 0 (A - v)))) hb'.le
  have hstep : 4 * ((n : ℝ) * b + max 0 (A - v)) ≤
      4 * ((n : ℝ) + max 0 (A - v)) * b := by
    nlinarith only [mul_le_mul_of_nonneg_left hb (le_max_left 0 (A - v))]
  exact hstep.trans hmul

/-- Above any prescribed tolerance of the optimized closed-endpoint power
value, all large shapes lose uniformly over the scale coordinate. -/
theorem large_shape_upper {d : Fin n → ℝ} {D : ℝ} (hd : ∀ i, d i ≤ D)
    (hD : 0 < D) {j l : Fin n} (hj : d j = 0) (hl : d l = D) {v : ℝ}
    (hv : powerValue d D < v) :
    ∀ᶠ b : ℝ in atTop, ∀ u > 0, jointProfile d b u < v := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  let S := ∑ i, (D - d i)
  have hS : 0 < S := gap_sum_pos (fun i ↦ sub_nonneg.mpr (hd i))
    ⟨j, by rwa [hj, sub_zero]⟩
  have hc : ContinuousAt (fun e : ℝ ↦ (n : ℝ) * (log (n / (S - n * e)) - 1)) 0 := by
    apply ContinuousAt.const_mul
    apply ContinuousAt.sub _ continuousAt_const
    have hden : Continuous (fun e : ℝ ↦ S - (n : ℝ) * e) := by fun_prop
    have hden0 : S - (n : ℝ) * 0 ≠ 0 := by simpa using hS.ne'
    exact (continuousAt_const.div hden.continuousAt hden0).log (div_ne_zero hn.ne' hden0)
  have hv' : (n : ℝ) * (log (n / (S - n * (0 : ℝ))) - 1) < v := by
    simpa [powerValue, S] using hv
  let hslow : ∀ᶠ e : ℝ in 𝓝 0, 0 < S - (n : ℝ) * e :=
    ((continuous_const.sub (continuous_const.mul continuous_id)).continuousAt.eventually
      (Ioi_mem_nhds (by simpa using hS : 0 < S - (n : ℝ) * 0)))
  have hpos : ∀ᶠ e : ℝ in 𝓝[>] 0, e ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨ε, ⟨hε, hSv⟩, hval⟩ := (((hpos).and
    (hslow.filter_mono nhdsWithin_le_nhds)).and
    ((hc.eventually (Iio_mem_nhds hv')).filter_mono nhdsWithin_le_nhds)).exists
  obtain ⟨C, hC, hbound⟩ := max_time_bound hD hj hl v
  filter_upwards [hbound, eventually_ge_atTop (max 1 (2 * C / ε^2))] with b hb hb' u hu
  by_contra! h
  have hb0 : 0 < b := one_pos.trans_le ((le_max_left _ _).trans hb')
  let t := times d b u l
  have ht : 0 < t := times_pos d b hu l
  have hC' : 2 * C ≤ b * ε^2 := (div_le_iff₀ (sq_pos_of_pos hε)).mp ((le_max_right _ _).trans hb')
  have he := pow_div_factorial_le_exp (ε * b) (mul_nonneg hε.le hb0.le) 2
  norm_num at he
  have hlog : log t ≤ b * ε := by
    rw [mul_comm, ← log_exp (ε * b)]
    apply log_le_log ht
    exact (hb u hu h).trans (by
      nlinarith only [he, mul_le_mul_of_nonneg_right hC' hb0.le])
  have htimes (i : Fin n) : log (times d b u i) = log t - b * (D - d i) := by
    simp only [t, times, hl, log_mul hu.ne' (exp_pos _).ne', log_exp]
    ring
  have hH : b * (S - n * ε) ≤ H d b u := by
    have hh := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
      (loss_bounds (times_pos d b hu i)).2.1)
    simp only [htimes, neg_sub, Finset.sum_sub_distrib, ← Finset.mul_sum,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hh
    simp only [S, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    unfold H
    nlinarith only [hh, mul_le_mul_of_nonneg_left hlog hn.le]
  have hr : (∑ i, residue (times d b u i)) ≤ 0 :=
    Finset.sum_nonpos fun i _ ↦ residue_nonpos (times_pos d b hu i)
  have hlogH := log_le_log (mul_pos hb0 hSv) hH
  rw [log_mul hb0.ne' (ne_of_gt hSv)] at hlogH
  have hupper : jointProfile d b u ≤ (n : ℝ) * (log (n / (S - n * ε)) - 1) := by
    unfold jointProfile
    rw [log_div hn.ne' (H_pos hu j).ne', log_div hn.ne' (ne_of_gt hSv)]
    nlinarith only [hr, mul_le_mul_of_nonneg_left hlogH hn.le]
  exact (hupper.trans_lt hval).not_ge h

/-- The inverse-Weibull face with log-scale optimized at each shape,
on the translated log-data scale. -/
def inverseProfile (d : Fin n → ℝ) (k : ℝ) : ℝ :=
  (n : ℝ) * (log k + log n - 1 - log (∑ i, exp (-(k * d i)))) - k * ∑ i, d i

private theorem small_shape_retention {d : Fin n → ℝ} {j : Fin n} (hj : d j = 0)
    (hd : ∀ i, 0 ≤ d i) (hne : ∃ i, 0 < d i) (v U : ℝ) (hU : 1 ≤ U) :
    ∃ K > 0, ∀ᶠ b : ℝ in 𝓝[>] 0, ∀ u > 0, v ≤ jointProfile d b u →
      U ≤ u ∧ b * u ≤ K := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  let S := ∑ i, d i
  have hS : 0 < S := gap_sum_pos hd hne
  let κ := S / (2 * n)
  have hκ : 0 < κ := by positivity
  let C := (n : ℝ) * (-log κ + log n - 2 + loss 1) + S
  let K := max 1 ((C - v) / (n * κ))
  have hK : 0 < K := one_pos.trans_le (le_max_left _ _)
  have hlog : Tendsto (log : ℝ → ℝ) (𝓝[>] 0) atBot := tendsto_log_nhdsGT_zero
  have huv : ∀ᶠ b : ℝ in 𝓝[>] 0, (n : ℝ) * (log b + log n - 1 + U) < v := by
    have ht := hlog.atBot_add (tendsto_const_nhds (x := log (n : ℝ) - 1 + U))
    have ht' := (ht.const_mul_atBot hn).eventually (eventually_lt_atBot v)
    filter_upwards [ht'] with b h
    change (n : ℝ) * (log b + (log n - 1 + U)) < v at h
    linarith only [h]
  refine ⟨K, hK, ?_⟩
  filter_upwards [self_mem_nhdsWithin,
    mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (show (0 : ℝ) < 1 by norm_num)), huv]
    with b (hb : 0 < b) (hb1 : b < 1) hbv u hu hv
  have huU : U ≤ u := by
    by_contra! h
    have hr : ∑ i, residue (times d b u i) ≤ 0 :=
      Finset.sum_nonpos fun i _ ↦ residue_nonpos (times_pos d b hu i)
    nlinarith only [profile_residue_bound hb hu hj, hr, hv, hbv,
      mul_le_mul_of_nonneg_left h.le hn.le]
  refine ⟨huU, (le_max_right _ _).trans' ?_⟩
  have hsum : b * S ≤ ∑ i, (exp (b * d i) - 1) := by
    dsimp [S]
    rw [Finset.mul_sum]
    exact Finset.sum_le_sum fun i _ ↦ by linarith [add_one_le_exp (b * d i)]
  have h := hv.trans (profile_large_time hb (hU.trans huU) hd hj)
  have hlogk := log_le_sub_one_of_pos (mul_pos hκ (mul_pos hb hu))
  rw [log_mul hκ.ne' (mul_pos hb hu).ne'] at hlogk
  have he : 2 * (n : ℝ) * κ = S := by dsimp [κ]; field_simp
  apply (le_div_iff₀ (mul_pos hn hκ)).mpr
  dsimp [C]
  nlinarith only [h, mul_le_mul_of_nonneg_left hlogk hn.le,
    mul_le_mul_of_nonneg_right (show b ≤ 1 from hb1.le) hS.le,
    mul_le_mul_of_nonneg_left hsum hu.le, congrArg (fun z : ℝ ↦ b * u * z) he]

private theorem loss_tendsto : Tendsto loss atTop (𝓝 0) := by
  have he : Tendsto (fun u : ℝ ↦ exp (-u)) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp tendsto_neg_atTop_atBot
  have h : Tendsto (fun u : ℝ ↦ 1 - exp (-u)) atTop (𝓝 1) := by
    simpa using he.const_sub 1
  have h' : Tendsto (fun u : ℝ ↦ -log (1 - exp (-u))) atTop (𝓝 (-log 1)) :=
    ((continuousAt_log one_ne_zero).tendsto.comp h).neg
  rw [log_one, neg_zero] at h'
  exact h'

/-- Above any prescribed tolerance of an upper bound on the optimized
inverse-Weibull face, all small shapes lose uniformly over scale. -/
theorem small_shape_upper {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hne : ∃ i, 0 < d i)
    {j : Fin n} (hj : d j = 0) {I v : ℝ}
    (hI : ∀ k > 0, inverseProfile d k ≤ I) (hv : I < v) :
    ∀ᶠ b : ℝ in 𝓝[>] 0, ∀ u > 0, jointProfile d b u < v := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast (Nat.zero_lt_of_lt j.isLt)
  let ε := (v - I) / (4 * n)
  have hε : 0 < ε := div_pos (sub_pos.mpr hv) (by positivity)
  have hεeq : 4 * (n : ℝ) * ε = v - I := by dsimp [ε]; field_simp
  obtain ⟨U, hU⟩ := eventually_atTop.mp (loss_tendsto.eventually (Iio_mem_nhds hε))
  obtain ⟨K, hK, hret⟩ := small_shape_retention hj hd hne v (max 1 U) (le_max_left _ _)
  let S := ∑ i, d i
  have hS : 0 < S := gap_sum_pos hd hne
  have hB : ∀ᶠ b : ℝ in 𝓝[>] 0, b * S < n * ε := by
    have hh : Tendsto (fun b : ℝ ↦ b * S) (𝓝[>] 0) (𝓝 0) := by
      simpa using (tendsto_id.const_mul S).congr (fun b ↦ by simp [id, mul_comm]) |>.mono_left
        (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
    exact hh.eventually (Iio_mem_nhds (mul_pos hn hε))
  have hW : ∀ᶠ b : ℝ in 𝓝[>] 0, ∀ i, (exp (b * d i) - 1) / b < d i + ε / K := by
    rw [Filter.eventually_all]
    intro i
    have hder : HasDerivAt (fun b : ℝ ↦ exp (b * d i)) (d i) 0 := by
      simpa using ((hasDerivAt_id (0 : ℝ)).mul_const (d i)).exp
    have hlim : Tendsto (fun b : ℝ ↦ (exp (b * d i) - 1) / b) (𝓝[>] 0) (𝓝 (d i)) := by
      refine hder.tendsto_slope_zero_right.congr (fun b ↦ ?_)
      simp [div_eq_mul_inv, mul_comm]
    exact hlim.eventually (Iio_mem_nhds (lt_add_of_pos_right _ (div_pos hε hK)))
  filter_upwards [self_mem_nhdsWithin, hret, hB, hW] with b (hb : 0 < b) hret hbS hW u hu
  by_contra! h
  obtain ⟨huU, hkK⟩ := hret u hu h
  have hlu : loss u < ε := hU u ((le_max_right _ _).trans huU)
  let k := b * u
  have hk : 0 < k := mul_pos hb hu
  let Z := ∑ i, exp (-(k * d i))
  have hZ : 0 < Z :=
    Finset.sum_pos (fun i _ ↦ exp_pos _) ⟨j, Finset.mem_univ _⟩
  have hl (i : Fin n) : k * d i ≤ times d b u i - u := by
    have he := mul_le_mul_of_nonneg_left (add_one_le_exp (b * d i)) hu.le
    unfold k times
    nlinarith only [he]
  have hg (i : Fin n) : times d b u i - u ≤ k * d i + ε := by
    have he := (div_lt_iff₀ hb).mp (hW i)
    have hεK : K * (ε / K) = ε := by field_simp
    have hh := mul_le_mul_of_nonneg_right hkK (div_pos hε hK).le
    unfold times k
    nlinarith only [mul_le_mul_of_nonneg_left he.le hu.le, hεK, hh]
  have hlower : exp (-u - ε) * Z ≤ H d b u := by
    unfold Z H
    rw [Finset.mul_sum]
    apply Finset.sum_le_sum
    intro i _
    rw [← exp_add]
    exact (exp_le_exp.mpr (by linarith only [hg i])).trans
      (loss_bounds (times_pos d b hu i)).1
  have hH := log_le_log (mul_pos (exp_pos _) hZ) hlower
  rw [log_mul (exp_pos _).ne' hZ.ne', log_exp] at hH
  have hres : ∑ i, residue (times d b u i) ≤
      (n : ℝ) * (log u - u + ε) + b * S - k * S := by
    have hh := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
      show residue (times d b u i) ≤ log u - u + ε + b * d i - k * d i by
        have ht : u ≤ times d b u i := by
          linarith only [hl i, mul_nonneg hk.le (hd i)]
        have ha := loss_anti hu ht
        unfold residue
        rw [show log (times d b u i) = log u + b * d i by
          simp [times, log_mul hu.ne' (exp_pos _).ne']]
        linarith only [ha, hlu, hl i])
    simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum] at hh
    dsimp [S]
    linarith only [hh]
  have hlo := hI k hk
  unfold inverseProfile at hlo
  change (n : ℝ) * (log k + log n - 1 - log Z) - k * S ≤ I at hlo
  dsimp only [k] at hlo
  rw [log_mul hb.ne' hu.ne'] at hlo
  unfold jointProfile at h
  rw [log_div hn.ne' (H_pos hu j).ne'] at h
  nlinarith only [h, hres, mul_le_mul_of_nonneg_left hH hn.le, hlo, hbS, hεeq,
    mul_pos hn hε]

private theorem inverse_fitted_scale {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) {m k : ℝ} (hk : 0 < k) :
    let d : Fin n → ℝ := fun i ↦ log (x i) - m
    ∃ a > 0, Weibull.Inverse.logLik x k a = inverseProfile d k - ∑ i, log (x i) := by
  intro d
  let Z := ∑ i, exp (-(k * d i))
  have hZ : 0 < Z := Finset.sum_pos (fun i _ ↦ exp_pos _) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  let a := exp (m + log (n / Z) / k)
  have ha : 0 < a := exp_pos _
  have hlog : k * (log a - m) = log (n / Z) := by
    dsimp [a]
    rw [log_exp]
    field_simp
    ring
  have hpow (i : Fin n) : (x i / a) ^ (-k) = (n / Z) * exp (-(k * d i)) := by
    rw [rpow_def_of_pos (div_pos (hx i) ha), log_div (hx i).ne' ha.ne',
      ← exp_log (div_pos hn' hZ), ← exp_add]
    congr 1
    dsimp [d]
    linarith only [hlog]
  have hsum : (∑ i, (x i / a) ^ (-k)) = n := by
    simp only [hpow, ← Finset.mul_sum]
    change (n / Z) * Z = n
    exact div_mul_cancel₀ _ hZ.ne'
  refine ⟨a, ha, ?_⟩
  unfold Weibull.Inverse.logLik Weibull.Inverse.logDensity inverseProfile
  simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, hsum]
  change _ = (n : ℝ) * (log k + log n - 1 - log Z) - k * (∑ i, (log (x i) - m)) - _
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  rw [log_div hn'.ne' hZ.ne'] at hlog
  nlinarith only [congrArg (fun z : ℝ ↦ (n : ℝ) * z) hlog]

/-- Optimized scale and exponent profile for the original observations. -/
def shapeProfile (x : Fin n → ℝ) (b : ℝ) : ℝ :=
  sSup {v | ∃ p ∈ Weibull.domain, ShapeFacesExpWeibull.logLik x b p.1 p.2 = v}

private theorem shape_winner {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {b : ℝ} (hb : 0 < b) :
    ∃ p ∈ Weibull.domain, ShapeFacesExpWeibull.logLik x b p.1 p.2 = shapeProfile x b ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesExpWeibull.logLik x b q.1 q.2) Weibull.domain p := by
  obtain ⟨p, hp, hm⟩ := ShapeFacesExpWeibull.held_shape_attains hn hx hb hne
  have hL : IsLUB {v | ∃ p ∈ Weibull.domain, ShapeFacesExpWeibull.logLik x b p.1 p.2 = v}
      (ShapeFacesExpWeibull.logLik x b p.1 p.2) :=
    ⟨by rintro _ ⟨q, hq, rfl⟩; exact hm hq, fun _ h ↦ h ⟨p, hp, rfl⟩⟩
  exact ⟨p, hp, (hL.csSup_eq ⟨_, p, hp, rfl⟩).symm, hm⟩

private theorem chart_le_profile {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (m : ℝ) {b u : ℝ} (hb : 0 < b) (hu : 0 < u) :
    jointProfile (fun i ↦ log (x i) - m) b u - ∑ i, log (x i) ≤ shapeProfile x b := by
  let a := exp (m - log u / b)
  have ha : 0 < a := exp_pos _
  have he : exp (b * (m - log a)) = u := by
    dsimp [a]
    rw [log_exp, show b * (m - (m - log u / b)) = log u by field_simp; ring, exp_log hu]
  have heval := exponent_profile hn hx (b := b) (m := m) ha
  dsimp only at heval
  rw [he] at heval
  obtain ⟨p, hp, hv, hm⟩ := shape_winner hn hx hne hb
  rw [← heval.2.1, ← hv]
  let q : ℝ × ℝ := (a, (n : ℝ) / H (fun i ↦ log (x i) - m) b u)
  exact hm (a := q) ⟨ha, heval.1⟩

private theorem chart_winner {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (m : ℝ) {b : ℝ} (hb : 0 < b) :
    ∃ u > 0, shapeProfile x b = jointProfile (fun i ↦ log (x i) - m) b u - ∑ i, log (x i) := by
  obtain ⟨p, hp, hv, hm⟩ := shape_winner hn hx hne hb
  let u := exp (b * (m - log p.1))
  have hu : 0 < u := exp_pos _
  have H := exponent_profile hn hx (b := b) (m := m) (show 0 < p.1 from hp.1)
  refine ⟨u, hu, le_antisymm ?_ (chart_le_profile hn hx hne m hb hu)⟩
  rw [← H.2.1, ← hv]
  exact H.2.2 p.2 hp.2

private theorem log_gaps {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ j l : Fin n, (∀ i, log (x j) ≤ log (x i)) ∧
      (∀ i, log (x i) ≤ log (x l)) ∧ log (x j) < log (x l) := by
  let y := fun i ↦ log (x i)
  have hs : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_min_image Finset.univ y hs
  obtain ⟨l, _, hl⟩ := Finset.exists_max_image Finset.univ y hs
  refine ⟨j, l, fun i ↦ hj i (Finset.mem_univ _), fun i ↦ hl i (Finset.mem_univ _), ?_⟩
  by_contra! h
  obtain ⟨a, b, hab⟩ := hne
  have he (i : Fin n) : y i = y j :=
    le_antisymm ((hl i (Finset.mem_univ _)).trans h) (hj i (Finset.mem_univ _))
  exact hab (log_injOn_pos (hx a) (hx b) ((he a).trans (he b).symm))

private theorem shape_continuous {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) : ContinuousOn (shapeProfile x) (Ioi 0) := by
  obtain ⟨j, l, hj, hl, hjl⟩ := log_gaps hn hx hne
  let m := log (x j)
  let d := fun i ↦ log (x i) - m
  let S := ∑ i, log (x i)
  have hd (i : Fin n) : 0 ≤ d i := sub_nonneg.mpr (hj i)
  have hne' : ∃ i, 0 < d i := ⟨l, sub_pos.mpr hjl⟩
  have hj' : d j = 0 := sub_self _
  intro b hb
  change 0 < b at hb
  let B := Icc (b / 2) (2 * b)
  have hbB : b ∈ B := ⟨by linarith, by linarith⟩
  have hB : B ∈ 𝓝 b := Icc_mem_nhds (by linarith) (by linarith)
  have hsub : B ⊆ Ioi 0 := fun _ ht ↦ (half_pos hb).trans_le ht.1
  suffices h : ContinuousOn (shapeProfile x) B from (h b hbB).continuousAt hB
    |>.continuousWithinAt
  rw [continuousOn_iff_lower_upperSemicontinuousOn]
  refine ⟨?_, ?_⟩
  · intro t ht v hv
    obtain ⟨u, hu, he⟩ := chart_winner hn hx hne m (hsub ht)
    have hc : ContinuousWithinAt (fun r ↦ jointProfile d r u - S) B t := by
      have hf : Continuous (fun r : ℝ ↦ (r,u)) := continuous_id.prodMk continuous_const
      have hm : MapsTo (fun r ↦ (r,u)) B (Ioi 0 ×ˢ Ioi 0) :=
        fun _ hr ↦ ⟨hsub hr, hu⟩
      exact (((joint_continuous (d := d) j).comp hf.continuousOn hm) t ht).sub_const S
    have hval : v < jointProfile d t u - S := by rwa [← he]
    filter_upwards [hc.eventually (Ioi_mem_nhds hval), self_mem_nhdsWithin] with r hr hrB
    exact hr.trans_le (chart_le_profile hn hx hne m (hsub hrB) hu)
  · apply upperSemicontinuousOn_iff_preimage_Ici.mpr
    intro v
    let C := (B ×ˢ Ioi (0 : ℝ)) ∩ (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) ⁻¹' Ici (v + S)
    have hC : IsCompact C := strip_compact hd hne' hj' (half_pos hb)
    have heq : Prod.fst '' C = B ∩ shapeProfile x ⁻¹' Ici v := by
      ext t
      constructor
      · rintro ⟨p, ⟨⟨hp, hu⟩, hv⟩, rfl⟩
        have hh := chart_le_profile hn hx hne m (hsub hp) hu
        exact ⟨hp, by change v ≤ shapeProfile x p.1; change v + S ≤ _ at hv; linarith⟩
      · rintro ⟨ht, hv⟩
        obtain ⟨u, hu, he⟩ := chart_winner hn hx hne m (hsub ht)
        refine ⟨(t,u), ⟨⟨ht, hu⟩, ?_⟩, rfl⟩
        change v + S ≤ jointProfile d t u
        change v ≤ shapeProfile x t at hv
        linarith
    exact ⟨_, (hC.image continuous_fst).isClosed, by rw [heq]; ext; simp⟩

/-- Closed power-face height on the original observation scale. The argument
is the sample maximum in log coordinates and includes every copy of that value. -/
def physicalPowerValue (x : Fin n → ℝ) (M : ℝ) : ℝ :=
  (n : ℝ) * (log (n / ∑ i, (M - log (x i))) - 1) - ∑ i, log (x i)

/-- The fully optimized shape profile tends to the fitted inverse-Weibull
value at zero and to the closed power value at infinity. -/
theorem profile_face_limits {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {M : ℝ} (hM : ∀ i, log (x i) ≤ M)
    (htop : ∃ i, log (x i) = M) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      Tendsto (shapeProfile x) (𝓝[>] 0) (𝓝 (Weibull.Inverse.logLik x p.1 p.2)) ∧
      Tendsto (shapeProfile x) atTop (𝓝 (physicalPowerValue x M)) := by
  obtain ⟨p, hp, hm, hpath⟩ := ShapeFacesExpWeibull.optimized_inverse_weibull_face hn hx hne
  obtain ⟨j,l,hj,hl,hjl⟩ := log_gaps hn hx hne
  let m := log (x j)
  let d := fun i ↦ log (x i) - m
  let D := M - m
  let S := ∑ i, log (x i)
  let I := Weibull.Inverse.logLik x p.1 p.2
  let P := physicalPowerValue x M
  have hj' : d j = 0 := sub_self _
  have hD : 0 < D := sub_pos.mpr (hjl.trans_le (hM l))
  have hD' (i : Fin n) : d i ≤ D := sub_le_sub_right (hM i) _
  have hd (i : Fin n) : 0 ≤ d i := sub_nonneg.mpr (hj i)
  have hne' : ∃ i, 0 < d i := ⟨l, sub_pos.mpr hjl⟩
  obtain ⟨e, he⟩ := htop
  have he' : d e = D := by dsimp [d, D]; rw [he]
  have hP : powerValue d D = P + S := by
    have h : ∀ i, D - d i = M - log (x i) := by intro i; dsimp [D,d]; ring
    simp only [powerValue, P, physicalPowerValue, h, S, sub_add_cancel]
  have hI (k : ℝ) (hk : 0 < k) : inverseProfile d k ≤ I + S := by
    obtain ⟨a, ha, heq⟩ := inverse_fitted_scale hn hx (m := m) hk
    have hh := hm (a := (k,a)) ⟨hk,ha⟩
    change Weibull.Inverse.logLik x k a ≤ I at hh
    change _ = inverseProfile d k - S at heq
    linarith only [hh, heq]
  refine ⟨p, hp, hm, ?_, ?_⟩
  · apply tendsto_order.mpr
    constructor
    · intro v hv
      have ht : Tendsto (fun b : ℝ ↦ p.1 / b) (𝓝[>] 0) atTop := by
        simpa [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop
          (show 0 < p.1 from hp.1)
      have hp1 : 0 < p.1 := hp.1
      have hev := (hpath.comp ht).eventually (Ioi_mem_nhds hv)
      filter_upwards [self_mem_nhdsWithin, hev] with b (hb : 0 < b) hval
      have hs : p.1 / (p.1 / b) = b := by field_simp
      change v < ShapeFacesExpWeibull.logLik x (p.1 / (p.1 / b)) _ _ at hval
      rw [hs] at hval
      obtain ⟨q, hq, heq, hmax⟩ := shape_winner hn hx hne hb
      rw [← heq]
      apply hval.trans_le
      exact hmax (a := (exp (log p.2 - log (p.1 / b) / b), exp (p.1 / b)))
        ⟨exp_pos _, exp_pos _⟩
    · intro v hv
      have hf := small_shape_upper hd hne' hj' hI (show I + S < v + S by linarith)
      filter_upwards [self_mem_nhdsWithin, hf] with b (hb : 0 < b) hu
      obtain ⟨u, hu', heq⟩ := chart_winner hn hx hne m hb
      have ht := hu u hu'
      change _ = jointProfile d b u - S at heq
      linarith only [ht, heq]
  · apply tendsto_order.mpr
    constructor
    · intro v hv
      have H := ShapeFacesExpWeibull.optimized_power_face hx hne hM ⟨e, he⟩
      dsimp only at H
      have ht := (H.2.2.comp Real.tendsto_sqrt_atTop).eventually (Ioi_mem_nhds hv)
      filter_upwards [eventually_gt_atTop (0 : ℝ), ht] with b hb hval
      change v < ShapeFacesExpWeibull.logLik x (√b ^ 2) _ _ at hval
      simp only [sq_sqrt hb.le] at hval
      obtain ⟨q,hq,heq,hmax⟩ := shape_winner hn hx hne hb
      rw [← heq]
      exact hval.trans_le (hmax
        (a := (exp (M + (√b)⁻¹), ((n : ℝ) / ∑ i, (M - log (x i))) / b))
        ⟨exp_pos _, div_pos H.1 hb⟩)
    · intro v hv
      have hf := large_shape_upper hD' hD hj' he' (show powerValue d D < v + S by
        rw [hP]; linarith)
      filter_upwards [eventually_gt_atTop (0 : ℝ), hf] with b hb hu
      obtain ⟨u, hu', heq⟩ := chart_winner hn hx hne m hb
      have ht := hu u hu'
      change _ = jointProfile d b u - S at heq
      linarith only [ht, heq]

private def decodeShape (t : ℝ) : ℝ := t / (1 - t)

private theorem decodeShape_pos {t : ℝ} (ht : t ∈ Ioo (0 : ℝ) 1) :
    0 < decodeShape t := div_pos ht.1 (sub_pos.mpr ht.2)

private theorem encodeShape {b : ℝ} (hb : 0 < b) :
    ∃ t ∈ Ioo (0 : ℝ) 1, decodeShape t = b := by
  have hden : 0 < 1 + b := by positivity
  let t := b / (1 + b)
  have ht : t < 1 := (div_lt_one hden).mpr (by linarith)
  refine ⟨t, ⟨div_pos hb hden, ht⟩, ?_⟩
  unfold decodeShape
  have he : 1 - t = 1 / (1 + b) := by dsimp [t]; field_simp; ring
  rw [he]
  dsimp [t]
  field_simp

private def closedProfile (x : Fin n → ℝ) (I P t : ℝ) : ℝ :=
  if t = 0 then I else if t = 1 then P else shapeProfile x (decodeShape t)

private theorem closed_profile_continuous {x : Fin n → ℝ} {I P : ℝ}
    (hc : ContinuousOn (shapeProfile x) (Ioi 0))
    (hI : Tendsto (shapeProfile x) (𝓝[>] 0) (𝓝 I))
    (hP : Tendsto (shapeProfile x) atTop (𝓝 P)) :
    ContinuousOn (closedProfile x I P) (Icc 0 1) := by
  let D := Ioo (0 : ℝ) 1
  have hdecode {t : ℝ} (ht : t ≠ 1) : ContinuousAt decodeShape t := by
    unfold decodeShape
    exact continuousAt_id.div (continuousAt_const.sub continuousAt_id)
      (by dsimp; exact sub_ne_zero.mpr ht.symm)
  have heq : ∀ t ∈ D, closedProfile x I P t = shapeProfile x (decodeShape t) :=
    fun t ht ↦ by simp [closedProfile, ht.1.ne', ht.2.ne]
  intro t ht
  have hmem {z : ℝ} : ∀ᶠ r in 𝓝[D] z, r ∈ D := self_mem_nhdsWithin
  by_cases ht0 : t = 0
  · subst t
    have hpos : Tendsto decodeShape (𝓝[D] 0) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      refine ⟨?_, hmem.mono (fun t ht ↦ decodeShape_pos ht)⟩
      have h := (hdecode (t := 0) zero_ne_one).continuousWithinAt (s := D)
      change Tendsto decodeShape (𝓝[D] 0) (𝓝 (decodeShape 0)) at h
      simpa [decodeShape] using h
    have h : ContinuousWithinAt (closedProfile x I P) D 0 := by
      change Tendsto (closedProfile x I P) (𝓝[D] 0) (𝓝 (closedProfile x I P 0))
      rw [closedProfile, ite_eq_left rfl]
      apply (hI.comp hpos).congr'
      exact hmem.mono (fun t ht ↦ (heq t ht).symm)
    apply (continuousWithinAt_congr_set (s := insert 0 D) (t := Icc 0 1) ?_).mp h.insert
    filter_upwards [Iio_mem_nhds (show (0 : ℝ) < 1 by norm_num)] with r hr
    change r < 1 at hr
    change (r = 0 ∨ 0 < r ∧ r < 1) ↔ 0 ≤ r ∧ r ≤ 1
    constructor
    · rintro (rfl | ⟨h0,h1⟩)
      · exact ⟨le_rfl, zero_le_one⟩
      · exact ⟨h0.le, h1.le⟩
    · exact fun h ↦ h.1.eq_or_lt.elim (fun h ↦ Or.inl h.symm) (fun h ↦ Or.inr ⟨h,hr⟩)
  · by_cases ht1 : t = 1
    · subst t
      have hden : Tendsto (fun r : ℝ ↦ 1 - r) (𝓝[D] 1) (𝓝[>] 0) := by
        apply tendsto_nhdsWithin_iff.mpr
        refine ⟨?_, hmem.mono (fun r hr ↦ sub_pos.mpr hr.2)⟩
        have h : ContinuousAt (fun r : ℝ ↦ 1 - r) 1 := by fun_prop
        have h' := h.continuousWithinAt (s := D)
        change Tendsto (fun r : ℝ ↦ 1 - r) (𝓝[D] 1) (𝓝 ((1 : ℝ) - 1)) at h'
        simpa using h'
      have hinf : Tendsto decodeShape (𝓝[D] 1) atTop := by
        have h := (tendsto_inv_nhdsGT_zero.comp hden).atTop_add
          (tendsto_const_nhds (x := (-1 : ℝ)))
        apply h.congr'
        filter_upwards [self_mem_nhdsWithin] with r hr
        have hne : 1 - r ≠ 0 := sub_ne_zero.mpr hr.2.ne'
        change (1 - r)⁻¹ + -1 = r / (1 - r)
        field_simp
        ring
      have h : ContinuousWithinAt (closedProfile x I P) D 1 := by
        change Tendsto (closedProfile x I P) (𝓝[D] 1) (𝓝 (closedProfile x I P 1))
        simp only [closedProfile, one_ne_zero, ite_false, ite_true]
        exact (hP.comp hinf).congr'
          (hmem.mono (fun t ht ↦ (heq t ht).symm))
      apply (continuousWithinAt_congr_set (s := insert 1 D) (t := Icc 0 1) ?_).mp h.insert
      filter_upwards [Ioi_mem_nhds (show (0 : ℝ) < 1 by norm_num)] with r hr
      change 0 < r at hr
      change (r = 1 ∨ 0 < r ∧ r < 1) ↔ 0 ≤ r ∧ r ≤ 1
      constructor
      · rintro (rfl | ⟨h0,h1⟩)
        · exact ⟨zero_le_one, le_rfl⟩
        · exact ⟨h0.le, h1.le⟩
      · exact fun h ↦ h.2.eq_or_lt.elim Or.inl (fun h ↦ Or.inr ⟨hr,h⟩)
    · have ht' : t ∈ D := ⟨lt_of_le_of_ne ht.1 (Ne.symm ht0), lt_of_le_of_ne ht.2 ht1⟩
      have hDa : D ∈ 𝓝 t := isOpen_Ioo.mem_nhds ht'
      have hm : MapsTo decodeShape D (Ioi (0 : ℝ)) := fun _ h ↦ decodeShape_pos h
      have hh := (hc (decodeShape t) (decodeShape_pos ht')).comp
        (hdecode ht1).continuousWithinAt hm
      exact ((hh.congr heq (heq t ht')).continuousAt hDa).continuousWithinAt

/-- Values attained by finite positive exponentiated-Weibull parameters. -/
def finiteValues (x : Fin n → ℝ) : Set ℝ :=
  {v | ∃ b > 0, ∃ p ∈ Weibull.domain, ShapeFacesExpWeibull.logLik x b p.1 p.2 = v}

private theorem face_comparison {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {I P : ℝ}
    (hI : Tendsto (shapeProfile x) (𝓝[>] 0) (𝓝 I))
    (hP : Tendsto (shapeProfile x) atTop (𝓝 P)) :
    BddAbove (finiteValues x) ∧
    ((∃ v, IsGreatest (finiteValues x) v) ↔ ∃ v ∈ finiteValues x, max I P ≤ v) ∧
    ((∀ v ∈ finiteValues x, v < max I P) ↔
      IsLUB (finiteValues x) (max I P) ∧ ¬ ∃ v, IsGreatest (finiteValues x) v) := by
  let D := Ioo (0 : ℝ) 1
  let K := Icc (0 : ℝ) 1
  let L := closedProfile x I P
  have hc : ContinuousOn L K := closed_profile_continuous (shape_continuous hn hx hne) hI hP
  have hsub : D ⊆ K := Ioo_subset_Icc_self
  have hmem (t : ℝ) (ht : t ∈ D) : L t ∈ finiteValues x := by
    have hb := decodeShape_pos ht
    obtain ⟨p, hp, he, _⟩ := shape_winner hn hx hne hb
    exact ⟨_, hb, p, hp, by simpa [L, closedProfile, ht.1.ne', ht.2.ne] using he⟩
  have hcof (v : ℝ) (hv : v ∈ finiteValues x) : ∃ t ∈ D, v ≤ L t := by
    obtain ⟨b, hb, q, hq, rfl⟩ := hv
    obtain ⟨t, ht, he⟩ := encodeShape hb
    obtain ⟨p, hp, he', hm⟩ := shape_winner hn hx hne hb
    refine ⟨t, ht, ?_⟩
    simpa [L, closedProfile, ht.1.ne', ht.2.ne, he, ← he'] using hm hq
  have hcap (v : ℝ) (hv : v ∈ upperBounds (finiteValues x)) : max I P ≤ v := by
    have hbound (t : ℝ) (ht : t ∈ closure D) : L t ≤ v := by
      have ht' : t ∈ K := (closure_minimal hsub isClosed_Icc) ht
      let : (𝓝[D] t).NeBot := mem_closure_iff_nhdsWithin_neBot.mp ht
      apply le_of_tendsto ((hc t ht').mono hsub)
      filter_upwards [self_mem_nhdsWithin] with r hr
      exact hv (hmem r hr)
    have he : closure D = K := closure_Ioo (by norm_num : (0 : ℝ) ≠ 1)
    have h0 := hbound 0 (by rw [he]; exact ⟨le_rfl, zero_le_one⟩)
    have h1 := hbound 1 (by rw [he]; exact ⟨zero_le_one, le_rfl⟩)
    exact max_le (by simpa [L, closedProfile] using h0)
      (by simpa [L, closedProfile] using h1)
  refine ⟨?_, ⟨?_, ?_⟩, ⟨?_, ?_⟩⟩
  · obtain ⟨t, ht, hm⟩ := isCompact_Icc.exists_isMaxOn
      (show K.Nonempty from ⟨0, le_rfl, zero_le_one⟩) hc
    refine ⟨L t, fun v hv ↦ ?_⟩
    obtain ⟨r, hr, hvr⟩ := hcof v hv
    exact hvr.trans (hm (hsub hr))
  · rintro ⟨v, hv, hm⟩
    exact ⟨v, hv, hcap v hm⟩
  · rintro ⟨v, hv, hB⟩
    obtain ⟨t, ht, hvt⟩ := hcof v hv
    have hboundary : ∀ s ∈ K \ D, L s ≤ L t := by
      intro s hs
      have he : s = 0 ∨ s = 1 := by
        by_contra! h
        exact hs.2 ⟨lt_of_le_of_ne hs.1.1 h.1.symm, lt_of_le_of_ne hs.1.2 h.2⟩
      rcases he with rfl | rfl
      · exact (show L 0 ≤ max I P by simp [L, closedProfile]).trans (hB.trans hvt)
      · exact (show L 1 ≤ max I P by simp [L, closedProfile]).trans (hB.trans hvt)
    obtain ⟨r, hr, hmax⟩ := Generic.exists_isMaxOn_of_boundary_le hsub
      isCompact_Icc hc ht hboundary
    refine ⟨L r, hmem r hr, fun w hw ↦ ?_⟩
    obtain ⟨s, hs, hws⟩ := hcof w hw
    exact hws.trans (hmax hs)
  · intro hlt
    refine ⟨⟨fun _ hv ↦ (hlt _ hv).le, fun v hv ↦ hcap v hv⟩, ?_⟩
    rintro ⟨v, hv, hm⟩
    exact (hlt v hv).not_ge (hcap v hm)
  · rintro ⟨hL, hnot⟩ v hv
    exact lt_of_le_of_ne (hL.1 hv) (fun he ↦ hnot ⟨v, hv, he ▸ hL.1⟩)

/-- Constants give an unbounded likelihood. On every nonconstant positive
sample the finite likelihood is bounded, and a finite maximizer exists
exactly when some finite point reaches the higher optimized face. If every
finite point loses, the higher face is the unattained supremum, with both
face labels valid when the heights are equal. -/
theorem all_free_classification {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {M : ℝ} (hM : ∀ i, log (x i) ≤ M) (htop : ∃ i, log (x i) = M) :
    let N := ∃ i j, x i ≠ x j
    (¬ N → ¬ BddAbove (finiteValues x) ∧ ¬ ∃ v, IsGreatest (finiteValues x) v) ∧
    (N → ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      let I := Weibull.Inverse.logLik x p.1 p.2
      let P := physicalPowerValue x M
      BddAbove (finiteValues x) ∧
      ((∃ v, IsGreatest (finiteValues x) v) ↔ ∃ v ∈ finiteValues x, max I P ≤ v) ∧
      ((∀ v ∈ finiteValues x, v < max I P) ↔
        IsLUB (finiteValues x) (max I P) ∧ ¬ ∃ v, IsGreatest (finiteValues x) v) ∧
      ((∀ v ∈ finiteValues x, v < max I P) →
        (IsLUB (finiteValues x) I ↔ P ≤ I) ∧ (IsLUB (finiteValues x) P ↔ I ≤ P))) := by
  intro N
  constructor
  · intro hN
    let i : Fin n := ⟨0, hn⟩
    let x₀ := x i
    have hconst : x = fun _ ↦ x₀ := by
      funext j
      by_contra h
      exact hN ⟨j,i,h⟩
    have ht := Shape2ExpWeibull.constant_concentration hn (hx i) (c := 1) one_pos
    have hu : ¬ BddAbove (finiteValues x) := by
      rintro ⟨v, hv⟩
      obtain ⟨b, hb, hval⟩ := ((eventually_gt_atTop (0 : ℝ)).and
        (ht.eventually (eventually_gt_atTop v))).exists
      apply hval.not_ge
      apply hv
      rw [hconst]
      exact ⟨b, hb, (x₀, 1), ⟨hx i, one_pos⟩, rfl⟩
    exact ⟨hu, fun ⟨v, _, hv⟩ ↦ hu ⟨v, hv⟩⟩
  · intro hN
    obtain ⟨p, hp, hm, hI, hP⟩ := profile_face_limits hn hx hN hM htop
    obtain ⟨hb, hc, hf⟩ := face_comparison hn hx hN hI hP
    refine ⟨p, hp, hm, hb, hc, hf, ?_⟩
    intro hlt
    let I := Weibull.Inverse.logLik x p.1 p.2
    let P := physicalPowerValue x M
    have hL : IsLUB (finiteValues x) (max I P) := (hf.mp hlt).1
    refine ⟨⟨fun h ↦ (le_max_right I P).trans (hL.unique h).le, ?_⟩,
      ⟨fun h ↦ (le_max_left I P).trans (hL.unique h).le, ?_⟩⟩
    · intro h
      change IsLUB (finiteValues x) I
      rwa [max_eq_left h] at hL
    · intro h
      change IsLUB (finiteValues x) P
      rwa [max_eq_right h] at hL

end
end UnivariateML.EVExpWeibull

end
