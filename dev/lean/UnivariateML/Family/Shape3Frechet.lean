module

public import UnivariateML.Family.AllFreeFrechet
public import Mathlib.Topology.Semicontinuity.Basic

set_option linter.style.header false

/-! # Fréchet with scale held

In the threshold-distance chart, the likelihood score changes sign once.
The argument allows ties, including a sample all at its minimum.
The all-free comparison covers unrestricted shape and at least half contact
on the fitted branch. The strict-sub-half Gumbel comparison remains open.
-/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.Shape3Frechet
open Frechet
noncomputable section

private theorem pos {n : ℕ} {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) {d : ℝ}
    (hd : 0 < d) (i : Fin n) : 0 < g i + d := add_pos_of_nonneg_of_pos (hg i) hd

private theorem sum_pos {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {d : ℝ} (hd : 0 < d) (p : ℝ) : 0 < powerSum g p d :=
  Finset.sum_pos (fun i _ ↦ rpow_pos_of_pos (pos hg hd i) _)
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private theorem sum_deriv {n : ℕ} {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {d : ℝ} (hd : 0 < d) (p : ℝ) :
    HasDerivAt (powerSum g p) (-p * powerSum g (p + 1) d) d := by
  unfold powerSum
  rw [Finset.mul_sum]
  refine HasDerivAt.fun_sum (A := fun i t ↦ (g i + t) ^ (-p)) (u := Finset.univ) ?_
  intro i _
  have h := (hasDerivAt_rpow_const (p := -p) (Or.inl (pos hg hd i).ne')).comp d
    ((hasDerivAt_id d).const_add (g i))
  refine h.congr_deriv ?_
  change -p * (g i + d) ^ (-p - 1) * 1 = -p * (g i + d) ^ (-(p + 1))
  rw [mul_one, show -p - 1 = -(p + 1) by ring]

private theorem moments {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {d : ℝ} (hd : 0 < d) {p : ℝ} (hp : 1 ≤ p) :
    powerSum g 2 d / powerSum g 1 d ≤ powerSum g (p + 1) d / powerSum g p d := by
  let q : Fin n → ℝ := fun i ↦ (g i + d)⁻¹
  have hq (i : Fin n) : 0 < q i := inv_pos.mpr (pos hg hd i)
  have hS (s : ℝ) : powerSum g s d = ∑ i, q i ^ s := by
    apply Finset.sum_congr rfl
    intro i _
    rw [show q i = (g i + d)⁻¹ by rfl, inv_rpow (pos hg hd i).le, rpow_neg (pos hg hd i).le]
  rw [div_le_div_iff₀ (sum_pos hn hg hd _) (sum_pos hn hg hd _)]
  simp only [hS, rpow_one]
  have hpow (i : Fin n) : q i ^ p = q i * q i ^ (p - 1) := by
    calc
      _ = q i ^ (1 + (p - 1)) := by congr 1; ring
      _ = _ := by rw [rpow_add (hq i), rpow_one]
  let f : Fin n → Fin n → ℝ := fun i j ↦
    (q i * q j) * ((q j - q i) * (q j ^ (p - 1) - q i ^ (p - 1)))
  have hf (i j : Fin n) : 0 ≤ f i j := by
    apply mul_nonneg (mul_nonneg (hq i).le (hq j).le)
    rcases le_total (q i) (q j) with h | h
    · exact mul_nonneg (sub_nonneg.mpr h)
        (sub_nonneg.mpr (rpow_le_rpow (hq i).le h (sub_nonneg.mpr hp)))
    · exact mul_nonneg_of_nonpos_of_nonpos (sub_nonpos.mpr h)
        (sub_nonpos.mpr (rpow_le_rpow (hq j).le h (sub_nonneg.mpr hp)))
  let F : Fin n → Fin n → ℝ := fun i j ↦ q i * q j ^ (p + 1) - q i ^ (2 : ℝ) * q j ^ p
  have he (i j : Fin n) : f i j = F i j + F j i := by
    have hpp (i : Fin n) : q i ^ (p + 1) = q i * (q i * q i ^ (p - 1)) := by
      rw [rpow_add (hq i), rpow_one, hpow i]
      ring
    dsimp [f, F]
    rw [hpp i, hpp j, hpow i, hpow j]
    simp only [rpow_two]
    ring
  have h := Finset.sum_nonneg (s := Finset.univ) fun i _ ↦
    Finset.sum_nonneg (s := Finset.univ) fun j _ ↦ hf i j
  simp only [he, Finset.sum_add_distrib,
    show (∑ i : Fin n, ∑ j : Fin n, F j i) = ∑ i : Fin n, ∑ j : Fin n, F i j
      from Finset.sum_comm] at h
  have hF : (∑ i : Fin n, ∑ j : Fin n, F i j) =
      (∑ i, q i) * (∑ j, q j ^ (p + 1)) - (∑ i, q i ^ (2 : ℝ)) * (∑ j, q j ^ p) := by
    simp only [F, Finset.sum_sub_distrib, ← Finset.mul_sum, ← Finset.sum_mul]
  rw [hF] at h
  linarith

private theorem ratio_deriv {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {d p : ℝ} (hd : 0 < d) :
    HasDerivAt (fun e ↦ log (powerSum g p e) - log (powerSum g 1 e))
      (-p * (powerSum g (p + 1) d / powerSum g p d) + powerSum g 2 d / powerSum g 1 d)
      d := by
  have h := ((sum_deriv hg hd p).log (sum_pos hn hg hd p).ne').sub
    ((sum_deriv hg hd 1).log (sum_pos hn hg hd 1).ne')
  refine h.congr_deriv ?_
  norm_num [mul_div_assoc]
  ring

private theorem ratio_decreasing {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {p : ℝ} (hp : 1 < p) :
    StrictAntiOn (fun e ↦ powerSum g p e / powerSum g 1 e) (Ioi 0) := by
  have h : StrictAntiOn (fun e ↦ log (powerSum g p e) - log (powerSum g 1 e)) (Ioi 0) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    · exact fun e he ↦ (ratio_deriv hn hg he).continuousAt.continuousWithinAt
    · intro e he
      rw [interior_Ioi] at he
      rw [(ratio_deriv hn hg he).deriv]
      have ha := moments hn hg he hp.le
      have hb := div_pos (sum_pos hn hg he (p + 1)) (sum_pos hn hg he p)
      nlinarith only [ha, mul_pos (sub_pos.mpr hp) hb]
  intro d hd e he hde
  have H := h hd he hde
  have hD := div_pos (sum_pos hn hg hd p) (sum_pos hn hg hd 1)
  have hE := div_pos (sum_pos hn hg he p) (sum_pos hn hg he 1)
  apply (log_lt_log_iff hE hD).mp
  simpa only [log_div (sum_pos hn hg hd p).ne' (sum_pos hn hg hd 1).ne',
    log_div (sum_pos hn hg he p).ne' (sum_pos hn hg he 1).ne'] using H

private theorem likelihood_deriv {n : ℕ} {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {d k s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    HasDerivAt (fun e ↦ logLik g k e s)
      (k * s ^ k * powerSum g (k + 1) d - (k + 1) * powerSum g 1 d) d := by
  have hlog : HasDerivAt (fun e ↦ ∑ i, log (g i + e)) (powerSum g 1 d) d := by
    unfold powerSum
    refine HasDerivAt.fun_sum (A := fun i t ↦ log (g i + t)) (u := Finset.univ) ?_
    intro i _
    simpa [rpow_neg_one] using (((hasDerivAt_id d).const_add (g i)).log (pos hg hd i).ne')
  let C := (n : ℝ) * (log k + k * log s)
  have h := ((hlog.const_mul (k + 1)).const_sub C).sub ((sum_deriv hg hd k).const_mul (s ^ k))
  have he : (fun e ↦ logLik g k e s) =ᶠ[𝓝 d] fun e ↦
      C - (k + 1) * (∑ i, log (g i + e)) - s ^ k * powerSum g k e := by
    filter_upwards [Ioi_mem_nhds hd] with e he
    unfold logLik logDensity powerSum C
    simp only [log_div (pos hg he _).ne' hs.ne',
      div_rpow (pos hg he _).le hs.le, rpow_neg hs.le, div_inv_eq_mul,
      Finset.sum_sub_distrib, ← Finset.mul_sum, ← Finset.sum_mul,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    ring
  refine (h.congr_of_eventuallyEq he).congr_deriv ?_
  ring

private theorem ratio_bounds {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {i : Fin n} (hi : g i = 0) {d k : ℝ}
    (hd : 0 < d) (hk : 0 < k) :
    d ^ (-k) / n ≤ powerSum g (k + 1) d / powerSum g 1 d ∧
      powerSum g (k + 1) d / powerSum g 1 d ≤ d ^ (-k) := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hS := sum_pos hn hg hd 1
  have hpow (j : Fin n) : (g j + d) ^ (-k) ≤ d ^ (-k) :=
    rpow_le_rpow_of_nonpos hd (le_add_of_nonneg_left (hg j)) (neg_nonpos.mpr hk.le)
  have hsum : powerSum g 1 d ≤ (n : ℝ) * d⁻¹ := by
    unfold powerSum
    calc
      _ ≤ ∑ _ : Fin n, d⁻¹ := by
        apply Finset.sum_le_sum
        intro j _
        simp only [rpow_neg_one]
        exact inv_anti₀ hd (le_add_of_nonneg_left (hg j))
      _ = _ := by simp [nsmul_eq_mul]
  have hmin : d ^ (-(k + 1)) ≤ powerSum g (k + 1) d := by
    simpa only [powerSum, hi, zero_add] using
      (Finset.single_le_sum (f := fun j : Fin n ↦ (g j + d) ^ (-(k + 1)))
        (fun j _ ↦ rpow_nonneg (pos hg hd j).le _) (Finset.mem_univ i))
  have hr (t : ℝ) (ht : 0 < t) : t ^ (-(k + 1)) = t ^ (-k) * t ^ (-(1 : ℝ)) := by
    rw [← rpow_add ht]
    congr 1
    ring
  constructor
  · apply (le_div_iff₀ hS).2
    calc
      _ ≤ (d ^ (-k) / n) * ((n : ℝ) * d⁻¹) :=
        mul_le_mul_of_nonneg_left hsum (by positivity)
      _ = d ^ (-(k + 1)) := by
        rw [hr d hd, rpow_neg_one]
        field_simp
      _ ≤ _ := hmin
  · apply (div_le_iff₀ hS).2
    unfold powerSum
    rw [Finset.mul_sum]
    apply Finset.sum_le_sum
    intro j _
    rw [hr _ (pos hg hd j)]
    exact mul_le_mul_of_nonneg_right (hpow j) (rpow_nonneg (pos hg hd j).le _)

/-- Every nonempty Fréchet sample, including constants, has a unique
location optimum with positive shape and positive scale held. -/
theorem held_shape_scale_location_mle {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    ∃! d, d ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun e ↦ logLik g k e s) (Ioi 0) d := by
  obtain ⟨i, hi⟩ := hz
  let R := fun e ↦ powerSum g (k + 1) e / powerSum g 1 e
  let T := (k + 1) / (k * s ^ k)
  have hks : 0 < k * s ^ k := mul_pos hk (rpow_pos_of_pos hs _)
  have hT : 0 < T := div_pos (by positivity) hks
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hR := ratio_decreasing hn hg (show 1 < k + 1 by linarith)
  have h0lim : Tendsto (fun d : ℝ ↦ d ^ (-k) / (n : ℝ)) (𝓝[>] 0) atTop :=
    Filter.Tendsto.atTop_div_const hnR (tendsto_rpow_neg_nhdsGT_zero (neg_lt_zero.mpr hk))
  have h0 := h0lim.eventually (eventually_gt_atTop T)
  obtain ⟨l, hl, hlt⟩ := (eventually_mem_nhdsWithin.and h0).exists
  have hlow : T < R l := hlt.trans_le (ratio_bounds hn hg hi hl hk).1
  have hInf := (tendsto_rpow_neg_atTop hk).eventually (Iio_mem_nhds hT)
  obtain ⟨r, hr, hrt⟩ := ((eventually_gt_atTop l).and hInf).exists
  have hrP : 0 < r := hl.trans hr
  have hhigh : R r < T := (ratio_bounds hn hg hi hrP hk).2.trans_lt hrt
  have hc : ContinuousOn R (Icc l r) := by
    intro e he
    have he' := hl.trans_le he.1
    exact (((sum_deriv hg he' (k + 1)).continuousAt).div
      (sum_deriv hg he' 1).continuousAt (sum_pos hn hg he' _).ne').continuousWithinAt
  obtain ⟨d, hd, hroot⟩ := intermediate_value_Icc' hr.le hc ⟨hhigh.le, hlow.le⟩
  have hdP := hl.trans_le hd.1
  have hsDer (e : ℝ) (he : 0 < e) :
      HasDerivAt (fun e ↦ logLik g k e s)
        (powerSum g 1 e * (k * s ^ k * (R e - T))) e := by
    refine (likelihood_deriv hg he hs).congr_deriv ?_
    dsimp [R, T]
    field_simp [(sum_pos hn hg he 1).ne', hks.ne']
  have hm₁ : StrictMonoOn (fun e ↦ logLik g k e s) (Ioc 0 d) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioc 0 d)
    · exact fun e he ↦ (hsDer e he.1).continuousAt.continuousWithinAt
    · intro e he
      rw [interior_Ioc] at he
      rw [(hsDer e he.1).deriv]
      exact mul_pos (sum_pos hn hg he.1 _) (mul_pos hks
        (sub_pos.mpr (hroot ▸ hR he.1 hdP he.2)))
  have hm₂ : StrictAntiOn (fun e ↦ logLik g k e s) (Ici d) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici d)
    · exact fun e he ↦ (hsDer e (hdP.trans_le he)).continuousAt.continuousWithinAt
    · intro e he
      rw [interior_Ici] at he
      have heP := hdP.trans he
      rw [(hsDer e heP).deriv]
      exact mul_neg_of_pos_of_neg (sum_pos hn hg heP _) (mul_neg_of_pos_of_neg hks
        (sub_neg.mpr (hroot ▸ hR hdP heP he)))
  have hstrict (e : ℝ) (he : 0 < e) (hne : e ≠ d) : logLik g k e s < logLik g k d s := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hm₁ ⟨he, h.le⟩ ⟨hdP, le_rfl⟩ h
    · exact hm₂ le_rfl h.le h
  refine ⟨d, ⟨hdP, fun e he ↦ ?_⟩, fun e he ↦ ?_⟩
  · by_cases h : e = d
    · simp [h]
    · exact (hstrict e he h).le
  · by_contra h
    exact (hstrict e he.1 h).not_ge (he.2 hdP)

/-- With shape and scale held the likelihood vanishes at both ends of
the location chart, including when the sample consists entirely of ties. -/
theorem held_shape_scale_location_ends {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    Tendsto (fun d ↦ logLik g k d s) (𝓝[>] 0) atBot ∧
      Tendsto (fun d ↦ logLik g k d s) atTop atBot := by
  obtain ⟨i, hi⟩ := hz
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let α := (1 + k) / k
  let B := log k - log s
  have hα : 0 < α := div_pos (by positivity) hk
  have hpoint (t : ℝ) (ht : 0 < t) :
      logDensity k s t = B + α * log ((t / s) ^ (-k)) - (t / s) ^ (-k) := by
    rw [logDensity, log_rpow (div_pos ht hs)]
    dsimp [B, α]
    field_simp
    ring
  have hbnd (u : ℝ) (hu : 0 < u) : α * log u - u ≤
      -u / 2 - α * (1 + log ((2 * α)⁻¹)) := by
    have hc : 0 < (2 * α)⁻¹ := by positivity
    have h := log_le_sub_one_of_pos (mul_pos hc hu)
    rw [log_mul hc.ne' hu.ne'] at h
    have hm := mul_le_mul_of_nonneg_left h hα.le
    have he : α * ((2 * α)⁻¹ * u) = u / 2 := by field_simp
    nlinarith only [hm, he]
  let C := B - α * (1 + log ((2 * α)⁻¹))
  constructor
  · have hsmall : Tendsto (fun d : ℝ ↦ d / s) (𝓝[>] 0) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_iff.mpr
      have h0 : Tendsto (fun d : ℝ ↦ d) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
      exact ⟨by simpa using h0.div_const s,
        eventually_mem_nhdsWithin.mono fun d hd ↦ div_pos hd hs⟩
    have hpow := (tendsto_rpow_neg_nhdsGT_zero (neg_lt_zero.mpr hk)).comp hsmall
    have hlim := tendsto_atBot_add_const_right _ ((n : ℝ) * C)
      (hpow.const_mul_atTop_of_neg (show (-1 / 2 : ℝ) < 0 by norm_num))
    refine tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [eventually_mem_nhdsWithin] with d hd
    have hpt (j : Fin n) : logDensity k s (g j + d) ≤
        C - ((g j + d) / s) ^ (-k) / 2 := by
      rw [hpoint _ (pos hg hd j)]
      dsimp only [C]
      have h := hbnd (((g j + d) / s) ^ (-k))
        (rpow_pos_of_pos (div_pos (pos hg hd j) hs) (-k))
      linarith only [h]
    have hsum := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦ hpt j
    simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul] at hsum
    have hmin : (d / s) ^ (-k) / 2 ≤ ∑ j : Fin n, ((g j + d) / s) ^ (-k) / 2 := by
      simpa only [hi, zero_add] using
        (Finset.single_le_sum (f := fun j : Fin n ↦ ((g j + d) / s) ^ (-k) / 2)
          (fun j _ ↦ div_nonneg (rpow_nonneg (div_pos (pos hg hd j) hs).le _) (by norm_num))
          (Finset.mem_univ i))
    change logLik g k d s ≤ (-1 / 2) * (d / s) ^ (-k) + (n : ℝ) * C
    unfold logLik
    linarith only [hsum, hmin]
  · have hlarge := tendsto_log_atTop.comp
      (Filter.Tendsto.atTop_div_const hs (tendsto_id : Tendsto id atTop atTop))
    have hlim := tendsto_atBot_add_const_right _ ((n : ℝ) * B)
      (hlarge.const_mul_atTop_of_neg (show -(n : ℝ) * (1 + k) < 0 by nlinarith))
    refine tendsto_atBot_mono' _ ?_ hlim
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with d hd
    have hpt (j : Fin n) : logDensity k s (g j + d) ≤ B - (1 + k) * log (d / s) := by
      have h := log_le_log (div_pos hd hs)
        (div_le_div_of_nonneg_right (le_add_of_nonneg_left (hg j)) hs.le)
      unfold logDensity
      dsimp only [B]
      nlinarith only [mul_le_mul_of_nonneg_left h (show 0 ≤ 1 + k by positivity),
        rpow_nonneg (div_pos (pos hg hd j) hs).le (-k)]
    have hsum := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦ hpt j
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hsum
    change logLik g k d s ≤ -(n : ℝ) * (1 + k) * log (d / s) + (n : ℝ) * B
    unfold logLik
    linarith only [hsum]

private theorem conditional_location_bound {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {k d : ℝ} (hk : 1 ≤ k) (hd : 0 < d)
    (hm : IsMaxOn (fun e ↦ logLik g k e 1) (Ioi 0) d) :
    -log (2 * n) / k ≤ log d ∧ d < 1 := by
  obtain ⟨i, hi⟩ := hz
  have hk0 : 0 < k := zero_lt_one.trans_le hk
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hsc := (hm.isLocalMax (Ioi_mem_nhds hd)).hasDerivAt_eq_zero
    (likelihood_deriv hg hd (s := 1) one_pos)
  rw [one_rpow, mul_one, sub_eq_zero] at hsc
  have hR : powerSum g (k + 1) d / powerSum g 1 d = (k + 1) / k := by
    rw [div_eq_div_iff (sum_pos hn hg hd 1).ne' hk0.ne']
    linarith only [hsc]
  obtain ⟨hlo, hhi⟩ := ratio_bounds hn hg hi hd hk0
  rw [hR] at hlo hhi
  have htwo : (k + 1) / k ≤ 2 := (div_le_iff₀ hk0).2 (by linarith)
  have hpow : d ^ (-k) ≤ 2 * (n : ℝ) := (div_le_iff₀ hnR).1 (hlo.trans htwo)
  have hl := log_le_log (rpow_pos_of_pos hd _) hpow
  rw [log_rpow hd] at hl
  refine ⟨(div_le_iff₀ hk0).2 (by linarith only [hl]), ?_⟩
  by_contra! h
  have hsmall := rpow_le_rpow_of_nonpos (show (0 : ℝ) < 1 by norm_num) h (neg_nonpos.mpr hk0.le)
  rw [one_rpow] at hsmall
  have hgt : 1 < (k + 1) / k := (lt_div_iff₀ hk0).2 (by linarith)
  exact hgt.not_ge (hhi.trans hsmall)

private theorem normalized_uniform_high_shape {n : ℕ} (hn : 0 < n)
    {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hp : ∃ j, 0 < g j) (v : ℝ) :
    ∃ K ≥ 1, ∀ k ≥ K, ∀ e > 0, logLik g k e 1 < v := by
  obtain ⟨j, hj⟩ := hp
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hn1 : 1 ≤ (n : ℝ) := by exact_mod_cast hn
  let c := log (1 + g j / 2)
  let B := log (2 * (n : ℝ))
  let q := fun k : ℝ ↦ exp (-B / k)
  have hc : 0 < c := log_pos (by linarith)
  have hB : 0 < B := log_pos (by nlinarith)
  have hq : Tendsto q atTop (𝓝 1) := by
    have h := (tendsto_inv_atTop_zero.const_mul (-B)).rexp
    simpa [q, div_eq_mul_inv] using h
  obtain ⟨K₀, hK₀⟩ := eventually_atTop.1
    (hq.eventually (Ioi_mem_nhds (show 1 - g j / 2 < 1 by linarith)))
  let r := c / (2 * n)
  have hr : 0 < r := div_pos hc (by positivity)
  let C := (n : ℝ) * (2 * B - log r - 1)
  let K := max 1 (max K₀ ((C - v) / (c / 2) + 1))
  refine ⟨K, le_max_left _ _, ?_⟩
  intro k hk e he
  have hk1 : 1 ≤ k := (le_max_left _ _).trans hk
  have hkP : 0 < k := zero_lt_one.trans_le hk1
  obtain ⟨d, ⟨hd, hm⟩, _⟩ :=
    held_shape_scale_location_mle hn hg hz hkP (s := 1) one_pos
  have hlog := (conditional_location_bound hn hg hz hk1 hd hm).1
  have hqle : q k ≤ d := by
    change exp (-B / k) ≤ d
    rw [← exp_log hd, exp_le_exp]
    exact hlog
  have hK : K₀ ≤ k := (le_max_left K₀ _).trans ((le_max_right (1 : ℝ) _).trans hk)
  have hgap : 1 + g j / 2 ≤ g j + d := by
    have hq' : 1 - g j / 2 < q k := hK₀ k hK
    linarith only [hqle, hq']
  have hsum0 (i : Fin n) : -(1 + k) * log (g i + d) ≤ 2 * B := by
    have h := log_le_log hd (le_add_of_nonneg_left (hg i))
    have h1 : -k * log d ≤ B := by
      have hh := (div_le_iff₀ hkP).1 hlog
      linarith only [hh]
    have h2 : -log d ≤ B := by
      have hh := div_le_div_of_nonneg_left hB.le zero_lt_one hk1
      rw [div_one] at hh
      linarith only [hlog, hh, show -B / k = -(B / k) by ring]
    nlinarith only [h1, h2, mul_le_mul_of_nonneg_left h (show 0 ≤ 1 + k by positivity)]
  have hsum :
      (∑ i : Fin n, -(1 + k) * log (g i + d)) ≤ (n : ℝ) * (2 * B) - (1 + k) * c := by
    have hpt (i : Fin n) : -(1 + k) * log (g i + d) ≤
        2 * B - (if i = j then (1 + k) * c else 0) := by
      by_cases h : i = j
      · subst i
        have h := log_le_log (show 0 < 1 + g j / 2 by positivity) hgap
        simp only [ite_true]
        nlinarith only [hB.le, mul_le_mul_of_nonneg_left h (show 0 ≤ 1 + k by positivity)]
      · simpa only [h, ite_false, sub_zero] using hsum0 i
    have hs := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hpt i
    simpa [Finset.sum_sub_distrib, Finset.sum_ite_eq', nsmul_eq_mul] using hs
  have hL : logLik g k d 1 ≤ (n : ℝ) * log k + (n : ℝ) * (2 * B) - (1 + k) * c := by
    unfold logLik
    have hs (i : Fin n) := rpow_nonneg (pos hg hd i).le (-k)
    simp only [logDensity, log_one, div_one, sub_zero, Finset.sum_sub_distrib,
      Finset.sum_const, Fintype.card_fin, Finset.card_univ, nsmul_eq_mul]
    have hP : 0 ≤ ∑ i : Fin n, (g i + d) ^ (-k) := Finset.sum_nonneg fun i _ ↦ hs i
    rw [← Finset.mul_sum] at hsum
    rw [← Finset.mul_sum]
    linarith only [hsum, hP]
  have ht := log_le_sub_one_of_pos (mul_pos hr hkP)
  rw [log_mul hr.ne' hkP.ne'] at ht
  have heq : (n : ℝ) * (r * k) = c / 2 * k := by dsimp [r]; field_simp
  have hlin : logLik g k d 1 ≤ C - c / 2 * k := by
    dsimp [C]
    linarith only [hL, mul_le_mul_of_nonneg_left ht hnR.le, heq, hc.le]
  have hbig : (C - v) / (c / 2) + 1 ≤ k :=
    (le_max_right K₀ _).trans ((le_max_right (1 : ℝ) _).trans hk)
  have hstrict : C - c / 2 * k < v := by
    have h := (div_lt_iff₀ (half_pos hc)).1 ((lt_add_one _).trans_le hbig)
    linarith only [h]
  exact (hm he).trans_lt (hlin.trans_lt hstrict)

private theorem normalized_competitive_rectangle {n : ℕ} (hn : 0 < n)
    {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hp : ∃ j, 0 < g j) (v : ℝ) :
    ∃ K lo hi : ℝ, 0 < lo ∧
      ∀ k e, 1 ≤ k → 0 < e → v ≤ logLik g k e 1 →
        (k, e) ∈ Icc 1 K ×ˢ Icc lo hi := by
  obtain ⟨K, hK, hfar⟩ := normalized_uniform_high_shape hn hg hz hp v
  obtain ⟨i, hi⟩ := hz
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let C := (n : ℝ) * (1 + K)
  have hC : 0 < C := by dsimp [C]; positivity [zero_lt_one.trans_le hK]
  have hlim := (tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos).const_mul (-C)
  have hsmall : Tendsto (fun e : ℝ ↦ (-C * log e) * e) (𝓝[>] 0) (𝓝 0) := by
    simpa only [rpow_one, mul_zero, ← mul_assoc] using hlim
  have htail := tendsto_atBot_add_const_right _ ((n : ℝ) * log K)
    (tendsto_inv_nhdsGT_zero.const_mul_atTop_of_neg (show (-1 / 2 : ℝ) < 0 by norm_num))
  obtain ⟨a, ha, h₀⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp
    ((hsmall.eventually (Iio_mem_nhds (show (0 : ℝ) < 1 / 2 by norm_num))).and
      (htail.eventually (eventually_lt_atBot v)))
  let b := exp ((log K - v / n) / 2)
  refine ⟨K, min a 1, max 1 b, lt_min ha zero_lt_one, ?_⟩
  intro k e hk he hval
  have hkK : k ≤ K := le_of_not_gt fun h ↦ (hfar k h.le e he).not_ge hval
  have hkP := zero_lt_one.trans_le hk
  have hlogK := log_le_log hkP hkK
  have hform : logLik g k e 1 =
      (n : ℝ) * log k - (1 + k) * (∑ j : Fin n, log (g j + e)) - powerSum g k e := by
    simp only [logLik, logDensity, log_one, div_one, sub_zero, Finset.sum_sub_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum,
      powerSum]
  have hsum : (n : ℝ) * log e ≤ ∑ j : Fin n, log (g j + e) := by
    simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] using
      (Finset.sum_le_sum (s := Finset.univ) fun j _ ↦
        log_le_log he (le_add_of_nonneg_left (hg j)))
  have hlo : min a 1 ≤ e := by
    by_contra! h
    have he1 : e ≤ 1 := h.le.trans (min_le_right _ _)
    have hh := h₀ ⟨he, h.trans_le (min_le_left _ _)⟩
    have hbound : -C * log e < 1 / 2 / e := (lt_div_iff₀ he).2 hh.1
    have hpower : e⁻¹ ≤ powerSum g k e := by
      have h1 := rpow_le_rpow_of_exponent_ge he he1 (show -k ≤ -(1 : ℝ) by linarith)
      rw [rpow_neg_one] at h1
      refine h1.trans ?_
      simpa only [powerSum, hi, zero_add] using
        (Finset.single_le_sum (f := fun j : Fin n ↦ (g j + e) ^ (-k))
          (fun j _ ↦ rpow_nonneg (pos hg he j).le _) (Finset.mem_univ i))
    have hcoef : (1 + k) * ((n : ℝ) * log e) ≥ C * log e := by
      dsimp [C]
      nlinarith only [mul_le_mul_of_nonpos_right
        (show (1 + k) * (n : ℝ) ≤ (n : ℝ) * (1 + K) by
          rw [mul_comm (n : ℝ) (1 + K)]
          exact mul_le_mul_of_nonneg_right (add_le_add_right hkK 1) hnR.le)
        (log_nonpos he.le he1)]
    have hupper : logLik g k e 1 ≤ (n : ℝ) * log K - C * log e - e⁻¹ := by
      rw [hform]
      linarith only [mul_le_mul_of_nonneg_left hlogK hnR.le, hpower, hcoef,
        mul_le_mul_of_nonneg_left hsum (show 0 ≤ 1 + k by positivity)]
    have hh₂ := hh.2
    change -1 / 2 * e⁻¹ + (n : ℝ) * log K < v at hh₂
    linarith only [hval, hupper, hbound, hh₂, show 1 / 2 / e = (1 / 2) * e⁻¹ by ring]
  have hhi : e ≤ max 1 b := by
    by_cases he1 : e ≤ 1
    · exact he1.trans (le_max_left _ _)
    have hlog : 0 ≤ log e := log_nonneg (le_of_lt (lt_of_not_ge he1))
    have hh : v ≤ (n : ℝ) * log K - 2 * ((n : ℝ) * log e) := by
      rw [hform] at hval
      linarith only [hval, (sum_pos hn hg he k).le,
        mul_le_mul_of_nonneg_left hlogK hnR.le, mul_le_mul_of_nonneg_left hsum
          (show 0 ≤ 1 + k by positivity),
        mul_le_mul_of_nonneg_right (show 2 ≤ 1 + k by linarith) (mul_nonneg hnR.le hlog)]
    apply le_trans _ (le_max_right (1 : ℝ) b)
    change e ≤ exp ((log K - v / n) / 2)
    rw [← exp_log he, exp_le_exp]
    have h := (div_le_iff₀ hnR).2 (show v ≤ (log K - 2 * log e) * (n : ℝ) by linarith)
    linarith only [h]
  exact ⟨⟨hk, hkK⟩, hlo, hhi⟩

private theorem scale_chart {n : ℕ} (g : Fin n → ℝ) (k d s : ℝ) :
    logLik g k d s = logLik (fun i ↦ g i / s) k (d / s) 1 - (n : ℝ) * log s := by
  unfold logLik
  have h (i : Fin n) : logDensity k s (g i + d) =
      logDensity k 1 (g i / s + d / s) - log s := by
    simp only [logDensity, ← add_div, div_one, log_one, sub_zero]
    ring
  simp only [h, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

private theorem joint_continuous {n : ℕ} {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {s : ℝ} (hs : 0 < s) :
    ContinuousOn (fun p : ℝ × ℝ ↦ logLik g p.1 p.2 s) (Ici 1 ×ˢ Ioi 0) := by
  let D : Set (ℝ × ℝ) := Ici 1 ×ˢ Ioi 0
  unfold logLik
  apply continuousOn_finsetSum
  intro i _
  have hu : ContinuousOn (fun p : ℝ × ℝ ↦ (g i + p.2) / s) D :=
    (continuousOn_const.add continuous_snd.continuousOn).div_const _
  have hup (p : ℝ × ℝ) (hp : p ∈ D) : 0 < (g i + p.2) / s := div_pos (pos hg hp.2 i) hs
  have hf : ContinuousOn (fun p : ℝ × ℝ ↦ p.1) D := continuous_fst.continuousOn
  have hk (p : ℝ × ℝ) (hp : p ∈ D) : 0 < p.1 := zero_lt_one.trans_le hp.1
  have hr : ContinuousOn (fun p : ℝ × ℝ ↦ ((g i + p.2) / s) ^ (-p.1)) D :=
    hu.rpow hf.neg (fun p hp ↦ Or.inl (hup p hp).ne')
  unfold logDensity
  exact (((hf.log (fun p hp ↦ (hk p hp).ne')).sub continuousOn_const).sub
    ((continuousOn_const.add hf).mul (hu.log (fun p hp ↦ (hup p hp).ne')))).sub hr

/-- On the fitted branch with scale held, constants diverge. Every
nonconstant sample has compact positive likelihood superlevels in
the fitted shape and location gap, and attains a finite maximum. -/
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik g p.1 p.2 s
    let D : Set (ℝ × ℝ) := Ici 1 ×ˢ Ioi 0
    ((∀ i, g i = 0) → Tendsto (fun k ↦ L (k, s)) atTop atTop) ∧
    ((∃ i, 0 < g i) → (∀ v, IsCompact (D ∩ L ⁻¹' Ici v)) ∧ ∃ p ∈ D, IsMaxOn L D p) := by
  intro L D
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  constructor
  · intro h
    have he (k : ℝ) : L (k, s) = (n : ℝ) * (log k - log s - 1) := by
      simp [L, logLik, logDensity, h, hs.ne', nsmul_eq_mul]
      ring
    have hlim := (tendsto_atTop_add_const_right _ (-log s - 1)
      tendsto_log_atTop).const_mul_atTop hnR
    exact hlim.congr (by intro k; rw [he]; congr 1; ring)
  · intro hp
    have hloc (v : ℝ) : ∃ K lo hi : ℝ, 0 < lo ∧
        D ∩ L ⁻¹' Ici v ⊆ Icc 1 K ×ˢ Icc lo hi := by
      obtain ⟨K, lo, hi, hlo, h⟩ := normalized_competitive_rectangle hn
        (g := fun i ↦ g i / s) (fun i ↦ div_nonneg (hg i) hs.le)
        (by obtain ⟨i, hi⟩ := hz; exact ⟨i, by simp [hi]⟩)
        (by obtain ⟨i, hi⟩ := hp; exact ⟨i, div_pos hi hs⟩) (v + (n : ℝ) * log s)
      refine ⟨K, lo * s, hi * s, mul_pos hlo hs, ?_⟩
      rintro ⟨k, d⟩ ⟨hD, hL⟩
      have hval : v ≤ logLik g k d s := hL
      rw [scale_chart g k d s] at hval
      have hh := h k (d / s) hD.1 (div_pos hD.2 hs) (by linarith only [hval])
      exact ⟨hh.1, (le_div_iff₀ hs).1 hh.2.1, (div_le_iff₀ hs).1 hh.2.2⟩
    have hk (v : ℝ) : IsCompact (D ∩ L ⁻¹' Ici v) := by
      obtain ⟨K, lo, hi, hlo, hp⟩ := hloc v
      let R := Icc 1 K ×ˢ Icc lo hi
      have hRD : R ⊆ D := fun p hp ↦ ⟨hp.1.1, hlo.trans_le hp.2.1⟩
      have he : D ∩ L ⁻¹' Ici v = R ∩ L ⁻¹' Ici v :=
        Subset.antisymm (fun p hp' ↦ ⟨hp hp', hp'.2⟩) (fun p hp' ↦ ⟨hRD hp'.1, hp'.2⟩)
      rw [he]
      exact ((joint_continuous hg hs).mono hRD).upperSemicontinuousOn.isCompact_inter_preimage_Ici
        (isCompact_Icc.prod isCompact_Icc) v
    refine ⟨hk, ?_⟩
    let p₀ : ℝ × ℝ := (1, s)
    have hp₀ : p₀ ∈ D ∩ L ⁻¹' Ici (L p₀) := ⟨⟨le_rfl, hs⟩, le_rfl⟩
    obtain ⟨p, hp, hm⟩ := (hk (L p₀)).exists_isMaxOn ⟨p₀, hp₀⟩
      ((joint_continuous hg hs).mono inter_subset_left)
    refine ⟨p, hp.1, fun q hq ↦ ?_⟩
    by_cases h : L p₀ ≤ L q
    · exact hm ⟨hq, h⟩
    · exact (le_of_not_ge h).trans (hm hp₀)

/-- The fitted held-scale Fréchet likelihood attains exactly for
nonconstant samples. -/
theorem held_scale_attains_iff {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik g p.1 p.2 s
    let D : Set (ℝ × ℝ) := Ici 1 ×ˢ Ioi 0
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ i j, g i ≠ g j := by
  intro L D
  obtain ⟨hconst, hfit⟩ := held_scale_classification hn hg hz hs
  obtain ⟨j, hj⟩ := hz
  constructor
  · rintro ⟨p, _, hm⟩
    by_contra! he
    have h0 (i : Fin n) : g i = 0 := (he i j).trans hj
    have ht := (hconst h0).eventually (eventually_gt_atTop (L p))
    obtain ⟨k, hk, hgt⟩ := ((eventually_ge_atTop (1 : ℝ)).and ht).exists
    exact hgt.not_ge (hm (show (k, s) ∈ D from ⟨hk, hs⟩))
  · rintro ⟨i, k, hik⟩
    have hp : ∃ i, 0 < g i := by
      by_contra! h
      exact hik ((le_antisymm (h i) (hg i)).trans (le_antisymm (h k) (hg k)).symm)
    exact (hfit hp).2

/-- Unrestricted Fréchet diverges. On the fitted branch, a minimum
majority also diverges, while exact half contact has an unattained
finite supremum reached by the shape-one collapse. -/
theorem all_free_collapse_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    let V := {v | ∃ k ≥ 1, ∃ p ∈ domain, logLik g k p.1 p.2 = v}
    let B := (n : ℝ) * (log 2 - 1) - 2 * ∑ i : Fin n, log (g i)
    (¬ BddAbove {v | ∃ k > 0, ∃ p ∈ domain, logLik g k p.1 p.2 = v}) ∧
    (n < 2 * minimumCount g → ¬ BddAbove V) ∧
    (n = 2 * minimumCount g → IsLUB V B ∧
      (∀ v ∈ V, v < B) ∧ Tendsto (profile g 1) (𝓝[>] 0) (𝓝 B)) := by
  refine ⟨AllFreeFrechet.unrestricted_unbounded hn hg hz,
    AllFreeFrechet.majority_unbounded hn hg hz, ?_⟩
  intro hc
  obtain ⟨hB, hs, ht⟩ := AllFreeFrechet.half_minimum_supremum hn hg hc
  exact ⟨hB, (by rintro v ⟨k, hk, p, hp, rfl⟩; exact hs k hk p hp), ht⟩

end
end UnivariateML.Shape3Frechet
end
