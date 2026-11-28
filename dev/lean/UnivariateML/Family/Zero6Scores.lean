module

public import UnivariateML.Family.Zero6Inflated

/-! Interior scalar scores for held geometric and Poisson inflation. -/

@[expose] public section

open Set Real

namespace UnivariateML.Zero6Scores

open Zero6Inflated TruncZero Truncation

noncomputable section

def score (z k d : ℕ) (rho q : ℝ) : ℝ :=
  ((d : ℝ) + k) / q - (k : ℝ) / (1 - q) -
    (z : ℝ) * (1-rho) / (1-(1-rho)*q)

private def objective (z k d : ℕ) (rho q : ℝ) : ℝ :=
  ((d : ℝ) + k) * log q + (k : ℝ) * log (1-q) + (z : ℝ) * log (1-(1-rho)*q)

private theorem zero_mass_pos {rho q : ℝ} (hr : 0 ≤ rho) (hq : q ∈ Ioo 0 1) :
    0 < 1-(1-rho)*q := by
  nlinarith [mul_nonneg hr hq.1.le, hq.2]

private theorem objective_deriv (z k d : ℕ) {rho q : ℝ} (hr : 0 ≤ rho)
    (hq : q ∈ Ioo 0 1) :
    HasDerivAt (objective z k d rho) (score z k d rho q) q := by
  have hq0 : 0 < q := hq.1
  have hq1 : 0 < 1-q := sub_pos.mpr hq.2
  have h0 : 0 < 1-(1-rho)*q := zero_mass_pos hr hq
  have hd₁ := ((hasDerivAt_id q).log hq0.ne').const_mul ((d : ℝ) + k)
  have hd₂ := (((hasDerivAt_const q (1 : ℝ)).sub (hasDerivAt_id q)).log hq1.ne').const_mul (k : ℝ)
  have hd₃ := (((hasDerivAt_const q (1 : ℝ)).sub
    ((hasDerivAt_id q).const_mul (1-rho))).log h0.ne').const_mul (z : ℝ)
  convert (hd₁.add hd₂).add hd₃ using 1
  · rfl
  · unfold score
    dsimp
    ring

private theorem score_strictAnti {z k d : ℕ} (hk : 0 < k) {rho : ℝ} (hr : rho ∈ Ico 0 1) :
    StrictAntiOn (score z k d rho) (Ioo 0 1) := by
  intro p hp q hq hpq
  have hk' : (0 : ℝ) < k := by exact_mod_cast hk
  have hA : (0 : ℝ) < (d : ℝ) + k := by positivity
  have h₁ : ((d : ℝ)+k)/q < ((d : ℝ)+k)/p := div_lt_div_of_pos_left hA hp.1 hpq
  have h₂ : (k : ℝ)/(1-p) ≤ (k : ℝ)/(1-q) :=
    (div_lt_div_of_pos_left hk' (sub_pos.mpr hq.2) (by linarith)).le
  have h₃ : (z : ℝ)*(1-rho)/(1-(1-rho)*p) ≤ (z : ℝ)*(1-rho)/(1-(1-rho)*q) :=
    div_le_div_of_nonneg_left (mul_nonneg (Nat.cast_nonneg _) (sub_nonneg.mpr hr.2.le))
      (zero_mass_pos hr.1 hq) (by nlinarith [mul_pos (sub_pos.mpr hr.2) (sub_pos.mpr hpq)])
  unfold score
  linarith

/-- Geom, pi held, zero:inflated criterion and uniqueness with positives.
The maximizing continuation probability is the unique interior root
of the exact held-weight score. -/
theorem geom_held_positive_score {z k d : ℕ} (hk : 0 < k)
    {rho : ℝ} (hr : rho ∈ Ico 0 1) :
    let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
    (∃! q, q ∈ Ioo 0 1 ∧ score z k d rho q = 0) ∧
      (∀ q ∈ Ico 0 1, IsMaxOn F (Ico 0 1) q ↔
        q ∈ Ioo 0 1 ∧ score z k d rho q = 0) := by
  dsimp only
  let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
  obtain ⟨q,hq,hmax,hpos⟩ :=
    ((geom_held (z := z) (d := d) (by omega) (fun h ↦ (hk.ne' h).elim)
      ⟨hr.1,hr.2.le⟩).2 hk).mpr hr.2
  have hq0 : 0 < q := lt_of_le_of_ne hq.1 (by
    rintro rfl
    simp [inflatedGeom, effective, bernoulli, zero_pow hk.ne'] at hpos)
  have hqi : q ∈ Ioo 0 1 := ⟨hq0,hq.2⟩
  have hform (p : ℝ) :
      F p = (1-(1-rho)*p)^z * ((1-rho)*p)^k * (p^d * (1-p)^k) := by
    have he : effective (1-p) rho = 1-(1-rho)*p := by unfold effective; ring
    simp [F,inflatedGeom,bernoulli,he]
  have hlog (p : ℝ) (hp : p ∈ Ioo 0 1) :
      log (F p) = objective z k d rho p + (k : ℝ) * log (1-rho) := by
    have hA := (zero_mass_pos hr.1 hp).ne'
    have hp0 : 0 < p := hp.1
    have hp1 := (sub_pos.mpr hp.2).ne'
    have hr1 := (sub_pos.mpr hr.2).ne'
    rw [hform, log_mul (mul_ne_zero (pow_ne_zero _ hA) (pow_ne_zero _ (mul_ne_zero hr1 hp0.ne')))
      (mul_ne_zero (pow_ne_zero _ hp0.ne') (pow_ne_zero _ hp1)),
      log_mul (pow_ne_zero _ hA) (pow_ne_zero _ (mul_ne_zero hr1 hp0.ne')),
      log_mul (pow_ne_zero _ hp0.ne') (pow_ne_zero _ hp1)]
    simp only [log_pow, log_mul hr1 hp0.ne', objective]
    ring
  have hpositive (p : ℝ) (hp : p ∈ Ioo 0 1) : 0 < F p := by
    rw [hform]
    exact mul_pos (mul_pos (pow_pos (zero_mass_pos hr.1 hp) _)
      (pow_pos (mul_pos (sub_pos.mpr hr.2) hp.1) _))
      (mul_pos (pow_pos hp.1 _) (pow_pos (sub_pos.mpr hp.2) _))
  have hstationary (p : ℝ) (hp : p ∈ Ioo 0 1) (hm : IsMaxOn F (Ico 0 1) p) :
      score z k d rho p = 0 := by
    have hM : IsMaxOn (objective z k d rho) (Ioo 0 1) p := by
      intro r hr'
      have hL := (log_le_log_iff (hpositive r hr') (hpositive p hp)).mpr
        (hm ⟨hr'.1.le,hr'.2⟩)
      rw [hlog r hr', hlog p hp] at hL
      exact le_of_add_le_add_right hL
    exact (hM.isLocalMax (Ioo_mem_nhds hp.1 hp.2)).hasDerivAt_eq_zero
      (objective_deriv z k d hr.1 hp)
  have hroot := hstationary q hqi hmax
  have hu := (score_strictAnti (z := z) (d := d) hk hr).injOn
  refine ⟨⟨q,⟨hqi,hroot⟩,fun p ⟨hp,he⟩ ↦ hu hp hqi (he.trans hroot.symm)⟩,?_⟩
  intro p hp
  constructor
  · intro hm
    have hV : 0 < F p := hpos.trans_le (hm hq)
    have hp0 : 0 < p := lt_of_le_of_ne hp.1 (by
      rintro rfl
      simp [F, inflatedGeom, effective, bernoulli, zero_pow hk.ne'] at hV)
    exact ⟨⟨hp0,hp.2⟩,hstationary p ⟨hp0,hp.2⟩ hm⟩
  · rintro ⟨hi,he⟩
    rwa [hu hi hqi (he.trans hroot.symm)]

def poissonHeldScore (z k S : ℕ) (rho r : ℝ) : ℝ :=
  (S : ℝ) / r - k -
    (z : ℝ) * ((1-rho)*exp (-r)) / effective (exp (-r)) rho

private def poissonHeldLog (z k S : ℕ) (rho r : ℝ) : ℝ :=
  (z : ℝ) * log (effective (exp (-r)) rho) +
    (k : ℝ) * log (1-rho) + (S : ℝ) * log r - (k : ℝ) * r

private theorem poissonHeld_deriv (z k S : ℕ) {rho r : ℝ} (hr : rho ∈ Ico 0 1)
    (hp : 0 < r) :
    HasDerivAt (poissonHeldLog z k S rho) (poissonHeldScore z k S rho r) r := by
  have h0 : 0 < effective (exp (-r)) rho :=
    add_pos_of_nonneg_of_pos hr.1 (mul_pos (sub_pos.mpr hr.2) (exp_pos _))
  have he : HasDerivAt (fun r ↦ effective (exp (-r)) rho) ((1-rho)*(exp (-r)*(-1))) r :=
    (((hasDerivAt_id r).neg.exp).const_mul (1-rho)).const_add rho
  have h₁ := (he.log h0.ne').const_mul (z : ℝ)
  have h₂ := hasDerivAt_const r ((k : ℝ) * log (1-rho))
  have h₃ := ((hasDerivAt_id r).log hp.ne').const_mul (S : ℝ)
  have h₄ := (hasDerivAt_id r).const_mul (k : ℝ)
  convert (((h₁.add h₂).add h₃).sub h₄) using 1
  · rfl
  · unfold poissonHeldScore
    dsimp
    ring

private theorem poissonHeld_log_eq {z k S : ℕ} {rho r : ℝ}
    (hr : rho ∈ Ico 0 1) (hp : 0 < r) :
    0 < inflatedPoisson z k S (r,rho) ∧
    log (inflatedPoisson z k S (r,rho)) = poissonHeldLog z k S rho r := by
  have hA : 0 < effective (exp (-r)) rho :=
    add_pos_of_nonneg_of_pos hr.1 (mul_pos (sub_pos.mpr hr.2) (exp_pos _))
  have hB := sub_pos.mpr hr.2
  change 0 < (effective (exp (-r)) rho ^ z * (1-rho)^k) *
      (r^S * exp (-(k : ℝ)*r)) ∧ _
  refine ⟨mul_pos (mul_pos (pow_pos hA _) (pow_pos hB _))
      (mul_pos (pow_pos hp _) (exp_pos _)), ?_⟩
  change log ((effective (exp (-r)) rho ^ z * (1-rho)^k) *
    (r^S * exp (-(k : ℝ)*r))) = _
  rw [log_mul (mul_ne_zero (pow_ne_zero _ hA.ne') (pow_ne_zero _ hB.ne'))
    (mul_ne_zero (pow_ne_zero _ hp.ne') (exp_pos _).ne'),
    log_mul (pow_ne_zero _ hA.ne') (pow_ne_zero _ hB.ne'),
    log_mul (pow_ne_zero _ hp.ne') (exp_pos _).ne', log_exp]
  simp only [log_pow, poissonHeldLog]
  ring

/-- Pois, pi held, zero:inflated criterion with positives. Exactly
the roots at the top held-weight score height maximize, with all ties. -/
theorem pois_held_positive_score {z k S : ℕ} (hk : 0 < k) (hS : 0 < S)
    {rho : ℝ} (hr : rho ∈ Ico 0 1) :
    let F := fun r ↦ inflatedPoisson z k S (r,rho)
    (∃ r ∈ Ioi 0, poissonHeldScore z k S rho r = 0) ∧
    (∀ r ∈ Ioi 0, IsMaxOn F (Ioi 0) r ↔
      poissonHeldScore z k S rho r = 0 ∧
      ∀ q ∈ Ioi 0, poissonHeldScore z k S rho q = 0 → F q ≤ F r) := by
  dsimp only
  let F := fun r ↦ inflatedPoisson z k S (r,rho)
  obtain ⟨t,ht,hm⟩ := poisson_zi_positive_attainment (z := z) hk hS
    (W := {rho}) isCompact_singleton (singleton_subset_iff.mpr ⟨hr.1,hr.2.le⟩)
    (mem_singleton _) hr.2
  have ht2 : t.2 = rho := ht.2
  have htop : IsMaxOn F (Ioi 0) t.1 := by
    intro p hp
    have H : inflatedPoisson z k S (p,rho) ≤ inflatedPoisson z k S t :=
      hm ⟨hp, mem_singleton _⟩
    change F p ≤ F t.1
    simpa only [F, ← ht2, Prod.mk.eta] using H
  have hstationary (r : ℝ) (hr' : r ∈ Ioi 0) (hm' : IsMaxOn F (Ioi 0) r) :
      poissonHeldScore z k S rho r = 0 := by
    have hM : IsMaxOn (poissonHeldLog z k S rho) (Ioi 0) r := by
      intro p hp
      obtain ⟨hF, he⟩ := poissonHeld_log_eq (z := z) (k := k) (S := S) hr hp
      obtain ⟨hR, hR'⟩ := poissonHeld_log_eq (z := z) (k := k) (S := S) hr hr'
      change poissonHeldLog z k S rho p ≤ poissonHeldLog z k S rho r
      rw [← he, ← hR']
      exact (log_le_log_iff hF hR).mpr (hm' hp)
    exact (hM.isLocalMax (Ioi_mem_nhds hr')).hasDerivAt_eq_zero (poissonHeld_deriv z k S hr hr')
  have htroot := hstationary _ ht.1 htop
  refine ⟨⟨t.1,ht.1,htroot⟩,?_⟩
  intro r hr'
  exact ⟨fun h ↦ ⟨hstationary r hr' h, fun _ hp _ ↦ h hp⟩,
    fun h _ hp ↦ (htop hp).trans (h.2 _ ht.1 htroot)⟩

end

end UnivariateML.Zero6Scores

end
