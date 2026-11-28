module

public import UnivariateML.Family.Trunc3CMB

/-! CMB positive-window size selection and the public odds fiber. -/

@[expose] public section

open Set Real

namespace UnivariateML.Trunc5CMB

noncomputable section

def weight (N k : ℕ) (r ν : ℝ) : ℝ :=
  r ^ k * (N.choose k : ℝ) ^ ν

/-- Positive-window likelihood for the sample `(1,2)`. The endpoint
probabilities have zero likelihood: zero has no positive component, and
one is incompatible with the mixed sample. -/
def mixedLikelihood (N : ℕ) (p ν : ℝ) : ℝ :=
  if 2 ≤ N ∧ 0 < p ∧ p < 1 then
    let r := p / (1-p)
    let Z := ∑ k ∈ Finset.Icc 1 N, weight N k r ν
    weight N 1 r ν * weight N 2 r ν / Z^2
  else 0

private theorem weight_pos {N k : ℕ} (hk : k ≤ N) {r : ℝ} (hr : 0 < r) (ν : ℝ) :
    0 < weight N k r ν :=
  mul_pos (pow_pos hr _) (rpow_pos_of_pos (by exact_mod_cast Nat.choose_pos hk) _)

private theorem mixed_bound (N : ℕ) (p ν : ℝ) :
    mixedLikelihood N p ν ≤ 1/4 ∧
      (mixedLikelihood N p ν = 1/4 ↔
        N = 2 ∧ 0 < p ∧ p < 1 ∧ p / (1-p) = (2 : ℝ)^ν) := by
  classical
  unfold mixedLikelihood
  split_ifs with hchart
  · obtain ⟨hN,hp⟩ := hchart
    let r := p / (1-p)
    have hr : 0 < r := div_pos hp.1 (sub_pos.mpr hp.2)
    let A := Finset.Icc 1 N
    let a := weight N 1 r ν
    let b := weight N 2 r ν
    let Z := ∑ k ∈ A, weight N k r ν
    have ha : 0 < a := weight_pos (by omega) hr _
    have hb : 0 < b := weight_pos hN hr _
    have hsub : ({1,2} : Finset ℕ) ⊆ A := by
      intro k hk
      simp only [Finset.mem_insert, Finset.mem_singleton] at hk
      rcases hk with rfl | rfl <;> exact Finset.mem_Icc.mpr ⟨by omega, by omega⟩
    have hpair : (∑ k ∈ ({1,2} : Finset ℕ), weight N k r ν) = a+b := by
      simp [a,b]
    have hn (k : ℕ) (hk : k ∈ A) : 0 ≤ weight N k r ν :=
      (weight_pos (Finset.mem_Icc.mp hk).2 hr _).le
    have hZ : a+b ≤ Z := hpair ▸ Finset.sum_le_sum_of_subset_of_nonneg hsub (fun k hk _ ↦ hn k hk)
    have hZpos : 0 < Z := (add_pos ha hb).trans_le hZ
    have hh : (a+b)^2 ≤ Z^2 := by
      have hpZ : 0 ≤ Z + (a+b) := by positivity
      nlinarith [mul_nonneg (sub_nonneg.mpr hZ) hpZ]
    change a*b/Z^2 ≤ 1/4 ∧ (a*b/Z^2 = 1/4 ↔ _)
    refine ⟨(div_le_iff₀ (sq_pos_of_pos hZpos)).mpr (by nlinarith [sq_nonneg (a-b)]), ?_⟩
    constructor
    · intro h
      have he := (div_eq_iff (pow_ne_zero 2 hZpos.ne')).mp h
      have habZ : Z = a+b := by
        by_contra! hne
        have hlt := lt_of_le_of_ne hZ (Ne.symm hne)
        have hpZ : 0 < Z + (a+b) := by positivity
        nlinarith [mul_pos (sub_pos.mpr hlt) hpZ, sq_nonneg (a-b)]
      have hab : a = b := by
        rw [habZ] at he
        nlinarith [sq_nonneg (a-b)]
      have htwo : N = 2 := by
        by_contra! hne
        have H : (∑ k ∈ ({1,2} : Finset ℕ), weight N k r ν) < Z :=
          Finset.sum_lt_sum_of_subset hsub
            (Finset.mem_Icc.mpr ⟨by omega, by omega⟩ : 3 ∈ A) (by decide)
            (weight_pos (by omega) hr _) (fun k hk _ ↦ hn k hk)
        rw [hpair, habZ] at H
        exact (lt_irrefl _) H
      refine ⟨htwo,hp.1,hp.2,?_⟩
      subst N
      change weight 2 1 r ν = weight 2 2 r ν at hab
      norm_num [weight, Nat.choose] at hab
      exact (mul_left_cancel₀ hr.ne' (show r * r = r * (2 : ℝ)^ν by nlinarith))
    · rintro ⟨rfl,_,_,hridge⟩
      have hA : A = {1,2} := by decide
      have hsum : Z = a+b := by simp [Z,hA,a,b]
      have hab : a = b := by
        change weight 2 1 r ν = weight 2 2 r ν
        norm_num [weight, Nat.choose]
        change r = (2 : ℝ)^ν at hridge
        rw [← hridge]
        ring
      apply (div_eq_iff (pow_ne_zero 2 hZpos.ne')).mpr
      rw [hsum, hab]
      ring
  · refine ⟨by norm_num, iff_of_false (by norm_num) ?_⟩
    rintro ⟨rfl,hp,hp',_⟩
    exact hchart ⟨le_rfl,hp,hp'⟩

/-- CMB, all free, `zero:truncated`, uniqueness. For the positive sample
`(1,2)`, every maximizing design has size two and the public probability
has odds (2^ν). All finite dispersions are allowed on this ridge. -/
theorem positive_one_two {cap : ℕ} (hcap : 2 ≤ cap) {N : ℕ} {p ν : ℝ} :
    ((N,p,ν) ∈ {q : ℕ × ℝ × ℝ | q.1 ≤ cap ∧ q.2.1 ∈ Icc 0 1} ∧
      IsMaxOn (fun q : ℕ × ℝ × ℝ ↦ mixedLikelihood q.1 q.2.1 q.2.2)
        {q | q.1 ≤ cap ∧ q.2.1 ∈ Icc 0 1} (N,p,ν)) ↔
      N = 2 ∧ 0 < p ∧ p < 1 ∧ p / (1-p) = (2 : ℝ)^ν := by
  have hw : mixedLikelihood 2 (1/2) 0 = 1/4 :=
    (mixed_bound _ _ _).2.mpr ⟨rfl, by norm_num, by norm_num, by norm_num⟩
  constructor
  · rintro ⟨_,H⟩
    have h := H (a := (2,1/2,0)) ⟨hcap, by norm_num⟩
    change mixedLikelihood 2 (1/2) 0 ≤ mixedLikelihood N p ν at h
    rw [hw] at h
    exact (mixed_bound _ _ _).2.mp (le_antisymm (mixed_bound _ _ _).1 h)
  · rintro ⟨rfl,hp,hp',h⟩
    refine ⟨⟨hcap,hp.le,hp'.le⟩, ?_⟩
    intro q _
    change mixedLikelihood q.1 q.2.1 q.2.2 ≤ mixedLikelihood 2 p ν
    rw [(mixed_bound _ _ _).2.mpr ⟨rfl,hp,hp',h⟩]
    exact (mixed_bound _ _ _).1

open Classical in
/-- CMB conditional sample likelihood at a fixed size, on its effective
carrier. The odds tags represent the true parent endpoints. -/
def sliceLikelihood {n : ℕ} (x : Fin n → ℕ) (A : Finset ℕ) (N : ℕ)
    (p : Sum ℝ Bool × ℝ) : ℝ :=
  if ∀ i, x i ∈ A then
    match p.1 with
    | .inl η =>
      let w := fun k : ℕ ↦ exp (η*k+p.2*log (N.choose k : ℝ))
      ∏ i, w (x i) / ∑ k ∈ A, w k
    | .inr b =>
      let e := if b then N else 0
      if e ∈ A ∧ ∀ i, x i = e then 1 else 0
  else 0

/-- CMB, size free (any holds on odds and dispersion), `truncate:known`
and `zero:truncated`, criterion and uniqueness. Given the slice heights
and exact sets reaching them, finite size comparison keeps every tied
size and every attaining coefficient in each tied slice. A tied excluded
height remains a greatest height, even when another size attains it. -/
theorem finite_size_comparison {n : ℕ} (x : Fin n → ℕ)
    (S : Finset ℕ) (hS : S.Nonempty) (A : ℕ → Finset ℕ)
    (D roots : ℕ → Set (Sum ℝ Bool × ℝ)) (H : ℕ → ℝ)
    (hH : ∀ N ∈ S, IsLUB (sliceLikelihood x (A N) N '' D N) (H N))
    (hroot : ∀ N ∈ S, roots N = {p ∈ D N | sliceLikelihood x (A N) N p = H N}) :
    let D' := {p : ℕ × (Sum ℝ Bool × ℝ) | p.1 ∈ S ∧ p.2 ∈ D p.1}
    let L' := fun p : ℕ × (Sum ℝ Bool × ℝ) ↦ sliceLikelihood x (A p.1) p.1 p.2
    ∃ M ∈ S, IsMaxOn H (↑S : Set ℕ) M ∧ IsLUB (L' '' D') (H M) ∧
      (∀ N p, ((N,p) ∈ D' ∧ IsMaxOn L' D' (N,p)) ↔
        N ∈ S ∧ H N = H M ∧ p ∈ roots N) ∧
      ((∃ p ∈ D', IsMaxOn L' D' p) ↔ ∃ N ∈ S, H N = H M ∧ (roots N).Nonempty) := by
  intro D' L'
  obtain ⟨M,hM,Hmax⟩ := S.exists_max_image H hS
  have hbest : IsMaxOn H (↑S : Set ℕ) M := fun _ h ↦ Hmax _ h
  have hsup : IsLUB (L' '' D') (H M) := by
    constructor
    · rintro _ ⟨⟨N,p⟩,⟨hN,hp⟩,rfl⟩
      exact ((hH N hN).1 ⟨p,hp,rfl⟩).trans (hbest hN)
    · intro B hB
      apply (hH M hM).2
      rintro _ ⟨p,hp,rfl⟩
      exact hB ⟨(M,p),⟨hM,hp⟩,rfl⟩
  have hf (N : ℕ) (p : Sum ℝ Bool × ℝ) : ((N,p) ∈ D' ∧ IsMaxOn L' D' (N,p)) ↔
      N ∈ S ∧ H N = H M ∧ p ∈ roots N := by
    constructor
    · rintro ⟨⟨hN,hp⟩,hmax⟩
      obtain ⟨he,hNmax⟩ :=
        (Trunc3CMB.size_slice_winners (S := (↑S : Set ℕ)) hH hN hp).mp hmax
      exact ⟨hN,le_antisymm (hbest hN) (hNmax hM),(hroot N hN).symm ▸ ⟨hp,he⟩⟩
    · rintro ⟨hN,hEq,hr⟩
      have hp : p ∈ {q ∈ D N | sliceLikelihood x (A N) N q = H N} := hroot N hN ▸ hr
      exact ⟨⟨hN,hp.1⟩,(Trunc3CMB.size_slice_winners
        (S := (↑S : Set ℕ)) hH hN hp.1).mpr ⟨hp.2,fun _ h ↦ (hbest h).trans_eq hEq.symm⟩⟩
  refine ⟨M,hM,hbest,hsup,hf,?_⟩
  constructor
  · rintro ⟨⟨N,p⟩,hp,Hmax⟩
    obtain ⟨hN,hEq,hp⟩ := (hf N p).mp ⟨hp,Hmax⟩
    exact ⟨N,hN,hEq,p,hp⟩
  · rintro ⟨N,hN,hEq,p,hp⟩
    exact ⟨(N,p),(hf N p).mpr ⟨hN,hEq,hp⟩⟩

end

end UnivariateML.Trunc5CMB

end
