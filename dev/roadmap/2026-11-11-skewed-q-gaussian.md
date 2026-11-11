# Skewed q-Gaussian

**Status:** banked, 2026-11-11, after a quick survey; not scheduled. The
symmetric location-scale family `qgauss` (Student t through the normal into
Pearson II, shape $\eta = 1/\nu$) is planned separately as TASK-201 to
TASK-203.

Evidence labels: **[derived]** short calculation here, **[repo]** existing
repository result, **[search]** search-result level only, full text not read.

## Question

Is there a canonical skew extension of `qgauss`, and is it worth adding?

## Answer

No single construction is canonical. Each standard one either breaks down at
one of the family's distinguished members (the normal at $\eta = 0$, the
uniform at $\eta = -1$) or turns into the whole Pearson system. The gain is
small: `sstd`, `st` and, later, Pearson IV cover skewed heavy tails, and the
beta families cover skewed bounded support. If a skewed `qgauss` is ever
wanted, the two-piece construction in mode and half-scale coordinates is the
cheapest.

## Candidates

Write $\psi = -g'/g$ for the symmetric standardized density $g$, so the
location score is $\psi(z)/\sigma$. For `qgauss`,
$\psi(z) = (1+\eta)z/(1+\eta z^2)$, which equals $z$ at the normal.

**Two-piece (Fernández–Steel, as in `snorm`, `sstd` and `sged`).** The two
sides of the mode have scales $\sigma\gamma$ and $\sigma/\gamma$. For
$\eta < 0$ the support becomes $[\mu - h/\gamma,\ \mu + h\gamma]$ with equal
endpoint exponents on both sides.

- Regular at symmetry. At $\gamma = 1$ the score in $\log\gamma$ is
  $\lvert z\rvert\,\psi(z)$, which is not collinear with $\psi(z)$ for any
  $\eta$, including the normal ($z\lvert z\rvert$ against $z$). [derived;
  matches the [two-piece DQM review](../research/evaluations/2026-11-04-dqm-b-report.md#two-piece-models-snorm-sstd-and-sged)
  for `snorm`, `sstd` and `sged`, repo]
- Loses identifiability at the uniform. At $\eta = -1$ the two-piece density is
  uniform on $[\mu - \sigma/\gamma,\ \mu + \sigma\gamma]$ for every $\gamma$,
  so three parameters map onto a two-parameter face, and identification is
  weak near it. [derived] A fit would need a rule for this face, such as
  holding $\gamma = 1$ at $\eta = -1$ or excluding the face when $\gamma$ is
  free.
- New escape faces as $\gamma \to 0$ or $\infty$ (half-q-Gaussian limits, as
  `ht` is for `st`). On the Student side the `sstd` certificate structure
  should carry over ([skew theory](../research/evaluations/2026-11-07-skew-theory-report.md),
  [repo]). The bounded side is new.
- Use mode and half-scale coordinates, not `sstd`'s mean and sd: the mean does
  not exist at $\eta = 1$ (Cauchy).

**Azzalini perturbation, $2g(z)G(\alpha z)$ (skew-normal and skew-t
style).** Tasaki and Koike's skew-q-Gaussian multiplies the density by a
skewing factor built from a CDF and has a maximum likelihood section. [search]
The support is unchanged.

- Singular at the normal. At $\alpha = 0$ the score in $\alpha$ is
  $2G'(0)\,z$. At $\eta = 0$ this is collinear with the location score $z$, so
  the skew-normal information singularity sits at the centre of the family.
  For $\eta \ne 0$ the scores are not collinear. [derived]
- Costs at least what `st` cost (TASK-110, TASK-116.15), plus that singular
  point.

**The Pearson system.** Type IV is the skewed type VII (t), and type I is the
skewed type II. Joining them with nonzero skewness passes through types VI, V
and III, so the skewed continuation of the whole symmetric axis is the full
four-parameter system. That is the banked
[Pearson system](2026-11-08-pearson-system.md) project (Pearson IV: TASK-160,
TASK-161; geometry: TASK-166), not an add-on. `qgauss` is its symmetric axis.
[repo]

**Other constructions.** Jones–Faddy skew t (separate tail df),
Theodossiou's and Hansen's skewed (generalized) t (two-piece variants),
sinh-arcsinh transforms, and skew Pearson VII (Zheng, Gupta and Liu; a
truncated version by Nadarajah and Gupta) all work with positive df only and
do not continue past the normal. Budini builds an asymmetric q-Gaussian from
two gamma variables with different shapes. [search]

## Trigger to revisit

- `qgauss` (TASK-203) has landed, and a user or the
  [ML-difficulties paper](2026-11-02-ml-difficulties-paper.md) wants one
  family that is skewed and runs from bounded support to heavy tails.
- The Pearson system work needs a comparison along its symmetric axis.

## Verification when resumed

- Read Tasaki and Koike and Budini in full text (current evidence is search
  snippets only).
- Check numerically that the two-piece information is nonsingular at
  $\gamma = 1$ across $\eta \in [-1, 1]$, and characterize the ridge and weak
  identification near $\eta = -1$.

## Sources

- K. Tasaki and K. Koike, "Skew-q-gaussian distribution", RIMS Kôkyûroku
  ([pdf](https://www.kurims.kyoto-u.ac.jp/~kyodo/kokyuroku/contents/pdf/1999-01.pdf)),
  in Japanese. [search]
- A. A. Budini, "Extended q-Gaussian and q-exponential distributions from
  Gamma random variables", Phys. Rev. E (2015),
  [arXiv:1501.01037](https://arxiv.org/abs/1501.01037). [search]
- A. Azzalini and A. Capitanio, "Distributions generated by perturbation of
  symmetry with emphasis on a multivariate skew t distribution",
  [arXiv:0911.2342](https://arxiv.org/abs/0911.2342). [search]
- Zheng, Gupta and Liu (2012), "A matrix variate generalization of the skew
  Pearson type VII and skew t distribution",
  [ETSU record](https://dc.etsu.edu/etsu-works/74). [search]
- [q-Gaussian literature check](../research/evaluations/2026-11-10-qgauss-lit-report.md)
  (TASK-184) for the symmetric family. [repo]
