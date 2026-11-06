# Free-subset inventory

All hold values must be finite and in the stated domain. "Open" below means
the existing fitter is retained, without a global certificate; it does not
mean the subset is unsupported. Every one of the 16 subsets is accounted for.
The literal condition xi=1 always means xi held exactly at one.

| Free coefficients | Result of this job | Missing certificate |
|---|---|---|
| none | Existing exact density evaluation | No optimization |
| mean | Analytic for n=2 and xi=1; exact tie stop for two maxima | General n and nonunit skew: all location modes and comparisons |
| sd | Certified for xi=1: unique root or correct collapse stop | Nonunit skew: shifted-residual scale roots and comparisons |
| nu | Open | Shape profile, nu=2 and infinite-nu tails |
| xi | Open | Skew profile, both one-sided limits, moment constraints |
| mean, sd | Open | Fixed-shape two-piece location/scale branches and collapse |
| mean, nu | Open | Location modes plus shape limits with sd held |
| mean, xi | Open | Two-piece modes/skew under held sd constraint |
| sd, nu | Open | Held-mean scale/shape branches and collapse |
| sd, xi | Open | Held-mean scale/skew branches and one-sided limits |
| nu, xi | Open | Both moment constraints and coupled shape/skew limits |
| mean, sd, nu | Certified by std when xi=1; otherwise open | Nonunit held-skew global profile and its limiting faces |
| mean, sd, xi | Open | Fixed-shape two-piece global search, half-t limits |
| mean, nu, xi | Open | Held-sd curved constraint and coupled limits |
| sd, nu, xi | Open | Held-mean curved constraint and coupled limits |
| mean, sd, nu, xi | Open; default unchanged | Complete joint stationary/escape exclusion |

No finite-grid or multistart result is promoted to a certificate. All numerical
legacy search windows and fixed legacy iteration caps remain outside the
certified paths introduced here. Changing them belongs to completion of their
algorithms, rather than silently changing the all-free default in this fallback.
