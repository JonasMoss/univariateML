# sads 0.6.5: what it has that we lack (2026-11-12)

sads (species abundance distributions) fits positive counts, usually with
zero truncation for unseen species. Its 15 `d*` functions against this
library; signatures and bodies read from the installed package.

| sads | here | verdict |
|---|---|---|
| `dls` (Fisher log-series, $N,\alpha$) | `lgser`, $\theta=N/(N+\alpha)$ | covered |
| `dpoig` (Poisson–gamma) | `nbinom` | covered |
| `dpoix` (Poisson–exponential) | `geom` | covered |
| `dpower` | `zeta` | covered |
| `dzipf` | `zipf` | covered; defects in [KNOWN_ISSUES](../../KNOWN_ISSUES.md) |
| `dpareto`, `fitexp/gamma/lnorm/weibull/geom/nbinom` | same families | covered |
| `dpoilog` | [TASK-195](../../roadmap/2026-11-11-poisson-lognormal.md) | banked |
| `trunc=` on every fitter, `dtrunc` | zt families; [modifiers](../../roadmap/2026-11-11-zero-and-truncation-modifiers.md) | covered / planned |
| `fit*C` (binned abundance classes) | interval censoring, TASK-142 | planned |
| `dbs`, `drbs` (broken stick) | — | no free parameter: $N$, $S$ come from the data; `dbs` is $\mathrm{Beta}(1,S-1)$ on $[0,N]$ |
| `dgs` (geometric series) | geometric + truncation to $\{1..S\}$ | covered by modifiers; also a limit of Zipf–Mandelbrot |
| `dpowbend` | — | [banked, preferred](../../roadmap/2026-11-12-powbend.md) |
| `dmand` (Zipf–Mandelbrot) | — | [banked](../../roadmap/2026-11-12-zipf-mandelbrot.md) |
| `dmzsm` | — | [banked, low](../../roadmap/2026-11-12-mzsm-volkov.md) |
| `dvolkov` | — | not proposed; see the same file |

Power-bend checks: [`powbend_check.py`](2026-11-12-sads-survey/powbend_check.py).
