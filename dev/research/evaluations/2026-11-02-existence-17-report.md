# existence-17: Azzalini skew-normal existence and prose

Completed 2026-11-02 for TASK-101.6.4.1 in `codex/existence-17`.
Tested source: `2a51746fd07cc838e78bdc4f0b8724d89ccc7aa9`, with the
uncommitted `sn/README.md` changes described here. No commits were made.

## Family result

**sn — statement:** With all coefficients free, a half-normal supremum can prevent a finite MLE; holding finite shape gives a unique fit on nonconstant data, while holding location gives an exact two-sided-residual criterion for a unique finite free-shape fit, with the constant-data exceptions stated explicitly.

**Grade:** Joint uniqueness **unknown**, recorded as `uniqueness: unknown`.
The fixed-shape and held-location results are **proved** by the short
arguments below. The general sufficient attainment condition is **argued**
by compactness and the documented shape-tail bounds. Numerical likelihood-gap
bounds do not establish uniqueness.

- With finite shape fixed, inverse scale and location divided by scale give
  a strictly concave likelihood. For nonconstant data, the Gaussian envelope
  excludes zero or infinite scale and divergent location on a likelihood
  superlevel set. This gives attainment and uniqueness. Holding scale leaves
  a strictly concave, coercive location problem even for constant data.
  Holding location instead gives a unique scale maximum whenever some
  observation differs from it.
- With location held, write `d = x - xi`, `eta = 1/omega`, and
  `beta = alpha*eta`. Apart from a constant the likelihood is
  `n*log(eta) - eta^2*sum(d^2)/2 + sum(log(Phi(beta*d)))`.
  The scale term has a unique maximum when `sum(d^2) > 0`. The shape term
  is strictly concave when some residual is nonzero. Its score crosses zero
  when both residual signs occur; with only one nonzero sign it is strictly
  monotone and has an infinite-shape supremum. Holding scale preserves
  this criterion. If every residual is zero, free scale collapses, whereas
  held scale makes shape unidentified.
- With location and scale free, constant data have unbounded likelihood,
  already along the normal member with location equal to their common value
  and scale tending to zero. Holding finite shape does not remove collapse:
  keep a fixed standardized residual while shrinking scale.
- For nonconstant data with free location and shape, a finite point strictly
  above both limiting half-normal suprema guarantees attainment. The
  Gaussian envelope bounds location and scale, and the existing tail bounds
  exclude escape through infinite shape above that level. This is a
  sufficient condition, not an asserted necessary-and-sufficient tie rule.

The revised prose explains the meaning and sign of shape, the distinction
between location/scale and mean/standard deviation, held-coefficient effects,
and the normal member's information singularity when location and shape
are both free. R-specific fitting instructions are under **R notes**.
The existing search bounds, score gates, cache rationale, rejected warm-start
and grid changes, probability evaluation, and range-portability history are
retained under **Decision history**. No numerical behavior was changed.

## Statuses, consistency and open questions

**Status changes: none.** Both half-normal statuses, their `firmness: proved`
grades, `holds: []`, and all other pre-existing machine-readable fields are
unchanged. There are no advice or hold-reason strings to synchronize, so
neither the binding nor its R test regexes needed edits. An automated
front-matter comparison confirmed that only `existence` and the two limits'
`when`/`detect` text changed.

The half-normal limiting laws are proved, but choosing a face within
`1e-8*n` is a numerical preference. It can reject a finite fit that is
slightly better than the limit. The dossier now explicitly separates that
preference from exact non-existence. Existing global-search guarantees remain
conditional on computed comparisons, with the documented practical
`2e-8*n` floating-point allowance; they are not outward-rounded intervals.
Held-location and held-shape solver checks remain numerical.

Open matters and concrete missing work:

1. **Joint uniqueness and exact ties:** no argument here proves uniqueness
   when location and shape are both free, including held scale. An analysis
   of the joint profile or a verified theorem covering these subsets and
   exact ties with the limiting suprema is still needed. Strict concavity of
   each fixed-shape inner problem does not supply that theorem.
2. **Generated R limit introduction:**
   `bindings/r/scripts/codegen_wrappers.R`, `limit_sentence()` (lines 69–86),
   distinguishes numerical limits only when `numerical: true` is present.
   The existing `sn` records omit that flag, so generated `man/mlsn.Rd`
   introduces both limits with an unconditional non-existence claim before
   the new tolerance qualification. This overstates the numerical decision.
   A follow-up should align the records' numerical flags or the generation
   rule with the tolerance-based decision. Numerical flags and generator
   scripts were explicitly outside this job's permitted edits. The proved
   limiting-law firmness itself needs no downgrade.
3. **Limit inventory coverage:** the existing two limit records do not
   enumerate constant-data scale collapse. The new existence statement
   covers it, consistently with the fitter's rejection of the free-scale
   collapse cases. Adding a machine-readable collapse record is a separate
   inventory change, outside the permitted limit-text edits here.

There is no structural held-subset exception: the failures above depend on
observations, so keeping `holds: []` agrees with A7. There is no fit-domain
restriction to reconcile. The original blanket information-singularity
sentence was qualified by the relevant free subset.

## References checked

The existing bibliography was retained. Two web search queries verified
bibliographic identity; neither paper is cited as a joint uniqueness theorem.

- Azzalini, A. (1985). A class of distributions which includes the normal
  ones. *Scandinavian Journal of Statistics*, **12**, 171–178.
  [Author's publication list](https://www.azzalini.it/SN/abstracts.html).
- Sartori, N. (2006). Bias prevention of maximum likelihood estimates for
  scalar skew normal and skew t distributions. *Journal of Statistical
  Planning and Inference*, **136**, 4259–4275.
  [Institutional record](https://iris.unive.it/handle/10278/14585) and
  [bibliographic record with pagination](https://pascal-francis.inist.fr/vibad/index.php?action=getRecordDetail&idt=18054760).

## Validation

All runs were local on the workstation, Ubuntu 26.04.1 LTS, using
R 4.6.1 (`/opt/R/4.6.1/bin/Rscript`) and GNU C++ 15.2.0.
Every compute command used `/tmp/umvl-cpu/run` or `run4`; those launchers
apply `nice -n 10` and single-thread BLAS/OpenMP settings. Heavy commands
reserved four slots. Queue waits were allowed to finish without interruption.
Temporary files and logs stayed under `build/tmp` and `build/scratch`.

Environment setup, from the worktree root:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR" build/scratch
```

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Pass, exit 0. |
| Requested targeted R command below | Pass: FAIL 0, WARN 0, SKIP 3, PASS 7965. The filter also matches `snorm`; all three skips require the full tier. |
| Requested `tools::checkRd` loop, through `run` | Pass, exit 0 and no output across all `man/ml*.Rd`. |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | Pass: 99 families derived. |
| Front-matter comparison against tested HEAD | Pass: all pre-existing metadata except limit prose unchanged. |
| `/tmp/umvl-cpu/run4 just relations` | Pass: 157 edges, 0 failed. |
| Requested control-character grep on `sn/README.md` | Pass: no output. No bracket-style display math. |
| `/tmp/umvl-cpu/run4 just cpp-test` | Pass: 106/106 dev shards; 0 failures; ctest elapsed 78.61 seconds. |
| Final `/tmp/umvl-cpu/run4 just test` | Pass: FAIL 0, WARN 0, SKIP 151, PASS 20438; R test elapsed 136.962 seconds. |
| `git diff --check` | Pass. |

Targeted R command, from `univariateML-r`:

```sh
/tmp/umvl-cpu/run4 env \
  UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions \
  Rscript -e "testthat::test_local('.', filter = 'dist-(sn)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
```

Rd check, from `univariateML-r`:

```sh
/tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'
```

The final dev run's 151 skips are reported, not counted as passes. No new
simulation workloads, seeds, tolerances, goldens or test cases were introduced.
The full R tier and no-long-double suite were not run: this job changes prose
and existence documentation, not fitting, d/p/q/r or test infrastructure.
CMake emitted a nonfatal CMP0167 developer warning, and shell startup printed
missing `.cargo/env` messages; neither caused a check failure. Numerical test
passes are regression evidence, not proofs of the mathematical claims above.
