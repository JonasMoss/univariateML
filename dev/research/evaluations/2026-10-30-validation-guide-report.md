# Validation guide, TASK-101.6.1

Source: `3e86486a5011a85efee0ea60028c0cb6529044e3`, branch
`codex/validation-guide`, with the documentation and test-comment changes in
this worktree. No numerical code, default fits or goldens changed.

## Checks run

Local Ubuntu 26.04.1 x86_64, GCC 15.2.0, R 4.6.1, BH 1.90.0.1
(CMake Boost headers from BH), Python 3.14.5, mpmath 1.3.0, SciPy 1.18.0,
SymPy 1.14.0, NumPy 2.4.6 and PyYAML 6.0.3. Computations used the slot
lock, BLAS/OpenMP one thread, `TMPDIR` and `XDG_RUNTIME_DIR` in `build/tmp`;
long commands used `nice -n 10`. Logs are in `build/scratch/`.

- `run4 nice -n 10 just cpp-test`: 6/6 ctest targets passed; distribution
  test ran 176.84 s. All family fixtures and seeded truth sampler checks used
  their source-defined tolerances and seeds; see the guide.
- `run distribution_tests --test-case='exp*'`: 4 cases and 1,695 assertions
  passed; 918 other cases filtered out. `run ctest --test-dir
  univariateML-cpp/build -R '^tails_tests$' --output-on-failure`: 1/1 passed.
- `run4 nice -n 10 just regen`: exit 0. Roxygen printed 976 unresolved-link
  diagnostics during generation; these are not testthat failures and need
  separate investigation. Initial `just cpp-test` could not create a temporary
  directory under read-only `/run/user/1000`; setting `XDG_RUNTIME_DIR` to
  `build/tmp` resolved it, and the complete rerun passed.
- `run4 nice -n 10 env NOT_CRAN=true UMVL_DISTRIBUTIONS_DIR=<dossiers>
  Rscript -e 'testthat::test_local("univariateML-r", filter="dist-exp",
  stop_on_failure=TRUE)'`: FAIL 0, WARN 0, SKIP 0, PASS 42. Equivalent targeted
  `run` commands for `generated-dpqr` and `parameter_infrastructure` gave
  respectively 11,108 and 17 passes, no failures, warnings or skips.
- `run nice -n 10 just quality exp`: pass on the frozen exponential sample.
  The oracle uses seed 20261028 and source-defined score/likelihood thresholds.
- `run nice -n 10 just dpqr-truth exp`: four points verified, 84 value and
  176 quantile rows. `git diff` showed identical numerical rows but changed
  four elapsed times and `density_sha256` (the hash is of the whole current
  README); the audit-status JSON also gained `sampler_points` and changed time.
  Therefore byte-identical reproduction failed. Both tracked files were
  restored from HEAD and their diffs checked empty. No other truth table or
  golden was regenerated.
- `run env PYTHONPATH=dev/validation python3 -m unittest discover -s
  dev/validation/quality -p 'test_*.py'`: 38 tests passed.
- `run python3 dev/tools/check_links.py`: 279 Markdown files, 291 local
  destinations, zero broken. `git diff --check`: clean.

The full R suite, full quality audit and isolated exports were not repeated:
this job calls for targeted checks. The [step-5 report](2026-10-30-r-standalone-report.md)
records the full suite and isolated checks on its stated source and worktree.
Its post-review isolated rerun has three skips: optional sn, CRAN special
functions and repository codegen. Its earlier two-skip run is not the final
rerun. No claim of full-suite coverage is inferred from targeted runs here.

## Discrepancies and gaps

Corrected stale golden-test generator paths, bit-for-bit/tolerance comments and
BFGS default comment; corrected CLAUDE's incomplete target list, unsupported
sweep timing and universal score-test claim. STATUS's obsolete fixture failure
claim now links the later standalone result. The guide notes the truth generator
can add a ninth point beyond the eight historical points stated in the old
truth report. Historical reports remain unchanged.

Open: roxygen unresolved-link diagnostics above; stale historical references
such as `notes/evaluations/` in family comments (for example bnbinom
`test.cpp`); `bindings/r/scripts/goldens.R` still describes approximate uniform
bit-for-bit tolerance, although family regression tolerances differ. These
files are outside the permitted edits or are retained historical evidence.
The artifact viewer rejected the guide with HTTP 403 because this worktree is
outside its configured readable roots; the guide remains available by file link.
The detailed rationale for several check thresholds could not be recovered;
the guide identifies those as unknown. No independent global fitting oracle
covers all families, subsets or parameter regions. Truth tables share dossier
formulas with quality; boundary and sampling coverage is finite. The guide
links the reviewed inventory, including fitter-only and quantile-only boundary
cases, and identifies quality and CRAN skips.

A future evidence-table generator should read dossier metadata and `holds`,
`limits`, `fit_domain`, each golden and truth JSON and truth route, the family
C++ and R test registrations, reference fixtures, and machine-readable audit
results with source revision and command. Human-reviewed boundary and proof
annotations should cite exact cases or derivations; file presence or a passing
family status must not be promoted to a proof or complete-coverage claim.
