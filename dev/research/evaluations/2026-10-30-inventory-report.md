# TASK-101.2 inventory report — incomplete

Source revision: `5f11fb9e878219ac47a9a372e985eb69da6198e3` plus
uncommitted documentation changes. The fitter CSV indexes 98 families;
96 rows explicitly include unknown rationale, including gamma's empirical
start constants. Method labels and held-subset classifications are not yet
fully verified. The numerical note is preliminary and does not enumerate
every routine or Boost call path. The task is therefore not complete.

Main gaps: per-subset existence and failure handling, global search and
integer-tail certificates, exhaustive numerical users/ranges/fallbacks,
and historical rationale. The concurrent TASK-78 owns `special/`, d/p/q/r,
truth and code generation; no edits to those paths are proposed here.

Pilot: exp, gamma and GB2 prose adds method, boundary, strength and decision
history. The current generator carries Details into R help but omits the
separate Decision history section; see the schema proposal. The history
references still need durable links and a fuller evidence review.

Validation: local `just gen` passed. Simbox `just test`: 37,713 passed,
0 failed, 0 warnings, 0 skipped. Simbox `just check` failed during install
when the filesystem became read-only and returned I/O errors. Local fallback,
`/tmp/umvl-cpu/run nice -n 10 just check`, finished `Status: OK`; vignettes
were skipped by the recipe. Local environment: R 4.6.1, C++17, one slot,
OMP/BLAS threads set to one, `TMPDIR=build/tmp`. The tests cover the pilot
as it stood during these runs; they do not validate mathematical claims.
