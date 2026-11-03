# TASK-139 supporting files

See the [completed report](../2026-11-03-boundary-lit-report.md) and [note source](../../boundary-points.tex).

- `metadata.py`, `metadata.json`: public Crossref DOI identity fields, retrieval time and the research authorization. The network script transmits DOI values only. It is optional to rerun; the retained record is sufficient to inspect the field comparisons. Provider errors are retained rather than treated as nonexistent works.
- `searches.txt`: all 55 web search queries, scope, provider/source preference and stopping rule. Direct URL opens and DOI retrievals are additional read-only lookups.
- `check.py`, `checks.json`: exact SymPy identities and 60-digit mpmath illustrations. No package code, random simulations, fitting or optimizer calls. BB sums cover counts 0–160 and use a separately recorded analytic bound on omitted probability. Numerical quadrature is not outward-rounded interval arithmetic.
- `validate.py`, `validation.json`: local citation keys, DOI-to-record agreement and the exact tested note SHA-256. Identity fields and source support require the separate review recorded in the report.
- `build-note.sh`: the native recipe's two pdflatex passes, without starting its background viewer. Fails on unresolved references, rerun requests, overfull boxes or `??` in extracted text.
- `final-checks.sh`: sequential mathematical checks, source validation, PDF build, first/last-page inspection images and environment reporting. No background work.

Run from the repository root, with the existing dependencies and no installation:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-03-boundary-lit-data/final-checks.sh
```

The scripts write retained JSON beside themselves and temporary logs/images under `build/scratch/`. The PDF follows the repository workflow at `dev/research/boundary-points.pdf`. No report-linked figures depend on scratch images. Machine and package regression commands/results are in the report. Sources are linked there; copyrighted full-text source files are not redistributed here.
