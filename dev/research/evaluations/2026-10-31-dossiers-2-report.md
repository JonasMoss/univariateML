# Dossiers round two — TASK-101.6.4 / TASK-101.6.5

2026-10-31. Tested source: `998a3a6776ccf2204d2085f596c281067528c11a`
plus this uncommitted worktree. No commits, board edits, installs, fitter,
adapter, density, golden or dossier-body changes. Only front matter and the
assigned schema, loader, relation checker/tests and site rendering changed.
Canonical taxonomy plan: [family taxonomy](../../roadmap/2026-10-31-family-taxonomy.md).

## Certification and presentation fields

- `yes`: **46**; `partial`: **43**; `no`: **9**; `unknown`: **0**.
- Closed forms, unique monotone score equations, strict concavity or exhaustive
  bounded integer searches support `yes` across supported default/held fits.
  Unsupported structural subsets stop and are not silently treated as estimates.
  `partial` names particular certified subsets/regions; `no` denotes numerical
  grids, multistart or local profiles without such a certificate. All-held density
  evaluation alone does not count. `unknown` would require missing evidence.
- Each note cites its family README Details, fitter and tests. Lognormal is
  closed-form normal fitting on log observations; gamma transformations retain
  their scoped guarantees. Hypergeometric finite grids have a search certificate,
  with numerical likelihood/tie comparisons, not exact-arithmetic ordering.
- Concavity/monotonicity is a mathematical statement; solver tolerances and
  existing tests are numerical evidence. No new global optimization theorem,
  uniform floating-point bound or inference-validity claim is made.
- All 98 now have summary, aliases and one to three selected key references;
  the 88 missing blocks were filled. Empty alias lists mean no additional name
  was verified. Classification reasons now identify family-specific modelling roles.
- Six tentative uncommon families become specialist: fpareto, invparalogis,
  invtrgamma, lgamma and paralogis have documented actuarial roles; nsbeta is
  interval-rescaled beta. Bhattacharjee remains uncommon: dimensional tolerance
  chains are documented, but wider adoption was not established. These are
  editorial judgements, not frequency estimates (23 classical, 74 specialist, 1 uncommon).

## Group changes

- EMG and Bhattacharjee → new **normal convolutions** group: independent
  exponential/uniform components explain their shape better than full-real support.
- foldnorm → normal and Student; hcauchy → skewed and robust location-scale:
  parent-family affinity, while support/folding facets retain their distinctions.
- dnorm → normal and Student; dgamma/dweibull → gamma and Weibull;
  dlaplace → skewed and robust location-scale. Discretisation does not make these
  Poisson/binomial relatives; integer support remains independently filterable.
- nsbeta and kumar → beta and generalised beta; llogis joins that group through
  its Burr/GB2 membership. These are navigational affinities, not disjoint classes.

## Added relations

21 new edges, alongside the original 39 (60 total):

- Symmetric members: norm → snorm (xi=1),
  std → sstd (xi=1), ged → sged (xi=1). Existing norm → sn at alpha=0 is retained.
- Translated folding: hnorm → norm, hcauchy → cauchy, ht → lst;
  parent location zero, ordinary scale preserved, absolute value then endpoint shift.
- `truncation_of`: tnorm → norm, tpois → pois, tbinom → binom.
- `modification_of`, zero-truncated: ztpois → pois, ztgeom → geom,
  ztbinom → binom, ztnbinom → nbinom.
- `modification_of`, zero-modified: zmpois → pois, zmgeom → geom,
  zmbinom → binom, zmnbinom → nbinom, zmlogarithmic → lgser.
- `modification_of`, zero-inflated: zip → pois, zib → binom, zinb → nbinom.
- Parent maps cover every coefficient. Truncation checks independently integrate
  or sum parent mass on child support; modifications test zero and positive branches.
  Both directions render with distinct labels; type, zero mass and folding shift
  are displayed. Catalog facets derive their relation kinds from these same records.
- Check status: **60/60 pass**, 11 symbolic and 49 numerical. Per-edge evidence is written by
  `just relations` to `build/quality/relations.json` (seed 20261031, 24 trials,
  35-digit evaluation, relative log-density tolerance 2e-9).
- These checks distinguish symbolic simplification from numerical agreement.
  They do not prove all-parameter identities or test asymptotic limit records.
  Zero-denominator boundary extensions remain family-test responsibilities.

## Reference verification and omissions

- Verified publisher records for Johnson–Kotz–Balakrishnan volumes
  [1 (1994)](https://www.wiley-vch.de/en/areas-interest/mathematics-statistics/continuous-univariate-distributions-978-0-471-58495-7)
  and [2 (1995)](https://www.wiley-vch.de/de/fachgebiete/mathematik-und-statistik/continuous-univariate-distributions-978-0-471-58494-0),
  and [Johnson–Kemp–Kotz (2005)](https://doi.org/10.1002/0471715816).
  Corrected erroneous volume-one years; consolidated redundant discrete-book entries.
- Verified [Dutang–Goulet–Pigeon (2008)](https://doi.org/10.18637/jss.v025.i07)
  and [Dutang–Goulet–Langevin (2022)](https://www.jstatsoft.org/article/view/v103i06)
  as actuarial sources. The latter is selected for Feller–Pareto.
- Verified original sources/DOIs for [folded normal](https://doi.org/10.1080/00401706.1961.10489974),
  [EMG chromatography](https://doi.org/10.1021/ac60319a011),
  [CMP](https://doi.org/10.1111/j.1467-9876.2005.00474.x),
  [Simon](https://doi.org/10.1093/biomet/42.3-4.425) and
  [Nakagami](https://doi.org/10.1016/B978-0-08-009306-2.50005-4).
- Definition/coordinate sources include the
  [extraDistr manual](https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf),
  [actuar documentation](https://search.r-project.org/CRAN/refmans/actuar/html/00Index.html)
  and [fGarch documentation](https://search.r-project.org/CRAN/refmans/fGarch/html/dist-std.html).
  These are implementation/definition references, not newly asserted original sources.
- Bhattacharjee (1963) title/authors/pages are verified in its
  [reference entry](https://search.r-project.org/CRAN/refmans/extraDistr/html/Bhattacharjee.html);
  no DOI was added. Azzalini (1985) is selected without an unverified DOI.
- Verified aliases include Birnbaum–Saunders, generalized Waring, Wald,
  non-standard beta, exponential power and two-piece normal. No unsupported
  hurdle, Pearson-type or generalized-logistic aliases were inferred.
  The actuar log-gamma is exp(gamma), not log(gamma); no ambiguous alias was added.
- Unselected historical bibliography was retained; this is not a complete audit
  of every inherited citation. Missing original sources are represented by a
  verified manual/book, rather than invented author/year/DOI metadata.

## Validation and remaining catalog limitations

- Final `PYTHONPATH=dev/validation python3 -m unittest quality.test_relations`:
  **13/13 pass**, including normalization, zero branches, bad maps/supports,
  shifted folds, symmetric members and loader vocabulary validation.
- Final `just relations`: **60/60 pass**; `NOT_CRAN=true just test` (dev):
  **FAIL 0 | WARN 0 | SKIP 151 | PASS 18887** (tier skips).
- `NOT_CRAN=true just test-full`: **FAIL 0 | WARN 0 | SKIP 1 | PASS 39040**;
  the single skip is optional `{sn}` absent. Computational sources were unchanged
  throughout; final presentation-only edits are covered by the dev rerun.
- Final `just site-check`: **PASS**, 98 MathML family pages, all relation links,
  all 98 C++17 snippets compile/link. Catalog source-function checks pass
  relation/group facets, OR/AND selections, alias search and reset.
- Logs: `build/scratch/{relations-unit,relations,r-test,r-test-full,site-check,catalog}-final.log`.
  Source/body audit: `build/scratch/metadata-final.log`; `git diff --check` passes.
- The first relation run exposed SymPy's handling of floating integer Sum bounds;
  integer seeds and explicit finite sums fixed it before all final green runs.
  A pre-existing norm→sn edge was deduplicated before the final relation rerun.
- Commands run in the foreground through shared slot locks, nice priority,
  single-thread BLAS/OpenMP; temporary files/logs stay under build. Site dependencies
  were copied with cp -a from the supplied checkout; no installs.
- Structural audit verifies all 98 bodies and nonassigned computational fields
  unchanged. No default fit, fixture or settled d/p/q/r source was edited.
- Existing catalog caveats remain: existence `always` means no declared obstacle,
  not a theorem; prevalence is editorial; groups overlap mathematically; support
  tags summarise parameter-dependent shapes. Many fits retain documented global
  certification gaps; metadata does not repair their numerical searches.
- Relation checks cover seed neighbourhoods, not every parameter or limiting
  member. The new truncation checker currently sums finite integer windows;
  zero truncation uses the parent's exact zero-mass complement.

Tested code/dossier patch SHA-256 (report excluded): `a180fb593c886fac5019e9797bcefb8033f725c863c400708cea37f42cce13c6`.

No additional verified alias was supplied for: bbinom, betapr, bhatt, binom, cauchy, cmb, cmp, dgamma, dlaplace, dnorm, dunif, dweibull, emg, foldnorm, fpareto, frechet, gamma, genbeta, geom, gompertz, gpd, gumbel, hcauchy, hnorm, ht, huber, hyper, invburr, invexp, invgamma, invparalogis, invpareto, invtrgamma, kumar, lgamma, llogis, logis, lomax, lst, naka, nbinom, nhyper, paralogis, pareto, pareto2, pareto3, pareto4, pig, pois, power, rayleigh, sged, sgomp, skellam, slash, sn, std, tbinom, tlambda, tnorm, tpois, triang, unif, weibull, yulesimon, zeta, zib, zinb, zip, zipf, zmbinom, zmgeom, zmlogarithmic, zmnbinom, zmpois, ztbinom, ztgeom, ztnbinom, ztpois.

Artifact-pane display was attempted; the viewer returned HTTP 403 because this worktree is outside its readable roots. No viewer configuration or other checkout was changed.
