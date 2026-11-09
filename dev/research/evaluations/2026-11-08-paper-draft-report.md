# TASK-159: certified maximum likelihood paper draft

Completed the first full draft and all required final test runs. The [LaTeX source](../paper/draft.tex) builds to a **29-page PDF**, including embedded references: 28 bibliography entries, 24 with DOIs. The four entries without verified DOIs are explicitly identified preprint/conference versions. The draft is dated 8 November as requested; final validation completed on 9 November 2026.

Read-only base: `e8f019799955bdb62cab27be4cd015cd69682348`, branch `codex/paper-draft`. No commits, Git write commands, board edits, sub-agents, dependency installations or publication. Pre-existing tracked files, fitters, bindings, goldens, truth fixtures, dossiers, status files and roadmaps remain unchanged.

## Sections and results

1. What should a likelihood fit return?
2. The certificate standard
3. Existence and uniqueness from the likelihood
4. Explicit failures of uniqueness
5. Boundaries, reachable likelihoods and limit models
6. Interior regularity and inference
7. Software findings and coverage
8. Open problems and discussion

An appendix records source mapping and draft reproducibility. Analytic proofs, sample-specific certificates conditional on computed comparisons, and numerical profiles retain their separate evidence grades. No novelty claim is made; literature wording and search scope come from the supplied showcase and its literature ledgers.

The nine theorems, each with a proof or proof sketch from the source material, are:

- Ordinary NB.
- PIG dispersion criterion.
- Kumaraswamy.
- Burr XII, scale held.
- Held-shape Fréchet.
- ZTNB and conditional ZINB.
- GPD scalar reduction.
- Established all-free ZINB regions.
- Proved reductions for the ZTNB remainder.

The eight propositions cover GPD equal global maxima; ZINB held-inflation crossover; Two global SGED scales; Conditional beta-prime global tie; An off-diagonal BNB maximum; One-observation paralogistic likelihood; Location-scale $t$ profiles; Near-ZIP sample. Explicit samples are printed, including all nine beta-prime observations, both weighted t samples, and a lossless frequency table for the 500-count BNB sample. The half-t and held-skew-t profiles remain explicitly numerical.

## Every author-decision note

These are all nine calls to the defined `\todo` macro; line numbers refer to the frozen source.

1. Line 28: `\todo{Confirm authorship, affiliations, and acknowledgements.}`

2. Line 130: `\todo{Choose the final Statistical Science emphasis: a theorem-led account with software evidence, or a broader review with these results as examples. The current draft follows the former.}`

3. Line 184: `\todo{Reconcile the uniform $10^{-10}$ accuracy target with each delivered certificate before presenting that target as an achieved library-wide guarantee. Decide the treatment of coefficients equal or close to zero.}`

4. Line 293: `\todo{Obtain Wang's full note and the published Arag\'on--Eberly--Eberly proof before identifying this preprint gap with Wang's exact objection.}`

5. Line 1012: `\todo{Choose whether to retain the endpoint-factor derivation in the paper or a supplement.}`

6. Line 1143: `\todo{Decide which derived closure experiments belong in the main paper. Obtain theorem-level comparisons and verify each family's assumptions before stating inferential calibration as established.}`

7. Line 1170: `\todo{Choose and freeze the release revision for the coverage table, reconcile 79 versus 81 with the showcase, and audit whole-family claims against supported holds and delivered coefficient bounds.}`

8. Line 1221: `\todo{Choose whether to retain the versioned external-package defect examples. Confirm the archived versions and parameter conventions for publication, and do not imply that a historical defect persists in later releases.}`

9. Line 1381: `\todo{Before circulation, settle the main-text versus supplement allocation, select a reproducibility release and archive, and review every open mathematical and literature qualification with the authors. No priority claim is proposed.}`

## Source verification and qualifications

The [document validation record](2026-11-08-paper-draft-data/validation.json) records source hashes and checks all 45 local showcase destinations, sample transcriptions, references/citations, environments, page count and PDF diagnostics. The [reproducer](2026-11-08-paper-draft-data/validate.py) checks manuscript transcription and structure, not the correctness of every source theorem. No new literature search or exhaustive independent proof audit is claimed.

The following showcase statements could not be carried over without qualification:

- **Coverage:** the showcase says 79 fully certified and 20 partial; the canonical dossier census at this revision says **81 and 18**, total 99. Both dated rows are printed. The supplied material does not reconcile them to one frozen release inventory; author decision 7 requests that freeze. Metadata coverage is not an independent audit of all certificate implementations.
- **Universal contract and accuracy:** the showcase framing is stronger than the numerical routes retained by partially certified families. The paper distinguishes the design contract from achieved, scoped guarantees. The proposed relative `tol = 1e-10` is not asserted as achieved by every delivered certificate: the held-scale Burr report gives `1e-6` coefficient-image widths, and PIG gives a log-shape bracket target at least `2e-7` before polishing. These are globality/accuracy distinctions, not changes to the theorems or fixtures.
- **extraDistr argument naming:** the showcase's named `mu = 0.5` call is not the retained reproducer for the approximately `-6923.75` reference. The saved script uses positional arguments. Direct inspection and reproduction with installed **extraDistr 1.10.0.5** confirms the third argument is `prob`; the draft uses the retained call and explains the discrepancy. [Signature and reference calculation](2026-11-08-paper-draft-data/extraDistr-signature.txt). The separate actuar finding stays attached to the archived version 3.3.7, rather than being advertised as a defect in an untested later release.
- **Interior rank wording and source pointer:** the linked DQM-a report supplies generalized beta, while DQM-b and DQM-c supply the Tukey-lambda and BNB details. The draft names all three. At Tukey shapes 1 and 2 the singular matrix is a **formal pointwise score second moment**, not regular Fisher information: the endpoint-moving model is non-DQM there. Generalized-beta rank loss is stated with its upper endpoint held; the standardized skew-normal matrix is stated at unit scale.

All other showcased mathematical findings used in the draft were located in their dossiers/reports or retained inputs. Their proof/certificate/numerical grades were preserved. The source-reported open problems remain explicit in Section 8: Sampford's scalar remainder and its general ZINB consequence; all-free GEV and other missing joint covers; extreme-input numerical availability; the joint fitted GED Hessian; estimator transfer, point-process calibration and face-selection laws. Inaccessible literature and exact theorem-overlap questions are not treated as absence proofs.

## Final validation

Every final command below finished with exit zero. Runs used the prescribed slot wrappers, niceness and single-thread BLAS/OpenMP settings; work stayed in this worktree or its explicitly authorized simbox mirror. Temporary files and scratch logs stayed under `build/`.

| Check | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13.3 | 106/106 dev shards |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 |
| `just test` | workstation; four workers, three-CPU affinity, four-slot reservation | FAIL 0, WARN 0, SKIP 154, PASS 22,153 |
| Two final `pdflatex` passes | workstation | 29 pages; references and citations resolved; no overfull boxes or rerun warnings |
| Manuscript/source/PDF validation | workstation | 9 theorems, 8 propositions, 9 todos; 45 local showcase destinations; all transcription checks passed |

The R dev skips are tier exclusions, not passes. Simbox dropped during the earlier R dev attempt; an independent SSH probe confirmed unreachability, the lost connection was stopped, and the complete suite passed locally. The long local slot wait was retained in the foreground. The earlier completed simbox C++ results remain counted. Full R, JS and site runs were not triggered: no fitting, d/p/q/r, shared test infrastructure, binding messages, dossier content or public fitter signatures changed.

The final PDF was visually inspected, including the arithmetic standard, GPD theorem/tie, sample table and references. Intermediate TeX macro and log-encoding issues were corrected before the final passing checks. [Commands, machines, hashes and final outcomes](2026-11-08-paper-draft-data/suite-results.json) retain the validation record. Scratch logs remain under `build/scratch/paper-draft/`; the PDF is deliberately untracked under `build/paper-draft/`.

**Open delivery issue:** the required artifact-pane display of the PDF returned HTTP 403 because this worktree is outside the viewer's readable roots. Display needs this worktree registered in the viewer configuration; no settings or files outside this worktree were changed. The built PDF remains at `build/paper-draft/draft.pdf`. Display of the report was also attempted and returned the same HTTP 403.
