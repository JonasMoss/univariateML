"""Canonical procedure availability; conservative projection of TASK-102.1/4/5/7/8.

Run directly to regenerate inference-availability.json. No inference is inferred
from a DQM flag, a historical method name, or a successful optimizer.
"""
import argparse
import csv
import json
import re
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = Path(__file__).with_name('inference-availability.json')
INVENTORY = 'dev/research/evaluations/2026-10-30-inference-inventory.csv'
REPORT = 'dev/research/evaluations/2026-10-31-inference-{}-report.md'
STATUSES = ['implemented', 'historically recoverable', 'available but missing',
            'unreviewed', 'invalid for the procedure']
PREFIX = 'univariateML-cpp/include/cppunivariate/distributions/'


VCOV_REPORT = 'dev/research/evaluations/2026-11-01-vcov-scores-report.md'
VCOV_R_REPORT = 'dev/research/evaluations/2026-11-01-vcov-r-report.md'
VCOV_DATA = 'dev/research/evaluations/2026-11-01-vcov-scores-data/'


def covariance_rows(eligibility):
    """Project dossier scopes; retain numerical evidence without extrapolating it."""
    report = (ROOT / VCOV_REPORT).read_text()
    pilot = json.loads((ROOT / (VCOV_DATA + 'summary.json')).read_text())
    validators = {}
    for path in sorted((ROOT / VCOV_DATA).glob('validator-*.json')):
        data = json.loads(path.read_text())
        # The reference files record family keys in nested result mappings.
        def contains(value, family):
            if isinstance(value, dict):
                return family in value or any(contains(v, family) for v in value.values())
            if isinstance(value, list):
                return any(contains(v, family) for v in value)
            return value == family
        for family in eligibility:
            if contains(data, family):
                validators.setdefault(family, []).append(str(path.relative_to(ROOT)))
    followups = {}
    for path in sorted((ROOT / VCOV_DATA / 'simbox').glob('*.json')):
        data = json.loads(path.read_text())
        for family, result in data['families'].items():
            followups.setdefault(family, []).append((str(path.relative_to(ROOT)), data, result))
    # Retain the report's interpretations verbatim as data, including later
    # corrections to the first follow-up; never synthesize a coverage claim.
    followup_text = report.split('## Follow-up on simbox', 1)[1]
    for family, scopes in eligibility.items():
        caveats = [' · '.join(part.strip() for part in line.strip('|').split('|')) for line in followup_text.splitlines()
                   if line.startswith('| ' + family + ' ') or line.startswith('| ' + family + ',')]
        if family == 'bnbinom':
            caveats.append(followup_text.split('At n = 500, bnbinom', 1)[1].strip())
            caveats[-1] = 'At n = 500, bnbinom ' + caveats[-1]
        evidence = [dict(label='Independent derivative validation', path=VCOV_REPORT+'#independent-derivative-validation'),
                    dict(label='Pilot Monte Carlo table', path=VCOV_REPORT+'#monte-carlo-sanity-check'),
                    dict(label='R vcov interface and assumptions', path=VCOV_R_REPORT+'#interface-and-mathematical-scope')]
        evidence += [dict(label='Derivative results: '+Path(path).stem, path=path)
                     for path in validators.get(family, [])]
        result = pilot['families'].get(family)
        runs = []
        if result:
            evidence.append(dict(label='Pilot Monte Carlo data', path=VCOV_DATA+'summary.json'))
            runs.append(dict(path=VCOV_DATA+'summary.json', sample_size=pilot['sample_size'], result=result))
        for path, data, result in followups.get(family, []):
            evidence.append(dict(label='Simbox Monte Carlo data', path=path))
            runs.append(dict(path=path, sample_size=data['sample_size'], result=result))
        if family in followups:
            evidence += [dict(label='Simbox follow-up table', path=VCOV_REPORT+'#follow-up-on-simbox-2026-11-01'),
                         dict(label='Second simbox follow-up table', path=VCOV_REPORT+'#second-simbox-follow-up-truth-points-away-from-singular-members')]
        runs = [dict(path=r['path'], sample_size=r['sample_size'],
                     **{k: r['result'].get(k) for k in ('truth', 'held', 'free', 'status', 'replications', 'successes')},
                     failure_count=len(r['result'].get('failures', []))) for r in runs]
        for scope in scopes:
            available = scope['available']
            reason = scope.get('reason', '')
            unreviewed = any(word in reason.lower() for word in ('not yet', 'open', 'not been completed', 'unvalidated', 'unimplemented'))
            yield dict(family=family, regime=scope['regime'], kind='covariance',
                target='Free-coefficient covariance',
                status='implemented' if available else ('unreviewed' if unreviewed else 'invalid for the procedure'),
                procedures='Sandwich and observed covariance · C++ and R vcov (default sandwich)',
                theory='Implemented numerical covariance scope; availability does not establish estimator asymptotics or coverage',
                assumptions=scope['condition']+'; '+
                    'IID; successful fit interior to the stated free submodel, correct free mask and nonsingular well-conditioned sensitivity. '
                    'Observed inference requires correct specification and regular estimator asymptotics; '
                    'robust sandwich inference additionally requires finite score variance, an identifiable KL target and an estimating-equation expansion.',
                dqm='Conditional classification in the linked source',
                information='Derivative eligibility only; no automatic finite-information claim',
                rank='Available scopes exclude structural null directions; runtime singular, indefinite and ill-conditioned solves still fail',
                evidence=scope['classification'], calibration=VCOV_REPORT+'#monte-carlo-sanity-check',
                evidence_links=[dict(label='Canonical dossier', path=PREFIX+family+'/README.md'), *evidence],
                implementation=([dict(path='univariateML-cpp/include/cppunivariate/core/inference_families.hpp', function='family_inference'),
                    dict(path='univariateML-cpp/include/cppunivariate/core/inference.hpp', function='inference_covariance'),
                    dict(path='bindings/r/R/code_vcov.R', function='vcov.univariateML')] if available else []),
                checks=['dev/validation/inference-numeric-registry.py', 'bindings/r/tests/testthat/test-vcov.R'] if available else [],
                gap=(reason+' ' if reason else '')+'Pointwise SE/SD comparisons are not coverage validation. '
                    'Evidence uses recorded truth points and held subsets, not every regime; failed replications can select results. '
                    'Near-singular sn/bhatt members, bnbinom weak identification at n = 500 and invtrgamma finite-sample bias require caution.',
                caveats=caveats, monte_carlo=runs, historical_sources='', historical_validation='')


def generate():
    rows = []
    for r in csv.DictReader((ROOT / INVENTORY).open()):
        rows.append(dict(family=r['id'], regime=r['regime'], target='Other hypotheses: not reviewed',
            status='unreviewed', procedures='No audited procedure for this inventory scope',
            theory='Unreviewed estimator limit; DQM alone is insufficient',
            assumptions=r['dqm_conditions'], dqm=r['dqm_status'],
            information=r['finite_information'], rank=r['information_rank'],
            evidence=REPORT.format(1)+'#family-by-family-classification', calibration='',
            implementation=[], checks=[], gap=r['estimator_asymptotics'],
            historical_sources=r['historical_named_hessian_sources'],
            historical_validation=r['historical_hessian_validation']))

    def add(family, regime, target, procedures, theory, assumptions, functions=(), report=5,
            anchor='procedures-and-theory', calibration='', status='implemented', gap='', header='inference.hpp'):
        rows.append(dict(family=family, regime=regime, target=target, status=status,
            procedures=procedures, theory=theory, assumptions=assumptions,
            dqm='See inventory scope and linked derivation', information='Procedure-specific; see derivation',
            rank='No joint rank claim beyond the stated target',
            evidence=REPORT.format(report)+'#'+anchor,
            calibration=REPORT.format(report)+'#'+calibration if calibration else '',
            implementation=[dict(path=PREFIX+family+'/'+header, function=f) for f in functions],
            checks=([PREFIX+family+'/test.cpp', f'dev/validation/inference-{report}.py'] if functions else []),
            gap=gap, historical_sources='', historical_validation=''))

    model = ('IID correctly specified model; interior identifiable target; every nuisance fitted as stated; '
             'successful fit and covariance status required. Root-n consistency and local likelihood expansion; '
             'finite nonsingular information. Conditioning can reject a full-rank model.')
    robust = ('IID; interior identifiable KL maximum, nonsingular expected Hessian sensitivity, finite score '
              'second moments, consistent fit and estimating-equation expansion. Targets are pseudo-true; '
              'sandwich uses observation score products, not total-score outer products.')
    pilots = [('norm','mean','sd fitted','norm_mean_inference','norm_mean_lr_interval'),
              ('gamma','mean','shape fitted, scale=mean/shape','gamma_target_inference','gamma_target_lr_interval'),
              ('gamma','shape','scale fitted','gamma_target_inference','gamma_target_lr_interval'),
              ('beta','mean','concentration fitted','beta_mean_inference','beta_mean_lr_interval'),
              ('pois','rate','no nuisance','pois_rate_inference','pois_rate_lr_interval')]
    for family,target,nuisance,fn,lr in pilots:
        add(family,'interior; '+nuisance,target+' under model',
            'Model Fisher/observed-Hessian Wald; nuisance-aware Rao and profile LR tests; Wald and LR intervals',
            'Asymptotic root-n target; chi-square(1) tests; model-only',model,
            (fn,lr),4,'implemented-formulas-and-contracts','complete-calibration-tables',header='score.hpp',
            gap='Pointwise grid only; small-n undercoverage and conditioning failures: see calibration. Rao is a pointwise confidence set, not necessarily a two-endpoint interval.')
        add(family,'misspecification; '+nuisance,target+' at KL projection',
            'Sandwich Wald interval/test (Rao and LR remain model-only)',
            'Asymptotic root-n; no finite-sample robustness guarantee',robust,(fn,),4,
            'implemented-formulas-and-contracts','complete-calibration-tables',header='score.hpp',
            gap='Gamma lognormal alternative remains poorly calibrated at n=1000; other alternatives are pointwise evidence, not uniform guarantees.')
    for family,target,fn,assumption in [
        ('norm','mean','norm_mean_exact_interval','IID normal, positive sd, mean and sd fitted, n>=2; Student t pivot.'),
        ('pois','rate','pois_rate_exact_interval','IID Poisson, rate>=0, integer nonnegative observations; Garwood conservative interval, including zero total.')]:
        add(family,'model; nuisance fitted' if family=='norm' else 'model; including zero total',target+' exact interval',
            'Student t interval' if family=='norm' else 'Garwood interval','Exact model-only (Garwood conservative)',assumption,
            (fn,),4,'implemented-formulas-and-contracts','complete-calibration-tables',header='score.hpp')
    for family in ['norm','pois']:
        add(family,'model; nuisance fitted' if family=='norm' else 'model; including zero total',
            'mean score interval' if family=='norm' else 'rate score interval','Analytic Rao inversion',
            'Asymptotic root-n; model-only','IID model; positive normal sd or nonnegative Poisson rate; supplied chi-square cutoff.',
            (family+('_mean' if family=='norm' else '_rate')+'_score_interval',),4,
            'implemented-formulas-and-contracts','complete-calibration-tables',header='score.hpp')
    for family,target in [('gamma','mean'),('gamma','shape'),('beta','mean')]:
        add(family,'interior; nuisance fitted',target+' score confidence-set endpoints',
            'Full pointwise Rao set inversion','Asymptotic root-n at fixed null; model-only',model,
            functions=(('gamma_target_inference' if family=='gamma' else 'beta_mean_inference'),), header='score.hpp',
            report=4,anchor='score-confidence-sets-resolved-obstruction-and-open-entries',status='available but missing',
            gap='Pointwise statistic exists; certified enumeration of all accepted components is missing. Gamma mean has a remote unbounded accepted tail; no two-root shortcut.')
    add('laplace','sigma>0; scale fitted','location model Wald','Model Fisher Wald interval',
        'Asymptotic root-n; model-only','IID Laplace, positive scale; ordinary location Hessian unsuitable.',
        ('laplace_location_wald_interval',),calibration='laplace-and-the-continuous-normal-alternative',
        gap='Substantial finite-n undercoverage, still visible at n=1000.')
    add('laplace','continuous sampling; unique median','median exact sign interval','Nonrandomized sign/order-statistic interval',
        'Exact conservative; model-independent','IID continuous sampling, unique median; no atoms/ties theory; infinite limits may be deliberate.',
        ('median_sign_interval',),calibration='laplace-and-the-continuous-normal-alternative')
    add('laplace','continuous sampling; g(m)>0','median robust interval','MAD-bandwidth KDE median sandwich',
        'Asymptotic root-n median; no coverage-error rate asserted',
        'IID, unique median, positive density there, local KDE consistency, positive consistent MAD; Gaussian KDE rule fixed in source.',
        ('median_sandwich','median_sandwich_interval'),calibration='laplace-and-the-continuous-normal-alternative',
        gap='Conservative at Laplace cusp; zero MAD fails. No adaptive bandwidth guarantee.')
    for known in [False,True]:
        add('pareto','a,b>0; b '+('known' if known else 'fitted as min X'),'shape exact interval',
            'Chi-square log-spacing pivot','Exact model-only; shape root-n; fitted endpoint rate n',
            'IID Pareto; '+('b genuinely known, n>=1, positive log sum; 2n df.' if known else 'n>=2, positive log spacing sum; 2(n-1) df.'),
            ('pareto_shape_exact_interval',),calibration='pareto-shape-known-and-unknown-endpoint')
    add('exp','origin zero known; rate>0','rate exact interval','Chi-square pivot','Exact model-only; root-n rate',
        'IID exponential, zero origin known, n>=1 and positive total; 2n df.',('exp_rate_exact_interval',),
        calibration='exponential-rate-and-normal-variance')
    for known in [False,True]:
        add('norm','positive variance; mean '+('known' if known else 'fitted'),'variance exact interval',
            'Chi-square residual-sum pivot','Exact model-only; root-n variance',
            'IID normal; positive squared residual sum; '+('known mean, n>=1, n df.' if known else 'fitted sample mean, n>=2, n-1 df.'),
            ('norm_variance_exact_interval',),calibration='exponential-rate-and-normal-variance')
    for target in ['minimum','maximum','width']:
        add('unif','a<b; both endpoints fitted',target+' marginal interval','Extreme/spacing pivot',
            'Exact model-only; rate n; non-DQM endpoints','IID continuous uniform, n>=2, positive sample range; marginal, not simultaneous.',
            ('unif_exact_interval',),calibration='uniform-endpoints-and-width')
    for target in ['minimum','maximum']:
        add('unif','a<b; opposite endpoint known',target+' (and translated width) interval','One-extreme pivot',
            'Exact model-only; rate n; non-DQM endpoint','IID uniform; opposite endpoint genuinely known, n>=1, positive extreme gap.',
            ('unif_endpoint_known_opposite_interval',),calibration='uniform-endpoints-and-width')
    add('triang','known a<b; a<c<b','interior mode model inference','Model Wald interval; pointwise Rao and LR tests',
        'Asymptotic root-n; chi-square(1) tests; model-only',
        'IID triangular, both endpoints known, mode strictly interior; certified successful finite-candidate mode fit.',
        ('triang_mode_wald_interval','triang_mode_tests'),calibration='triangular-mode-with-known-endpoints',
        gap='Wald poor even at n=1000. Null-specific independent simulated LR calibrated in driver only; no simulated confidence-set API.')
    add('triang','known a<b; a<c<b','mode exact score inversion','Binomial count inversion',
        'Exact model-only candidate','IID triangular; count below null c is Binomial(n,(c-a)/(b-a)); endpoints known.',
        functions=('triang_mode_tests',), anchor='triangular-mode-with-known-endpoints',status='available but missing',
        gap='Production returns model-information statistic; exact binomial inversion API and its calibration are missing.')
    for family,regime,target,reason in [
        ('pareto','b free','joint endpoint/shape regular Wald','Free endpoint is non-DQM; shape pivot remains valid separately.'),
        ('unif','any endpoint free','regular endpoint Fisher Wald','Free endpoint is non-DQM; use exact spacing procedures.'),
        ('laplace','location free','ordinary location Hessian inverse','Pointwise Hessian omits nonsmooth median sensitivity; use Fisher or median sandwich.'),
        ('triang','known endpoints; interior mode','ordinary pointwise Hessian inverse','Expected negative pointwise Hessian is -I and misses kink contribution.'),
        ('triang','known endpoints; interior mode','single connected LR interval assumption','Explicit sample gives separated LR components; connectedness disproved.')]:
        add(family,regime,target,'Invalid procedure/assumption','No valid ordinary regular procedure claimed',reason,
            status='invalid for the procedure',anchor='recommendations-and-task-1026-availability-handoff',gap=reason)
    for regime,gap in [('boundary mode or free endpoints','Separate one-sided boundary theory and calibration; free endpoint squared scores diverge.'),
                       ('misspecification','Kink sensitivity, identification, density estimation and calibration unvalidated.')]:
        add('triang',regime,'mode inference','No audited procedure','Unreviewed',gap,status='unreviewed',
            anchor='recommendations-and-task-1026-availability-handoff',gap=gap)
    for row in rows:
        if row['status']=='implemented' and 'Wald' in row['procedures'] and row['family'] in ('norm','gamma','beta','pois'):
            row['implementation'].append(dict(path='univariateML-cpp/include/cppunivariate/core/inference.hpp', function='inference_wald_interval'))
        if row['family']=='norm' and row['target']=='mean score interval':
            row['assumptions'] += ' Bounded interval API requires n > critical; otherwise it fails explicitly.'
    # Dossier eligibility is separate from audited procedure availability.
    # Every canonical dossier supplies explicit scopes; unreviewed scopes are
    # unavailable with reasons, never inferred from DQM alone.
    numeric_eligibility = {}
    families = ROOT / PREFIX
    for dossier in sorted(families.glob('*/README.md')):
        parts = dossier.read_text(encoding='utf-8').split('---', 2)
        if len(parts) != 3:
            raise ValueError(f'invalid dossier front matter: {dossier}')
        meta = yaml.safe_load(parts[1])
        scopes = meta.get('inference_numeric')
        if scopes is None:
            raise ValueError(f'missing inference scopes: {dossier}')
        if scopes is not None:
            if not isinstance(scopes, list) or not scopes:
                raise ValueError(f'invalid inference scopes: {dossier}')
            seen = set()
            for scope in scopes:
                required = ('regime', 'condition', 'classification')
                if (not isinstance(scope, dict) or
                    set(scope) - {'regime', 'available', 'condition', 'reason', 'classification'} or
                    any(not isinstance(scope.get(k), str) or not scope[k].strip() for k in required) or
                    not isinstance(scope.get('available'), bool) or
                    (not scope['available'] and (not isinstance(scope.get('reason'), str) or not scope['reason'].strip())) or
                    ('reason' in scope and (not isinstance(scope['reason'], str) or not scope['reason'].strip())) or
                    scope['regime'] in seen):
                    raise ValueError(f'invalid inference scope: {dossier}')
                ref = scope['classification'].split('#', 1)
                if len(ref) != 2 or not ref[1] or not (ROOT / ref[0]).is_file():
                    raise ValueError(f'invalid inference classification link: {dossier}')
                headings=(line.lstrip('#').strip().lower() for line in (ROOT/ref[0]).read_text().splitlines() if line.startswith('#'))
                slugs={re.sub(r'[^\w\- ]','',line).replace(' ','-') for line in headings}
                if ref[1] not in slugs:
                    raise ValueError(f'missing inference classification anchor {scope["classification"]}: {dossier}')
                seen.add(scope['regime'])
        # Metadata cannot enable an absent family adapter. Subset/regime guards
        # remain explicit in those adapters and are checked by regime tests.
        if any(scope['available'] for scope in scopes):
            core = ROOT / 'univariateML-cpp/include/cppunivariate/core'
            dispatch = '\n'.join((core / name).read_text() for name in (
                'inference_families.hpp', 'inference_numeric_generated.hpp',
                'inference_numeric_special.hpp'))
            if f'family=="{meta["id"]}"' not in dispatch and f'family == "{meta["id"]}"' not in dispatch:
                raise ValueError(f'available inference family has no adapter: {dossier}')
        numeric_eligibility[meta['id']] = scopes
    # The dated inventory is historical evidence, not a list of all current
    # families. New dossiers receive explicitly unreviewed procedure rows
    # without rewriting the historical inventory or inferring availability.
    inventoried = {row['family'] for row in rows}
    for family, scopes in numeric_eligibility.items():
        if family in inventoried:
            continue
        for scope in scopes:
            rows.append(dict(family=family, regime=scope['regime'],
                target='Other hypotheses: not reviewed', status='unreviewed',
                procedures='No audited procedure for this scope',
                theory='Unreviewed estimator limit; DQM alone is insufficient',
                assumptions=scope['condition'], dqm='Unreviewed',
                information='Unreviewed', rank='Unreviewed',
                evidence=scope['classification'], calibration='', implementation=[],
                checks=[], gap=scope.get('reason', 'No audited procedure for this scope'),
                historical_sources='', historical_validation=''))
    rows.extend(covariance_rows(numeric_eligibility))
    return dict(schema_version=2, sources=[VCOV_REPORT, VCOV_R_REPORT, INVENTORY, *[REPORT.format(i) for i in [1,4,5]]],
                statuses=STATUSES, rows=rows, numeric_eligibility=numeric_eligibility)


if __name__ == '__main__':
    data = generate()
    parser = argparse.ArgumentParser()
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    if args.check:
        if not OUTPUT.is_file() or json.loads(OUTPUT.read_text()) != data:
            raise SystemExit('stale inference-availability.json; regenerate without --check')
    else:
        OUTPUT.write_text(json.dumps(data, indent=2)+'\n')
    from collections import Counter
    print(len(data['rows']), dict(Counter(r['status'] for r in data['rows'])))
