"""Extract base outcome sites, index held-subset observations, and summarize replay evidence."""
import collections as C
import csv
import gzip
import json
from pathlib import Path
import re

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
EVAL = HERE.parent
CORE = 'univariateML-cpp/include/cppunivariate/core'
FACTORIES = dict(estimate='estimate', undetermined='estimate', limit='limit',
                 nonexistence='nonexistence', budget='budget', failure='failure')
FACTORY = re.compile(r'FitOutcome::(\w+)\(')


CLOSED = set('dunif exp geom hnorm invexp invgauss laplace lnorm logitnorm norm pareto pois power rayleigh unif dlaplace'.split())
LEMMA = set('beta cmp gamma gumbel hcauchy invgamma invweibull lgamma lgser llogis logis naka nbinom nsbeta pareto3 weibull yulesimon zeta'.split())
INTEGER = set('binom hyper nhyper'.split())
KNOWN_HELD_FORM = {
    'beta': set(), 'gamma': {'shape'}, 'invgamma': {'shape'}, 'lgamma': {'shape'},
    'naka': {'shape', 'm'}, 'weibull': {'shape'}, 'invweibull': {'shape'},
    'binom': {'size'}, 'nbinom': {'size'},
}
LOMAX = {'lomax', 'invpareto', 'pareto2'}
HELD_ROOT = set('beta gamma invgamma lgamma naka nsbeta gumbel hcauchy lgser yulesimon zeta weibull invweibull kumar'.split())
# Recorded at the acceptance test, rather than inferred from a default flag.
COVER = set('cauchy dgamma dweibull emg fatigue foldnorm genbeta gev gpd ht huber kumar paralogis invparalogis pig sgomp skellam slash sn snorm triang trgamma invtrgamma'.split())
PER_INPUT = set('cauchy binom hyper nhyper lomax invpareto pareto2'.split())
HEURISTIC = {
    'lomax': 'Newton corrections below tol/4 with contraction <= 1/4 or 64 eps floor; alternative signed enclosure',
    'invpareto': 'Lomax Newton contraction or signed enclosure after reciprocal transform',
    'pareto2': 'positive-shift Lomax delegate: Newton contraction or signed enclosure',
}
# Exact-data face predicates, as distinguished from approximate equality of objectives.
ANALYTIC_FACES = {
    'cmp': {'TwoPointLimit', 'PointMassLimit'}, 'dweibull': {'PointMass', 'BernoulliLimit', 'TwoPointLimit'},
    'dgamma': {'PointMass', 'TwoPointLimit'}, 'dnorm': {'PointMass', 'TwoPointLimit'},
    'dlaplace': {'PointMass'}, 'zeta': {'PointMass'}, 'yulesimon': {'PointMass'},
    'lgser': {'PointMass'}, 'hyper': {'AllZero'}, 'cmb': {'PointMass', 'AdjacentLimit', 'EndpointLimit'},
}

def save(name, rows):
    with (HERE / name).open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)

def records(path):
    with gzip.open(path, 'rt') as stream:
        for line in stream:
            yield json.loads(line)

def ref(path, line):
    return f'{path.relative_to(ROOT)}:{line}'

def metadata(path):
    front = path.read_text().split('---', 2)[1].splitlines()
    fields = {}
    for number, line in enumerate(front, 2):
        m = re.match(r'  (method|certified|uniqueness):\s*(.*)', line)
        if m:
            fields[m[1]] = (m[2].strip("'\""), ref(path, number))
    limits = []
    model = ''
    inside = False
    for number, line in enumerate(front, 2):
        if line.startswith('limits:'):
            inside = True
        elif line and not line.startswith((' ', '-')):
            inside = False
        if inside:
            m = re.match(r'\s*(?:- )?(model|status|firmness|numerical):\s*(.*)', line)
            if m:
                if m[1] == 'model':
                    model = m[2]
                limits.append((model, m[1], m[2], ref(path, number)))
    return fields, limits

def sites(path):
    lines = path.read_text().splitlines()
    for start, line in enumerate(lines):
        if not re.search(r'\bFitOutcome outcome\(', line):
            continue
        depth = 0
        status = ''
        for i in range(start, len(lines)):
            code = lines[i].split('//', 1)[0].strip()
            m = re.search(r'case \w+::(\w+)', code)
            if m:
                status = m[1]
            if re.search(r'return\s+(?:FitOutcome)?\{\};', code):
                yield (ref(path, i+1), 'default', 'failure', 'OutcomeReason::Evaluation', code)
            for expression in FACTORY.finditer(code):
                factory = expression[1]
                nesting = 1
                end = expression.end()
                while end < len(code) and nesting:
                    nesting += (code[end] == '(') - (code[end] == ')')
                    end += 1
                args = code[expression.end():end-1]
                if factory in FACTORIES:
                    yield (ref(path, i+1), status or 'pre-switch', factory, args, code)
            if re.search(r'\bcomparison_outcome(?:_of)?\(', code):
                yield (ref(path, i+1), status or 'pre-switch', 'comparison', '', code)
            if re.search(r'\bequal_faces_outcome\(', code):
                yield (ref(path, i+1), status or 'pre-switch', 'equal_faces', '', code)
            if re.search(r'\breturn outcome\(', code):
                yield (ref(path, i+1), status or 'pre-switch', 'delegate', '', code)
            depth += code.count('{') - code.count('}')
            if depth == 0:
                break

def flags(factory, args):
    first, _, second = args.partition(',')
    if factory == 'estimate':
        return first or 'true', second.strip() or 'true (default)'
    if factory == 'undetermined':
        return 'false', 'NA'
    if factory in {'limit', 'equal_faces'}:
        return first or 'true', 'NA'
    return ('true' if factory == 'nonexistence' else 'false'), 'NA'

def classify(family, status, kind, held, cert, unique, meta, tag):
    if kind in {'failure', 'budget'}:
        return 'none', 'NA', 'NA', 'control/status predicate'
    if kind == 'nonexistence':
        if family == 'pareto2':
            return 'proof-exact-comparison', 'NA', 'NA', 'threshold multiplicity and FMA residual of shape × positive count versus threshold count'
        if family in CLOSED and status in {'Unbounded', 'DegenerateData'}:
            return 'proof-exact-comparison', 'NA', 'NA', 'sample degeneracy or support predicate'
        return 'unclassified', 'NA', 'NA', 'trace collapse exponent or strict face exclusion at status producer'
    if kind == 'limit':
        if family in LOMAX and status == 'ExponentialLimit':
            return 'proof-practical-margin', 'NA', 'NA', '1e-8 n gain threshold plus computed eta pads; attained tie or smaller interior gain can remain'
        if (family, status) in {('ht', 'NormalLimit'), ('sn', 'RightHalfNormalBoundary'),
                                 ('sn', 'LeftHalfNormalBoundary'), ('trgamma', 'LognormalBoundary'),
                                 ('trgamma', 'PowerBoundary')} and tag != 'comparison':
            margin = '64 eps (1 + |Delta| + |Delta_face|), plus the 1e-8 per observation unresolved-gain policy' if family == 'trgamma' else '1e-8 n likelihood gap with family cover and face comparisons'
            return 'proof-practical-margin', 'NA', 'NA', margin + '; verify held scope and attainment separately'
        if tag == 'comparison' or cert == 'false':
            return 'numerical-selection', 'NA', 'NA', 'candidate or face comparison; check family threshold and whether a cover also closed'
        if status in ANALYTIC_FACES.get(family, ()):
            return 'proof-exact-comparison', 'NA', 'NA', 'count/support pattern; inspect held design and missing boundary'
        return 'unclassified', 'NA', 'NA', 'trace detector to prove strict exclusion or establish its numerical margin'
    if held == 'all-held':
        return 'closed-form', 'fixed vector', 'universal-proof', 'singleton feasible parameter vector after validation'
    if family == 'fatigue' and unique == 'false':
        return 'proof-exact-comparison', 'analytic reciprocal roots; 12 eps enclosure checked against tol', 'typed-set', 'proved constant-sample reciprocal tie, reconstructed only after accuracy check'
    if unique == 'false':
        basis = 'typed-set' if cert == 'true' else 'unclassified'
    elif unique == 'NA':
        basis = 'NA'
    elif family in PER_INPUT and cert == 'true':
        basis = 'per-input-certificate'
    elif meta.get('uniqueness', ('', ''))[0] == 'proved' and cert == 'true':
        basis = 'universal-proof'
    else:
        basis = 'not-addressed'
    if cert == 'false' or tag == 'comparison':
        return 'numerical-selection', 'unclassified', basis, 'trace candidate selection and score/polish gate independently'
    if family in CLOSED or held in KNOWN_HELD_FORM.get(family, ()):
        return 'closed-form', 'formula rounding', basis, 'analytic reduction or singleton'
    if family in LOMAX:
        if held in {'lambda', 'scale'}:
            return 'closed-form', 'formula rounding', 'universal-proof', 'conditional shape n / sum(log1p(rate*x)); delegate uses transformed sample'
        if held in {'kappa', 'shape'}:
            return 'proof-practical-margin', 'signed bracket <= tol/2 plus restoration <= tol/4', 'universal-proof', 'signed log-rate endpoints beyond computed score errors'
        return 'proof-practical-margin', HEURISTIC[family], basis, 'gain > 1e-8, score and curvature pads 4 eta and strict domination outside winning box; positive Lomax route'
    if family == 'dblpareto':
        return 'proof-practical-margin', 'analytic conditional rates and kink location', basis, '128 eps (1 + |s1| + |s2|) in profile coordinates; exact product flat checks'
    if family == 'skellam':
        return 'proof-practical-margin', 'shared Newton polish for positive rates; boundary return bypasses polish', basis, 'joint cover proves a likelihood gap <= 1e-8 n, with face selected within 5e-9 n'
    if family in INTEGER:
        return 'proof-practical-margin', 'integer comparison; continuous coordinates need their own gate', basis, 'adjacent ratios or exhaustive bounded cells; exact ties where available'
    if family in LEMMA or family in COVER:
        local = 'signed bracket on applicable scalar route' if family in HELD_ROOT else 'unclassified'
        if family in {'emg', 'frechet', 'ht', 'huber', 'sgomp', 'sn'}:
            local = 'Newton correction/observed contraction on polish subroute; other subroutes use separate gates'
        return 'proof-practical-margin', local, basis, 'conditional on score/cover comparisons; consult replay margin cells for sampled comparisons'
    return 'unclassified', 'unclassified', basis, 'trace optimization acceptance and local error conversion on this held subset'

def replay_index():
    counters = C.Counter()
    margins = C.defaultdict(set)
    for path in sorted(EVAL.glob('*replay*-data/records.jsonl.gz')):
        for record in records(path):
            for item in record.get('comparisons', []):
                key = (record.get('family', ''), record.get('mask', ''), record.get('kind', ''),
                       item.get('name', ''), item.get('rigor', ''), item.get('verdict', ''), path.parent.name)
                counters[key] += 1
                if item.get('margin') is not None:
                    margins[key].add(str(item['margin']))
    rows = []
    for key, count in sorted(counters.items(), key=lambda pair: str(pair[0])):
        row = dict(zip(('family', 'mask', 'record_kind', 'comparison', 'level', 'verdict', 'archive'), key))
        row.update(records=count, example_margins=' | '.join(sorted(margins[key])[:3]))
        rows.append(row)
    save('replay-coverage.csv', rows)
    return {row['family'] for row in rows}

def observations():
    counts = C.Counter()
    examples = {}
    for mode in ('native', 'double'):
        path = EVAL / '2026-11-15-fit-snapshot-data' / f'{mode}.jsonl.gz'
        for record in records(path):
            r = record['result']
            o = r.get('outcome')
            if not o:
                continue
            key = (record['family'], record['mask'], ','.join(sorted(record['held'])), o['kind'],
                   o['reason'], str(o.get('certified')).lower(), str(o.get('unique')).lower(),
                   r.get('model', ''), r.get('core_status'), bool(r.get('maximizer_set')))
            counts[key] += 1
            examples.setdefault(key, f'{mode}:{record["source"]}:{record["case"]}')
    rows = []
    for key, count in sorted(counts.items(), key=lambda pair: str(pair[0])):
        row = dict(zip(('family', 'mask', 'held', 'kind', 'reason', 'certified', 'unique',
                        'limit_model', 'core_status', 'has_typed_set'), key))
        row.update(observations=count, example=examples[key])
        rows.append(row)
    save('held-observations.csv', rows)
    return rows

def run():
    replay = replay_index()
    archive = observations()
    ledger = []
    metadata_rows = []
    for path in sorted(DIST.glob('*/mle.hpp')):
        family = path.parent.name
        meta, limits = metadata(path.with_name('README.md'))
        for model, field, value, source in limits:
            metadata_rows.append(dict(family=family, model=model, field=field, value=value, source=source))
        for source, status, tag, args, code in sites(path):
            options = [(FACTORIES.get(tag, 'limit'), *flags(tag, args))]
            if tag == 'comparison':
                options = [('estimate', 'false', 'true (helper default)'), ('limit', 'false', 'NA'),
                           ('failure', 'false', 'NA')]
            elif tag == 'delegate':
                continue
            elif tag == 'equal_faces':
                options = [('limit', 'r.certified' if 'r.certified,' in code or 'r.certified)' in code else 'true', 'NA'),
                           ('failure', 'false', 'NA')]
            for kind, cert, unique in options:
                # The static row applies to all holds that reach this return.
                # Archive observations resolve concrete flags for sampled masks.
                holds = ['route-dependent']
                if family == 'lomax' and kind == 'estimate':
                    holds = ['none', 'kappa', 'lambda', 'all-held']
                elif family in {'invpareto', 'pareto2'} and tag == 'estimate':
                    holds = ['none', 'shape', 'scale', 'all-held']
                elif family in LOMAX and kind == 'limit':
                    holds = ['none']
                elif tag == 'estimate':
                    matches = [row for row in archive if row['family'] == family and row['kind'] == 'estimate'
                               and (cert not in {'true','false'} or row['certified'] == cert)]
                    holds = sorted({row['held'] or 'none' for row in matches}) + ['other/unobserved']
                branches = ['constant positive transformed sample', 'nonconstant positive transformed sample'] if (
                    family in LOMAX and status == 'ExponentialLimit' and kind == 'limit') else ['']
                for held, branch in ((held, branch) for held in holds for branch in branches):
                    global_, local, basis, note = classify(family, status, kind, held, cert, unique, meta, tag)
                    mismatch = []
                    if branch == 'constant positive transformed sample':
                        global_, note = ('proof-exact-comparison',
                                         'observations equal; (1+q) log(1+q) > q for q>0 proves nonattainment')
                        mismatch.append('proved-nonexistence-labelled-uncertified')
                    if kind == 'estimate' and 'default' in unique and basis == 'not-addressed':
                        mismatch.append('unique-default-unsubstantiated')
                    if kind == 'limit' and cert == 'false' and global_ == 'proof-practical-margin':
                        mismatch.append('bound-hidden-by-limit-flag')
                    if family == 'fatigue' and kind == 'estimate' and unique == 'false':
                        mismatch.append('proved-set-labelled-uncertified')
                    if family == 'skellam' and kind == 'estimate' and cert == 'true':
                        mismatch.append('likelihood-gap-labelled-global')
                    if kind == 'limit' and family == 'sn' and status in {'RightHalfNormalBoundary','LeftHalfNormalBoundary'} and tag == 'limit':
                        mismatch.append('numerical-detector-unmarked')
                    if global_ == 'proof-practical-margin' and 'Newton' in local and cert != 'false':
                        mismatch.append('global-flag-does-not-grade-local-tol')
                    fit, doc = meta.get('certified', ('', ''))
                    uq, uq_doc = meta.get('uniqueness', ('', ''))
                    producer = {
                        'lomax': 'lomax/certificate.hpp:202; lomax/mle.hpp:183; lomax/mle.hpp:260',
                        'invpareto': 'invpareto/mle.hpp:30; lomax/certificate.hpp:202; lomax/mle.hpp:183',
                        'pareto2': 'pareto2/mle.hpp:20; pareto2/mle.hpp:55; pareto2/mle.hpp:83; lomax/certificate.hpp:202',
                        'fatigue': 'fatigue/mle.hpp:172; fatigue/certificate.hpp:97; fatigue/certificate.hpp:154',
                        'skellam': 'skellam/mle.hpp:145; skellam/mle.hpp:165; core/likelihood_polish.hpp:136',
                        'dblpareto': 'dblpareto/mle.hpp:172; dblpareto/README.md:89',
                        'ht': 'ht/README.md:136; ht/README.md:159',
                        'sn': 'sn/README.md:140',
                        'trgamma': 'trgamma/README.md:204',
                    }.get(family, '')
                    ledger.append(dict(
                        family=family, held=held, status=status, path_class=tag, data_branch=branch,
                        hold_source='code' if family in LOMAX else 'held-observations.csv (family/kind; applicability inferred)' if held != 'route-dependent' else 'code (any hold reaching site)',
                        outcome=kind, reason=args.removeprefix('OutcomeReason::') if 'OutcomeReason::' in args else '',
                        certified=cert, unique=unique, target='family tol; default 1e-10' if kind == 'estimate' and held != 'all-held' else 'NA',
                        global_backing=global_, local_backing=local, uniqueness_basis=basis,
                        note=note, backing_source=producer, mismatch=';'.join(mismatch), outcome_source=source, expression=code,
                        contract_source=f'dev/design/accuracy-contract.md:8; {CORE}/outcome.hpp:23',
                        fit_certified=fit, fit_source=doc, dossier_uniqueness=uq, uniqueness_source=uq_doc,
                        replay_source='replay-coverage.csv (join by family, mask, comparison)' if family in replay else '',
                        flag_confidence='read-and-verified (expression)', evidence_confidence='inferred' if family != 'lomax' else 'read-and-verified' if kind in {'estimate', 'limit'} else 'inferred',
                    ))
    save('ledger.csv', ledger)
    save('limit-metadata.csv', metadata_rows)
    notes = []
    for family in sorted({row['family'] for row in ledger}):
        family_rows = [row for row in ledger if row['family'] == family and row['mismatch']]
        for label in sorted({label for row in family_rows for label in row['mismatch'].split(';')}):
            matched = [row for row in family_rows if label in row['mismatch'].split(';')]
            locations = sorted({row['outcome_source'] for row in matched})
            field = 'uniqueness_source' if label == 'unique-default-unsubstantiated' else 'fit_source'
            notes.append(dict(family=family, flag=label, distinct_return_sites=len(locations),
                              code='; '.join(locations), dossier='; '.join(sorted({row[field] for row in matched})),
                              confidence=matched[0]['evidence_confidence']))
    save('family-notes.csv', notes)
    assert len({row['family'] for row in ledger}) == 86
    assert {row['outcome'] for row in ledger} == set(FACTORIES.values())
    for column in ('outcome', 'global_backing', 'mismatch'):
        print(column, dict(C.Counter(row[column] for row in ledger)))
        if column == 'mismatch':
            for label in sorted(set(row[column] for row in ledger) - {''}):
                print(label, len({row['family'] for row in ledger if row[column] == label}))
    print('rows', len(ledger), 'observed classes', len(archive))
    for label in sorted({label for row in ledger for label in row['mismatch'].split(';') if label}):
        matching = [row for row in ledger if label in row['mismatch'].split(';')]
        print(label, len(matching), len({row['family'] for row in matching}),
              len({(row['outcome_source'], row['path_class']) for row in matching}))
    print('universal', len([row for row in ledger if row['uniqueness_basis'] == 'universal-proof']),
          len({row['family'] for row in ledger if row['uniqueness_basis'] == 'universal-proof'}))
    print('per-input', len([row for row in ledger if row['uniqueness_basis'] == 'per-input-certificate']),
          len({row['family'] for row in ledger if row['uniqueness_basis'] == 'per-input-certificate'}))
    print('replay families', ', '.join(sorted(replay)))

if __name__ == '__main__':
    run()
