#!/usr/bin/env python3
"""Join dossier regimes to reviewed evidence and write the existence atlas.

Run python3 dev/tools/existence_atlas.py --overview to refresh the overview's
generated section. The Lean map records reviewed theorem coverage. Nested CSV
fields use JSON so each distinct boundary path retains its own provenance.
"""
import argparse
import csv
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from existence_keys import limit_groups

DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
ATLAS = ROOT / 'dev/research/existence-atlas'
QUESTIONS = ('criterion', 'failure', 'uniqueness')
TRUNCATE_MODIFIERS = tuple(f'truncate:{value}' for value in
                           ('known', 'lower-estimated', 'upper-estimated', 'both-estimated'))
ZERO_MODIFIERS = tuple(f'zero:{value}' for value in ('truncated', 'modified', 'inflated'))
ZERO_ATOMS = {'zero:modified': 'p0', 'zero:inflated': 'pi'}
MODIFIER_PROOF_FIELDS = ('family', 'held', 'modifier', 'question', 'claim', 'direction',
                         'status', 'evidence', 'frontier', 'origin')
MODIFIER_DIRECTIONS = {'iff', 'exact', 'one-sided', 'open'}
MODIFIER_STATUSES = {'written', 'cited', 'unproved'}
PROVED = {'lean', 'written', 'cited'}
LIMIT_GRADES = {'proved', 'numerical', 'argued', 'conjecture'}
PROOF_RANK = {'lean': 2, 'written': 1, 'cited': 1, 'unproved': 0}
START = '<!-- BEGIN GENERATED ATLAS -->'
END = '<!-- END GENERATED ATLAS -->'


def read_csv(name):
    with (ATLAS / name).open(newline='', encoding='utf-8') as stream:
        return list(csv.DictReader(stream))


def source(path, node):
    return f'{path.relative_to(ROOT)}:{node.start_mark.line + 2}'


def key(node, name):
    return next((value for label, value in node.value if label.value == name), None)


def encode(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True)


def dossiers():
    for path in sorted(DIST.glob('*/README.md')):
        front = path.read_text(encoding='utf-8').split('---', 2)[1].removeprefix('\n')
        yield path, yaml.safe_load(front), yaml.compose(front)


def proof_reference_kind(ref):
    if ref.startswith('UnivariateML.'):
        return 'lean'
    if ref.startswith('cite: '):
        return 'cited'
    return 'written'


def proof_anchors(path):
    anchors = set()
    counts = defaultdict(int)
    fence = ''
    for line in path.read_text(encoding='utf-8').splitlines():
        marker = re.match(r'^\s{0,3}(`{3,}|~{3,})', line)
        if marker:
            if not fence:
                fence = marker[1]
            elif marker[1].startswith(fence):
                fence = ''
            continue
        if fence:
            continue
        anchors.update(re.findall(r'''\b(?:id|name)=["']([^"']+)["']''', line))
        heading = re.match(r'^\s{0,3}#{1,6}\s+(.+?)(?:\s+#+)?$', line)
        if heading:
            explicit = re.search(r'\{#([^}]+)\}$', heading[1])
            if explicit:
                anchors.add(explicit[1])
                continue
            slug = re.sub(r'[^\w -]', '', heading[1].lower()).replace(' ', '-')
            count = counts[slug]
            anchors.add(f'{slug}-{count}' if count else slug)
            counts[slug] += 1
    return anchors


def check_proof_references(entries):
    declarations = {row['theorem'] for _, row in entries if not row['proof_marker']}
    anchors = {}
    issues = []
    for path, meta, _ in dossiers():
        records = ((scope, index, record) for scope, group in limit_groups(meta)
                   for index, record in enumerate(group, 1))
        for scope, index, record in records:
            location = f'{path.relative_to(ROOT)}: {scope}[{index}]'
            for field in ('firmness', 'detect_firmness'):
                if field in record and (not isinstance(record[field], str) or
                                        record[field] not in LIMIT_GRADES):
                    issues.append(f'{location}.{field}: unknown proof grade')
            if 'numerical' in record:
                issues.append(f'{location}: use detect_firmness for the detector grade')
            for field in ('proof', 'detect_proof'):
                refs = record.get(field, [])
                field_location = f'{location}.{field}'
                if not isinstance(refs, list) or any(
                        not isinstance(ref, str) or not ref.strip() for ref in refs):
                    issues.append(f'{field_location}: expected a list of proof references')
                    continue
                for ref in refs:
                    kind = proof_reference_kind(ref)
                    if kind == 'lean':
                        if ref not in declarations:
                            issues.append(f'{field_location}: unmapped declaration {ref}')
                    elif kind == 'cited':
                        if not ref.removeprefix('cite: ').strip():
                            issues.append(f'{field_location}: empty citation')
                    else:
                        file, _, anchor = ref.partition('#')
                        target = (ROOT / file).resolve()
                        if Path(file).is_absolute() or not target.is_relative_to(ROOT) or not target.is_file():
                            issues.append(f'{field_location}: missing proof file {file}')
                            continue
                        if target not in anchors:
                            anchors[target] = proof_anchors(target)
                        if anchor not in anchors[target]:
                            issues.append(f'{field_location}: missing proof anchor {ref}')
    return issues


def lean_map():
    paths = [ATLAS / 'lean-map.csv', *sorted((ATLAS / 'lean-map.d').glob('*.csv'))]
    aliases = {(row['family'], row['alias'].casefold()): row['held']
               for row in read_csv('lean-regime-aliases.csv')}
    canonical = {'joint': 'all free', 'all free': 'all free', 'generic': 'generic'}
    for path in paths:
        for line, row in enumerate(read_csv(path), 2):
            label = row['regime'].strip()
            regime = label.casefold()
            row['regime'] = aliases.get((row['family'], regime), canonical.get(regime, label))
            yield f'{path.relative_to(ROOT)}:{line}', row


def lean_coverage(entries):
    coverage = defaultdict(lambda: defaultdict(set))
    for location, row in entries:
        if row['proof_marker']:
            continue
        family, regime = row['family'], row['regime']
        for target, grade in json.loads(row.get('coverage') or '{}').items():
            mask, separator, question = target.partition('/')
            if regime == 'generic' and separator:
                held = '' if mask == 'all free' else mask
            elif regime != 'generic' and not separator:
                held = '' if regime == 'all free' else regime
                question = target
            else:
                raise ValueError(f'{location}: invalid scope for Lean claim {target}')
            if (question not in QUESTIONS and
                    not question.startswith(('limit:', 'path:', 'case:'))):
                raise ValueError(f'{location}: unknown Lean claim {question}')
            if grade not in {'full', 'partial'}:
                raise ValueError(f'{location}: invalid Lean coverage for {question}')
            coverage[family, held, question][grade].add(row['theorem'])
    return coverage


def with_lean(proof, coverage):
    proof = dict(proof)
    full = sorted(coverage.get('full', ()))
    partial = sorted(coverage.get('partial', ()))
    if full:
        if proof['direction'] == 'open':
            raise ValueError(f'full Lean coverage needs an audited claim: {full}')
        proof.update(status='lean', evidence='; '.join(full))
    elif not proof['status']:
        proof['status'] = 'unproved'
    if partial:
        proof['lean_partial'] = '; '.join(partial)
    return proof


def audit(coverage):
    used_coverage = set()

    def apply_lean(proof, held, question):
        claim_key = family, held, question
        used_coverage.add(claim_key)
        return with_lean(proof, coverage.get(claim_key, {}))

    reviewed = defaultdict(dict)
    named = defaultdict(set)
    for row in read_csv('proofs.csv'):
        family, held, question = (row[field] for field in ('family', 'held', 'question'))
        if question not in QUESTIONS and not question.startswith(('limit:', 'path:', 'case:')):
            raise ValueError(f'{family} {held}: unknown question {question}')
        if row['status'] not in {'written', 'cited', 'unproved', ''}:
            raise ValueError(f'{family} {held}: unknown proof status')
        if row['status'] == 'written':
            file = row['evidence'].partition('#')[0]
            if not ((ROOT / file).is_file() or (ATLAS / file).is_file()):
                raise ValueError(f'{family} {held}: missing written proof source {file}')
        if question in reviewed[family, held]:
            raise ValueError(f'duplicate proof key: {family} {held} {question}')
        reviewed[family, held][question] = {
            field: row[field] for field in ('claim', 'direction', 'status', 'evidence', 'frontier')
        }
        if row['origin']:
            named[family].add((held, row['origin']))

    scopes = {(row['family'], int(row['index'])): row
              for row in read_csv('limit-scopes.csv')}
    consumed_scopes = set()
    consumed = set()
    for path, meta, syntax in dossiers():
        family = meta['id']
        ex = meta.get('existence') or {}
        ex_node = key(syntax, 'existence')
        ex_source = source(path, ex_node) if ex_node else str(path.relative_to(ROOT))
        params = {p['name']: p for p in meta['params']}
        origins = defaultdict(set)
        holds = defaultdict(list)
        limits = defaultdict(list)
        origins[''].add('all free')
        for held, origin in named[family]:
            origins[held].add(origin)
        required = sorted(p['name'] for p in meta['params']
                          if p.get('kind') in {'design', 'threshold'})
        if required:
            origins['+'.join(required)].add('required design or threshold parameters')
        for field in ('holds', 'limits'):
            records = meta.get(field, [])
            nodes = key(syntax, field)
            for index, record in enumerate(records):
                location = source(path, nodes.value[index])
                held = '+'.join(sorted(record.get('held', record.get('fixed', []))))
                origins[held].add(f'{field}[{index + 1}] at {location}')
                if field == 'holds':
                    holds[held].append(record)
                    continue
                number = index + 1
                scope = scopes.get((family, number))
                effective = scope['held_masks'].split(';') if scope else [held or 'all free']
                if scope:
                    consumed_scopes.add((family, number))
                for effective_held in effective:
                    mask = '' if effective_held == 'all free' else effective_held
                    if scope:
                        origins[mask].add(f'limits[{number}].detect or when at {location}')
                    limits[mask].append((number, record, location, held, scope))
        equivariance = ex.get('equivariance') or {}
        if equivariance.get('held'):
            held = '+'.join(sorted(equivariance['held']))
            origins[held].add(f'existence.equivariance at {ex_source}')
        for held, origin in sorted(origins.items()):
            mask = set(filter(None, held.split('+')))
            if mask - params.keys() or held != '+'.join(sorted(mask)):
                raise ValueError(f'{family}: invalid or unsorted held mask {held}')
            evidence = reviewed.get((family, held), {})
            consumed.update((family, held, question) for question in evidence
                            if question in QUESTIONS or question.startswith('case:'))
            structural = holds[held]
            grade = ex.get('uniqueness', 'missing')
            grade = 'unknown' if grade == 'proved' else grade
            rows = {}
            for question in QUESTIONS:
                if mask == params.keys():
                    rows[question] = {
                        'criterion': ('The supplied feasible parameter vector attains the likelihood on the singleton domain.', 'iff'),
                        'failure': ('There is no parameter escape on a singleton feasible domain.', 'exact'),
                        'uniqueness': ('The supplied feasible vector is the sole candidate.', 'exact'),
                    }[question]
                    claim, direction = rows[question]
                    rows[question] = dict(claim=claim, direction=direction, status='written',
                                          evidence='overview.md#existence-atlas', frontier='')
                else:
                    frontier = {
                        'criterion': 'Prove attainment on a compact likelihood superlevel set and give the converse for every omitted sample branch.',
                        'failure': 'Exhaust the escaping sequences under this mask and bound their joint likelihood, including support contacts and ties.',
                        'uniqueness': 'On the attained regime, establish a strict global profile bound or characterize the entire maximizing set.',
                    }[question]
                    rows[question] = evidence.get(question, dict(
                        claim='No complete audited result for this regime.', direction='open',
                        status='unproved', evidence=grade if question == 'uniqueness' else 'missing',
                        frontier=frontier,
                    ))
            for question in QUESTIONS:
                rows[question] = apply_lean(rows[question], held, question)
                rows[question].setdefault('lean_partial', '')
            cases = []
            for index, limit, location, declared, scope in limits[held]:
                record = dict(index=index, source=location, declared_fixed=declared or 'unspecified',
                              scope_note=scope['note'] if scope else '', model=limit.get('model'),
                              name=limit.get('name'), kind=limit.get('kind', 'limit'),
                              path=limit.get('when'), detector=limit.get('detect'),
                              dossier_grade=limit.get('firmness', 'missing'),
                              detect_firmness=limit.get('detect_firmness', 'missing'))
                for label, purpose in [('path', 'Construct an eventually feasible path and derive its joint sample limit.'),
                                       ('limit', 'Show the stated detector implies global dominance and nonattainment.')]:
                    proof_key = f'{label}:{index}'
                    proof_mask = held if proof_key in evidence else declared
                    override = reviewed.get((family, proof_mask), {}).get(proof_key)
                    if override:
                        consumed.add((family, proof_mask, proof_key))
                        if label == 'path' and override['claim'] != limit.get('when'):
                            raise ValueError(f'{location}: reviewed path has changed')
                    field = 'proof' if label == 'path' else 'detect_proof'
                    refs = limit.get(field, [])
                    grade = (record['dossier_grade'] if label == 'path' else
                             record['detect_firmness'])
                    proof = override or dict(
                        status='unproved', direction='open',
                        claim=(limit.get('when') if label == 'path' else limit.get('detect')) or '',
                        evidence='unknown' if grade == 'proved' else grade,
                        frontier=purpose)
                    result = apply_lean(proof, held, proof_key)
                    if not override and result['status'] == 'unproved' and grade == 'proved' and refs:
                        kinds = {proof_reference_kind(ref) for ref in refs}
                        status = 'written' if 'written' in kinds else 'cited' if 'cited' in kinds else 'lean'
                        result.update(status=status, direction='exact', evidence='; '.join(refs),
                                      frontier='')
                    result['dossier_references'] = refs
                    record[label + '_proof'] = result
                cases.append(record)
            yield dict(family=family, held=held or 'all free',
                       free='+'.join(sorted(params.keys() - mask)), origin=' | '.join(sorted(origin)),
                       stage=meta.get('stage', 'fit'),
                       domains=encode({name: {field: p[field] for field in ('domain', 'fit_domain', 'kind')
                                              if field in p} for name, p in params.items()}),
                       dossier=ex_source, dossier_statement=ex.get('statement', ''),
                       dossier_uniqueness=ex.get('uniqueness', 'missing'),
                       structural=encode(structural), limits=encode(cases),
                       modifier_limits=encode({
                           scope: group for scope, group in limit_groups(meta)
                           if not held and scope != 'limits' and group}),
                       proved_cases=encode({question.removeprefix('case:'): apply_lean(proof, held, question)
                                            for question, proof in evidence.items()
                                            if question.startswith('case:')}),
                       strict_layer=encode({name: ex[name] for name in ('collapse', 'two_point',
                                            'strict_exceptions', 'min_n') if name in ex}),
                       **{f'{question}_{field}': value for question, proof in rows.items()
                          for field, value in proof.items()})
    unused = {(family, held, question) for (family, held), entries in reviewed.items()
              for question in entries} - consumed
    if scopes.keys() - consumed_scopes:
        raise ValueError(f'limiting paths missing from dossiers: {sorted(scopes.keys() - consumed_scopes)}')
    if unused:
        raise ValueError(f'proof entries without dossier regime: {sorted(unused)}')
    if coverage.keys() - used_coverage:
        raise ValueError(f'Lean coverage without atlas claim: {sorted(coverage.keys() - used_coverage)}')


def modifier_regimes(base_rows):
    metadata = {meta['id']: meta for _, meta, _ in dossiers()}
    for base in base_rows:
        family, held = base['family'], base['held']
        meta = metadata[family]
        declared = meta.get('modifiers', {})
        support = meta['support']
        zero_eligible = (
            support['type'] == 'integer' and str(support['lower']) in ('0', '0.0')
            and not set(meta.get('construction', [])) & {'truncation', 'modification'}
        ) or declared.get('zero_exception', False)
        mask = set() if held == 'all free' else set(held.split('+'))
        modifiers = TRUNCATE_MODIFIERS + (ZERO_MODIFIERS if zero_eligible else ())
        for modifier in modifiers:
            kind, value = modifier.split(':')
            if kind == 'zero':
                capability = declared.get('zero', {}).get(value, {})
                exceptions = capability.get('holds', meta.get('holds', []))
            else:
                capability = declared.get('truncate', {})
                exceptions = capability.get('holds', [])
            status = capability.get('status', 'not-implemented')
            if (base['stage'] != 'fit' or
                    not set(capability.get('requires_held', ())) <= mask or
                    set(capability.get('requires_free', ())) & mask):
                status = 'not-implemented'
            if status == 'implemented':
                status = next((entry['status'] for entry in exceptions
                               if set(entry['held']) == mask), status)
            regime = dict(family=family, held=held, modifier=modifier,
                          implemented=str(status == 'implemented').lower(),
                          implementation_status=status,
                          implementation_domain=capability.get('domain_bounded', ''))
            yield regime
            if modifier in ZERO_ATOMS:
                yield dict(regime, held='+'.join(sorted(mask | {ZERO_ATOMS[modifier]})))


def read_modifier_proofs(path, regimes):
    families = {row['family'] for row in regimes}
    base_masks = {(row['family'], row['held']) for row in regimes
                  if row['modifier'] == 'truncate:known'}
    keys = {(row['family'], row['held'], row['modifier']) for row in regimes}
    reviewed = defaultdict(dict)
    seen = set()
    anchors = {}
    issues = []
    with path.open(newline='', encoding='utf-8') as stream:
        reader = csv.DictReader(stream, strict=True)
        try:
            if tuple(reader.fieldnames or ()) != MODIFIER_PROOF_FIELDS:
                return reviewed, [f'{path}: expected header {",".join(MODIFIER_PROOF_FIELDS)}']
            for row in reader:
                location = f'{path}:{reader.line_num}'
                if None in row or None in row.values():
                    issues.append(f'{location}: wrong number of fields')
                    continue
                family, held, modifier, question = (row[field] for field in MODIFIER_PROOF_FIELDS[:4])
                regime = family, held or 'all free', modifier
                label = f'{location}: {" / ".join((*regime, question))}'
                errors = []
                if family not in families:
                    errors.append('unknown family')
                if modifier not in TRUNCATE_MODIFIERS + ZERO_MODIFIERS:
                    errors.append('unknown modifier')
                elif family in families and regime not in keys:
                    mask = set(regime[1].split('+'))
                    atom = ZERO_ATOMS.get(modifier)
                    parent_held = '+'.join(sorted(mask - {atom})) or 'all free'
                    if ((family, parent_held) in base_masks
                            and regime[1] == '+'.join(sorted(mask))
                            and (family, parent_held, modifier) not in keys):
                        errors.append('ineligible modifier')
                    else:
                        errors.append('no modifier regime for held mask')
                if question not in QUESTIONS and not (
                        question.startswith('case:') and question.removeprefix('case:').strip()):
                    errors.append('unknown question')
                if row['status'] not in MODIFIER_STATUSES:
                    errors.append('unknown proof status')
                if row['direction'] not in MODIFIER_DIRECTIONS:
                    errors.append('unknown proof direction')
                if row['status'] in PROVED and (row['direction'] == 'open' or not row['claim'].strip()):
                    errors.append('proved claim requires wording and a non-open direction')
                if row['status'] == 'written':
                    for ref in row['evidence'].split('; '):
                        file, _, anchor = ref.partition('#')
                        target = (ROOT / file).resolve()
                        if not target.is_file():
                            target = (ATLAS / file).resolve()
                        if (Path(file).is_absolute() or not target.is_relative_to(ROOT)
                                or not target.is_file()):
                            errors.append(f'missing written proof file {file!r}')
                            continue
                        if target not in anchors:
                            anchors[target] = proof_anchors(target)
                        if anchor not in anchors[target]:
                            errors.append(f'missing proof anchor {ref!r}')
                if row['status'] == 'cited' and not row['evidence'].strip():
                    errors.append('empty citation')
                claim_key = (*regime, question)
                if claim_key in seen:
                    errors.append('duplicate proof key')
                seen.add(claim_key)
                if errors:
                    issues.append(f'{label}: {"; ".join(errors)}')
                else:
                    reviewed[regime][question] = {
                        field: row[field] for field in MODIFIER_PROOF_FIELDS[4:]
                    }
        except csv.Error as error:
            issues.append(f'{path}:{reader.line_num}: {error}')
    return reviewed, issues


def modifier_atlas(regimes, reviewed):
    for regime in regimes:
        proofs = reviewed.get(tuple(regime[field] for field in MODIFIER_PROOF_FIELDS[:3]), {})
        row = dict(regime)
        for question in QUESTIONS:
            proof = proofs.get(question, dict(claim='', direction='open', status='unproved',
                                              evidence='', frontier='', origin=''))
            row.update({f'{question}_{field}': value for field, value in proof.items()})
        row['proved_cases'] = encode({question.removeprefix('case:'): proof
                                      for question, proof in proofs.items()
                                      if question.startswith('case:')})
        yield row


def modifier_overview(rows):
    lines = [
        '# Modifier existence atlas', '',
        'Generated by `python3 dev/tools/existence_atlas.py`.',
        'See the [format](README.md) and [regime data](modifier-atlas.csv).', '',
        f'{len(rows)} eligible regimes. The question columns count proved directions as',
        'complete or one-sided. The remaining regimes for each question are open.', '',
        '| Modifier | Eligible | Implemented | Criterion complete / one-sided | '
        'Failure complete / one-sided | Uniqueness complete / one-sided |',
        '| --- | ---: | ---: | ---: | ---: | ---: |',
    ]
    for modifier in TRUNCATE_MODIFIERS + ZERO_MODIFIERS:
        subset = [row for row in rows if row['modifier'] == modifier]
        cells = [modifier, str(len(subset)), str(sum(row['implemented'] == 'true' for row in subset))]
        for question in QUESTIONS:
            proved = [row[f'{question}_direction'] for row in subset
                      if row[f'{question}_status'] in PROVED]
            complete = sum(direction in {'iff', 'exact'} for direction in proved)
            cells.append(f"{complete} / {proved.count('one-sided')}")
        lines.append('| ' + ' | '.join(cells) + ' |')
    return '\n'.join(lines) + '\n'


def group(row):
    if row['criterion_status'] not in PROVED or row['criterion_direction'] != 'iff':
        return 'Criterion open'
    if all(row[f'{question}_status'] in PROVED and
           row[f'{question}_direction'] in {'exact', 'iff'} for question in QUESTIONS):
        return 'All three questions proved'
    if row['uniqueness_status'] not in PROVED or row['uniqueness_direction'] != 'exact':
        return 'Criterion proved, uniqueness open'
    return 'Criterion and cardinality proved, failure classification open'


def overview(rows):
    groups = defaultdict(list)
    for row in rows:
        groups[group(row)].append(row)
    lines = [f'{len({row["family"] for row in rows})} families, {len(rows)} regimes.']
    for label in ('All three questions proved', 'Criterion proved, uniqueness open',
                  'Criterion and cardinality proved, failure classification open', 'Criterion open'):
        subset = groups[label]
        if not subset:
            continue
        lines.extend(['', f'### {label} ({len(subset)})', '',
                      '| Family | Held coefficients | Criterion | Failure modes | Cardinality |',
                      '| --- | --- | --- | --- | --- |'])
        for row in subset:
            cells = [row['family'], row['held']]
            for question in QUESTIONS:
                value = row[f'{question}_status']
                direction = row[f'{question}_direction']
                partial = row[f'{question}_lean_partial']
                suffix = ' · partial Lean' if partial and value != 'lean' else ''
                cells.append(f'{value} ({direction}){suffix}')
            lines.append('| ' + ' | '.join(cells) + ' |')
    return '\n'.join(lines)


def atlas_claims(rows):
    for row in rows:
        family, held = row['family'], row['held']
        for question in QUESTIONS:
            yield (family, held, question), {
                field: row[f'{question}_{field}'] for field in ('status', 'evidence')}
        for question, proof in json.loads(row['proved_cases']).items():
            yield (family, held, f'case:{question}'), proof
        for limit in json.loads(row['limits']):
            for label in ('path', 'limit'):
                yield (family, held, f"{label}:{limit['index']}"), limit[label + '_proof']


def check(entries, rows):
    families = {row['family'] for row in rows}
    regimes = {(row['family'], row['held']) for row in rows}
    mapped = defaultdict(set)
    declarations = {row['theorem'] for _, row in entries if not row['proof_marker']}
    generic_coverage = lean_coverage(
        (location, row) for location, row in entries if row['regime'] == 'generic')
    issues = []
    for location, row in entries:
        family, regime = row['family'], row['regime']
        if ((family, regime) not in regimes and
                not (regime == 'generic' and family in families | {'generic'})):
            issues.append(f'{location}: no atlas regime for {family} / {regime}')
        if not row['proof_marker']:
            mapped[family, regime].add(row['theorem'])

    for (family, held, question), proof in atlas_claims(rows):
        if proof['status'] == 'lean':
            generic = generic_coverage.get(
                (family, '' if held == 'all free' else held, question), {}).get('full', ())
            for theorem in proof['evidence'].split('; '):
                if (theorem not in mapped[family, held] and theorem not in generic and
                        not (theorem in proof.get('dossier_references', ()) and theorem in declarations)):
                    issues.append(f'{family} / {held} / {question}: unmapped theorem {theorem}')
    return issues


def check_regressions(rows):
    fields = ('family', 'held', 'question')
    current = {key: proof['status'] for key, proof in atlas_claims(rows)}
    exceptions = {}
    for row in read_csv('proof-status-exceptions.csv'):
        key = tuple(row[field] for field in fields)
        if not row['reason'].strip() or key in exceptions or row['status'] not in PROOF_RANK:
            raise ValueError(f'invalid or duplicate proof status exception: {key}')
        exceptions[key] = row['status']

    issues = []
    for row in read_csv('proof-status-baseline.csv'):
        key = tuple(row[field] for field in fields)
        before = row['status']
        after = current.get(key, 'absent')
        if PROOF_RANK.get(after, 0) < PROOF_RANK[before] and exceptions.get(key) != after:
            issues.append(f"{' / '.join(key)}: proof status regressed from {before} to {after}")
    return issues


def write_atlas(name, rows):
    with (ATLAS / name).open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--overview', action='store_true')
    checks = parser.add_mutually_exclusive_group()
    checks.add_argument('--check', action='store_true',
                        help='validate Lean map scopes and both saved atlases without rewriting them')
    checks.add_argument('--check-modifier-proofs', metavar='PATH', type=Path,
                        help='validate an alternate modifier proof file without writing outputs')
    args = parser.parse_args()
    entries = list(lean_map())
    if args.check:
        saved = read_csv('atlas.csv')
        issues = check_proof_references(entries) + check(entries, saved) + check_regressions(saved)
        if not issues:
            regenerated = list(audit(lean_coverage(entries)))
            issues = check(entries, regenerated) + check_regressions(regenerated)
            if not issues and regenerated != saved:
                issues.append('atlas.csv is stale; regenerate the atlas')
        if issues:
            for issue in issues:
                print(issue)
            return 1
        rows = regenerated
    else:
        rows = list(audit(lean_coverage(entries)))
    regimes = list(modifier_regimes(rows))
    proof_path = args.check_modifier_proofs or ATLAS / 'modifier-proofs.csv'
    reviewed, issues = read_modifier_proofs(proof_path, regimes)
    if issues:
        for issue in issues:
            print(issue)
        return 1
    if args.check_modifier_proofs:
        return 0
    modified = list(modifier_atlas(regimes, reviewed))
    summary = modifier_overview(modified)
    if args.check:
        if read_csv('modifier-atlas.csv') != modified:
            issues.append('modifier-atlas.csv is stale; regenerate the atlas')
        if (ATLAS / 'modifier-overview.md').read_text(encoding='utf-8') != summary:
            issues.append('modifier-overview.md is stale; regenerate the atlas')
        for issue in issues:
            print(issue)
        return int(bool(issues))
    write_atlas('atlas.csv', rows)
    write_atlas('modifier-atlas.csv', modified)
    (ATLAS / 'modifier-overview.md').write_text(summary, encoding='utf-8')
    table = overview(rows)
    if args.overview:
        target = ATLAS / 'overview.md'
        before, _, after = target.read_text(encoding='utf-8').partition(START)
        if END not in after:
            raise ValueError('missing generated overview markers')
        target.write_text(f'{before}{START}\n\n{table}\n\n{END}{after.split(END, 1)[1]}',
                          encoding='utf-8')
    print('\n'.join(line.lstrip('# ') for line in table.splitlines()
                    if line.startswith('### ') or ' families, ' in line))


if __name__ == '__main__':
    raise SystemExit(main())
