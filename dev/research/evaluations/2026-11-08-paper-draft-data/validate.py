#!/usr/bin/env python3
"""Check manuscript structure, retained inputs, coverage and final PDF diagnostics.
Run from repository root after two pdflatex passes. This checks transcription,
not the correctness of every source theorem or floating-point comparison.
"""
from pathlib import Path
import collections
import hashlib
import json
import re
import subprocess

ROOT = Path.cwd()
OUT = ROOT / 'dev/research/evaluations/2026-11-08-paper-draft-data'
TEX = ROOT / 'dev/research/paper/draft.tex'
s = TEX.read_text()
assert '\u2014' not in s and '---' not in s, 'em dash'
labels = re.findall(r'\\label\{([^}]+)\}', s)
refs = re.findall(r'\\(?:eqref|ref)\{([^}]+)\}', s)
bib = re.findall(r'\\bibitem\{([^}]+)\}', s)
cites = [x.strip() for g in re.findall(r'\\cite\{([^}]+)\}', s) for x in g.split(',')]
assert len(labels) == len(set(labels))
assert len(bib) == len(set(bib))
assert set(refs) <= set(labels), set(refs)-set(labels)
assert set(cites) <= set(bib), set(cites)-set(bib)
assert set(bib) <= set(cites), 'unused bibliography entry'
stack = []
for kind, name in re.findall(r'\\(begin|end)\{([^}]+)\}', s):
    if kind == 'begin':
        stack.append(name)
    else:
        assert stack and stack.pop() == name, name
assert not stack

# Extract macro arguments with nested braces for a complete author-decision list.
def arguments(text, macro):
    answer = []
    for m in re.finditer(re.escape('\\' + macro + '{'), text):
        i = m.end(); depth = 1; j = i
        while depth:
            if text[j] == '{' and text[j-1] != '\\': depth += 1
            if text[j] == '}' and text[j-1] != '\\': depth -= 1
            j += 1
        answer.append({'line': text.count('\n', 0, m.start())+1, 'text': text[i:j-1]})
    return answer

D = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
coverage = collections.defaultdict(list)
for p in sorted(D.glob('*/README.md')):
    m = re.search(r'^  certified: (.+)', p.read_text(), re.M)
    assert m, str(p)
    coverage[m[1].strip("'\"")].append(p.parent.name)
assert {k:len(v) for k,v in coverage.items()} == {'yes':81, 'partial':18}
assert '79&20&99' in s and '81&18&99' in s

# Compare the printed BNB histogram with all 500 frozen observations.
golden = json.loads((D/'bnbinom/golden.json').read_text())['data']
block = s.split('These 500 observations')[0].split('all omitted counts')[1]
rows = re.findall(r'^ (x|n_x)&([^\n]+)', block, re.M)
printed = {}
for i in range(0, len(rows), 2):
    xs = list(map(int, re.findall(r'\d+', rows[i][1].split('\\\\')[0])))
    ns = list(map(int, re.findall(r'\d+', rows[i+1][1].split('\\\\')[0])))
    assert len(xs) == len(ns)
    printed.update(zip(xs, ns))
assert printed == dict(collections.Counter(golden)), (printed, collections.Counter(golden))
assert sum(printed.values()) == 500

# Exact decimal strings of beta-prime, weighted t, and held-skew t samples.
data = ROOT/'dev/research/evaluations'
beta = json.loads((data/'2026-11-07-betapr-prof-data/counterexample-refined.json').read_text())
for value in beta['x'][2:] + [beta['a']]:
    assert str(value) in s, value
assert '1.9901008294052028' in s and '2.209857117039297' in s
fixtures = json.loads((data/'2026-11-04-t-closure-data/fixtures.json').read_text())
for fixture in fixtures.values():
    for value in fixture['x']:
        assert str(value) in s, value
skew = json.loads((data/'2026-11-06-sstd-cert-data/skew-scale.json').read_text())
for value in skew['data']:
    assert str(value) in s, value

# Inventory every local showcase destination, including source reports.
showcase = ROOT/'dev/research/showcase.md'
destinations = []
for target in re.findall(r'\[[^\]]*\]\(([^)]+)\)', showcase.read_text()):
    if '://' in target or target.startswith('#'): continue
    path = (showcase.parent/target.split('#')[0]).resolve()
    assert path.exists(), str(path)
    destinations.append(path)
source_paths = set(destinations) | {showcase}
for relative in [
 'dev/design/work-budgets.md', 'dev/validation/diagnostics.md',
 'dev/research/evaluations/2026-11-04-dqm-b-report.md',
 'dev/research/evaluations/2026-11-04-dqm-c-report.md',
 'dev/research/evaluations/2026-11-06-pig-cert-data/concavity-proof.md',
 'dev/research/evaluations/2026-11-07-betapr-prof-data/counterexample-refined.json',
 'dev/research/evaluations/2026-11-06-bnbinom-cert-data/nonuniqueness.json',
 'dev/research/evaluations/2026-11-04-t-closure-data/fixtures.json',
 'dev/research/evaluations/2026-11-06-sstd-cert-data/skew-scale.json',
 'dev/research/evaluations/2026-11-06-nbfam-cert-data/zinb-tie.json',
 'dev/research/upstream/actuar.md', 'dev/research/upstream/extraDistr.md',
 'dev/research/upstream/verify.R', 'dev/research/upstream/verify.out',
]: source_paths.add(ROOT/relative)
for family in ['nbinom','pig','kumar','burr','ztnbinom','zinb','gpd','frechet','std','sged','bnbinom']:
    source_paths.add(D/family/'README.md')
source_paths.add(D/'bnbinom/golden.json')
manifest = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(source_paths) if p.is_file()}

pdf = ROOT/'build/paper-draft/draft.pdf'
assert pdf.stat().st_mtime_ns >= TEX.stat().st_mtime_ns, 'stale PDF'
log = (pdf.parent/'draft.log').read_text(encoding='latin-1')
assert not re.search(r'undefined|LaTeX Error|Overfull \\[hv]box|Rerun to get|Label\(s\) may have changed', log), 'PDF diagnostics'
info = subprocess.check_output(['pdfinfo', str(pdf)], text=True)
pages = int(re.search(r'^Pages:\s+(\d+)', info, re.M)[1])
assert 20 <= pages <= 30, pages
pdftext = subprocess.check_output(['pdftotext', str(pdf), '-'], text=True)
assert '??' not in pdftext
normalized = re.sub(r'\s+', ' ', pdftext.replace('-\n', ''))
assert 'Other families use different documented' in normalized
signature = (ROOT/'build/scratch/paper-draft/extraDistr-signature.txt').read_text()
assert 'function (x, size, prob, pi, log = FALSE)' in signature
assert '1.10.0.5' in signature
for path in [TEX, OUT/'validate.py']:
    assert all(line == line.rstrip() for line in path.read_text().splitlines()), path
results = {
 'source_sha256':hashlib.sha256(TEX.read_bytes()).hexdigest(),
 'pdf_sha256':hashlib.sha256(pdf.read_bytes()).hexdigest(),
 'pdf_pages':pages, 'sections':re.findall(r'\\section\{([^}]+)\}', s),
 'theorems':re.findall(r'\\begin\{theorem\}\[([^]]+)\]', s),
 'propositions':re.findall(r'\\begin\{proposition\}\[([^]]+)\]', s),
 'todos':arguments(s,'todo'), 'bibliography_entries':len(bib),
 'doi_entries':len(re.findall(r'\\href\{https://doi.org/', s)),
 'extraDistr_signature': 'dzinb(x, size, prob, pi, log = FALSE), version 1.10.0.5',
 'coverage_counts':{k:len(v) for k,v in coverage.items()}, 'coverage_families':coverage,
 'showcase_local_destinations_checked':len(destinations),
 'source_hashes':manifest,
 'checks':['labels and citations resolved', 'balanced environments', 'no em dashes',
           'BNB lossless 500-count histogram', 'beta-prime sample transcription',
           'both weighted t samples', 'held-skew t sample transcription',
           'showcase local destinations exist', '20 to 30 pages',
           'no undefined references, overfull boxes, or PDF question-mark placeholders'],
}
(OUT/'validation.json').write_text(json.dumps(results, indent=2, ensure_ascii=False)+'\n')
print(json.dumps({k:results[k] for k in ['pdf_pages','bibliography_entries','doi_entries','coverage_counts']}, indent=2))
print(f"PASS: {len(results['theorems'])} theorems, {len(results['propositions'])} propositions, {len(results['todos'])} author decisions")
