"""Blocking static acceptance checks; no R runtime or numerical suite needed."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit, unquote
import json, subprocess, re, os, shutil, sys
import sympy
ROOT=Path(__file__).resolve().parents[2]
DIST=ROOT/'site/dist'
DATA=json.loads((ROOT/'site/src/data/families.json').read_text())
class Document(HTMLParser):
    def __init__(self, text):
        super().__init__(); self.links=[]; self.assets=[]; self.ids=set(); self.math=0; self.display_math=0; self.scripts=[]; self.in_script=False; self.inline_js=0
        self.records={}; self.feed(text)
    def handle_starttag(self, tag, attrs):
        a=dict(attrs)
        for k in ('data-limit','data-holds','data-domain'):
            if k in a:
                key=(k,a[k])
                if key in self.records: raise AssertionError(f'duplicate catalogue row {key}')
                self.records[key]=json.loads(a['data-record'])
        if 'id' in a: self.ids.add(a['id'])
        if tag=='math':
            self.math+=1
            if a.get('display')=='block': self.display_math+=1
        if tag=='script':
            self.scripts.append(a.get('src','')); self.in_script=not a.get('src')
        for key in ('src','href','component-url','renderer-url'):
            if a.get(key): self.links.append((tag,key,a[key]))
        if tag in ('script','link'):
            for key in ('src','href'):
                if a.get(key): self.assets.append(a[key])

    def handle_data(self,data):
        if self.in_script: self.inline_js+=len(data.encode())
    def handle_endtag(self,tag):
        if tag=='script': self.in_script=False

def target(file,url):
    url=unquote(urlsplit(url).path)
    p=DIST/url.lstrip('/') if url.startswith('/') else file.parent/url
    if p.is_dir(): p=p/'index.html'
    return p
pages={p:Document(p.read_text()) for p in DIST.rglob('*.html')}
errors=[]
# Compare rendered catalogue records directly with each canonical YAML dossier.
import yaml
canonical=[]
for f in DATA:
    dossier=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/f['id']/'README.md'
    canonical.append(yaml.safe_load(dossier.read_text().split('---',2)[1]))
for article in ('existence','limits','holds','restrictions'):
    p=DIST/f'articles/{article}/index.html'
    assert p in pages, f'missing article {article}'
    assert pages[p].math or article in ('holds','restrictions'), f'missing article math {article}'
expected_limits={("data-limit",f"{f['id']}:{i}"):e for f in canonical for i,e in enumerate(f.get('limits',[]))}
expected_holds={("data-holds",f['id']):{'params':[p['name'] for p in f['params']],'holds':f.get('holds',[])} for f in canonical}
expected_domains={("data-domain",f"{f['id']}:{p['name']}"):p['fit_domain'] for f in canonical for p in f['params'] if p.get('fit_domain')}
for article,expected in [('limits',expected_limits),('holds',expected_holds),('restrictions',expected_domains)]:
    assert pages[DIST/f'articles/{article}/index.html'].records==expected, f'{article} catalogue differs from dossiers'
# Boundary grades and non-reachable external laws are visible, not only embedded metadata.
lomax_html=(DIST/'distributions/lomax/index.html').read_text()
assert 'firmness proved' in lomax_html and 'numerical detection' in lomax_html
assert 'Lévy' in (DIST/'distributions/invgauss/index.html').read_text()
assert 'Firmness' in (DIST/'articles/limits/index.html').read_text()
# The Markdown transformer otherwise intentionally converts unresolved paths to text.
for source in (ROOT/'univariateML-cpp/notes').glob('*.md'):
    for url in re.findall(r'(?<!!)\[[^]]+\]\(([^)]+)\)',source.read_text()):
        if url.startswith(('/', '#', 'http:', 'https:')): continue
        assert (source.parent/url.split('#')[0]).is_file(), f'unresolved article source link {source}: {url}'
required_article_links={'/articles/existence/#what-can-fail', '/articles/limits/#declared-limits', '/articles/holds/#meaning-and-allowed-subsets', '/articles/restrictions/#scope-and-exemptions'}
for f in canonical:
    page=pages[DIST/f"distributions/{f['id']}/index.html"]
    assert required_article_links <= {url for tag,key,url in page.links if tag=='a' and key=='href'}, f['id']
index=pages[DIST/'articles/index.html']
assert all(f'/articles/{name}/' in {url for tag,key,url in index.links if tag=='a' and key=='href'} for name in ('existence','limits','holds','restrictions'))
print(f'PASS: four articles; dossier catalogues: {len(expected_limits)} limits, {len(expected_holds)} families, {len(expected_domains)} fit domains.')
# Availability is checked against the canonical generator, never inferred from DQM.
sys.path.insert(0,str(ROOT/'dev/validation'))
from inference_availability import generate, OUTPUT, STATUSES
availability=generate()
assert json.loads(OUTPUT.read_text())==availability, 'stale canonical inference availability'
assert {r['family'] for r in availability['rows']}=={f['id'] for f in DATA}
for f in DATA:
    assert f['inference']==[r for r in availability['rows'] if r['family']==f['id']], f['id']

def evidence_anchor(ref):
    path,anchor=ref.split('#',1)
    text=(ROOT/path).read_text()
    # GitHub-compatible slugs for the plain-text headings in these reports.
    slugs={re.sub(r'[^\w\- ]','',line.lstrip('#').strip().lower()).replace(' ','-')
           for line in text.splitlines() if line.startswith('#')}
    assert anchor in slugs, f'missing evidence anchor: {ref}'

keys=set()
for r in availability['rows']:
    key=(r['family'],r['regime'],r['target'])
    assert key not in keys, f'duplicate inference scope: {key}'
    keys.add(key)
    assert r['status'] in STATUSES and r['assumptions'] and r['theory'] and r['procedures']
    evidence_anchor(r['evidence'])
    if r['calibration']: evidence_anchor(r['calibration'])
    if r['status'] in ('implemented','historically recoverable','available but missing'):
        assert r['implementation'], f'positive scope without supporting implementation: {key}'
        for implementation in r['implementation']:
            text=(ROOT/implementation['path']).read_text()
            assert re.search(r'\b'+re.escape(implementation['function'])+r'\s*\(',text),key
    if r['status']=='implemented':
        assert r['calibration'] and r['checks'], key
    for check in r['checks']: assert (ROOT/check).is_file(),check
inference_page=DIST/'inference/index.html'
inference_html=inference_page.read_text()
assert len(re.findall(r'<tr\b[^>]*\bdata-inference-row(?:\s|>)',inference_html))==len(availability['rows']), 'missing static inference rows'
assert 'C++ only' in inference_html and '<noscript>' in inference_html
assert not re.search(r'<astro-island',inference_html), 'availability must render without hydration'
assert not re.search(r'<tr[^>]*data-inference-row[^>]*\shidden(?:[ >])',inference_html)
for status in STATUSES: assert status in inference_html
print(f"PASS: inference: {len(availability['rows'])} unique scopes, 98 families; implementation symbols, evidence anchors, calibration and checks resolve; full static table.")
subprocess.run([str(ROOT/'site/node_modules/.bin/node'), str(ROOT/'site/scripts/inference-browser.mjs')], check=True)

# Theory defaults must be complete before hydration, and match the live fit API.
theory_html=(DIST/'theory/index.html').read_text()
assert all(f'id="{section}"' in theory_html for section in ['profiles','escapes','faces'])
assert theory_html.count('data-profile=')==5 and 'data-faces' in theory_html
assert theory_html.count('data-limit-line')==4
assert all(f'data-escape="{id}"' in theory_html for id in ['lomax','std','nbinom','burr'])
assert '<noscript>' in theory_html
assert 'data-conditional' in theory_html and '<math' in theory_html
assert not re.search(r'<script[^>]*(?:mathjax|katex)', theory_html, re.I)
gompertz_record=next(f for f in DATA if f['id']=='gompertz')
assert any(limit.get('model')=='exp' and limit.get('fixed')==['a'] for limit in gompertz_record['limits'])
for source,model in [('lomax','exp'),('std','norm'),('nbinom','pois'),('burr','pareto')]:
    record=next(f for f in DATA if f['id']==source)
    assert any(limit.get('model')==model and (source!='burr' or limit.get('fixed')==['scale']) for limit in record.get('limits',[])), f'undeclared theory escape: {source}'
subprocess.run([str(ROOT/'site/node_modules/.bin/node'), str(ROOT/'site/src/components/theory/check.mjs')], check=True)
subprocess.run([str(ROOT/'site/node_modules/.bin/node'), str(ROOT/'site/src/components/theory/browser.mjs')], check=True)

# Check the atlas independently against every dossier-derived record.
graph_page=DIST/'graph/index.html'
assert graph_page.exists(), 'missing /graph/'
graph_html=graph_page.read_text()
node_ids=re.findall(r'data-node="([^\"]+)"',graph_html)
assert sorted(node_ids)==sorted(f['id'] for f in DATA), 'graph node coverage/uniqueness'
expected=[]
for f in DATA:
    for kind, edges in f.get('relations',{}).items():
        if kind == 'closure_point': continue
        for i,e in enumerate(edges):
            targets=list(dict.fromkeys(c['family'] for c in e['components'])) if kind=='convolution_of' else list(dict.fromkeys([e['family'],e['mixing']['family']])) if kind=='mixture_of' else [e['family']]
            expected.extend(f"{f['id']}-{kind}-{i}-{j}" for j in range(len(targets)))
    expected.extend(f"{f['id']}-limit-{i}" for i,e in enumerate(f.get('limits',[])) if e.get('model'))
assert sorted(re.findall(r'data-edge="([^\"]+)"',graph_html))==sorted(expected), 'graph edge coverage'
assert sorted(re.findall(r'data-map-node="([^"]+)"',graph_html))==sorted(f['id'] for f in DATA), 'map node coverage'
assert sorted(re.findall(r'data-map-edge="([^"]+)"',graph_html))==sorted(expected), 'map edge coverage'
assert graph_html.count('data-family-map=""')==1, 'one overview SVG'
node=str(ROOT/'site/node_modules/.bin/node')
command=[node,str(ROOT/'site/scripts/graph-check.mjs')]
first=subprocess.check_output(command); second=subprocess.check_output(command)
assert first==second, 'non-deterministic SVG generator'
for svg in json.loads(first):
    # Empty limit panels are deliberately omitted.
    if 'data-edge=' in svg or 'data-node=' in svg or 'data-family-map=' in svg:
        assert svg in graph_html, 'rendered SVG differs from deterministic generator'
assert all(target(graph_page,url).exists() for tag,key,url in pages[graph_page].links if tag=='a' and key=='href' and url.startswith('/distributions/'))
print(f'PASS: graph: {len(node_ids)} unique nodes, {len(expected)} edges, resolving family links; SVG generated twice byte-identically and matches built HTML.')
coverage=json.loads((ROOT/'site/src/data/binding.json').read_text())
for f in DATA:
    html=(DIST/f"distributions/{f['id']}/index.html").read_text()
    assert ('aria-label="Distribution explorer"' in html)==(f['id'] in coverage),f['id']
if coverage:
    assert (DIST/'binding/univariateML.wasm').exists()
    assert (DIST/'binding/index.mjs').exists()
else:
    assert not (DIST/'binding/univariateML.wasm').exists()
print('PASS: explorer coverage matches binding registry; optional build assets agree.')
family_pages=list((DIST/'distributions').glob('*/index.html'))
assert len(family_pages)==98, len(family_pages)
for f in DATA:
    p=DIST/f"distributions/{f['id']}/index.html"
    if p not in pages: errors.append(f'missing {p}')
    elif pages[p].math==0 or pages[p].display_math==0: errors.append(f'no display MathML: {p}')
for f in DATA:
    p=DIST/f"distributions/{f['id']}/index.html"
    links={url for tag,key,url in pages[p].links if tag=='a' and key=='href'}
    for relation in f['related']:
        expected=f"/distributions/{relation['family']}/"
        if expected not in links or not target(p,expected).exists():
            errors.append(f"missing relation link {f['id']}: {expected}")
print('PASS: every declared and inverted relation and limit link is rendered and resolves.')
for p,d in pages.items():
    for src in d.scripts:
        if re.search(r'mathjax|katex',src,re.I): errors.append(f'math runtime {p}: {src}')
    text=p.read_text()
    if re.search(r'<script\b[^>]*>[^<]*(mathjax|katex)',text,re.I): errors.append(f'inline math runtime {p}')
    if 'katex-error' in text: errors.append(f'unrendered math {p}')
    for a in d.assets:
        if urlsplit(a).scheme or a.startswith('//'): errors.append(f'external runtime {p}: {a}')
    for tag,key,url in d.links:
        if urlsplit(url).scheme or url.startswith('//'): continue
        dest=target(p,url)
        if not dest.exists(): errors.append(f'broken {p.relative_to(DIST)}: {url}')
        fragment=unquote(urlsplit(url).fragment)
        if fragment and dest in pages and fragment not in pages[dest].ids: errors.append(f'broken fragment {p.relative_to(DIST)}: {url}')
for p,d in pages.items():
    if re.search(r'class="[^"]*(?:math-inline|math-display)',p.read_text()) and not d.math:
        errors.append(f'math without MathML {p}')
# Stylesheets must also be self-contained (including @import and font URLs).
for p in DIST.rglob('*.css'):
    if re.search(r'(?:url\(\s*[\'\"]?|@import\s*[\'\"])(?:https?:|//)',p.read_text(),re.I): errors.append(f'external CSS URL {p}')
if errors: raise SystemExit('\n'.join(errors))
print(f'PASS: {len(family_pages)} family pages; MathML on every family; no math runtime or external runtime assets; internal links and fragments resolve.')
scratch=ROOT/'build/scratch/site-snippets';scratch.mkdir(parents=True,exist_ok=True)
# Use a standalone Boost include path; an already installed BH is only a
# workstation fallback and is read as headers, without invoking R.
boost=[]
if os.environ.get('BOOST_INCLUDE_DIR'): boost=[Path(os.environ['BOOST_INCLUDE_DIR'])]
elif not Path('/usr/include/boost/version.hpp').exists():
    candidates=[Path('/usr/local/include'),Path('/usr/local/lib/R/site-library/BH/include'),Path('/usr/lib/R/site-library/BH/include'),*Path.home().glob('R/*/*/BH/include')]
    boost=next(([p] for p in candidates if (p/'boost/version.hpp').exists()),[])
compiler=([shutil.which('ccache')] if shutil.which('ccache') else [])+['g++']
print(f'Environment: Python {sys.version.split()[0]}, SymPy {sympy.__version__}; '+subprocess.check_output(['g++','--version'],text=True).splitlines()[0])
print('Boost headers: '+(', '.join(str(p) for p in boost) or 'compiler default search path'))
# Compile every generated example, beyond the three mandatory pilots.
for f in DATA:
    file=scratch/(f['id']+'.cpp');file.write_text(f['cpp'])
    subprocess.run(compiler+['-std=c++17','-c','-I'+str(ROOT/'univariateML-cpp/include')]+['-I'+str(p) for p in boost]+[str(file),'-o',str(file.with_suffix('.o'))],check=True)
    subprocess.run(['g++',str(file.with_suffix('.o')),'-o',str(file.with_suffix('.exe'))],check=True)
print('PASS: 98 generated C++ snippets compile and link as C++17 (including exp, gamma, gb2).')
# Representative HTML inspection assertions: pilots plus ten contrasting laws.
spots=['exp','gamma','gb2','binom','beta','hyper','sstd','gev','gpd','tnorm','pois','unif','pareto']
for id in spots:
    p=DIST/f'distributions/{id}/index.html';text=p.read_text();d=pages[p]
    assert d.math>0 and 'Definition' in text and 'Usage' in text and 'Implementation' in text,id
    assert all(f'data-language="{lang}"' in text for lang in ['r','cpp','python','js']),id
print('PASS: pilot and ten additional HTML spot checks: '+', '.join(spots))
print('Log-density fallback: '+', '.join(f['id'] for f in DATA if not f['density_latex']))
# Include inline JS (already part of HTML) and the local module dependency closure.
def page_weight(p,d):
    css={target(p,a) for a in d.assets if urlsplit(a).path.endswith('.css')}
    js={target(p,a) for a in d.assets if urlsplit(a).path.endswith('.js')}
    js.update(target(p,url) for tag,key,url in d.links if key in ('component-url','renderer-url'))
    queue=list(js)
    while queue:
        module=queue.pop()
        for match in re.finditer(r"(?:from\s*|import\s*\(\s*|import\s*)[\"']([^\"']+)[\"']",module.read_text()):
            url=match[1]
            if not url.startswith(('.', '/')) or not url.endswith('.js'): continue
            dependency=target(module,url)
            if dependency.exists() and dependency not in js:
                js.add(dependency);queue.append(dependency)
    return {'html':p.stat().st_size,'css':sum(x.stat().st_size for x in css),'js':sum(x.stat().st_size for x in js)+d.inline_js}
weights={str(p.relative_to(DIST)):page_weight(p,d) for p,d in pages.items()}
for id in ['exp','gamma','gb2','binom','hyper','sstd']:
    w=weights[f'distributions/{id}/index.html']
    print(f"WEIGHT {id}: HTML {w['html']} B; CSS {w['css']} B; JS {w['js']} B (inline included in HTML)")
(ROOT/'build/scratch/site-weights.json').write_text(json.dumps(weights,indent=2)+'\n')
print(f'Total dist: {sum(p.stat().st_size for p in DIST.rglob("*") if p.is_file())} B')
