"""Validate retained research evidence and completed dev-tier logs."""
from pathlib import Path
import hashlib
import json
import platform
import re
import subprocess
from decimal import Decimal

import mpmath as mp
import scipy

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
mp.mp.dps = 120
a = json.loads((HERE/'checks.json').read_text())
b = json.loads((HERE/'checks-110.json').read_text())
assert a['grid_cells'] == b['grid_cells'] == 1012
assert len(a['rows']) == len(b['rows']) == 1012
assert a['rational_majorant_checks'] == b['rational_majorant_checks'] == 14094
max_discrepancy = mp.mpf(0)
for x,y in zip(a['rows'],b['rows']):
    assert mp.mpf(x['m']) == mp.mpf(y['m'])
    assert abs(mp.mpf(x['r'])-mp.mpf(y['r'])) < mp.mpf('1e-65')*(1+mp.mpf(y['r']))
    assert x['j'] == y['j']
    u,v = mp.mpf(x['scaled_margin']),mp.mpf(y['scaled_margin'])
    assert min(u,v) > mp.mpf('1e-45')
    discrepancy = abs(u-v)/(1+abs(v))
    max_discrepancy = max(max_discrepancy,discrepancy)
    assert discrepancy < mp.mpf('1e-35')
literature = json.loads((HERE/'literature.json').read_text())
assert len(literature['queries']) == 20
cpp = (ROOT/'build/scratch/ztnb-cpp-dev.txt').read_text()
r = (ROOT/'build/scratch/ztnb-r-dev.txt').read_text()
cmatch = re.search(r'100% tests passed, 0 tests failed out of (\d+)', cpp)
assert cmatch
summaries = re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',r)
assert summaries, 'No completed R testthat summary'
assert summaries[-1][0:2] == ('0','0'), summaries[-1]
for name, data in [('cpp-dev.txt',cpp),('r-dev.txt',r)]:
    (HERE/name).write_text(data)
sources = [ROOT/'CLAUDE.md',ROOT/'univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md',
           ROOT/'dev/roadmap/2026-10-29-parameter-rules.md']
for family in ['nbinom','ztnbinom','zinb']:
    p=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/family
    sources += [q for q in p.iterdir() if q.is_file()]
source_hashes = {str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(sources)}
(HERE/'source-sha256.json').write_text(json.dumps(source_hashes,indent=2)+'\n')
acquired=[]
for p in (ROOT/'build/scratch/ztnb-sources').glob('*.pdf'):
    url = ('https://www.casact.org/sites/default/files/database/proceed_proceed61_61045.pdf' if p.stem == 'simon'
           else f'https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/{p.stem}/download')
    acquired.append(dict(file=p.name,url=url,bytes=p.stat().st_size,sha256=hashlib.sha256(p.read_bytes()).hexdigest()))
record = dict(revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
              machine='workstation; no simbox runs',python=platform.python_version(),
              scipy=scipy.__version__,mpmath=mp.__version__,research_cells=1012,
              distinct_parameter_cells=len({(Decimal(z['m']),Decimal(z['r'])) for z in b['rows']}),
              cross_precision_scaled_discrepancy=str(max_discrepancy),
              min_scaled_margin=b['minimum_margin'],
              support_range=[min(z['j'] for z in b['rows']),max(z['j'] for z in b['rows'])],
              cpp_dev_shards=int(cmatch.group(1)),
              r_dev=dict(zip(['fail','warn','skip','pass'],map(int,summaries[-1]))),
              acquired_primary_pdfs=acquired,
              commands=['/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-ztnb-conjecture-data/check.py',
                        '/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-ztnb-conjecture-data/check.py --dps 110 --output dev/research/evaluations/2026-11-07-ztnb-conjecture-data/checks-110.json',
                        '/tmp/umvl-cpu/run4 bash -c \'just cpp-test && just test\'',
                        '/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-ztnb-conjecture-data/validate.py'],
              numerical_qualification='85/110 digit point calculations, signed margin threshold 1e-45, relative cross-precision tolerance 1e-35; no interval enclosure or universal inequality proof',
              compiler_warnings='Retained in cpp-dev.txt; zero failed CTest shards is distinct from warning-free compilation.',
              full_tier='Not required: research-only files; no fitting, d/p/q/r or test-infrastructure changes.')
(HERE/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2))
