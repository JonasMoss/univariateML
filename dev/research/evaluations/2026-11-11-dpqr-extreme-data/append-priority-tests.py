"""Append exact-input mpmath witnesses after the final existing C++ case."""
import json
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
rows=json.loads((HERE/'priority-after.json').read_text())
for ident in ('weibull','llogis'):
    path=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/ident/'test.cpp'
    title=ident+': exact-input large-shape displacement'
    assert title not in path.read_text()
    lines=['','// Independent 100-digit mpmath values; priority.py records exact double inputs.', 'TEST_CASE("'+title+'") {']
    for row in rows:
        if row['family']!=ident or row['parameters'][0]!=1e12:continue
        if row['input']==1000:continue
        name=('d' if row['function']=='d' else 'p')+ident
        args=[repr(row['input']), '1e12', '1000.']+(['true'] if row['function']=='d' else ['false','true'])
        lines.append('  CHECK(cppunivariate::distributions::'+name+'('+','.join(args)+') == doctest::Approx('+repr(row['expected'])+').epsilon(1e-10));')
    lines.append('}')
    path.write_text(path.read_text()+'\n'.join(lines)+'\n')
