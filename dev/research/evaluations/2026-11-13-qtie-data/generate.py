"""Generate the reproducible truth-point round-trip probe and appended tests.
Baseline: fbc13bf8; obtain old headers with git archive, never store a source archive.
"""
import json, pathlib
root=pathlib.Path('univariateML-cpp/include/cppunivariate/distributions')
out=pathlib.Path(__file__).parent
retired=set('tbinom tnorm tpois zipf zib zinb zip zmbinom zmgeom zmlogarithmic zmnbinom zmpois ztbinom ztgeom ztnbinom ztpois'.split())
ids=sorted(p.parent.name for p in root.glob('*/truth.json') if p.parent.name not in retired and 'type: integer' in (p.parent/'README.md').read_text())
(out/'families.json').write_text(json.dumps(ids,indent=2)+'\n')
s=['#include <cstdio>','#include <cmath>','#include <vector>','#include <string>','#include "test_support.hpp"']
for id in ids: s.append(f'#include "cppunivariate/distributions/{id}/dpqr.hpp"')
s.append('int main() { std::setvbuf(stdout,nullptr,_IOLBF,0);')
for id in ids:
 d=json.loads((root/id/'truth.json').read_text()); args=', '.join(f't[{i}]' for i in range(len(d['parameter_names'])))
 s.append(f'''mltest::discrete_roundtrips("{id}",
 [](double x,const std::vector<double>& t,bool l,bool g) {{return cppunivariate::distributions::p{id}(x,{args},l,g);}},
 [](double x,const std::vector<double>& t,bool l,bool g) {{return cppunivariate::distributions::q{id}(x,{args},l,g);}},true);''')
s.append('}')
(out/'roundtrip.cpp').write_text('\n'.join(s)+'\n')
# Stored adapters are mechanically generated, parameters remain read from truth.json.
for id in ids:
 p=root/id/'test.cpp'; txt=p.read_text(); marker=f'TEST_CASE("{id}: slow reference discrete quantile round trips on every truth point")'
 if marker in txt: continue
 d=json.loads((root/id/'truth.json').read_text()); args=', '.join(f't[{i}]' for i in range(len(d['parameter_names'])))
 p.write_text(txt+f'''\n{marker} {{
  mltest::discrete_roundtrips("{id}",
    [](double x,const std::vector<double>& t,bool l,bool g) {{return cppunivariate::distributions::p{id}(x,{args},l,g);}},
    [](double x,const std::vector<double>& t,bool l,bool g) {{return cppunivariate::distributions::q{id}(x,{args},l,g);}});
}}
''')
