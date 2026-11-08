#!/usr/bin/env bash
# Caller reserves one CPU slot. Baseline is the frozen base revision 9fee8c34.
set -euo pipefail
boost_path=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests "-I$boost_path" "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"")
python3 - <<'PY'
from pathlib import Path
import re,subprocess
root=Path.cwd();directory=root/'univariateML-cpp/include/cppunivariate/distributions/betapr'
s=subprocess.check_output(['git','show','9fee8c34b838db7b11b7518a071f61495d23f282:univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp'],text=True)
s=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((directory/m[1]).resolve())+'"',s)
(root/'build/scratch/baseline-mle.hpp').write_text(s)
s=(root/'dev/research/evaluations/2026-11-08-betapr-joint-data/timing.cpp').read_text()
s=s.replace('"cppunivariate/distributions/betapr/mle.hpp"','"baseline-mle.hpp"')
(root/'build/scratch/timing-baseline.cpp').write_text(s)
PY
c++ "${flags[@]}" build/scratch/timing-baseline.cpp -o build/scratch/betapr-before
c++ "${flags[@]}" dev/research/evaluations/2026-11-08-betapr-joint-data/timing.cpp -o build/scratch/betapr-after
python3 - <<'PY'
import json,pathlib,statistics,subprocess
records=[]
for iteration in range(7):
    for label in (['before','after'] if iteration%2==0 else ['after','before']):
        for line in subprocess.check_output(['build/scratch/betapr-'+label],text=True).splitlines():
            values=line.split()
            records.append({'iteration':iteration,'label':label,'n':int(values[0]),'ms':float(values[1]),
                            'shape1':float(values[2]),'shape2':float(values[3]),'scale':float(values[4]),
                            'logLik':float(values[5]),'status':int(values[6])})
summary=[]
for n in [8,500]:
    for label in ['before','after']:
        times=[r['ms'] for r in records if r['n']==n and r['label']==label]
        summary.append({'n':n,'label':label,'median_ms':statistics.median(times),'min_ms':min(times),'max_ms':max(times)})
for n in [8,500]:
    reference=next(r for r in records if r['n']==n)
    assert all(all(r[k]==reference[k] for k in ['shape1','shape2','scale','logLik','status']) for r in records if r['n']==n)
output={'calls_per_measurement':10,'rounds':7,'baseline':'9fee8c34b838db7b11b7518a071f61495d23f282','records':records,'summary':summary,'coefficient_and_loglik_drift':0}
pathlib.Path('dev/research/evaluations/2026-11-08-betapr-joint-data/timings.json').write_text(json.dumps(output,indent=2)+'\n')
print(json.dumps(summary,indent=2))
PY
