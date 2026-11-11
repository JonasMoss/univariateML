set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
python3 - <<'PY'
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-10-gev-held2-data/combination-study.cpp')
s=p.read_text().replace('#include <chrono>','#include <chrono>\n#include <ctime>')
s=s.replace('for(int hold=0;hold<2;++hold)','for(int hold=0;hold<0;++hold)')
s=s.replace('certified,milliseconds,loglik','certified,milliseconds,loglik,cpu_milliseconds,evaluations,lambda')
s=s.replace('      const auto start=std::chrono::steady_clock::now();\n      const auto r=cppunivariate::frechet_mle','      const auto cpu=std::clock();cppunivariate::gev_held_detail::study_evaluations=0;\n      const auto start=std::chrono::steady_clock::now();\n      const auto r=cppunivariate::frechet_mle')
s=s.replace("<<ms<<','<<r.logLik<<std::endl;\n      record(frechet_sample", "<<ms<<','<<r.logLik<<','<<1000.*(std::clock()-cpu)/CLOCKS_PER_SEC<<','<<cppunivariate::gev_held_detail::study_evaluations<<','<<r.lambda<<std::endl;\n      record(frechet_sample")
p.with_name('combination-frechet-modes.cpp').write_text(s)
PY
for mode in final all-growth no-growth; do
  python3 - "$mode" <<'PY'
import sys,shutil
from pathlib import Path
p=Path('build/scratch/frechet-mode-'+sys.argv[1]);shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
f=p/'cppunivariate/distributions/gev/held_certificate.hpp';s=f.read_text().replace('namespace gev_held_detail {','namespace gev_held_detail {\ninline std::int64_t study_evaluations=0;',1).replace('++out.evaluations;return evaluate(b);','++out.evaluations;++study_evaluations;return evaluate(b);')
if sys.argv[1]=='no-growth':s=s.replace('j=0;!std::isfinite(face_value)','j=0;false&&!std::isfinite(face_value)')
f.write_text(s)
if sys.argv[1]=='all-growth':
 f=p/'cppunivariate/distributions/frechet/certificate.hpp';f.write_text(f.read_text().replace('{-INFINITY,INFINITY},NAN,false);','{-INFINITY,INFINITY},NAN,true);'))
PY
  g++ -std=c++17 -O2 -I"build/scratch/frechet-mode-$mode" -I"$b" "$d/combination-frechet-modes.cpp" -o "build/scratch/frechet-mode-$mode.bin"
done
for repeat in 1 2 3; do
  for mode in final all-growth no-growth; do
    "build/scratch/frechet-mode-$mode.bin" 100 > "$d/combination-frechet-$mode-$repeat.csv"
  done
done
