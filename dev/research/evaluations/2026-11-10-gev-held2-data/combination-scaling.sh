set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
for mode in growth no-profile-growth; do
  python3 - "$mode" <<'PY'
import sys,shutil
from pathlib import Path
p=Path('build/scratch/scaling-'+sys.argv[1]);shutil.copytree('univariateML-cpp/include',p,dirs_exist_ok=True)
f=p/'cppunivariate/distributions/gev/held_certificate.hpp';s=f.read_text().replace('namespace gev_held_detail {','namespace gev_held_detail {\ninline std::int64_t study_evaluations=0;',1).replace('++out.evaluations;return evaluate(b);','++out.evaluations;++study_evaluations;return evaluate(b);')
if sys.argv[1]=='no-profile-growth':s=s.replace('j=0;!std::isfinite(face_value)&&j<maxit','j=0;!profile&&!std::isfinite(face_value)&&j<maxit')
f.write_text(s)
PY
  g++ -std=c++17 -O2 -DUMVL_STUDY_COUNTS -I"build/scratch/scaling-$mode" -I"$b" "$d/combination-large.cpp" -o "build/scratch/scaling-$mode.bin"
  "build/scratch/scaling-$mode.bin" > "$d/combination-scaling-$mode.csv"
done
