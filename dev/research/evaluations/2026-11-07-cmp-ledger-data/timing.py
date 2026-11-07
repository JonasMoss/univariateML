from pathlib import Path
import subprocess, base64, zlib, json, shlex
names=['univariateML-cpp/include/cppunivariate/special/cmp.hpp','univariateML-cpp/include/cppunivariate/distributions/cmp/mle.hpp','univariateML-cpp/include/cppunivariate/distributions/cmb/mle.hpp','bindings/common/distributions/cmp/fit.hpp','bindings/common/distributions/cmb/fit.hpp']
files={p:subprocess.check_output(['git','show','d34a44cd1b0d7324e1f486d66e6b3f7e3db9210a:'+p]).decode() for p in names}
payload=base64.b64encode(zlib.compress(json.dumps(files).encode())).decode()
script='''import base64,zlib,json,shutil
from pathlib import Path
for src in ['univariateML-cpp/include','bindings/common']:
 shutil.copytree(src,Path('build/scratch/before')/src,dirs_exist_ok=True)
for name,text in json.loads(zlib.decompress(base64.b64decode('''+repr(payload)+'''))).items():
 (Path('build/scratch/before')/name).write_text(text)
'''
command='python3 -c '+shlex.quote(script)+' && boost=$(Rscript -e \'cat(system.file("include",package="BH"))\') && for phase in before after; do\n if [ "$phase" = before ]; then inc=build/scratch/before/univariateML-cpp/include; bind=build/scratch/before/bindings; else inc=univariateML-cpp/include; bind=bindings; fi\n c++ -O2 -std=c++17 -DPHASE=\\"$phase\\" -I"$boost" -I"$inc" -I"$bind" -Ibindings -IunivariateML-cpp/tests dev/research/evaluations/2026-11-07-cmp-ledger-data/timing.cpp -o build/scratch/timing-paired || exit\n build/scratch/timing-paired || exit\n done'
raise SystemExit(subprocess.call(['/tmp/umvl-cpu/simbox-run','bash','-c',command]))
