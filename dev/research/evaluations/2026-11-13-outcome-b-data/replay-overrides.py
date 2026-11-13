#!/usr/bin/env python3
"""Replay the two adapter-rejected Ok results; run under /tmp/umvl-cpu/run."""
import gzip
import importlib.util
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[4]
spec = importlib.util.spec_from_file_location('snapshot', ROOT / 'dev/validation/fit-snapshot.py')
snapshot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(snapshot)
archive = json.loads(gzip.decompress(snapshot.ARCHIVE.read_bytes()))
w = next(c for c in archive if c['family'] == 'weibull' and c['index'] == 8443)
scratch = ROOT / 'build/scratch/outcome-b-overrides'
scratch.mkdir(parents=True, exist_ok=True)
values = ','.join(float(v).hex() for v in w['data'])
shape, scale = (float(w['held'][k]).hex() for k in ('shape', 'scale'))
source = scratch / 'replay.cpp'
source.write_text('''#include "cppunivariate/distributions/gompertz/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include <iostream>
#include <vector>
int main() {
  std::cout << std::hexfloat;
  std::vector<double> g{0x0.0000000000001p-1022,0x0.0000000000002p-1022,0x0.0000000000003p-1022};
  const auto gr=cppunivariate::gompertz_mle(cppunivariate::span{g.data(),g.size()},1.220703e-4,100,NAN,NAN);
  std::cout << "gompertz class subnormal: status=" << static_cast<int>(gr.status)
    << " a=" << gr.a << " b=" << gr.b << " logLik=" << gr.logLik << " iterations=" << gr.iterations << '\\n';
  std::vector<double> w{''' + values + '''};
  const auto wr=cppunivariate::weibull_mle(cppunivariate::span{w.data(),w.size()},1e-10,100,NAN,''' + shape + ',' + scale + ''');
  std::cout << "weibull archive 008443: status=" << static_cast<int>(wr.status)
    << " shape=" << wr.shape << " scale=" << wr.scale << " logLik=" << wr.logLik << " iterations=" << wr.iterations << '\\n';
}
''')
for mode in ('native', 'double'):
    exe = scratch / mode
    flags = ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode == 'double' else []
    subprocess.run([os.environ.get('CXX', 'c++'), '-std=gnu++17', '-O2', '-DNDEBUG', *flags,
                    '-I' + str(ROOT / 'univariateML-cpp/include'), '-I' + snapshot.boost_include(),
                    str(source), '-o', str(exe)], check=True)
    print(mode, flush=True)
    subprocess.run([str(exe)], check=True)
