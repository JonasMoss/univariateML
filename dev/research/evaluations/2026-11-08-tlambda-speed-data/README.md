# Reproducing the tlambda speed investigation

Scratch drivers for [the report](../2026-11-08-tlambda-speed-report.md). `proto.hpp`
is a prototype, not production code: it has no certificate, no tie guards
beyond the legacy fitter's, and debug output under `PROTO_DEBUG`. Run from
this directory with one CPU slot per command and nice +10:

```sh
I=../../../../univariateML-cpp/include
for f in profile iters guess dcost compare2 dercheck concav concav2 big; do
  g++ -O2 -std=c++17 -I "$I" "$f.cpp" -o "/tmp/tl-$f"; done
python3 -c "import json;g=json.load(open('$I/cppunivariate/distributions/tlambda/golden.json'));open('golden_data.txt','w').write('\n'.join(repr(v) for v in g['data']))"
g++ -O2 -std=c++17 -I "$I" golden.cpp -o /tmp/tl-golden
R=/tmp/umvl-cpu/run
for l in 0.14 -0.5 0.7; do $R nice -n 10 /tmp/tl-profile 101 $l; done   # profile_n101.log
$R nice -n 10 /tmp/tl-compare2 30 8      # c30.log;  also 101 6 -> c101.log, 1000 2 -> c1000.log
for n in 101 10000 100000; do $R nice -n 10 /tmp/tl-big $n; done        # big.log
$R nice -n 10 /tmp/tl-golden > golden_fit.txt && $R nice -n 10 python3 ref.py
$R nice -n 10 /tmp/tl-concav; $R nice -n 10 /tmp/tl-concav2           # concav.log, concav2.log
$R nice -n 10 python3 scale_concavity.py                               # scale_concavity.log
$R nice -n 10 python3 closed_forms.py                                  # closed_forms.log
```

`series_radius.py` and `logconcave_window.py` are the high-precision side
checks quoted in the report; each runs in under ten minutes.

Data and controls: `profile`, `big` use `qtlambda((i+.5)/n,0,1,lambda)`;
`compare2` draws seeded samples from 11 generators (seed `777+n`); the
golden sample is the frozen n=500 fixture. Legacy fits use tol=1e-7,
iterlim=1000. Agreement means |Δℓ| < 1e-7 with both log likelihoods
evaluated by `dtlambda`. Timings ran on the shared workstation under
load; ratios are more stable than absolute times.
