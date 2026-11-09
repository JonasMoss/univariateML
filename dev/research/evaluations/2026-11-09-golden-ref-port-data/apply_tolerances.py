#!/usr/bin/env python3
"""Apply measured per-family golden allowances, leaving other assertions alone.
Run audit.py after captures, then this script. Review the golden-only diff.
"""
import json,re
from pathlib import Path
root=Path(__file__).resolve().parents[4]
rows=json.loads((Path(__file__).parent/'audit.json').read_text())
# Reasons describe the actual default stop, not a mathematical error bound.
from policy import explanation
for r in rows:
 r['reason']=explanation(r['id'],r['tolerance'])
(Path(__file__).parent/'audit.json').write_text(json.dumps(rows,indent=2)+'\n')
p=root/'univariateML-cpp/tests/test_support.hpp';s=p.read_text()
a=s.index('// Relative tolerance against goldens.') if '// Relative tolerance against goldens.' in s else s.index('// Rounding floor for analytic');b=s.index('// Defaults of the R adapters',a)
s=s[:a]+'''// Rounding floor for analytic comparisons outside golden tests.
constexpr double kTol = 1e-10;
// With double-width intermediates the Bhattacharjee simplex stops at a
// slightly different point: relative coefficient changes were <=1.61e-8 on
// the workstation, 2.27e-7 on simbox (GCC 13 and 15) and 4.39e-7 in the macOS
// R golden drift; its independent score norm was 5.73e-6 (gate 1e-3). As for
// the R golden table: at least 4x the largest observed drift. Applies under
// the proxy macro and on platforms whose long double is double (macOS arm64, MSVC).
#if defined(__EMSCRIPTEN__)
constexpr double kBhattGoldenTol = 5e-7;
#elif defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) || LDBL_MANT_DIG <= DBL_MANT_DIG
constexpr double kBhattGoldenTol = 2e-6;
#else
constexpr double kBhattGoldenTol = 1e-12;
#endif

// Finite-difference BFGS with gradtol 1e-8: observed arm64 golden
// coefficient drift <= 2.37e-7; score and likelihood are checked separately.
constexpr double kBurrGoldenTol = 1e-6;

// The shifted Burr double-only baseline differs by <=4.18e-7 relatively;
// independent total log-coordinate score norm is 2.02e-5 (gate 1e-3),
// and its independent likelihood differs from the golden by 1.1e-12.
// TASK-132 report and dev/validation/pareto4-precision.cpp reproduce this.
#if defined(__EMSCRIPTEN__) || defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS) || LDBL_MANT_DIG <= DBL_MANT_DIG
constexpr double kPareto4GoldenTol = 5e-7;
#else
constexpr double kPareto4GoldenTol = 1e-8;
#endif

// Independent golden references: componentwise relative accuracy measured on
// native GCC, double intermediates, R and WASM. See the TASK-149.1 report and
// its retained captures. These empirical allowances are not error bounds.
'''+'\n'.join((f'// {r["id"]}: {r["reason"]}.\n' if r['tolerance']>1e-10 else '')+
 f'constexpr double kGolden_{r["id"]} = {r["tolerance"]:.0e};' for r in rows)+'\nconstexpr double kGolden_bhatt = 2e-6; // Deferred to bhatt-allfree.\n\n'+s[b:]
a=s.index('// Golden-regression rule for finite-difference') if '// Golden-regression rule for finite-difference' in s else s.index('// Legacy allowance');b=s.index('constexpr double kFdTol',a)
s=s[:a]+'// Legacy allowance for non-golden finite-difference comparisons.\n'+s[b:]
p.write_text(s)
for r in rows:
 id=r['id'];p=root/f'univariateML-cpp/include/cppunivariate/distributions/{id}/test.cpp';s=p.read_text()
 chunks=re.split(r'(?=TEST_CASE\()',s)
 for j,c in enumerate(chunks):
  if f'load_distribution_golden("{id}")' not in c:continue
  if id=="cmp" and "preserves all golden hold subsets" in c:continue
  if "compressed profile reject" in c:continue
  if any(name in c.split("\n",1)[0] for name in ['all four free-shape golden subsets', 'fixed-size certificate holds and golden likelihood', 'held-probability size branch certificate']):continue
  variables=re.findall(r'(\w+)\s*=\s*mltest::load_distribution_golden',c)
  lines=c.splitlines(keepends=True)
  for i,line in enumerate(lines):
   if not any(re.search(r'\b'+v+r'\.(?:estimates|logLik)',line) for v in variables):continue
   # Macro expansion only for the reference comparison (not its definition).
   for macro in re.findall(r'#define (\w+)\(x\) doctest::Approx\(x\)\.epsilon',s):
    line=re.sub(r'\b'+macro+r'\((\w+\.(?:estimates\.at\("[^"\n]+"\)|logLik))\)',
      lambda m:'doctest::Approx('+m[1]+').epsilon(mltest::kGolden_'+id+')',line)
   if 'doctest::Approx' in line:
    line=re.sub(r'(doctest::Approx\(\w+\.(?:estimates\.at\("[^"\n]+"\)|logLik)\)\.epsilon\()[^)]+\)',
       r'\g<1>mltest::kGolden_'+id+')',line)
   lines[i]=line
  chunks[j]=''.join(lines)
 s=''.join(chunks)
 s=re.sub(r'(\.epsilon\(mltest::kGolden_'+id+r'\))(?!\.scale)',r'\1.scale(0)',s)
 p.write_text(s)
# R golden regression: explicit per-family table and the actual control values.
p=root/'bindings/r/tests/testthat/test_golden_regression.R';s=p.read_text()
a=s.index('# Iterative fit coefficients') if '# Iterative fit coefficients' in s else s.index('# Measured accuracy against independent references');b=s.index('if (nzchar(DISTRIBUTIONS_DIR)',a)
table=['# Measured accuracy against independent references, not drift from our output.',
 '# See dev/research/evaluations/2026-11-09-golden-ref-port-report.md and its captures.',
 'GOLDEN_TOLERANCE <- c(']
for i,r in enumerate(rows):
 table.append(f'  {r["id"]} = {r["tolerance"]:.0e}'+(',' if i<len(rows)-1 else '')+
   (' # '+r['reason'] if r['tolerance']>1e-10 else ''))
table+=['  , bhatt = 2e-6 # Deferred to bhatt-allfree', ')','', '# Declared default controls; zero means no exposed scalar control, not zero error.', 'GOLDEN_STOP_TOLERANCE <- c(']
controls={}
for r in rows:
 id=r['id'];text=(root/f'bindings/common/distributions/{id}/fit.hpp').read_text()
 sig=re.search(r'inline FitResult fit_'+id+r'\((.*?)\) \{',text,re.S)[1]
 found=re.search(r'double (?:gradtol|reltol|abstol)\s*=\s*([0-9.eE+-]+)',sig)
 controls[id]=float(found[1]) if found else 2e-14 if id=='ht' else 0.
table += [f'  {id} = {v:.8g}'+(',' if i<len(rows)-1 else '') for i,(id,v) in enumerate(controls.items())]
table+=['  , bhatt = 1e-8', ')','stopifnot(identical(names(GOLDEN_TOLERANCE), names(GOLDEN_STOP_TOLERANCE)))','', '']
s=s[:a]+'\n'.join(table)+s[b:]
s=s.replace('else 1e-12','else stop("Missing measured golden tolerance: ", g$id)')
s=s.replace('# This is the canary for the C++ refactor (de-Armadillo, file split,\n# vendoring): a structural change that does not change algorithms must\n# leave these untouched. To intentionally accept a new baseline, re-run\n# scripts/goldens.R and inspect the diff.', '# Samples and fit arguments are frozen. Estimates/logLik are independent\n# high-precision references; refresh with just golden-reference --write.')
p.write_text(s)
