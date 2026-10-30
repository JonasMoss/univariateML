"""Derived presentation data only; canonical dossiers remain the content source."""
from pathlib import Path
import sys, re, json, math, time
import yaml
import sympy as sp
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from quality.density import density_expression
FAMILIES = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
# Match R's YAML 1.1 treatment, except ambiguous single-letter names are quoted
# in the canonical dossiers. JSON cannot represent infinite floating values.
def clean(v):
    if isinstance(v, float) and not math.isfinite(v): return str(v)
    if isinstance(v, dict): return {str(k): clean(x) for k,x in v.items()}
    if isinstance(v, list): return [clean(x) for x in v]
    return v

def snippet(folder, m):
    header = (folder/'mle.hpp').read_text()
    # Public result-returning overloads, excluding detail solvers.
    matches = list(re.finditer(r'inline\s+([\w:]+)\s+((?:\w+_mle|fit_\w+))\s*\(\s*(i?span)\s+\w+\s*[,)]', header))
    adapter = (ROOT/f'bindings/r/distributions/{folder.name}/adapter.cpp').read_text()
    choices = [s for s in matches if s[2] in adapter]
    if not choices: raise ValueError(f'No public fitter for {folder.name}')
    def required_arguments(match):
        declaration=header[match.start():header.index('{',match.start())]
        # The shortest public overload gives a minimal call, leaving optional
        # controls and holds at their actual C++ defaults.
        return declaration.split('=',1)[0].count(',')
    s = min(choices, key=required_arguments)
    start = header.index('(', s.start())+1
    depth=1; end=start
    while depth:
        if header[end]=='(': depth+=1
        if header[end]==')': depth-=1
        end+=1
    args = header[start:end-1].split(',')
    values=['x']
    params={p['name']: (m.get('fit_args',{}).get(p['name']) if m.get('fit_args',{}).get(p['name']) is not None else p['simulate']) for p in m['params']}
    for arg in args[1:]:
        if '=' in arg: break
        name=arg.strip().split()[-1].replace('/*','').replace('*/','')
        if name in ('tol','reltol','abstol','gradtol','epsilon'): value='1e-8'
        elif name in ('maxit','iterlim'): value='1000'
        else:
            key=name.removesuffix('_or_nan').removeprefix('held_')
            if key not in params:
                design=[p['name'] for p in m['params'] if p.get('kind') in ('design','threshold')]
                if len(design)==1: key=design[0]
                else: raise ValueError(f'{folder.name}: required argument {name}')
            value=str(params[key])
            if value=='inf': value='std::numeric_limits<double>::infinity()'
            if value=='-inf': value='-std::numeric_limits<double>::infinity()'
        values.append(value)
    typ='int' if s[3]=='ispan' else 'double'
    sample=json.loads((folder/'golden.json').read_text())['data'][:5]
    observations=', '.join(str(int(v)) if typ=='int' else format(v,'.7g') for v in sample)
    return f'''#include <cppunivariate/special/special.hpp>
#include <cppunivariate/distributions/{folder.name}/mle.hpp>
#include <vector>
#include <limits>

int main() {{
  const std::vector<{typ}> observations{{{observations}}};
  const cppunivariate::{s[3]} x{{observations.data(), observations.size()}};
  const auto fit = cppunivariate::{s[2]}({', '.join(values)});
  // Inspect fit.status before using the estimated coefficients.
  (void)fit;
}}
'''

started=time.monotonic()
out=[]
for folder in sorted(FAMILIES.iterdir()):
    if not (folder/'README.md').exists(): continue
    _, front, body = (folder/'README.md').read_text().split('---',2)
    m=yaml.safe_load(front)
    # Neutral real assumptions avoid treating design-dependent domains as numbers.
    syms,x,expr=density_expression(folder,m,[(-math.inf,math.inf)]*len(m['params']), prefer_spec=False)
    expr=expr.xreplace({s:sp.Symbol(p['name']) for s,p in zip(syms,m['params'])})
    # expand power/log identities only formally: formula is stated on dossier support.
    expanded=sp.expand_mul(expr.replace(sp.loggamma, lambda z:sp.log(sp.gamma(z))))
    powers=[]; remainder=[]
    # On the real log-density domain, each log argument is positive, so
    # exp(c*log(a)) = a**c. Preserve non-log terms together (e.g. normal kernels).
    for term in sp.Add.make_args(expanded):
        logs=[a for a in sp.Mul.make_args(term) if a.func==sp.log]
        if len(logs)==1:
            powers.append(sp.Pow(logs[0].args[0],term/logs[0]))
        else:
            remainder.append(term)
    density=sp.powsimp(sp.Mul(*powers)*sp.exp(sp.Add(*remainder)))
    symbol_names={s:r'\mathrm{'+s.name+'}' for s in expr.free_symbols
                  if len(s.name)>1 and not sp.latex(s).startswith('\\')}
    latex=sp.latex(density,symbol_names=symbol_names)
    readable=not density.has(sp.Piecewise,sp.Sum,sp.Integral) and sp.count_ops(density)<=55 and len(latex)<=700
    m.update(summary=m.get('summary') or re.split(r'\n\s*\n|\n## ',body.strip())[0].replace('\n',' '),
             log_latex=sp.latex(expr,symbol_names=symbol_names), density_latex=latex if readable else None,
             cpp=snippet(folder,m))
    out.append(clean(m))
path=ROOT/'site/src/data/families.json'
path.write_text(json.dumps(out,indent=2,allow_nan=False)+'\n')
print(f'Data generation: {time.monotonic()-started:.2f} s')
print(f'Derived {len(out)} families; log-density fallback: '+', '.join(m['id'] for m in out if not m['density_latex']))
