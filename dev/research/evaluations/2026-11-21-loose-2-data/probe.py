"""Emit a small standalone observation probe from the checked-in test fixtures."""
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
DISTS = ROOT / "univariateML-cpp/include/cppunivariate/distributions"

for family in ("bhatt", "gb2", "gengamma", "kumar", "sgomp", "sstd", "st"):
    print(f'#include "cppunivariate/distributions/{family}/mle.hpp"')
print(r'''#include "cppunivariate/distributions/nbinom/zinb_mle.hpp"
#include <cstdio>
#include <vector>
void row(const char* id, std::initializer_list<double> values) {
  std::printf("%s", id);
  for(double value : values) std::printf(",%.17g", value);
  std::puts("");
}
int main() {
  const auto span = [](const std::vector<double>& x) {
    return cppunivariate::span{x.data(), x.size()};
  };
''')

def golden(path):
    return json.loads(path.read_text())

def vector(values):
    return "{" + ",".join(map(str, values)) + "}"

b = golden(DISTS / "bhatt/golden.json")
print(f'''const std::vector<double> bx{vector(b["data"])};
const auto b=cppunivariate::bhatt_mle(span(bx));
row("bhatt/golden",{{b.mu,b.sigma,b.a,b.logLik}});
const cppunivariate::bhatt_detail::CertModel model(span(bx),NAN,NAN,NAN,true);
std::array<double,3> p{{}};
p[model.location]=-0.0038000682197418395/1.057897350270679;
p[model.scale]=1/1.057897350270679;
p[model.shape]=1.7336058787117143/1.057897350270679;
const auto root=cppunivariate::bhatt_detail::cert_newton(model,p,1e-10,500);
double mu,sigma,a;
model.coordinates(root.center,mu,sigma,a);
row("bhatt/newton",{{mu,sigma,a}});
''')
source = (DISTS / "bhatt/test.cpp").read_text().split('TEST_CASE("Bhatt near-normal width')[1]
source = source.split('TEST_CASE("Bhatt compensated normal members')[0]
for i, (data, call) in enumerate(re.findall(r"std::vector<double> x\{([^}]+)\};\s*const auto r=(.*?);", source)):
    print(f'''{{ const std::vector<double> x{{{data}}};
    const auto r={call.replace("mltest::as_span(x)", "span(x)")};
    row("bhatt/near/{i}",{{r.mu,r.sigma,r.a}}); }}''')

burr = golden(DISTS / "burr/golden.json")
print(f'''const std::vector<double> gx{vector(burr["data"])};
const auto g=cppunivariate::gb2_mle(span(gx),1e-8,1000,NAN,NAN,1,NAN);
row("gb2/burr",{{g.shape1,g.shape2,g.scale}});
''')
print(r'''const std::vector<double> mixed{.03,.1,.3,.7,1,2,5,20};
for(bool inverse:{false,true}) {
  for(double tol:{1e-7,1e-10}) {
    const auto r=cppunivariate::gb2_mle(span(mixed),tol,tol==1e-7?300:200,
                                       inverse?NAN:2,NAN,inverse?3:NAN,1);
    row(tol==1e-7?(inverse?"gb2/mixed/1":"gb2/mixed/0"):
                  (inverse?"R/gb2/mixed/1":"R/gb2/mixed/0"),
        {inverse?r.shape1:r.shape2,inverse?r.shape2:r.shape3});
  }
}
const std::vector<double> clipped(200,1e-12);
auto cx=clipped;
for(std::size_t i=1;i<cx.size();i+=2)cx[i]=1.-1e-12;
const auto k=cppunivariate::kumar_mle(span(cx),1e-12,100);
row("kumar/clipped",{k.a});
const std::vector<double> skew{-35.321072334324725,11.459074428000962,-.015351125824354262,
    -.2513099360462538,-297.3459307209368,.5881721220894813};
const auto s=cppunivariate::sstd_mle(span(skew),1e-6,200,0,NAN,3,10);
row("sstd/log-scale",{std::log(s.sigma)});
const std::vector<double> tied{-2,2};
const auto t=cppunivariate::sstd_mle(span(tied),1e-6,200,NAN,1,3,1);
row("sstd/ties",{t.stop_detail.candidates.front().coef[0],t.stop_detail.candidates.back().coef[0]});
const std::vector<double> slant{-3,-1,-.2,.1,.8,2,4};
const auto st=cppunivariate::st_mle(span(slant),1e-6,200,0,1,NAN,4);
row("st/slant",{st.alpha});
const std::vector<double> normal{-1.,0.,1.,2.};
row("gengamma/normal",{cppunivariate::gengamma_fit_detail::location_scale(span(normal),0.,1e-10,100).mu});
const std::vector<double> exponential{0,0,0,1,10};
const auto e=cppunivariate::sgomp_mle(span(exponential));
row("sgomp/exponential",{e.b,e.eta});
''')
z = golden(ROOT / "univariateML-cpp/tests/modifiers/fixtures/zinb/golden.json")
print(f'''const std::vector<int> zx{vector(z["data"])};
const auto z=cppunivariate::zinb_fixed_size_mle({{zx.data(),zx.size()}},{z["estimates"]["size"]},
                                              NAN,{z["estimates"]["pi"]});
row("zinb/held-inflation",{{z.fit.prob,static_cast<double>(z.fit.status)}});
}}''')
