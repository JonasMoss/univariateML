// Complete active continuous grid, with the consumer's unchanged forward rules.
import {readFileSync, writeFileSync} from 'node:fs';
import create from '../../../../build/js/univariateML.mjs';
const m=await create();
const here=new URL('./',import.meta.url);
const dists=new URL('../../../../univariateML-cpp/include/cppunivariate/distributions/',here);
const families=JSON.parse(readFileSync(new URL('../../../../build/js/generated/families.json',here)));
const decisions=[], counts={}; let total=0, forwardFailures=0;
for(const f of families) {
  if(/type: integer/.test(readFileSync(new URL(`${f}/README.md`,dists),'utf8'))) continue;
  const t=JSON.parse(readFileSync(new URL(`${f}/truth.json`,dists)),(_,v)=>v==='Inf'?Infinity:v==='-Inf'?-Infinity:v);
  counts[f]=0;
  for(const [index,row] of t.quantiles.entries()) {
    total++;
    const [i,lower,log,x,b,lo,hi,lattice,abs]=row;
    const a=m['q'+f](new Float64Array([x]),t.parameters[i],!!lower,!!log)[0];
    const ok=a===b||Number.isFinite(a)&&Number.isFinite(b)&&Math.abs(a-b)<=Math.max(1e-9*Math.abs(b),abs)||(lattice?a===lo||a===hi:a>=lo&&a<=hi);
    if(ok)continue;
    forwardFailures++;
    if(!Number.isFinite(a)||!Number.isFinite(b)||!(log?Number.isFinite(x)&&x<0:x>0&&x<1))continue;
    const at=m['p'+f](new Float64Array([a]),t.parameters[i],!!lower,!!log)[0];
    if(Number.isFinite(at)&&Math.abs(at-x)<=1e-12*(log?Math.max(1,Math.abs(x)):Math.abs(x))) {
      counts[f]++;decisions.push({family:f,index,row,parameters:t.parameters[i],actual:a,cdf:at});
    }
  }
}
writeFileSync(new URL('js-decisions.json',here),JSON.stringify(decisions,null,2)+'\n');
writeFileSync(new URL('js-counts.json',here),JSON.stringify({total_rows:total,families:counts,forward_failures:forwardFailures,backward_accepted:decisions.length},null,2)+'\n');
console.log({total,accepted:decisions.length});
const row=[0,1,1,-7.3890561137087625,-1.9991476033510408592152e-18,-1.999147603351041e-18,-1.9991476033510405e-18,0,0];
const parameters=[1e9,-999999998,1e9];
const actual=m.qfrechet(new Float64Array([row[3]]),parameters,true,true)[0];
const cdf=m.pfrechet(new Float64Array([actual]),parameters,true,true)[0];
writeFileSync(new URL('js-probe-decisions.json',here),JSON.stringify([{family:'frechet',index:'regression probe (not a truth row)',row,parameters,actual,cdf}],null,2)+'\n');
