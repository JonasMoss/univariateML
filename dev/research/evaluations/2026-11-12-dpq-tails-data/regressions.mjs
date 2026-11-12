import create from '../../../../build/js/univariateML.mjs';
import {readFileSync,writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
const m=await create(),results=[];
const read=name=>JSON.parse(readFileSync(new URL(name,import.meta.url),'utf8'));
for(const r of read('density-references.json')) {
 const actual=m['d'+r.family](new Float64Array(r.data),r.parameters,true,true);
 actual.forEach((v,i)=>{
  const error=Math.abs(v-r.expected[i])/Math.max(1,Math.abs(r.expected[i]));
  results.push({family:r.family,index:r.index,x:r.data[i],actual:v,expected:r.expected[i],error});
  assert.ok(error<=1e-10,JSON.stringify(results.at(-1)));
 });
}
for(const r of read('../2026-11-11-oracle-gaps-data/row-exclusions.json').filter(r=>['beta','betapr','fpareto','gb2'].includes(r.family))) {
 const ref=r.measurements[0];
 const actual=m['p'+r.family](new Float64Array([ref.input]),r.parameters,ref.lower,false)[0];
 const error=Math.abs(actual-ref.expected)/ref.expected;
 results.push({family:r.family,input:ref.input,actual,expected:ref.expected,error});
 assert.ok(error<=1e-10,JSON.stringify(results.at(-1)));
}
writeFileSync(new URL('js-regressions.json',import.meta.url),JSON.stringify(results,null,2)+'\n');
console.log('WASM independent regression checks',results.length,'maximum error',Math.max(...results.map(r=>r.error)));
