// Public WASM regressions for the new core-owned decisions; run after js-build.
import create from '../../../../bindings/js/index.mjs';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
const api=await create();
let checks=0;
const check=(condition,message)=>{assert.ok(condition,message);++checks;};
const stop=(run,kind,verify=()=>true)=>{assert.throws(run,e=>e.kind===kind&&verify(e));++checks;};
for(const truncate of [[NaN,NaN],[1,NaN],[NaN,1]])
  stop(()=>api.norm.fit([1,1,1],{mean:0,sd:.5},{truncate}),'nomle',e=>/window collapses/.test(e.message));
stop(()=>api.norm.fit([1,1],{mean:0,sd:.5},{truncate:[1,1]}),'invalid');
const right=values=>({lower:Float64Array.from(values),upper:Float64Array.from(values,()=>Infinity)});
stop(()=>api.weibull.fit(right([.25,.5]),{scale:1}),'limit',e=>e.model==='unavailable:point-mass'&&e.estimates.point===1&&e.limit_loglik===0&&e.certified);
stop(()=>api.weibull.fit(right([.5,2]),{scale:1}),'nomle');
const quarter=api.weibull.fit(right([.25,2]),{scale:1});
check(Math.abs(quarter.estimates.shape/(1/3)-1)<1e-10,'analytic censored Weibull shape 1/3');
const reference=JSON.parse(readFileSync(new URL('./supplementary-reference.json',import.meta.url),'utf8')).find(r=>r.mode==='double'&&r.family==='thin-weibull');
const thin=api.weibull.fit(right([1.5,2/3]),{scale:1});
check(thin.certified&&Math.abs(thin.estimates.shape/Number(reference.reference_coefficient)-1)<1e-10,'120-digit thin Weibull shape');
for(const family of ['binom','nbinom']) {
  check(api[family].fit([0,0]).unique===false,`${family}: all-zero free parent is unidentified`);
  check(api[family].fit([0,0],{size:2}).unique===true,`${family}: held positive size identifies probability`);
}
check(api.pois.fit([0,0],{},{zero:'inflated'}).unique===false,'all-zero inflated Poisson parent is unidentified');
const held=api.pois.fit([0,0],{lambda:2},{zero:'modified'});
check(held.unique===true&&(held.warnings??[]).length===0,'held hurdle parent removes AllZero warning');
const below={lower:Float64Array.from([.5,2]),upper:Float64Array.from([Infinity,2])};
check(Math.abs(api.pareto.fit(below,{b:1}).estimates.a*Math.log(2)-1)<1e-12,'below-support Pareto censor contributes survival one');
console.log(JSON.stringify({checks,status:'pass',machine:'workstation',backend:'WASM/double'}));
