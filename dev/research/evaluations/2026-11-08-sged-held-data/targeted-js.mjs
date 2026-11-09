import assert from 'node:assert/strict';
import {writeFileSync} from 'node:fs';
import create from '../../../../bindings/js/index.mjs';
const api=await create(),cases=[],errors=[];
const x=new Float64Array([-3,-1,-.2,0,.3,.4,2]);
function stopped(data,held,kind,controls={gradtol:1e-10}) {
  let error;
  try {api.sged.fit(new Float64Array(data),held,controls);} catch(e) {error=e;}
  assert.ok(error);assert.equal(error.kind,kind);return error;
}
for(const [nu,reference] of [[1,.9031327675352691400],[1.7,.9107183730121939529]]) {
  const f=api.sged.fit(x,{mean:.1,sd:1.4,nu},{gradtol:1e-10});
  assert.equal(f.status,'Ok');assert.equal(f.estimates.mean,.1);assert.equal(f.estimates.sd,1.4);assert.equal(f.estimates.nu,nu);
  const error=Math.abs(f.estimates.xi/reference-1);assert.ok(error<2e-7);errors.push({nu,relative_xi_error:error});
  const params=['mean','sd','nu','xi'].map(k=>f.estimates[k]);
  const ll=api.sged.d(x,params,{log:true}).reduce((a,b)=>a+b,0);
  assert.ok(Math.abs(ll-f.logLik)<1e-8);cases.push(`finite-${nu}`);
}
const tie=stopped([-1.1,1.1],{mean:0,sd:1,nu:1},'tie',{gradtol:1e-9});
assert.equal(tie.candidates.length,2);assert.ok(Number.isFinite(tie.loglik_upper));
assert.ok(Math.abs(tie.candidates[0].xi*tie.candidates[1].xi-1)<2e-8);cases.push('finite-tie');
for(const sign of [-1,1]) {
  const e=stopped([sign*.2,sign*.2],{mean:0,sd:1,nu:20},'limit');
  assert.equal(e.model,sign>0?'unavailable:reflected-half-ged':'unavailable:half-ged');
  assert.ok(Math.abs(e.estimates.mu-sign*1.71974012888042667)<1e-10);
  assert.ok(Math.abs(e.estimates.scale/3.51957323030042365-1)<1e-10);
  assert.ok(Math.abs(e.limit_loglik+2.462973438266148297)<1e-10);
  assert.ok(Number.isFinite(e.loglik_upper));cases.push(`half-ged-${sign}`);
}
const faces=stopped([0,0],{mean:0,sd:1,nu:20},'tie');
assert.deepEqual(new Set(faces.candidates.map(c=>c.model)),new Set(['unavailable:half-ged','unavailable:reflected-half-ged']));
assert.ok(Number.isFinite(faces.loglik_upper));cases.push('face-tie');
const budget=stopped(x,{mean:.1,sd:1.4,nu:1.7},'budget',{iterlim:1});
assert.ok(budget.candidates.length>0);assert.equal(budget.loglik_upper,Infinity);cases.push('budget');
const contact=stopped([-1,.2],{mean:0,sd:1,nu:1},'numerical');
assert.equal(contact.loglik_upper,Infinity);cases.push('endpoint-contact');
writeFileSync(new URL('./targeted-js.json',import.meta.url),JSON.stringify({passed:cases.length,cases,errors},null,2)+'\n');
console.log(`TARGETED_JS_PASS: ${cases.length} SGED held-moment public API cases`);
