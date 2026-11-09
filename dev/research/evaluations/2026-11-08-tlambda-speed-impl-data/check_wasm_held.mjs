import {readFileSync,writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import create from '../../../../build/js/univariateML.mjs';
const row=readFileSync(new URL('./extended-data.txt',import.meta.url),'utf8').trim().split('\n').map(s=>s.split(' ').map(Number)).find(r=>r[0]===-20);
const reference=JSON.parse(readFileSync(new URL('./held-checks.json',import.meta.url)));
const m=await create();
const r=m.fittlambda(new Float64Array(row.slice(2)),{mu:0},{reltol:1e-10,iterlim:2000});
assert.equal(r.error,undefined,r.error);assert.equal(r.status,'Ok');assert.equal(r.estimates.mu,0);
const errors={};
for(const key of ['sigma','lambda']) {
 const expected=Number(key==='sigma'?reference.reference_sigma:reference.reference_shape);
 errors[key]=Math.abs(r.estimates[key]/expected-1);assert.ok(errors[key]<1e-10,`${key}: ${errors[key]}`);
}
const out={estimates:r.estimates,logLik:r.logLik,relative_errors:errors,passed:true};
writeFileSync(new URL('./wasm-held-checks.json',import.meta.url),JSON.stringify(out,null,2)+'\n');
console.log(JSON.stringify(out,null,2));
