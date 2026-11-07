import createApi from '../../../../bindings/js/index.mjs';
import assert from 'node:assert/strict';
const api=await createApi();
const x=new Float64Array([0,1,2,3]);
assert.throws(()=>api.frechet.fit(x,{lambda:1/3}),e=>{
  assert.equal(e.kind,'nomle');assert.equal(e.loglik_upper,Infinity);return true;
});
try {
  const r=api.frechet.fit(x,{lambda:1/3+Number.EPSILON/4});
  assert.equal(r.status,'Ok');
} catch(e) {assert.equal(e.kind,'numerical');}
assert.throws(()=>api.frechet.fit(new Float64Array([-1e308,1e308]),{lambda:1}),e=>{
  assert.equal(e.kind,'nomle');assert.ok(Number.isFinite(e.loglik_upper));
  assert.ok(Math.abs(e.loglik_upper-(-2-2*Math.log(1e308)))<1e-9);return true;
});
for(const [data,lambda] of [[[-1e308,1e308],2],[[0,.2,.5,1,3,20],1e100]])
  assert.throws(()=>api.frechet.fit(new Float64Array(data),{lambda}),e=>{assert.equal(e.kind,'numerical');return true;});
console.log('PASS: WASM collapse signs, finite overflowing-range supremum and numerical precision stops.');
