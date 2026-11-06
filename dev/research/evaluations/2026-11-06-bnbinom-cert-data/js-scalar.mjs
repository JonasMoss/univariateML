import assert from 'node:assert/strict';
import create from '../../../../bindings/js/index.mjs';
const api=await create();
for(const free of ['size','alpha','beta']) {
  const held={size:1,alpha:1,beta:1}; delete held[free];
  const x=new Float64Array(Array(12).fill(1));
  const fit=api.bnbinom.fit(x,held,{});
  assert.ok(Math.abs(fit.estimates[free]/Math.sqrt(2)-1)<1e-8);
  assert.throws(()=>api.bnbinom.fit(x,held,{iterlim:1}),error=>{
    assert.equal(error.kind,'budget');assert.equal(error.candidates.length,1);
    assert.equal(error.loglik_upper,Infinity);return true;
  });
}
assert.throws(()=>api.bnbinom.fit(new Float64Array([1,1]),{size:1,beta:1},{gradtol:1e-20}),error=>{
  assert.equal(error.kind,'numerical');assert.equal(error.candidates.length,1);return true;
});
console.log('Three scalar exact roots, three budget stops and one numerical stop passed in JS.');
