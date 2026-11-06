import assert from 'node:assert/strict';
import create from '../../../../bindings/js/index.mjs';
const api=await create();
for(const id of ['paralogis','invparalogis']) {
  const x=Math.exp(id==='paralogis'?-.001:.001);
  const fit=api[id].fit(new Float64Array([x]),{scale:1});
  assert.equal(fit.status,'Ok');
  assert.ok(Math.abs(fit.estimates.shape/9262.778134534776-1)<1e-8);
  assert.ok(Math.abs(fit.logLik-(id==='paralogis'?8.126729864909168:8.124729864909168))<1e-8);
  console.log(id,JSON.stringify(fit));
}
