import assert from 'node:assert/strict';
import create from '../../../../bindings/js/index.mjs';

const api = await create();
const cases = [
  ['sn', [-1, 0, 0, 0, 0, 0, 1], {}, ['xi', 'omega', 'alpha']],
  ['snorm', [-2, ...Array(10).fill(-1), ...Array(10).fill(1), 2], {}, ['mean', 'sd', 'xi']],
  ['snorm', [-1, 0, 1], {mean: 0, sd: 151 / 200}, ['mean', 'sd', 'xi']],
];
for (const [name, sample, holds, names] of cases) {
  const family = api[name];
  const fit = family.fit(sample, holds);
  assert.equal(fit.status, 'Ok');
  assert.equal(fit.certified, true);
  assert.equal(fit.unique, false);
  assert.deepEqual(fit.maximizer_set.coordinates, names);
  const [piece] = fit.maximizer_set.pieces;
  assert.equal(piece.kind, 'points');
  assert.equal(piece.points.length, 2);
  assert.equal(fit.maximizers.length, 2);
  const [a, b] = fit.maximizers;
  assert.ok(a[names[0]] === -b[names[0]]);
  assert.equal(a[names[1]], b[names[1]]);
  assert.notEqual(a[names[2]], b[names[2]]);
  if (name === 'sn') assert.equal(a.alpha, -b.alpha);
  else assert.ok(Math.abs(a.xi * b.xi - 1) < 1e-15);
  for (const row of [a, b]) {
    const ll = family.d(sample, names.map(p => row[p]), {log: true}).reduce((sum, v) => sum + v, 0);
    assert.ok(Math.abs(ll - row.loglik_lower) < 1e-12);
    assert.equal(row.loglik_lower, row.loglik_upper);
  }
  console.log(JSON.stringify({name, holds, maximizers: [a, b]}));
}
