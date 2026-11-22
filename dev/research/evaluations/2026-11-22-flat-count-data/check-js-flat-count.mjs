import assert from 'node:assert/strict';
import createApi from '../../../../bindings/js/index.mjs';

const api = await createApi();
const fit = api.cmb.fit(new Float64Array([1, 1]), {prob: .5, nu: 1});
assert.equal(fit.status, 'Ok');
assert.equal(fit.certified, true);
assert.equal(fit.unique, false);
assert.deepEqual([fit.estimates.size, fit.estimates.prob, fit.estimates.nu], [1, .5, 1]);
const rows = fit.maximizers ?? fit.candidates;
assert.deepEqual(rows.map(row => row.size), [1, 2]);
assert.ok(rows.every(row => row.prob === .5 && row.nu === 1));
assert.equal(fit.logLik, -2 * Math.log(2));
assert.equal(fit.loglik_upper, fit.logLik);
assert.match(fit.outcome_message, /adjacent sizes/);
console.log('CMB exact tie confirmed through JavaScript.');

const zero = api.bbinom.fit(new Float64Array([0, 0]), {});
assert.equal(zero.status, 'Ok');
assert.equal(zero.certified, true);
assert.equal(zero.unique, false);
assert.equal(zero.estimates.size, 0);
assert.equal(zero.logLik, 0);
assert.equal(zero.maximizers.length, 2);
assert.ok(zero.maximizers.every(row => row.size === 0 && row.alpha > 0 && row.beta > 0));
assert.throws(() => api.bbinom.fit(new Float64Array([0, 1, 2]), {}), error =>
  error.kind === 'unsupported' && /Estimating size together with both shapes is not implemented/.test(error.message));
console.log('Beta-binomial zero estimate and nonzero unsupported outcome confirmed through JavaScript.');
