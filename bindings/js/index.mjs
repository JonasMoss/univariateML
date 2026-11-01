import {families, fitFamilies, coefficients} from './families.mjs';
export {families, fitFamilies} from './families.mjs';
import createModule from '../../build/js/univariateML.mjs';

/** Load the private generated binding. Copy boundaries return owned Float64Arrays. */
export default async function create(options = {}) {
  const module = await createModule(options);
  return Object.fromEntries(families.map(id => [id, {
    d: (x, params, {log = false} = {}) => module[`d${id}`](x, params, true, log),
    p: (x, params, {lower = true, log = false} = {}) => module[`p${id}`](x, params, lower, log),
    q: (x, params, {lower = true, log = false} = {}) => module[`q${id}`](x, params, lower, log),
    r: (n, params, seed) => {
      if (!Number.isSafeInteger(n) || n < 0 || n > 0xffffffff) throw new RangeError('n must be an unsigned 32-bit integer.');
      if (!Number.isInteger(seed) || seed < 0 || seed > 0xffffffff) throw new RangeError('seed must be an unsigned 32-bit integer.');
      return module[`r${id}`](n, params, seed);
    },
    ...(fitFamilies.includes(id) ? {vcov: (x, fit, type = 'sandwich') => {
      if (!['sandwich', 'observed'].includes(type)) throw new Error('type must be "sandwich" or "observed"');
      if (x == null) throw new Error('Covariance requires the observations from a fitted model');
      const names = coefficients[id];
      if (!fit || fit.status !== 'Ok' || !fit.estimates || !fit.restricted || !Array.isArray(fit.fixed) ||
          new Set(fit.fixed).size !== fit.fixed.length ||
          Object.keys(fit.estimates).join() !== names.join() ||
          fit.fixed.some(name => !names.includes(name)))
        throw new Error('Covariance requires the original coefficient order and free mask');
      if (Object.keys(fit.restricted).length) throw new Error('Covariance unavailable at a fitting restriction');
      const free = names.filter(name => !fit.fixed.includes(name));
      const mask = names.reduce((mask, name, i) => mask | (free.includes(name) ? 1 << i : 0), 0);
      const result = module.vcov(x, id, names.map(name => fit.estimates[name]), mask, type === 'sandwich');
      const reasons = ['ok', 'invalid parameters', 'invalid observation', 'empty sample',
        'unavailable coefficient subset', 'nonfinite or numerical failure', 'singular information',
        'ill-conditioned information', 'too many coefficients', 'indefinite information',
        'family unavailable', 'regime unavailable', 'boundary or limit fit'];
      if (result.status) throw new Error(`Covariance unavailable: ${reasons[result.status]}${result.validity ? '; ' + result.validity : ''}`);
      return {matrix: result.matrix, names: free, held: [...fit.fixed], type,
        condition: result.condition, validity: result.validity,
        warnings: result.condition > 1e6 ? ['Observed information is poorly conditioned in natural coordinates; covariance may be numerically sensitive'] : []};
    }, fit: (x, held = {}, control = {}) => {
      const result = module[`fit${id}`](x, held, control);
      if (result.error) {
        const error = new Error(result.error); error.warnings = result.warnings || []; throw error;
      }
      return result;
    }} : {}),
  }]));
}
