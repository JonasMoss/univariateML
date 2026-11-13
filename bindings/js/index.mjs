import {families, fitFamilies, coefficients} from './families.mjs';
export {families, fitFamilies} from './families.mjs';
import {modifierAPI} from './modifiers.mjs';
import {modifiers} from './modifier-metadata.mjs';
import createModule from '../../build/js/univariateML.mjs';

/** Load the private generated binding. Copy boundaries return owned Float64Arrays. */
export default async function create(options = {}) {
  const module = await createModule(options);
  return Object.fromEntries(families.map(id => {
    const modifier=modifierAPI(module,id,modifiers[id].eligibility,modifiers[id].capabilities);
    return [id, {
    d: (x, params, flags = {}) => modifier.evaluate("d",x,params,flags),
    p: (x, params, flags = {}) => modifier.evaluate("p",x,params,flags),
    q: (x, params, flags = {}) => modifier.evaluate("q",x,params,flags),
    r: (n, params, seed, flags = {}) => {
      if (!Number.isSafeInteger(n) || n < 0 || n > 0xffffffff) throw new RangeError('n must be an unsigned 32-bit integer.');
      if (!Number.isInteger(seed) || seed < 0 || seed > 0xffffffff) throw new RangeError('seed must be an unsigned 32-bit integer.');
      if(flags.zero && flags.zero!=="none" || flags.truncate) {
        // The seeded uniform source remains native and deterministic.
        const u=module.runif(n,[0,1],seed);
        return modifier.sampleQuantile(u,params,flags);
      }
      return module[`r${id}`](n, params, seed);
    },
    ...(fitFamilies.includes(id) ? {vcov: (x, fit, type = 'sandwich') => {
      if(fit?.censored || fit?.truncate || fit?.zero && fit.zero!=='none') throw new Error('Covariance for modified or censored fits is not implemented');
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
      if(x && 'lower' in x && x.lower.length===x.upper?.length &&
          Array.from(x.lower).every((v,i)=>v===x.upper[i])) x=x.lower;
      if(control.zero && control.zero!=="none" || control.truncate || x && "lower" in x) {
        const allowed=[...coefficients[id],...(control.zero==='modified'?['p0']:control.zero==='inflated'?['pi']:[])];
        if(Object.keys(held).some(k=>!allowed.includes(k))) throw new Error('Unknown held coefficient for modifier');
        if(Object.values(held).some(v=>v!==null && (typeof v!=='number' || !Number.isFinite(v)))) throw new Error('held coefficients must be finite scalars');
      }
      const result = control.zero && control.zero!=="none" || control.truncate ||
        (x && !ArrayBuffer.isView(x) && !Array.isArray(x) && "lower" in x) ?
        modifier.fit(x,held,control) : module[`fit${id}`](x, held, control);
      if (result.error) {
        const error = new Error(result.error);
        for (const key of ['kind', 'attainable_tol', 'model', 'estimates', 'candidates', 'limit_loglik', 'loglik_lower', 'loglik_upper', 'segment_loglik_upper', 'certified'])
          if (key in result) error[key] = result[key];
        error.warnings = result.warnings || []; throw error;
      }
      return result;
    }} : {}),
  }];}));
}
