import create, {Covariance} from '../index';
async function check() {
  const api = await create();
  const data = new Float64Array([1, 2, 3]);
  const fit = api.gamma.fit(data, {shape: 2});
  const covariance: Covariance<{shape: number; scale: number}> = api.gamma.vcov(data, fit, 'observed');
  const name: 'shape' | 'scale' = covariance.names[0];
  const row: Float64Array = covariance.matrix[0];
  // @ts-expect-error unsupported covariance kind
  api.gamma.vcov(data, fit, 'fisher');
  // @ts-expect-error incompatible fit coefficients
  api.norm.vcov(data, fit);
  return {name, row};
}
void check;
