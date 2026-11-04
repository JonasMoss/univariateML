import type {GeneratedAPI} from './family-types';
export interface Tail { lower?: boolean; log?: boolean }
export interface Fit<P> { estimates: P; logLik: number; iterations: number; status: 'Ok'; fixed: (keyof P)[]; warnings: string[]; restricted: Record<string, string>; [field: string]: unknown }
/** Selected stopped fits; catch as Error and inspect kind before narrowing. */
export interface FitStop extends Error {
  kind: 'tie' | 'limit' | 'budget' | 'nomle';
  model: string;
  estimates: Record<string, number>;
  candidates: Array<{model: string; loglik_lower: number; loglik_upper: number;
    [coefficient: string]: string | number}>;
  limit_loglik: number; loglik_lower: number; loglik_upper: number;
  segment_loglik_upper: number;
  warnings: string[];
}
export interface Covariance<P> {
  matrix: Float64Array[]; names: (keyof P)[]; held: (keyof P)[];
  type: 'sandwich' | 'observed'; condition: number; validity: string; warnings: string[];
}
export interface Evaluation<A extends number[] = number[]> {
  d(x: Float64Array, params: A, flags?: Pick<Tail, 'log'>): Float64Array;
  p(x: Float64Array, params: A, flags?: Tail): Float64Array;
  q(x: Float64Array, params: A, flags?: Tail): Float64Array;
  r(n: number, params: A, seed: number): Float64Array;
}
export interface Family<P, A extends number[], C> extends Evaluation<A> {
  vcov(x: Float64Array, fit: Fit<P>, type?: 'sandwich' | 'observed'): Covariance<P>;
  fit(x: Float64Array, held?: Partial<{[K in keyof P]: number | null}>, control?: C): Fit<P>;
}
export default function create(options?: Record<string, unknown>): Promise<Record<string, Evaluation> & GeneratedAPI>;
/** Generated family IDs with d/p/q/r coverage. */
export const families: readonly string[];

/** Family IDs whose fit validation and diagnostics use the shared layer. */
export const fitFamilies: readonly string[];
