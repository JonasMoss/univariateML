export interface Tail { lower?: boolean; log?: boolean }
export interface Fit<P> { estimates: P; logLik: number; iterations: number; status: 'Ok'; fixed: (keyof P)[] }
export interface Evaluation<A extends number[] = number[]> {
  d(x: Float64Array, params: A, flags?: Pick<Tail, 'log'>): Float64Array;
  p(x: Float64Array, params: A, flags?: Tail): Float64Array;
  q(x: Float64Array, params: A, flags?: Tail): Float64Array;
  r(n: number, params: A, seed: number): Float64Array;
}
export interface Family<P, A extends number[], C> extends Evaluation<A> {
  fit(x: Float64Array, held?: Partial<{[K in keyof P]: number | null}>, control?: C): Fit<P>;
}
export default function create(options?: Record<string, unknown>): Promise<Record<string, Evaluation> & {
  exp: Family<{rate: number}, [rate: number], Record<string, never>>;
  gamma: Family<{shape: number; scale: number}, [shape: number, scale: number], {abstol?: number; iterlim?: number}>;
  gb2: Family<{shape1: number; shape2: number; shape3: number; scale: number}, [shape1: number, shape2: number, shape3: number, scale: number], {gradtol?: number; iterlim?: number}>;
}>;
/** Generated family IDs with d/p/q/r coverage. */
export const families: readonly string[];

/** Family IDs whose fit validation and diagnostics use the shared layer. */
export const fitFamilies: readonly string[];
