// Layout-lab helpers: TeX for domains and supports, grouped ML obstacles.
// Presentation only; every fact comes from families.json (the dossiers).

export function symbols(f: any): Map<string, string> {
  return new Map<string, string>(f.params.map((p: any) => [p.name, p.symbol || p.name]));
}

const INF = /^[+-]?inf(inity)?$/i;

export function exprTex(s: string, sym: Map<string, string>): string {
  s = String(s).trim();
  const m = s.match(/^if\s*\((.+?)\)\s*(.+?)\s+else\s+(.+)$/);
  if (m) return `\\begin{cases} ${exprTex(m[2], sym)} & ${exprTex(m[1], sym)} \\\\ ${exprTex(m[3], sym)} & \\text{otherwise} \\end{cases}`;
  return s
    .replace(/(-?)\binf(inity)?\b/gi, (_x, minus) => `${minus}\\infty`)
    .replace(/\b[A-Za-z_][A-Za-z0-9_]*\b/g, w => sym.get(w) ?? w)
    .replace(/==/g, '=').replace(/>=/g, '\\ge ').replace(/<=/g, '\\le ')
    .replace(/\*/g, ' ');
}

function intervalTex(name: string, lb: string, lo: string, hi: string, rb: string, integer: boolean, sym: Map<string, string>): string {
  const loInf = INF.test(lo), hiInf = INF.test(hi);
  const L = exprTex(lo, sym), H = exprTex(hi, sym);
  if (integer) {
    const n = Number(lo);
    const start = Number.isFinite(n) ? String(lb === '(' ? n + 1 : n) : L;
    const next = Number.isFinite(n) ? String(Number(start) + 1) : `${L}+1`;
    if (hiInf) return `${name} \\in \\{${start}, ${next}, \\dots\\}`;
    return `${name} \\in \\{${start}, \\dots, ${H}\\}`;
  }
  if (loInf && hiInf) return `${name} \\in ℝ`;
  if (hiInf) return `${name} ${lb === '(' ? '>' : '\\ge'} ${L}`;
  if (loInf) return `${name} ${rb === ')' ? '<' : '\\le'} ${H}`;
  if (/cases/.test(L + H)) return `${name} \\in ${lb === '(' ? '(' : '['}${L},\\ ${H}${rb === ')' ? ')' : ']'}`;
  return `${L} ${lb === '(' ? '<' : '\\le'} ${name} ${rb === ')' ? '<' : '\\le'} ${H}`;
}

export function domainTex(p: any, sym: Map<string, string>): string {
  const m = String(p.domain).match(/^([\[(])\s*([^,]+?)\s*,\s*([^\])]+?)\s*([\])])$/);
  const s = sym.get(p.name) || p.name;
  if (!m) return `${s} \\in ${p.domain}`;
  return intervalTex(s, m[1], m[2], m[3], m[4], !!p.integer, sym);
}

export function supportTex(f: any, sym: Map<string, string>): string {
  const s = f.support;
  return intervalTex('x', s.closed[0] ? '[' : '(', String(s.lower), String(s.upper), s.closed[1] ? ']' : ')', s.type === 'integer', sym);
}

// Replace coefficient names in plain text by their TeX symbols (as notices.ts does).
export function phrase(text: string, sym: Map<string, string>): string {
  const symbol = (name: string) => `$${sym.get(name) || name}$`;
  const parts = String(text).replace(/\\mathrm\{([^}]+)\}/g, (match, name) => sym.get(name) || match).trim().replace(/[.;]+$/, '').split(/(\$[^$]*\$|`[^`]*`)/g);
  const out = parts.map((part, i) => i % 2 ? (part.startsWith('`') && sym.has(part.slice(1, -1)) ? symbol(part.slice(1, -1)) : part) : part.replace(/\b[A-Za-z][A-Za-z0-9_]*\b/g, w => sym.has(w) ? symbol(w) : w)).join('');
  return out.charAt(0).toUpperCase() + out.slice(1);
}

export const heldTex = (set: string[], sym: Map<string, string>) => set.map(n => sym.get(n) || n).join(', ');

export interface LimitGroup { model?: string; name?: string; status?: string; path: string; detect?: string; when: string; free: boolean; held: string[][]; nonexistence: boolean }

const NONEXISTENCE = new Set(['Unbounded', 'Degenerate', 'UnboundedLikelihood', 'DegenerateData', 'NoMLE']);

// One row per limit model (or named boundary), not one per held subset.
// path: the mathematical description (`when`); detect: the fitter's criterion.
export function groupLimits(f: any): LimitGroup[] {
  const groups = new Map<string, any>();
  for (const l of f.limits || []) {
    const key = l.model ? `m:${l.model}` : `n:${l.name || l.status}`;
    let g = groups.get(key);
    if (!g) { g = { model: l.model, name: l.name, status: l.status, kind: l.kind, free: false, held: [], freeRec: null, heldRec: null }; groups.set(key, g); }
    if (l.fixed?.length) { g.held.push(l.fixed); g.heldRec ??= l; }
    else { g.free = true; g.freeRec ??= l; g.name = l.name || g.name; g.status = l.status || g.status; g.kind = l.kind || g.kind; }
  }
  return [...groups.values()].map(g => {
    const r = g.freeRec || g.heldRec;
    const nonexistence = !g.model && (g.kind === 'nonexistence' || NONEXISTENCE.has(g.status));
    return { model: g.model, name: g.name, status: g.status, path: r.when, detect: r.detect, when: nonexistence ? (r.detect || r.when) : r.when, free: g.free, held: g.held, nonexistence };
  }).sort((a, b) => Number(b.free) - Number(a.free));
}

export function restrictions(f: any): { param: string; lower?: number; upper?: number; reason: string; exempt: string[][] }[] {
  const out: any[] = [];
  for (const p of f.params) for (const d of (Array.isArray(p.fit_domain) ? p.fit_domain : p.fit_domain ? [p.fit_domain] : []))
    out.push({ param: p.name, lower: d.lower, upper: d.upper, reason: d.restriction.reason, exempt: d.restriction.exempt_when_held || [] });
  return out;
}

export const existenceLabel: Record<string, string> = {
  'always': 'no obstacle declared',
  'degenerate-only': 'fails only for degenerate data',
  'escapes': 'can escape to a limit model',
  'restricted': 'fitted on a restricted domain',
  'needs-held': 'some coefficients must be held',
  'partial': 'some held subsets not implemented',
};

export const methodLabel: Record<string, string> = {
  'closed-form': 'closed form',
  'scalar-solve': 'one-dimensional solve',
  'convex-solve': 'convex solve',
  'profile-search': 'profile search',
  'integer-search': 'integer search',
  'multistart': 'multistart optimisation',
  'delegation': 'delegates to another family',
};

// Dated change notes belong to the dossier source, not the page.
export const dated = (text: string) => /^\s*\d{4}-\d{2}-\d{2}\b/.test(text) || /\(\d{4}-\d{2}-\d{2}\)/.test(text.split('\n')[0]);
