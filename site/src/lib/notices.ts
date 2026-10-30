// All notice wording lives here; family facts remain in the dossiers.
export function notices(f: any): string[] {
 const params = new Map<string,string>(f.params.map((p:any)=>[p.name,p.symbol || p.name]));
 const symbol = (name:string) => `$${params.get(name) || name}$`;
 const phrase = (text:string) => {
   // Keep authored TeX intact; replace coefficient names only in plain text.
   const parts = text.replace(/\\mathrm\{([^}]+)\}/g, (match,name)=>params.get(name) || match).trim().replace(/[.;]+$/, '').split(/(\$[^$]*\$|`[^`]*`)/g);
   return parts.map((part,i)=>i%2 ? (part.startsWith('`') && params.has(part.slice(1,-1)) ? symbol(part.slice(1,-1)) : part) : part.replace(/\b[A-Za-z][A-Za-z0-9_]*\b/g, word=>params.has(word) ? symbol(word) : word)).join('');
 };
 const result = (f.limits || []).map((l:any) => {
   const scope = l.fixed?.length ? ` With ${l.fixed.map(symbol).join(', ')} held.` : '';
   const condition=phrase(l.detect || l.when);
   const sentence=condition.startsWith('$') ? condition : condition.charAt(0).toLowerCase()+condition.slice(1);
   if (l.kind==='nonexistence') return `No finite MLE exists when ${sentence}. The fit stops.${scope}`;
   return `The fit stops when ${sentence}.${l.model ? ' It points to [`ml'+l.model+'`](/distributions/'+l.model+'/).' : l.name ? ' Reported boundary: '+l.name+'.' : ''}${scope}`;
 });
 for (const h of f.holds || []) result.push(`With ${h.held.length ? h.held.map(symbol).join(', ')+' held' : 'all coefficients free'}, ${h.status==='not-implemented' ? 'this fit is not implemented' : 'no MLE exists'}. ${phrase(h.reason)}.`);
 for (const p of f.params) for (const d of (Array.isArray(p.fit_domain) ? p.fit_domain : p.fit_domain ? [p.fit_domain] : [])) {
   const r=d.restriction;
   const bounds=[d.lower!==undefined ? `$${p.symbol || p.name} \\ge ${d.lower}$` : '',d.upper!==undefined ? `$${p.symbol || p.name} \\le ${d.upper}$` : ''].filter(Boolean).join(' and ');
   result.push(`Fitting restricts ${bounds}. ${phrase(r.reason)}.${r.exempt_when_held?.length ? ' Exempt when '+r.exempt_when_held.map((set:string[])=>set.map(symbol).join(', ')+' held').join(' or ')+'.' : ''}`);
 }
 return result;
}
