// All notice wording lives here; no family-specific facts or exceptions.
export function notices(f: any): string[] {
 const result = (f.limits || []).map((l:any) => {
   const scope = l.fixed?.length ? ` With ${l.fixed.map((x:string)=>'`'+x+'`').join(', ')} held.` : '';
   return `The MLE may not exist: when ${l.detect || l.when}, the fit stops${l.model ? ' and points to [`ml'+l.model+'`](/distributions/'+l.model+'/)' : ' at '+(l.name || 'the declared boundary')}.${scope}`;
 });
 for (const h of f.holds || []) result.push(`With ${h.held.length ? h.held.map((x:string)=>'`'+x+'`').join(', ')+' held' : 'all coefficients free'}, ${h.status==='not-implemented' ? 'this fit is not implemented' : 'no MLE exists'}. ${h.reason}`);
 for (const p of f.params) for (const d of (Array.isArray(p.fit_domain) ? p.fit_domain : p.fit_domain ? [p.fit_domain] : [])) {
   const r=d.restriction;
   const bounds=[d.lower!==undefined ? '`'+p.name+' >= '+d.lower+'`' : '',d.upper!==undefined ? '`'+p.name+' <= '+d.upper+'`' : ''].filter(Boolean).join(' and ');
   result.push(`Fitting restricts ${bounds}. ${r.reason}${r.exempt_when_held?.length ? ' Exempt when '+r.exempt_when_held.map((set:string[])=>set.map(x=>'`'+x+'`').join(', ')+' held').join(' or ')+'.' : ''}`);
 }
 return result;
}
