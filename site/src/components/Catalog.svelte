<script>
 export let families=[];
 let query='', support='', measure='', count='', limits=false, holds=false;
 $: visible=families.filter(f=> {
  const lo=Number(f.support.lower), hi=Number(f.support.upper);
  const numericBound=v=>Number.isFinite(Number(v)) || /^[+-]?(?:inf|infinity)$/i.test(String(v));
  const dependent=!numericBound(f.support.lower)||!numericBound(f.support.upper);
  const shape=dependent?'coefficient-dependent':Number.isFinite(lo)&&Number.isFinite(hi)?'bounded':Number.isFinite(lo)||Number.isFinite(hi)?'half-line':'unbounded';
  return (!query || `${f.name} ${f.id} ${(f.aliases||[]).join(' ')}`.toLowerCase().includes(query.toLowerCase())) && (!support||shape===support) && (!measure||f.support.type===measure) && (!count||f.params.length===Number(count)) && (!limits||f.limits?.length>0) && (!holds||f.holds?.length>0);
 });
</script>
<div class="filters"><label>Name <input type="search" bind:value={query} placeholder="Gamma, binom…"/></label><label>Support <select bind:value={support}><option value="">Any</option><option value="bounded">Bounded</option><option value="half-line">Half-line</option><option value="unbounded">Unbounded</option><option value="coefficient-dependent">Coefficient-dependent</option></select></label><label>Measure <select bind:value={measure}><option value="">Any</option><option value="real">Continuous</option><option value="integer">Discrete</option></select></label><label>Coefficients <select bind:value={count}><option value="">Any</option>{#each [...new Set(families.map(f=>f.params.length))].sort() as n}<option value={String(n)}>{n}</option>{/each}</select></label><label><input type="checkbox" bind:checked={limits}/> Has limits</label><label><input type="checkbox" bind:checked={holds}/> Has holds exceptions</label></div>
<p class="count" aria-live="polite">{visible.length} of {families.length} families</p><ul class="cards">{#each visible as f}<li><a href={`/distributions/${f.id}/`}>{f.name}</a><p>{f.summary.replace(/[`$]/g,'')}</p><span class="count">{f.support.type==='integer'?'Discrete':'Continuous'} · {f.params.length} coefficient{f.params.length===1?'':'s'} · {f.id}</span></li>{/each}</ul>
