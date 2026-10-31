<script>
 export let families=[];
 let query='', selected={};
 const dimensions=[['group','Group'],['support_shape','Support'],['support_dependence','Support bounds'],['coefficient_count','Coefficient count'],['coefficient_kinds','Coefficient kinds'],['existence_tags','Existence'],['construction','Construction'],['method','Fit method'],['certified','Fit certification'],['areas','Application areas'],['classification','Classification'],['relation_tags','Declared relations']];
 const values=(f,key)=>['method','certified'].includes(key)?[f.fit[key]]:Array.isArray(f[key])?f[key]:[f[key]];
 const choices=key=>[...new Set(families.flatMap(f=>values(f,key)))].sort();
 const matches=(f,except='')=>(!query || `${f.name} ${f.id} ${(f.aliases||[]).join(' ')}`.toLowerCase().includes(query.toLowerCase())) && dimensions.every(([key])=>key===except || !selected[key]?.length || selected[key].some(v=>values(f,key).includes(v)));
 function toggle(key,value){const current=selected[key]||[];selected={...selected,[key]:current.includes(value)?current.filter(v=>v!==value):[...current,value]};}
 $: visible=(query, selected, families.filter(f=>matches(f)));
 $: counts=(query, selected, Object.fromEntries(dimensions.map(([key])=>[key,Object.fromEntries(choices(key).map(value=>[value,families.filter(f=>matches(f,key)&&values(f,key).includes(value)).length]))])));
 $: groups=[...new Set(visible.map(f=>f.group))].sort();
</script>
<label>Find a family <input type="search" bind:value={query} placeholder="Gamma, binom…"/></label>
<p>Select several values within a dimension to match any; selections across dimensions must all match.</p>
<div class="taxonomy-filters">{#each dimensions as [key,label]}<details><summary>{label}{selected[key]?.length ? ` (${selected[key].length} selected)` : ''}</summary><div class="choices">{#each choices(key) as value}<label><input type="checkbox" checked={(selected[key]||[]).includes(value)} on:change={()=>toggle(key,value)}/>{value} <span>({counts[key][value]})</span></label>{/each}</div></details>{/each}</div>
<button on:click={()=>{selected={};query='';}}>Clear filters</button>
<p class="count" aria-live="polite">{visible.length} of {families.length} families</p>
{#each groups as group}<section><h2>{group} <span class="count">({visible.filter(f=>f.group===group).length})</span></h2><ul class="cards">{#each visible.filter(f=>f.group===group) as f}<li><a href={`/distributions/${f.id}/`}>{f.name}</a><p>{f.summary.replace(/[`$]/g,'')}</p><span class="count">{f.support_tags.join(' · ')} · {f.params?.length || f.coefficient_tags[0]} coefficients · {f.id}</span></li>{/each}</ul></section>{/each}
<style>
.taxonomy-filters{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));align-items:start;gap:.6rem;margin:1rem 0}.taxonomy-filters details{margin:0;border:1px solid #aaa;border-radius:4px;padding:.5rem}.choices{display:flex;flex-direction:column;gap:.35rem;margin-top:.6rem;max-height:16rem;overflow:auto}.choices label{display:flex;gap:.4rem;align-items:baseline}.choices span{margin-left:auto;font-variant-numeric:tabular-nums}summary{cursor:pointer}button{margin:.5rem 0}
</style>
