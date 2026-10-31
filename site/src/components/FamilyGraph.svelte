<script>
 import {onMount} from 'svelte';
 import {kinds,edgeDash} from '../lib/graph';
 import Neighbourhood from './Neighbourhood.svelte';
 let {data}=$props();
 let group=$state(''), enabled=$state(Object.keys(kinds).filter(k=>k!=='limit')), selected=$state('norm'), query=$state(''), active=$state(''), root=$state(), selectionOpen=$state(false);
 const matches=$derived(query.trim()?data.nodes.filter(f=>(f.name+' '+f.id).toLowerCase().includes(query.toLowerCase())).slice(0,10):[]);
 function locate(id){selected=id;group='';query='';requestAnimationFrame(()=>{const el=root.querySelector(`[data-map-node="${id}"]`);el?.scrollIntoView({block:'center',behavior:'smooth'});el?.focus();});}
 onMount(()=>{
  const over=e=>{const a=e.target.closest('[data-family]');if(a){active=a.dataset.family;if(e.type==='focusin')selected=a.dataset.family;}};
  const out=()=>active='';
  const click=e=>{const a=e.target.closest('[data-map-node]');if(a&&!e.ctrlKey&&!e.metaKey&&!e.shiftKey&&!e.altKey){e.preventDefault();selected=a.dataset.family;selectionOpen=true;}};
  root.addEventListener('pointerover',over);root.addEventListener('focusin',over);root.addEventListener('pointerleave',out);root.addEventListener('focusout',out);root.addEventListener('click',click);
  return ()=>{root.removeEventListener('pointerover',over);root.removeEventListener('focusin',over);root.removeEventListener('pointerleave',out);root.removeEventListener('focusout',out);root.removeEventListener('click',click);};
 });
 $effect(()=>{if(!root)return;
  for(const e of root.querySelectorAll('[data-edge],[data-map-edge]')){e.style.display=enabled.includes(e.dataset.kind)?'':'none';e.style.opacity=!active||e.dataset.source===active||e.dataset.target===active?'1':'.08';}
  for(const n of root.querySelectorAll('[data-family]')){const id=n.dataset.family,near=!active||id===active||data.edges.some(e=>enabled.includes(e.kind)&&((e.source===active&&e.target===id)||(e.target===active&&e.source===id)));const inGroup=!group||data.nodes.find(f=>f.id===id).group===group;n.style.opacity=near&&inGroup?'1':'.18';}
  for(const s of root.querySelectorAll('.atlas>section'))s.hidden=!!group&&s.dataset.group!==group;
  for(const r of root.querySelectorAll('[data-region],[data-region-label]'))r.style.opacity=!group||(r.dataset.region||r.dataset.regionLabel)===group?'1':'.2';
 });
</script>
<div class="graph-controls">
 <label>Find a family <input type="search" bind:value={query} placeholder="Name or id, e.g. gamma" /></label>
 <label>Group <select aria-label="Group" bind:value={group}><option value="">All 13 groups</option>{#each data.groups as g}<option>{g}</option>{/each}</select></label>
 <fieldset><legend>Edge kinds / line styles</legend>{#each Object.entries(kinds) as [k,label]}<label><input type="checkbox" bind:group={enabled} value={k}/><svg class="key" viewBox="0 0 24 8" aria-hidden="true"><path d="M0 4 H24" stroke="currentColor" stroke-width="1.5" stroke-dasharray={edgeDash(k)}/></svg>{label}</label>{/each}</fieldset>
 {#if matches.length}<ul class="results">{#each matches as f}<li><button onclick={()=>locate(f.id)}>{f.name} · {f.id}</button></li>{/each}</ul>{:else if query}<p role="status">No matching family.</p>{/if}
</div>
<p class="hint">Arrows point to parents. Hover or focus a node to trace its neighbours; click or press Enter to select it below. Modified click opens its dossier. Bold labels mark common reference families. Limits are off initially. On mobile, use the atlas below for full-size labels.</p>
<div bind:this={root}>
 <slot name="map" />
 <details class="selection" bind:open={selectionOpen}><summary>Selected family: {data.nodes.find(f=>f.id===selected)?.name} · view neighbourhood</summary><Neighbourhood {data} {selected}/></details>
 <slot name="atlas" />
</div>
<style>
 .graph-controls{font:.8rem system-ui;display:flex;gap:1rem;flex-wrap:wrap;padding:.7rem;background:#ecece2;border:1px solid #d1d1c3}.graph-controls>label{flex-direction:column;align-items:start}fieldset{width:100%;border:0;padding:0;display:flex;gap:1rem;flex-wrap:wrap}legend{margin-bottom:.3rem}fieldset label{gap:.3rem}.key{width:24px;height:8px}.results{width:100%;margin:0;padding-left:1rem}.results li{display:inline-block;margin:.2rem}.hint{font-size:.85rem;color:#58635c}
</style>
