<script>
 import {kinds} from '../lib/graph';
 let {data, selected='norm'}=$props();
 const family=$derived(data.nodes.find(f=>f.id===selected));
 const edges=$derived(data.edges.filter(e=>e.source===selected||e.target===selected));
</script>
<section class="neighbourhood" aria-label="Selected family neighbourhood">
 <h2>{family?.name}: one step away</h2>
 <p><a href={`/distributions/${selected}/`}>Read the {selected} dossier →</a></p>
 {#if edges.length}<ul>{#each edges as e}<li><a href={`/distributions/${e.source}/`}>{data.nodes.find(f=>f.id===e.source)?.name}</a> → <a href={`/distributions/${e.target}/`}>{data.nodes.find(f=>f.id===e.target)?.name}</a> <small>({kinds[e.kind]})</small><p>{e.note}</p></li>{/each}</ul>{:else}<p>No declared relation or named limit connects this family.</p>{/if}
</section>
<style>.neighbourhood li{margin-bottom:.8rem}.neighbourhood li p{font: .75rem system-ui;overflow-wrap:anywhere;margin:.2rem 0;color:#626761}</style>
