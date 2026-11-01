import { unified } from 'unified';
import remarkParse from 'remark-parse';
import remarkMath from 'remark-math';
import remarkRehype from 'remark-rehype';
import rehypeKatex from 'rehype-katex';
import rehypeStringify from 'rehype-stringify';
import { existsSync, statSync } from 'node:fs';
import path from 'node:path';
import { repoRoot } from './paths';
function headingIds() {
 return (tree:any) => {
  const seen=new Map<string,number>();
  function text(n:any):string {return n.value || n.children?.map(text).join('') || '';}
  function visit(n:any) {
   if(n.type==='heading') {const base=text(n).toLowerCase().replace(/[^\p{L}\p{N}_ -]/gu,'').replace(/ /g,'-');const count=seen.get(base)||0;seen.set(base,count+1);n.data={...n.data,hProperties:{id:base+(count ? '-'+count : '')}};}
   n.children?.forEach(visit);
  }
  visit(tree);
 };
}
const root = repoRoot;
// Dossier links to local evidence become locally served source documents, never broken repository URLs.
export function sourceLinks(source: string) {
  return () => (tree: any) => {
    function visit(node: any) {
      if(node.type==='inlineCode' && /\.(md|tex|hpp|cpp|py|R|json)$/.test(node.value)) {
        const relative=node.value.startsWith('dev/') || node.value.startsWith('univariateML-cpp/') ? node.value : path.join(path.dirname(source),node.value);
        if(!relative.startsWith('..') && existsSync(path.join(root,relative))) {node.type='link';node.url='/sources/'+relative;node.children=[{type:'inlineCode',value:node.value}];delete node.value;return;}
      }
      if (node.type === 'link' || node.type === 'image') {
        const url = node.url;
        if (url && !/^(https?:|mailto:|#|\/)/.test(url)) {
          const [file, fragment] = url.split('#');
          const absolute = path.resolve(path.dirname(path.join(root, source)), file);
          const relative = path.relative(root, absolute);
          if (/^univariateML-cpp\/notes\/(existence|limits|holds|restrictions)\.md$/.test(relative)) {
            node.url = '/articles/' + path.basename(relative, '.md') + '/' + (fragment ? '#'+fragment : '');
          } else if (relative.startsWith('univariateML-cpp/include/cppunivariate/distributions/') && /\/README.md$/.test(relative)) {
            node.url = '/distributions/' + relative.split('/').at(-2) + '/' + (fragment ? '#'+fragment : '');
          } else if (!relative.startsWith('..') && existsSync(absolute) && statSync(absolute).isFile()) {
            node.url = '/sources/' + relative; // Copy performed by prepare.mjs.
          } else {
            // Directories and unresolved pointers stay text: only files are served.
            // Preserve an unresolved authored pointer as text; do not invent a target.
            node.type='text'; node.value=(node.children || []).map((x:any)=>x.value || '').join('')+' ('+url+')'; delete node.children;
          }
        }
      }
      node.children?.forEach(visit);
    }
    visit(tree);
  };
}
function displayFences(text:string) {
 // Some dossiers put $$ beside a multiline equation. CommonMark requires the
 // closing display fence on its own line. Keep fenced and inline code untouched.
 return text.split(/(```[\s\S]*?```|~~~[\s\S]*?~~~|`[^`\n]*`)/g).map((part,i)=>i%2 ? part : part.replace(/(?<!\\)\$\$([\s\S]*?)(?<!\\)\$\$/g, (_all,math)=>'\n$$\n'+math.trim()+'\n$$\n')).join('');
}
export async function markdown(text: string, source: string) {
  return String(await unified().use(remarkParse).use(remarkMath).use(headingIds).use(sourceLinks(source)).use(remarkRehype)
    .use(rehypeKatex, {output:'mathml', strict:false, throwOnError:true}).use(rehypeStringify).process(displayFences(text)));
}
