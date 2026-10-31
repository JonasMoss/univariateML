import './binding.mjs';
import {cpSync, mkdirSync, rmSync, readdirSync, readFileSync, existsSync, statSync} from 'node:fs';
import path from 'node:path';
import {articleSources} from '../articles.config.mjs';
const root=path.resolve('..');
mkdirSync('public',{recursive:true});
cpSync('../dev/design/logo/logo_F.svg','public/logo.svg');
// Copy only explicitly linked local evidence; no external requests at runtime.
rmSync('public/sources',{recursive:true,force:true});
const dossier='univariateML-cpp/include/cppunivariate/distributions';
const files=readdirSync(path.join(root,dossier),{withFileTypes:true}).filter(x=>x.isDirectory()).map(x=>`${dossier}/${x.name}/README.md`);
// Family pages link their dossier sources (implementation notes, decision history).
for(const file of files) for(const name of ['README.md','mle.hpp','dpqr.hpp']) {
 const rel=path.join(path.dirname(file),name);if(!existsSync(path.join(root,rel))) continue;
 mkdirSync(path.join('public/sources',path.dirname(rel)),{recursive:true});cpSync(path.join(root,rel),path.join('public/sources',rel));
}
files.push(...articleSources.map(a=>a.source).filter(source=>existsSync(path.join(root,source))));
for(const source of files) {
 const text=readFileSync(path.join(root,source),'utf8');
 for(const code of text.matchAll(/`([^`\n]+\.(?:md|tex|hpp|cpp|py|R|json))`/g)) {
  const rel=/^(dev|univariateML-cpp)\//.test(code[1]) ? code[1] : path.join(path.dirname(source),code[1]);
  const file=path.resolve(root,rel);if(rel.startsWith('..') || !existsSync(file) || !statSync(file).isFile()) continue;
  const dest=path.join('public/sources',rel);mkdirSync(path.dirname(dest),{recursive:true});cpSync(file,dest);
 }
 for(const match of text.matchAll(/!?\[[^\]]*\]\(([^\s)]+)(?:\s+[^)]*)?\)/g)) {
  const url=match[1];if(/^(https?:|mailto:|#|\/)/.test(url)) continue;
  const absolute=path.resolve(root,path.dirname(source),url.split('#')[0]);
  const relative=path.relative(root,absolute);
  if(relative.startsWith('..') || !existsSync(absolute) || !statSync(absolute).isFile() || /distributions\/[^/]+\/README.md$/.test(relative)) continue;
  const dest=path.join('public/sources',relative);mkdirSync(path.dirname(dest),{recursive:true});cpSync(absolute,dest);
 }
}
