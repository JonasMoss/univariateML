import { defineCollection } from 'astro:content';
import { glob } from 'astro/loaders';
import {existsSync, readdirSync} from 'node:fs';
// Read the canonical files in place. Generated JSON contains derived presentation data.
const base='../univariateML-cpp/include/cppunivariate/distributions';
const pending=readdirSync(base,{withFileTypes:true}).filter(d=>d.isDirectory() && !existsSync(`${base}/${d.name}/mle.hpp`)).map(d=>d.name);
const pattern=pending.length ? `!(${pending.join('|')})/README.md` : '*/README.md';
const families = defineCollection({ loader: glob({pattern, base, generateId: ({entry}) => entry.split('/')[0]}) });
export const collections = { families };
