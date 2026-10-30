import { existsSync } from 'node:fs';
import path from 'node:path';
import {repoRoot} from './paths';
import {articleSources} from '../../articles.config.mjs';
export const articles = articleSources.filter(a => existsSync(path.join(repoRoot,a.source)));
