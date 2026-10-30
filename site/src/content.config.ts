import { defineCollection } from 'astro:content';
import { glob } from 'astro/loaders';
// Read the canonical files in place. Generated JSON contains derived presentation data.
const families = defineCollection({ loader: glob({pattern:'*/README.md', base:'../univariateML-cpp/include/cppunivariate/distributions', generateId: ({entry}) => entry.split('/')[0]}) });
export const collections = { families };
