"""Retrieve public DOI metadata. No manuscript text is transmitted.

Run from the repository root; requires only Python's standard library.
The compact record retains the fields used in the bibliographic comparison.
"""
import datetime
import json
from pathlib import Path
import urllib.request

DOIS = {
    'chengiles': '10.1111/j.2517-6161.1990.tb01777.x',
    'chengtraylor': '10.1111/j.2517-6161.1995.tb02013.x',
    'chengbook': '10.1093/oso/9780198505044.001.0001',
    'chernoff': '10.1214/aoms/1177728725',
    'drton': '10.1214/07-AOS571',
    'ih': '10.1007/978-1-4899-0027-2',
    'prentice': '10.1093/biomet/61.3.539',
    'rcbr': '10.2307/3318576',
    'selfliang': '10.1080/01621459.1987.10478472',
    'smith': '10.1093/biomet/72.1.67',
    'vdv': '10.1017/CBO9780511802256',
    'watanabe': '10.1017/CBO9780511800474',
    'lecam': '10.1007/978-1-4612-4946-7',
    'brown': '10.1214/lnms/1215466757',
    'bn': '10.1002/9781118857281',
    'cm': '10.1214/009117904000000766',
    'geyer': '10.1214/08-EJS349',
    'shao': '10.1016/S0167-9473(03)00171-3',
    'watkins': '10.1016/S0167-9473(99)00024-9',
    'dcg': '10.1051/ps:1997111',
}

if __name__ == '__main__':
    records = {}
    for key, doi in DOIS.items():
        url = 'https://api.crossref.org/works/' + doi
        try:
            req = urllib.request.Request(url, headers={'User-Agent': 'boundary-literature-research/1.0'})
            with urllib.request.urlopen(req, timeout=30) as response:
                raw = json.load(response)['message']
            fields = ('DOI', 'title', 'author', 'published', 'published-print',
                      'published-online', 'container-title', 'publisher',
                      'volume', 'issue', 'page', 'ISBN', 'type')
            records[key] = {'url': url, 'metadata': {k: raw[k] for k in fields if k in raw}}
        except Exception as exc:
            records[key] = {'url': url, 'error': str(exc)}
        print(key, json.dumps(records[key], ensure_ascii=False), flush=True)
    out = {'retrieved_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
           'authorization': 'TASK-139: verify every bibliography entry; web search is on',
           'transmitted': 'DOI identifiers only', 'records': records}
    Path(__file__).with_name('metadata.json').write_text(json.dumps(out, indent=2, ensure_ascii=False) + '\n')
