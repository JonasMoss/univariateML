"""Regression tests for independent reference routes; run with unittest."""
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import mpmath as mp
import yaml
from quality import reference as tool
from quality.reference_integer import integer_profile

class ReferenceTests(unittest.TestCase):
    def fixture(self, xs, held=None):
        tmp=tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root=Path(tmp.name);folder=root/'laplace';folder.mkdir()
        meta={'params':[{'name':'mu','domain':'(-inf, inf)'}, {'name':'sigma','domain':'(0, inf)'}],
              'support':{'lower':'-inf','upper':'inf'},
              'existence':{'uniqueness':'proved'},
              'log_density':'-log(2*sigma)-Abs(x-mu)/sigma'}
        g={'id':'laplace','data':xs,'estimates':{'mu':0.1,'sigma':2.},'logLik':-20.,
           'fit_args':held or {},'seed':123,'sampler_args':{'mu':0,'sigma':1}}
        if held:g['estimates'].update(held)
        (folder/'README.md').write_text('---\n'+yaml.safe_dump(meta)+'---\n')
        (folder/'golden.json').write_text(json.dumps(g))
        return root,g

    def test_median_interval_and_mad(self):
        root,g=self.fixture([0.,1.,3.,10.])
        with patch.object(tool,'DISTS',root):
            r=tool.reference('laplace')
            self.assertEqual(list(map(float,r['median_interval'])),[1.,3.])
            self.assertEqual(float(r['estimates']['mu']),3.)
            self.assertEqual(float(r['estimates']['sigma']),3.)
            tool.write('laplace',r)
        new=json.loads((root/'laplace/golden.json').read_text())
        self.assertEqual({k:v for k,v in g.items() if k not in ('estimates','logLik')},
                         {k:v for k,v in new.items() if k not in ('estimates','logLik','reference')})

    def test_held_location_is_not_moved_to_median(self):
        root,g=self.fixture([0.,1.,3.],{'mu':0.25})
        with patch.object(tool,'DISTS',root):r=tool.reference('laplace')
        self.assertEqual(float(r['estimates']['mu']),.25)
        self.assertAlmostEqual(float(r['estimates']['sigma']),1.25)
        self.assertEqual(r['not_refined']['mu'],'held')

    def test_collapse_never_writes_a_reference(self):
        root,g=self.fixture([2.,2.,2.])
        with patch.object(tool,'DISTS',root),self.assertRaisesRegex(ValueError,'collapse'):
            tool.reference('laplace')

    def test_writer_preserves_frozen_field_bytes(self):
        original = '{"data": [0.0, 1.0000000000000000, 3.0], "seed":17, "estimates":{"mu":0}, "logLik":-2}\n'
        changed = tool.update_golden_text(original, {'estimates':{'mu':1.}, 'logLik':-1., 'reference':{'method':'test'}})
        self.assertIn('"data": [0.0, 1.0000000000000000, 3.0], "seed":17,',changed)
        self.assertEqual(json.loads(changed)['estimates'],{'mu':1.})
        again=tool.update_golden_text(changed,{'reference':{'method':'updated'}})
        self.assertIn('"data": [0.0, 1.0000000000000000, 3.0], "seed":17,',again)
        self.assertEqual(json.loads(again)['reference']['method'],'updated')

    def test_oracle_rescoring_uses_high_precision_density(self):
        root,g=self.fixture([0.,1.,3.])
        with patch.object(tool,'DISTS',root):
            r=tool.reference('laplace')
            checked=tool.rescore_oracle('laplace',r,{'estimates':{'mu':9.,'sigma':4.}})
        self.assertLess(mp.mpf(checked['gap']),0)
        self.assertEqual(checked['precision_digits'],50)

    def test_one_sided_score_with_integer_normalizer(self):
        root,g=self.fixture([2,3])
        (root/'laplace').rename(root/'zipf');folder=root/'zipf'
        meta={'params':[{'name':'N','domain':'[1, inf)','integer':True,'bound':'upper'},
                        {'name':'s','domain':'[0, inf)'}],
              'support':{'lower':1,'upper':'N'},
              'log_density':'-s*log(x)-log(Sum(k**(-s),(k,1,N)))'}
        (folder/'README.md').write_text('---\n'+yaml.safe_dump(meta)+'---\n')
        g['estimates']={'N':3,'s':0.};(folder/'golden.json').write_text(json.dumps(g))
        with patch.object(tool,'DISTS',root):r=tool.reference('zipf')
        self.assertNotIn('error',r)
        self.assertLess(mp.mpf(r['one_sided']['s']['score']),0)

    def test_one_sided_bound_score(self):
        root,g=self.fixture([-1.,-2.])
        folder=root/'laplace';folder.rename(root/'bounded');folder=root/'bounded'
        meta={'params':[{'name':'mu','domain':'[0, inf)'}],
              'support':{'lower':'-inf','upper':'inf'},
              'log_density':'-(x-mu)**2/2'}
        (folder/'README.md').write_text('---\n'+yaml.safe_dump(meta)+'---\n')
        g['estimates']={'mu':0.};(folder/'golden.json').write_text(json.dumps(g))
        with patch.object(tool,'DISTS',root):r=tool.reference('bounded')
        self.assertEqual(float(r['one_sided']['mu']['score']),-3.)
        g['data']=[1.,2.];(folder/'golden.json').write_text(json.dumps(g))
        with patch.object(tool,'DISTS',root):r=tool.reference('bounded')
        self.assertIn('violates',r['error'])

    def test_oracle_disagreement_does_not_change_fixture(self):
        root,g=self.fixture([0.,1.,3.])
        p=root/'laplace/README.md'
        p.write_text(p.read_text().replace('proved','unknown'))
        before=(root/'laplace/golden.json').read_text()
        with patch.object(tool,'DISTS',root),patch.object(tool,'check',return_value={
                'oracle':{'status':'fail','gap':1.,'starts':8}}):
            self.assertEqual(tool.main(['--write','laplace']),1)
        self.assertEqual(before,(root/'laplace/golden.json').read_text())

    def test_integer_enumeration_excludes_infinite_tail(self):
        with mp.workdps(60):
            g={'data':[0,1,1,2,2,2,3,3,4], 'estimates':{'size':5,'prob':.4}}
            t,e=integer_profile('binom',g)
            self.assertGreater(mp.mpf(e['tail_gap']),0)
            self.assertGreater(mp.mpf(e['runner_up_gap']),0)
            self.assertGreaterEqual(int(t['size']),4)
            self.assertLessEqual(int(t['size']),e['range'][1])
            with self.assertRaisesRegex(ValueError,'budget'):
                integer_profile('binom',g,max_cells=1)

    def test_held_integer_zero_digest_survives_reference_write(self):
        root,g=self.fixture([0.,1.,3.],{'mu':0})
        with patch.object(tool,'DISTS',root):
            before=tool.reference('laplace')
            tool.write('laplace',before)
            after=tool.reference('laplace')
        self.assertEqual(before['input_sha256'],after['input_sha256'])
        self.assertEqual(before['estimates'],after['estimates'])

if __name__=='__main__':unittest.main()
