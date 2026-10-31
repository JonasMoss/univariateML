"""Regression checks against incorrect maps, supports and Jacobians."""
import copy
import unittest
import shutil
import subprocess
import io
import json
import tempfile
from pathlib import Path
from contextlib import redirect_stdout
from unittest.mock import patch
from . import relations
from .relations import ROOT
from .relations import read_models, check_edge, skellam_boundary, declaration_errors
import sympy as sp

class RelationsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls): cls.models=read_models()

    def test_exponential_rate_map(self):
        edge=dict(family='gamma',at=dict(shape=1,scale='1/rate'))
        self.assertIn('verified',check_edge(self.models['exp'],self.models['gamma'],'special_case_of',edge))
        edge['at']['scale']='rate'
        with self.assertRaisesRegex(ValueError,'log-density mismatch'):
            check_edge(self.models['exp'],self.models['gamma'],'special_case_of',edge)

    def test_support_mismatch(self):
        child=copy.deepcopy(self.models['lnorm']);child['support']['lower']=1
        edge=dict(family='norm',at=dict(mean='meanlog',sd='sdlog'),transform='log')
        with self.assertRaisesRegex(ValueError,'support mismatch'):
            check_edge(child,self.models['norm'],'transform_of',edge)

    def test_log_jacobian(self):
        child=self.models['lnorm'];edge=dict(family='norm',at=dict(mean='meanlog',sd='sdlog'),transform='log')
        self.assertIn('verified',check_edge(child,self.models['norm'],'transform_of',edge))
        edge=copy.deepcopy(edge);edge['transform']='inverse'
        with self.assertRaises(ValueError): check_edge(child,self.models['norm'],'transform_of',edge)

    def test_coefficient_map_is_complete(self):
        with self.assertRaisesRegex(ValueError,'every parent coefficient'):
            check_edge(self.models['exp'],self.models['gamma'],'special_case_of',dict(at=dict(shape=1)))

    def test_discrete_closure(self):
        child=copy.deepcopy(self.models['geom']);child['support']['closed'][0]=False
        edge=child['relations']['special_case_of'][0]
        with self.assertRaisesRegex(ValueError,'endpoint inclusion'):
            check_edge(child,self.models['nbinom'],'special_case_of',edge)


@unittest.skipUnless(shutil.which('Rscript'), 'Rscript unavailable: optional loader integration')
class LoaderSchemaTest(unittest.TestCase):
    def test_loader_taxonomy_and_legacy_codegen_inputs(self):
        script = r"""
source('bindings/r/scripts/load_distributions.R')
root <- 'univariateML-cpp/include/cppunivariate/distributions'
stopifnot(length(load_distributions(root))==98)
original <- .umvl_read_front_matter
path <- file.path(root,'exp','README.md'); base <- original(path)
probe <- function(mutator, reject=TRUE) {
  modified <- base; modified$front <- mutator(modified$front)
  assign('.umvl_read_front_matter',function(p) if(identical(p,path)) modified else original(p),envir=.GlobalEnv)
  failed <- inherits(try(.umvl_from_readme(path,'exp'),silent=TRUE),'try-error')
  assign('.umvl_read_front_matter',original,envir=.GlobalEnv)
  stopifnot(identical(failed,reject))
}
probe(function(f) {f$group <- 'imaginary'; f})
probe(function(f) {f$construction <- 'imaginary'; f})
probe(function(f) {f$areas <- 'imaginary'; f})
probe(function(f) {f$fit$certified <- TRUE; f})
probe(function(f) {f$fit$method <- 'imaginary'; f})
probe(function(f) {f$relations$special_case_of[[1]]$family <- 'imaginary'; f})
probe(function(f) {f$relations$special_case_of[[1]]$at$scale <- 'typo'; f})
probe(function(f) {f$relations$special_case_of[[1]]$at$shape <- NULL; f})
probe(function(f) {f$relations$modification_of <- list(list(family='gamma',at=list(shape=1,scale='1/rate'),type='zero-inflated',mass=0.2)); f})
probe(function(f) {f$relations$modification_of <- list(list(family='gamma',at=list(shape=1,scale='1/rate'),type='bogus')); f})
probe(function(f) {f$relations$truncation_of <- list(list(family='pois',at=list(lambda='rate'))); f})
probe(function(f) {f$fit <- NULL; f})
probe(function(f) {f[c('group','construction','relations','fit','areas','classification','classification_reason')] <- NULL; f}, reject=FALSE)
"""
        result=subprocess.run(['Rscript','-e',script],cwd=ROOT,text=True,capture_output=True)
        self.assertEqual(result.returncode,0,result.stdout+result.stderr)


class ConditionalRelationsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls): cls.models=read_models()

    def check(self, id, kind):
        child=self.models[id];edge=child['relations'][kind][0]
        return check_edge(child,self.models[edge['family']],kind,edge)

    def test_truncations_use_parent_mass(self):
        for id in ('tnorm','tpois','tbinom'):
            with self.subTest(id=id): self.assertIn('verified',self.check(id,'truncation_of'))

    def test_zero_branches_and_positive_renormalization(self):
        for id in ('ztpois','ztgeom','ztbinom','ztnbinom','zmpois','zmgeom','zmbinom','zmnbinom','zmlogarithmic','zip','zib','zinb'):
            with self.subTest(id=id): self.assertIn('verified',self.check(id,'modification_of'))

    def test_wrong_zero_mass_and_variant_fail(self):
        child=self.models['zmpois'];edge=copy.deepcopy(child['relations']['modification_of'][0])
        edge['mass']='1-p0'
        with self.assertRaisesRegex(ValueError,'log-density mismatch'):
            check_edge(child,self.models['pois'],'modification_of',edge)
        edge['mass']='p0';edge['type']='zero-inflated'
        with self.assertRaisesRegex(ValueError,'log-density mismatch'):
            check_edge(child,self.models['pois'],'modification_of',edge)

    def test_wrong_truncation_parent_map_fails(self):
        child=self.models['tpois'];edge=copy.deepcopy(child['relations']['truncation_of'][0])
        edge['at']['lambda']='2*lambda'
        with self.assertRaisesRegex(ValueError,'log-density mismatch'):
            check_edge(child,self.models['pois'],'truncation_of',edge)

    def test_conditioning_support_and_closure(self):
        child=copy.deepcopy(self.models['tpois']);edge=child['relations']['truncation_of'][0]
        child['support']['lower']=-1
        with self.assertRaisesRegex(ValueError,'parent subset'):
            check_edge(child,self.models['pois'],'truncation_of',edge)
        child=copy.deepcopy(self.models['tpois']);child['support']['closed'][0]=False
        with self.assertRaisesRegex(ValueError,'endpoints must be included'):
            check_edge(child,self.models['pois'],'truncation_of',edge)

    def test_translated_folding(self):
        for id in ('hnorm','hcauchy','ht'):
            with self.subTest(id=id): self.assertIn('verified',self.check(id,'transform_of'))
        child=self.models['hcauchy'];edge=copy.deepcopy(child['relations']['transform_of'][0]);edge.pop('shift')
        with self.assertRaisesRegex(ValueError,'support mismatch'):
            check_edge(child,self.models['cauchy'],'transform_of',edge)

    def test_symmetric_members(self):
        for id in ('norm','std','ged'):
            for edge in self.models[id]['relations']['special_case_of']:
                self.assertIn('verified',check_edge(self.models[id],self.models[edge['family']],'special_case_of',edge))

    def test_tukey_lambda_attained_members(self):
        for child, at in (
            ('logis', dict(mu='location', sigma='scale', **{'lambda': 0})),
            ('unif', dict(mu='(min+max)/2', sigma='(max-min)/2', **{'lambda': 1})),
            ('unif', dict(mu='(min+max)/2', sigma='max-min', **{'lambda': 2})),
        ):
            with self.subTest(child=child, at=at):
                self.assertIn('verified', check_edge(self.models[child], self.models['tlambda'],
                              'special_case_of', dict(at=at)))
        with self.assertRaisesRegex(ValueError, 'log-density mismatch'):
            check_edge(self.models['logis'], self.models['tlambda'], 'special_case_of',
                       dict(at=dict(mu='location', sigma='2*scale', **{'lambda': 0})))

    def test_shifted_yule_simon(self):
        edge = dict(at=dict(size=1, alpha='rho', beta=1), transform='shift', shift=1)
        self.assertIn('verified', check_edge(self.models['yulesimon'], self.models['bnbinom'],
                                           'transform_of', edge))
        edge['shift'] = 0
        with self.assertRaisesRegex(ValueError, 'support mismatch'):
            check_edge(self.models['yulesimon'], self.models['bnbinom'], 'transform_of', edge)

    def test_bhattacharjee_removable_boundary(self):
        edge = dict(at=dict(mu='mean', sigma='sd', a=0))
        self.assertIn('verified', check_edge(self.models['norm'], self.models['bhatt'],
                                           'special_case_of', edge))
        edge['at']['sigma'] = '2*sd'
        with self.assertRaisesRegex(ValueError, 'log-density mismatch'):
            check_edge(self.models['norm'], self.models['bhatt'], 'special_case_of', edge)

    def test_logarithmic_attained_boundaries(self):
        for child, parent, at in (
            ('lgser', 'ztnbinom', dict(size=0, prob='1-theta')),
            ('zmlogarithmic', 'zmnbinom', dict(size=0, prob='1-prob', p0='p0')),
        ):
            with self.subTest(child=child):
                self.assertIn('verified', check_edge(self.models[child], self.models[parent],
                                                   'special_case_of', dict(at=at)))
                wrong = dict(at)
                wrong['prob'] = 'theta' if child == 'lgser' else 'prob'
                with self.assertRaisesRegex(ValueError, 'log-density mismatch'):
                    check_edge(self.models[child], self.models[parent],
                               'special_case_of', dict(at=wrong))

    def test_negative_hypergeometric_including_zero_stop(self):
        edge = self.models['nhyper']['relations']['transform_of'][0]
        for stop in (0, 3):
            child = copy.deepcopy(self.models['nhyper'])
            next(p for p in child['params'] if p['name'] == 'r')['simulate'] = stop
            with self.subTest(stop=stop):
                self.assertIn('verified', check_edge(child, self.models['bbinom'],
                                                   'transform_of', edge))
        wrong = copy.deepcopy(edge)
        wrong['shift'] = 'r+1'
        with self.assertRaisesRegex(ValueError, 'support mismatch'):
            check_edge(self.models['nhyper'], self.models['bbinom'], 'transform_of', wrong)

    def test_invalid_child_draws_do_not_count_as_trials(self):
        child = copy.deepcopy(self.models['nhyper'])
        next(p for p in child['params'] if p['name'] == 'r')['domain'] = '[100, inf)'
        edge = child['relations']['transform_of'][0]
        with self.assertRaisesRegex(ValueError, 'could not draw 24 valid child'):
            check_edge(child, self.models['bbinom'], 'transform_of', edge)

    def test_signed_poisson_skellam_faces(self):
        for kind in ('special_case_of', 'transform_of'):
            edge = next(e for e in self.models['pois']['relations'][kind] if e['family'] == 'skellam')
            self.assertIn('verified', check_edge(self.models['pois'], self.models['skellam'], kind, edge))
        edge = dict(at=dict(mu1='2*lambda', mu2=0))
        with self.assertRaisesRegex(ValueError, 'log-density mismatch'):
            check_edge(self.models['pois'], self.models['skellam'], 'special_case_of', edge)

    def test_skellam_extension_against_interior_and_zero_corner(self):
        a, b = sp.symbols('a b', nonnegative=True)
        x = sp.Symbol('x', integer=True)
        raw = -a-b+x*sp.log(a/b)/2+sp.log(sp.besseli(sp.Abs(x), 2*sp.sqrt(a*b)))
        original = copy.deepcopy(self.models['skellam'])
        for sign, at in ((1, {a: a, b: 0}), (-1, {a: 0, b: b})):
            meta, face = skellam_boundary(original, [a,b], x, raw, at)
            for k in (0, 1, 5):
                interior = {a: sp.Float('2.3') if sign == 1 else sp.Float('1e-9'),
                            b: sp.Float('1e-9') if sign == 1 else sp.Float('2.3'), x: sign*k}
                boundary = {a: sp.Float('2.3') if sign == 1 else 0,
                            b: 0 if sign == 1 else sp.Float('2.3'), x: sign*k}
                self.assertAlmostEqual(float(raw.evalf(35, subs=interior)),
                                       float(face.subs(boundary).evalf(35)), places=7)
            self.assertEqual(face.subs({a:0,b:0,x:0}), 0)
            self.assertEqual(face.subs({a:0,b:0,x:sign}), -sp.oo)
        self.assertEqual(original, self.models['skellam'])
        meta, face = skellam_boundary(original, [a,b], x, raw, {a:0,b:0})
        self.assertEqual((meta['support']['lower'],meta['support']['upper']), (0,0))
        with self.assertRaisesRegex(ValueError, 'unrecognized Skellam density'):
            skellam_boundary(original, [a,b], x, raw+1, {a:a,b:0})

    def test_cmp_series_members_keep_normalizer(self):
        for child, at in (('pois', {'lambda':'lambda','nu':1}),
                          ('geom', {'lambda':'1-prob','nu':0})):
            self.assertIn('verified', check_edge(self.models[child], self.models['cmp'],
                                               'special_case_of', dict(at=at)))
        with self.assertRaisesRegex(ValueError, 'log-density mismatch'):
            check_edge(self.models['pois'], self.models['cmp'], 'special_case_of',
                       dict(at={'lambda':'2*lambda','nu':1}))

    def test_zero_modified_zero_atom_removed(self):
        for child, parent in (('ztbinom','zmbinom'), ('ztgeom','zmgeom'),
                              ('ztnbinom','zmnbinom'), ('ztpois','zmpois'),
                              ('lgser','zmlogarithmic'), ('lgser','zmnbinom')):
            # lgser -> zmnbinom is deliberately not declared: it follows
            # through ztnbinom or zmlogarithmic. Still test the support fix.
            edge = (dict(at=dict(size=0, prob='1-theta', p0=0))
                    if (child,parent) == ('lgser','zmnbinom') else
                    next(e for e in self.models[child]['relations']['special_case_of']
                         if e['family'] == parent))
            self.assertIn('verified', check_edge(self.models[child], self.models[parent],
                                               'special_case_of', edge))
        wrong = copy.deepcopy(edge)
        wrong['at']['p0'] = .1
        with self.assertRaisesRegex(ValueError, 'support mismatch'):
            check_edge(self.models['lgser'], self.models['zmnbinom'], 'special_case_of', wrong)

    def test_inverse_gev_endpoint_cancels_before_rounding(self):
        # A regression probe for the endpoint evaluator, not a declaration:
        # reciprocal Weibull is represented in invweibull's dossier.
        edge = dict(transform='inverse', at=dict(mu='1/scale',
                    sigma='1/(scale*shape)', xi='1/shape'))
        self.assertIn('verified', check_edge(self.models['weibull'], self.models['gev'],
                                           'transform_of', edge))
        wrong = copy.deepcopy(edge)
        wrong['at']['mu'] = '-scale'
        with self.assertRaisesRegex(ValueError, 'inverse currently requires positive'):
            check_edge(self.models['weibull'], self.models['gev'], 'transform_of', wrong)


class ImmediateDeclarationTest(unittest.TestCase):
    def models(self, **relations_by_id):
        return {name: dict(id=name, params=[dict(name='scale', domain='(0, inf)')],
                           relations=value) for name,value in relations_by_id.items()}

    def special(self, family, scale='scale'):
        return dict(family=family, at=dict(scale=scale))

    def transform(self, family, transform='inverse', scale='scale', **kwargs):
        return dict(family=family, at=dict(scale=scale), transform=transform, **kwargs)

    def test_transitive_special_case_search(self):
        models = self.models(a=dict(special_case_of=[self.special('b'), self.special('d')]),
                             b=dict(special_case_of=[self.special('c')]),
                             c=dict(special_case_of=[self.special('d')]), d={})
        errors = declaration_errors(models)
        self.assertEqual(set(errors), {('a','special_case_of',1)})
        self.assertIn('a -> b -> c -> d', errors[('a','special_case_of',1)])
        models['a']['relations']['special_case_of'].pop()
        self.assertEqual(declaration_errors(models), {})

    def test_duplicates_include_equivalent_spelling_and_alias_maps(self):
        models = self.models(a=dict(special_case_of=[self.special('b'), self.special('b','2*scale')],
                                    transform_of=[self.transform('b'), self.transform('b',scale='(scale)')]), b={})
        errors = declaration_errors(models)
        for kind in ('special_case_of','transform_of'):
            self.assertIn('duplicate', errors[('a',kind,1)])

    def test_identity_transforms_are_algebraic(self):
        for transform, argument, value in (('power','power','scale/scale'),
                                           ('shift','shift','scale-scale')):
            models = self.models(a=dict(transform_of=[self.transform('b',transform,**{argument:value})]),b={})
            self.assertIn('identity transform', declaration_errors(models)[('a','transform_of',0)])
        models = self.models(a=dict(transform_of=[self.transform('b','abs',shift=0)]),b={})
        self.assertEqual(declaration_errors(models), {})

    def test_involutions_have_one_direction(self):
        for transform in ('inverse','negate'):
            models = self.models(a=dict(transform_of=[self.transform('b',transform)]),
                                 b=dict(transform_of=[self.transform('a',transform)]))
            self.assertEqual(len(declaration_errors(models)), 2)
            models['b']['relations'] = {}
            self.assertEqual(declaration_errors(models), {})

    def test_special_then_same_transform_is_family_level(self):
        models = self.models(a=dict(special_case_of=[self.special('b','2*scale')],
                                    transform_of=[self.transform('c','power','4*scale**2',power=2)]),
                             b=dict(transform_of=[self.transform('c','power','scale**2',power=2)]),c={})
        self.assertIn('derivable transform', declaration_errors(models)[('a','transform_of',0)])
        models['a']['relations']['transform_of'][0]['power'] = 3
        self.assertIn('derivable transform', declaration_errors(models)[('a','transform_of',0)])
        models['a']['relations']['transform_of'][0]['transform'] = 'log'
        self.assertEqual(declaration_errors(models), {})

    def test_transform_then_special_is_family_level(self):
        models = self.models(a=dict(transform_of=[self.transform('b',scale='1/scale'),
                                                  self.transform('c',scale='2/scale')]),
                             b=dict(special_case_of=[self.special('c','2*scale')]), c={})
        self.assertIn('derivable transform', declaration_errors(models)[('a','transform_of',1)])
        models['a']['relations']['transform_of'][1]['at']['scale'] = '3/scale'
        self.assertIn('derivable transform', declaration_errors(models)[('a','transform_of',1)])
        models['a']['relations']['transform_of'][1]['transform'] = 'negate'
        self.assertEqual(declaration_errors(models), {})

    def test_special_chains_on_both_sides_of_transform(self):
        models = self.models(a=dict(special_case_of=[self.special('b')],
                                    transform_of=[self.transform('e',scale='1/scale')]),
                             b=dict(special_case_of=[self.special('c')]),
                             c=dict(transform_of=[self.transform('d',scale='1/scale')]),
                             d=dict(special_case_of=[self.special('e')]),e={})
        self.assertIn('a -> b -> c -> d -> e', declaration_errors(models)[('a','transform_of',0)])

    def test_special_cycles_fail_without_recursing_forever(self):
        models = self.models(a=dict(special_case_of=[self.special('b')]),
                             b=dict(special_case_of=[self.special('a')]))
        self.assertTrue(all('cycle' in value for value in declaration_errors(models).values()))

    def test_cli_fails_even_when_density_checks_would_pass(self):
        models = self.models(a=dict(special_case_of=[self.special('b'),self.special('c')]),
                             b=dict(special_case_of=[self.special('c')]),c={})
        with tempfile.TemporaryDirectory(dir=ROOT/'build/tmp') as folder:
            with patch.object(relations,'ROOT',Path(folder)), patch.object(relations,'read_models',return_value=models), \
                    patch.object(relations,'check_edge',return_value='verified symbolically'), redirect_stdout(io.StringIO()):
                self.assertEqual(relations.main(), 1)
            rows = json.loads((Path(folder)/'build/quality/relations.json').read_text())['results']
            self.assertEqual(sum(row['status']=='failed' for row in rows), 1)
            self.assertIn('derivable special case', next(row['detail'] for row in rows if row['status']=='failed'))


class ConstructionRelationsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls): cls.models=read_models()

    def check(self, child, kind, mutation=None):
        edge=copy.deepcopy(self.models[child]['relations'][kind][-1])
        if mutation: mutation(edge)
        return check_edge(self.models[child],self.models[edge['family']],kind,edge)

    def test_each_construction_and_bad_map(self):
        for child,kind in [('emg','convolution_of'),('skellam','convolution_of'),('bbinom','mixture_of'),('slash','mixture_of'),('dnorm','discretisation_of')]:
            with self.subTest(child=child), patch.object(relations,'TRIALS',2):
                self.assertIn('numerically',self.check(child,kind))
                def corrupt(e):
                    if kind=='convolution_of': e['components'][0]['at'][next(iter(e['components'][0]['at']))]=9
                    else: e['at'][next(iter(e['at']))]=9
                with self.assertRaises(ValueError): self.check(child,kind,corrupt)

    def test_conditions_exclude_invalid_seeds_and_empty_region(self):
        child=copy.deepcopy(self.models['geom']);child['params'][0]['simulate']=1
        edge=dict(family='dweibull',at=dict(shape1='1-prob',shape2=1),when='prob < 1')
        with patch.object(relations,'TRIALS',2):
            self.assertIn('verified',check_edge(child,self.models['dweibull'],'special_case_of',edge))
            edge['when']='prob > 2'
            with self.assertRaisesRegex(ValueError,'could not draw'): check_edge(child,self.models['dweibull'],'special_case_of',edge)
        self.assertFalse(relations.condition('prob < 1 && prob > 0',dict(prob=1)))
        self.assertTrue(relations.condition('prob < 1 || prob == 1',dict(prob=1)))

    def test_new_transforms_and_wrong_jacobian(self):
        # Dossier edges point from the constructed family (lnorm, logitnorm hold
        # log/logit edges), so the reverse exp/expit identities are built here.
        built={('norm','exp'):dict(family='lnorm',at=dict(meanlog='mean',sdlog='sd'),transform='exp'),
               ('norm','expit'):dict(family='logitnorm',at=dict(mu='mean',sigma='sd'),transform='expit')}
        for child,tr in [('norm','exp'),('norm','expit'),('nsbeta','affine')]:
            edge=copy.deepcopy(built[(child,tr)]) if (child,tr) in built else next(copy.deepcopy(e) for e in self.models[child]['relations']['transform_of'] if e['transform']==tr)
            with self.subTest(transform=tr), patch.object(relations,'TRIALS',2):
                self.assertIn('verified',check_edge(self.models[child],self.models[edge['family']],'transform_of',edge))
                edge['transform']='negate'
                with self.assertRaises(ValueError):check_edge(self.models[child],self.models[edge['family']],'transform_of',edge)

    def test_conditional_path_is_not_unconditional_redundancy(self):
        models=copy.deepcopy(self.models)
        # The binomial-to-CMB edge cannot establish an unconditional path.
        models['cmb']['relations']={'special_case_of':[dict(family='bbinom',at={})]}
        models['binom']['relations']['special_case_of'].append(dict(family='bbinom',at={}))
        errors=declaration_errors(models)
        ref=('binom','special_case_of',len(models['binom']['relations']['special_case_of'])-1)
        self.assertNotIn(ref,errors)

    def test_yulesimon_mixture_is_true_but_derivable(self):
        edge=dict(family='geom',at=dict(prob='mix'),mixed='prob',mixing=dict(family='beta',at=dict(shape1='rho',shape2=1)),shift=1)
        models=copy.deepcopy(self.models)
        with patch.object(relations,'TRIALS',2):
            self.assertIn('numerically',check_edge(models['yulesimon'],models['geom'],'mixture_of',edge))
        models['yulesimon']['relations']['mixture_of']=[edge]
        self.assertIn('derivable mapped mixture',declaration_errors(models)[('yulesimon','mixture_of',0)])
        models['bnbinom']['relations'].pop('mixture_of')
        self.assertNotIn(('yulesimon','mixture_of',0),declaration_errors(models))

    def test_distinct_constructions_do_not_establish_redundancy(self):
        def model(rel):return dict(params=[],relations=rel)
        first=dict(family='b',mixed='prob',mixing=dict(family='d'))
        second=dict(family='c',mixed='prob',mixing=dict(family='e'))
        models={'a':model(dict(mixture_of=[first,second])),
                'b':model(dict(special_case_of=[dict(family='c')])),
                'c':model({}), 'd':model({}), 'e':model({})}
        self.assertEqual(declaration_errors(models),{})
        # Parallel constructions with different mixing laws are distinct.
        second['family']='b'
        self.assertEqual(declaration_errors(models),{})

    def test_construction_derivation_and_reverse_transform(self):
        def model(rel):return dict(params=[],relations=rel)
        models={'a':model(dict(discretisation_of=[dict(family='b'),dict(family='c')])),
                'b':model(dict(special_case_of=[dict(family='c')])), 'c':model({})}
        self.assertIn(('a','discretisation_of',1),declaration_errors(models))
        models={'a':model(dict(transform_of=[dict(family='b',transform='exp')])),
                'b':model(dict(transform_of=[dict(family='a',transform='log')]))}
        self.assertEqual(len(declaration_errors(models)),2)

@unittest.skipUnless(shutil.which('Rscript'), 'Rscript unavailable')
class ConstructionLoaderTest(unittest.TestCase):
    def test_construction_schema(self):
        script=r'''
source('bindings/r/scripts/load_distributions.R')
root <- 'univariateML-cpp/include/cppunivariate/distributions'
original <- .umvl_read_front_matter
probe <- function(id, mutate, reject=TRUE) {
 path <- file.path(root,id,'README.md'); modified <- original(path)
 modified$front <- mutate(modified$front)
 assign('.umvl_read_front_matter',function(p) if(identical(p,path)) modified else original(p),envir=.GlobalEnv)
 failed <- inherits(try(.umvl_from_readme(path,id),silent=TRUE),'try-error')
 assign('.umvl_read_front_matter',original,envir=.GlobalEnv)
 stopifnot(identical(failed,reject))
}
stopifnot(length(load_distributions(root))==98)
probe('emg',function(f) {f$relations$convolution_of[[1]]$operation <- 'product';f})
probe('emg',function(f) {f$relations$convolution_of[[1]]$components[[2]]$at <- list();f})
probe('emg',function(f) {f$relations$convolution_of[[1]]$components[[2]]$family <- 'absent';f})
probe('slash',function(f) {f$relations$mixture_of[[1]]$mixed <- 'absent';f})
probe('slash',function(f) {f$relations$mixture_of[[1]]$at$mean <- 'mix';f})
probe('slash',function(f) {f$relations$mixture_of[[1]]$mixing$at$max <- 'unknown';f})
probe('dnorm',function(f) {f$relations$discretisation_of[[1]]$family <- 'geom'; f$relations$discretisation_of[[1]]$at <- list(prob=.5);f})
probe('geom',function(f) {f$relations$special_case_of[[2]]$when <- 'unknown < 1';f})
probe('geom',function(f) {f$relations$special_case_of[[2]]$when <- 'prob';f})
probe('geom',function(f) {f$relations$special_case_of[[2]]$when <- 'system("false")';f})
probe('nsbeta',function(f) {f$relations$transform_of[[length(f$relations$transform_of)]]$scale <- NULL;f})
'''
        result=subprocess.run(['Rscript','-e',script],cwd=ROOT,capture_output=True,text=True)
        self.assertEqual(result.returncode,0,result.stdout+result.stderr)

if __name__=='__main__': unittest.main()
