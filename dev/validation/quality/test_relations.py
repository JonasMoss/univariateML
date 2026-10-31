"""Regression checks against incorrect maps, supports and Jacobians."""
import copy
import unittest
import shutil
import subprocess
from .relations import ROOT
from .relations import read_models, check_edge

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
        edge=child['relations']['transform_of'][0]
        with self.assertRaisesRegex(ValueError,'support mismatch'):
            check_edge(child,self.models['norm'],'transform_of',edge)

    def test_log_jacobian(self):
        child=self.models['lnorm'];edge=child['relations']['transform_of'][0]
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

if __name__=='__main__': unittest.main()
