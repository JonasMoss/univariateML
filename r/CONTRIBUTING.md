# Contributing to univariateML development

The goal of this guide is to facilitate contributing to `univariateML` as 
quickly as possible. The guide is divided into two main pieces:

1. Filing a bug report or feature request in an issue.
1. Suggesting a change via a pull request.

Please note that `univariateML` is released with a Contributor Code of Conduct 
(found in `CODE_OF_CONDUCT.md`). By contributing to this project, you agree to 
abide by its terms.

## Issues

When filing an [issue](https://github.com/JonasMoss/univariateML/issues), 
the most important thing is to include a minimal reproducible example so that 
we can quickly verify the problem, and then figure out how to fix it. There 
are three things you need to include to make your example reproducible: 
**required packages**, **data**, and **code**.

1.  **Packages** should be loaded at the top of the script, so it's easy to
    see which ones the example needs.

2.  Make sure any potential  **data** is easily available to us when we verify 
    your issue.
  
3.  Spend a little bit of time ensuring that your **code** is easy for others to
    read:
  
    * make sure you've used spaces and your variable names are concise, but
      informative
  
    * use comments to indicate where your problem lies
  
    * do your best to remove everything that is not related to the problem.  
     The shorter your code is, the easier it is to understand.
  
    * follow the [tidyverse style](https://style.tidyverse.org/).  

You can check you have actually made a reproducible example by starting up a 
fresh R session and pasting your script in.

(Unless you've been specifically asked for it, please don't include the output 
of `sessionInfo()`.)   

If you miss a particular estimator, make an issue explaining why its 
implementation should be prioritized. 

## Pull requests

To contribute a change to `univariateML`, you follow these steps:

1. Create a branch in git and make your changes.
1. Push branch to github and issue pull request (PR).
1. Discuss the pull request.
1. Iterate until either we accept the PR or decide that it's not
   a good fit for `univariateML`.

Each of these steps are described in more detail below. This might feel 
overwhelming the first time you get set up, but it gets easier with practice. 

If you're not familiar with git or github, please start by reading <http://r-pkgs.had.co.nz/git.html>

Pull requests will be evaluated against a seven point checklist:

1.  __Motivation__. Your pull request should clearly and concisely motivate the
    need for change.

1.  __Only related changes__. Before you submit your pull request, please
    check to make sure that you haven't accidentally included any unrelated
    changes. These make it harder to see exactly what's changed, and to
    evaluate any unexpected side effects.

    Each PR corresponds to a git branch, so if you expect to submit
    multiple changes make sure to create multiple branches. If you have
    multiple changes that depend on each other, start with the first one
    and don't submit any others until the first one has been processed.

1.  __Use the tidyverse coding style__. Please follow the
    [tidyverse style](https://style.tidyverse.org/) with camel case allowed for 
    the `ml***` functions. Maintaining a consistent style across the whole code 
    base makes it much easier to jump into the code. If you're modifying 
    existing `univariateML` code that doesn't follow the style guide, a 
    separate pull request to fix the style would be greatly appreciated. 

1.  If you're adding new parameters or a new function, you'll also need
    to document them with [roxygen](https://github.com/klutometis/roxygen).
    Make sure to re-run `devtools::document()` on the code before submitting.

1.  If fixing a bug or adding a new feature to a function,
    please add a [testthat](https://github.com/r-lib/testthat) unit test.
    
1.  If you wish to provide a new maximum likelihood estimator please read
    the *Implementing New Densities* section below. 

1.  Make better algorithms for maximum likelihood estimation if you can
    but please prepared to document the superiority of your new implementation.
    

This seems like a lot of work but don't worry if your pull request isn't 
perfect. It's a learning process and members of the `univariateML` team will be 
on hand to help you out. A pull request ("PR") is a process, and unless you've 
submitted a few in the past it's unlikely that your pull request will be 
accepted as is. All PRs require review and approval from at least one member of the 
`univariateML` development team before merge.

Finally, remember that `univariateML` is an in-development package. 
We honorably welcome pull requests and contributions. 

## Implementing New Densities

0. Make sure there is a CRAN package containing at least the density function. That is, `package::dyourdensity` is possible to import. If this does not exist your pull request won't be accepted.

1. Search the literature and try to find algorithms. Try to program a fast and reliable algorithm. If you fail to find good custom algorithms, use `nlm`, `constrOptim` or something similar. This is the approach taken in `mlcauchy` and `mllogis`. `mlbeta` also uses `nlm` but does so intelligently and is much faster than `mlcauchy` and `mllogit`.

2. Program an `ml***` function following the pattern already established by the other `ml***` functions. In particular, be sure to return the correct kind of object, be sure to test for `na` and numbers out of bounds, and try to make a fast variant of of `logLik` attribute. 

3. Add tests. There are plenty of patterns in the `test_that` directory. Be sure to test different kinds of options if applicable (see e.g. `mlbeta`). You *must* have 100% code coverage; run `covr::report()`. Please make more tests rather than fewer.
 
4. If the distribution has some strange behavior, such as `mllomax` has, add the appropriate section to the distributions vignette.

5. Make a pull request!

## Acknowledgements
This file is a lightly modified version of `CONTRIBUTE.md` of the 
`R` package [`cvcqv`](https://github.com/MaaniBeigy/cvcqv) available [here.](https://github.com/MaaniBeigy/cvcqv/blob/master/CONTRIBUTING.md)
