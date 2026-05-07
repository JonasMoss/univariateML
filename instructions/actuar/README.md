# actuar
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/actuar)](https://cran.r-project.org/package=actuar) ![downloads](http://cranlogs.r-pkg.org/badges/grand-total/actuar)

**actuar** is a package providing additional actuarial science
functionality to the [R](https://r-project.org) statistical system.
The project was officially launched in 2005 and is under active
development.

## Features

The current feature set of the package can be split into five main
categories: 

1. Additional probability distributions to model insurance loss
   amounts and loss frequency (23 continuous heavy tailed
   distributions, see the list [below](#continuous); the
   Poisson-inverse Gaussian discrete distribution; zero-truncated and
   zero-modified extensions of the standard discrete distributions,
   [below](#discrete); phase-type distributions);
2. Loss distributions modeling (extensive support for grouped data;
   empirical raw and limited moments; minimum distance estimation);
3. Risk and ruin theory (discretization of the claim amount
   distribution and computation of the aggregate claim amount
   distribution; computation of the adjustment coefficient and ruin
   probabilities); 
4. Simulation of discrete mixtures, compound models and compound
   hierarchical models;
5. Credibility theory (Bühlmann, Bühlmann-Straub, hierarchical,
   regression and linear Bayes models).

The package includes extensive documentation in the form of package
*vignettes*. Each vignette focuses on a feature set of the package. To
get the list of available vignettes, enter at the R command prompt:

```R
vignette(package = "actuar")
```

## Installation

You should install the stable version of the package from the 
[Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/package=actuar):
using:

```R
install.packages("actuar")
```

## Citation

To cite package **actuar** in publications see the output of

```R
citation(package = "actuar")
```

## License

**actuar** is free software licensed under the [GNU General Public
License (GPL)](https://www.gnu.org/copyleft/gpl.html), version 2 or later.

## Philosophy

As much as possible, the developers have tried to keep the user
interface of the various functions of the package consistent.
Moreover, the package follows the general R philosophy of working with
model objects. This means that instead of merely returning, say, a
vector of probabilities, many functions will return an object
containing, among other things, the said probabilities. The object can
then be manipulated at one's will using various extraction, summary or
plotting functions.

## <a name="continuous"></a> Additional continuous size distributions

**actuar** provides support functions for all the probability
distributions found in Appendix&nbsp;A of 
[*Loss Models: From Data to Decisions*](https://www.wiley.com/en-us/Loss+Models%3A+From+Data+to+Decisions%2C+5th+Edition-p-9781119523758),
by Klugman, Panjer and Willmot, and not already present in base R,
excluding the log-t, but including the loggamma distribution. The
package also provides support for the Feller--Pareto distribution and related Pareto
distributions with a location parameter, following the classification
of [*Pareto Distributions*](https://www.routledge.com/Pareto-Distributions-2nd-Edition/Arnold/p/book/9781466584846), 
by Arnold. These distributions mostly fall under the umbrella of
extreme value or heavy tailed distributions.

The list of continuous size distributions supported by **actuar** is
as follows, using the nomenclature of *Loss Models* and *Pareto
Distributions*.

###  Feller-Pareto family

- Feller Pareto
- Pareto IV
- Pareto III
- Pareto II
- Pareto
- Single parameter Pareto (Pareto I)
- Transformed beta
- Burr
- Loglogistic
- Paralogistic
- Generalized Pareto
- Inverse Burr
- Inverse Pareto
- Inverse paralogistic

### Transformed gamma family

- Transformed gamma
- Inverse transformed gamma
- Inverse gamma
- Inverse Weibull
- Inverse exponential

###  Other

- Loggamma
- Gumbel
- Inverse Gaussian
- Generalized beta

## <a name="discrete"></a> Additional discrete distributions

**actuar** brings additional support functions for counting
distributions commonly used in loss frequency modeling. A counting
distribution is a discrete distribution defined on the non-negative
integers.

The list of counting distributions supported by **actuar** is as
follows.

### Zero-truncated (a, b, 1) class

- Zero-truncated Poisson
- Zero-truncated negative binomial
- Zero-truncated geometric
- Zero-truncated binomial
- Logarithmic

### Zero-modified (a, b, 1) class

- Zero-modified Poisson
- Zero-modified negative binomial
- Zero-modified geometric
- Zero-modified binomial
- Zero-modified logarithmic

### Other

- Poisson-inverse Gaussian
