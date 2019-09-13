
<!-- README.md is generated from README.Rmd. Please edit that file -->

# igate

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/stefan-stein/igate.svg?branch=master)](https://travis-ci.org/stefan-stein/igate)
[![CRAN
Version](https://www.r-pkg.org/badges/version/igate)](https://CRAN.R-project.org/package=igate)
<!-- badges: end -->

The goal of igate is to provide you with a quick and powerful, yet easy
to understand toolbox that lets you extract relevant process parameters
from manufacturing data, validate these parameters and find their
optimal ranges and automatically create concise reports of the conducted
analysis.

# Methodology

The igate package implements the **i**nitial **G**uided **A**nalytics
for parameter **T**esting and controland **E**xtraction (iGATE)
framework for manufacturing data.

Having identified a manufacturing ‘problem’ to be investigated, a data
set is assembled for a ‘typical’ period of operation, i.e. excluding
known disturbances such as maintenance or equipment failures. This data
set includes the so called *target variable*, a direct indication or
proxy for the problem under consideration and the variale whose
variation we want to explain. It also includes a number of covariate
parameters representing *suspected sources of variation* (SSVs),
i.e. variables that we consider potentially influetial for the value of
the `target`. Parameters with known and explainable relationships with
the target variable should be excluded from the analysis, although this
can be addressed in an iterative manner though subsequent exclusion and
repeating of the process. The iGATE procedure consists of the following
seven steps (detailed explanations follow below):

1.  Select 8 Best of the Best (BOB) and 8 Worst of the Worst (WOW)
    products. The number of observations chosen can be changed using the
    `versus` argument of `igate`/ `categorical.igate`.
2.  Perform the Tukey-Duckworth test for each SSV (see details below).
3.  For each SSV selected by the said test, perform Wilcoxon Rank test.
4.  Extract upper/ lower control limit for kept parameters.
5.  Perform sanity check via regression plot; decide which parameters to
    keep.
6.  Validate choice of parameters and control limits.
7.  Report findings in standardized format.

Steps 1-4 are performed using the `igate` function for continuous target
variables or the `categorical.igate` function for categorical target
variables. Especially for categorical targets with few categories
`robust.categorical.igate` is a robustified version of
`categorical.igate` and should be considered.

When running `igate`/ `categorical.igate` with default settings, any
outliers for the target variable are excluded and the observations
corresponding to the best 8 (B) and worst 8 (W) instances of the target
variable are identified. For each of these 16 observations, each SSV is
inspected in turn. The distribution of the values of the SSV of the 8
BOB and 8 WOW are analyzed by applying the [Tukey-Duckworth
test](https://en.wikipedia.org/wiki/Tukey–Duckworth_test) (see reference
in link for original paper). If the critical value returned by the test
is larger than 6 (this corresponds to a p-value of less than 0.05), the
SSV is retained as being potentially significant. This test was chosen
for its simplicity and ease of interpretation and visualization. SSVs
failing the test are highly unlikely to be influential whilst SSVs
passing the test may be influential. The Wilcoxon-Rank test performed in
step three of iGATE serves as a possibly more widely known alternative,
that might, however, be harder to explain to non-statisticians. The main
function of these steps is to facilitate dimensionality reduction in the
data set to generate a manageable population for expert consideration.

Step 5 is performed by calling `igate.regressions`, resp.
`categorical.freqplot`. These functions produce a regression (for
continuous target) resp. frequency (for categorical targets) plot and
save it to the current working directory. A domain expert familiar with
the manufacturing process should review these plots and decide which
parameters to keep for further analysis based on goodness of fit of the
data to the plot.

For the validation step, the production period from which the validation
data is selected is dependent on the business situation, but should be
from a period of operation consistent with that from which the initial
population was drawn, i.e. similar product types, similar level of
equipment status etc. The validation step then considers all the
retained SSV as a collective in terms of good and bad bands, and
extracts from the validation sample all the records which satisfy the
condition that all retained SSVs lie within these bands. The expectation
is that where all the SSVs lie within the good band, then the target
should also correspond to the best performance, and vice versa where the
retained SSVs all lie in the bad bands we expect to see bad performance.
The application gives feedback on the extent to which this criterion is
satisfied in order to help the user conclude the exploration and make
recommendations for subsequent improvements. Validation is performed via
the `validate` function.

We consider the last step, the reporting of the results in a
standardized manner, an integral part of iGATE that ensures that
knowledge about past analyses is retained within a company. This is
achieved by calling the `report` function.

## Installation

You can install the package directly from CRAN.

``` r
install.packages("igate")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stefan-stein/igate")
```

## Example

This is a basic example which shows you how to conduct an iGATE analysis
for a continuous target variable. We are using the built in `iris` data
set and consider `"Sepal.Length"` as out target.

``` r
library(igate)

set.seed(123)
n <- nrow(iris)*2/3
rows <- sample(1:nrow(iris), n)
df <- iris[rows, ]
results <- igate(df, target = "Sepal.Length", good_end = "high", savePlots = TRUE)
results
```

The significant variables are shown alongside their count summary
statistic from the Tukey-Duckworth Test as well as the p-value from the
Wilcoxon-Rank test. Also, we see the good and bad control bands as well
as several summary statistics to ascertain the randomness in the results
(see documentation of `igate` for details). Remember to use the option
`savePlots = TRUE` if you want to save the boxplot of the target
variable as a png. This png will be needed for producing the final
report of the analysis.

For details on how to conduct the other steps in the iGATE framework,
please refer to the package vignette, by running

``` r
browseVignettes("igate")
```
