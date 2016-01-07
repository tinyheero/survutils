# survutils

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/survutils)](http://cran.r-project.org/package=survutils)

An R package for Survival Analysis. This package mainly serves as a wrapper around various survival analysis functions (e.g. `coxph`) to provide a more inituitive interface. It has the following features:

* Run univariate or multivariate cox regression (`get_cox_res`)
* Generate C-statistics (`get_c_stat`)

# How to Install

To install this package using devtools:

```{r}
devtools::install_github("tinyheero/survutils")
```

# Overview

To see the full list of exported functions:

```{r}
library("survutils")
ls("package:survutils")
```

A quick overview of some of the key functions:

* `get_cox_summary`: Provides a data.frame summary of a coxph.fit object.

* `get_nrisk_tbl`: Provides a number at risk table as typically seen in publications (e.g. https://mcfromnz.wordpress.com/2011/11/06/kaplan-meier-survival-plot-with-at-risk-table/)

* `get_c_stat`: Calculates C-statistic. This is a wrapper around the [survC1 R package](https://cran.r-project.org/web/packages/survC1/index.html)

* `get_surv_prob`: Calculates the survival probability at specified times from a survival curve.
