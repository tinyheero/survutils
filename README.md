# survutils

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/survutils)](http://cran.r-project.org/package=survutils)

An R package for Survival Analysis. This package mainly serves as a wrapper around various survival analysis functions (e.g. `coxph`) to provide a more inituitive interface. It has the following features:

* `get_cox_res`: Run univariate or multivariate cox regression. 
* `get_c_stat`: Calculate C-statistics. 
* `get_surv_prob`: Calculates the survival probability at specified times from a survival curve. 
* `get_nrisk_tbl`: Provides a number at risk table as [typically seen in publications](https://mcfromnz.wordpress.com/2011/11/06/kaplan-meier-survival-plot-with-at-risk-table/).

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

