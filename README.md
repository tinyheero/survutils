# survutils

An R package for Survival Analysis. To install this package using devtools:

```{r}
devtools::install_github("tinyheero/survutils")
```

# Overview

To see the full list of exported functions:

```{r}
library("survutils")
ls("package/survutils")
```

A quick overview of some of the key functions:

* plot_KM_curve: Plots a Kaplan-Meier curve in ggplot2
* get_cox_summary: Provides a data.frame summary of a coxph.fit object
* get_nrisk_tbl: Provides a number at risk table as typically seen in publications (e.g. https://mcfromnz.wordpress.com/2011/11/06/kaplan-meier-survival-plot-with-at-risk-table/)
