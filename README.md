<!-- README.md is generated from README.Rmd. Please edit that file -->
survutils
=========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/survutils)](http://cran.r-project.org/package=survutils)

An R package for Survival Analysis. This package mainly serves as a wrapper around various survival analysis functions (e.g. `coxph`, `survdiff`) to provide a more inituitive interface. It has the following features (grouped by major topics):

Cox Regression

-   `get_cox_res`: Run univariate or multivariate cox regression.
-   `plot_cox_res`: Generates a forest plot of the univariate or multivariate cox regression results from `get_cox_res`.

Kaplan Meier Estimates/Cuvres

-   `get_surv_prob`: Calculates the survival probability at specified times from a survival curve.
-   `get_nrisk_tbl`: Provides a number at risk table as [typically seen in publications](https://mcfromnz.wordpress.com/2011/11/06/kaplan-meier-survival-plot-with-at-risk-table/).
-   `get_logrank_res`: Runs a log-rank test.

Other

-   `get_c_stat`: Calculate C-statistics.

How to Install
==============

To install this package using devtools:

``` r
devtools::install_github("tinyheero/survutils")
```

Cox Regression
==============

`survutils` provides a nice wrapper function `get_cox_res` that allows you to quickly run an univariate or multivariate cox regression on a set of data. The input data is a data.frame for instance (taking the colon dataset from the `survival` R package as the example):

``` r
library("survival")
library("knitr")
library("survutils")
library("dplyr")

head(colon) %>%
    select(age, obstruct, time, status, rx) %>%
    kable
```

|  age|  obstruct|  time|  status| rx      |
|----:|---------:|-----:|-------:|:--------|
|   43|         0|  1521|       1| Lev+5FU |
|   43|         0|   968|       1| Lev+5FU |
|   63|         0|  3087|       0| Lev+5FU |
|   63|         0|  3087|       0| Lev+5FU |
|   71|         0|   963|       1| Obs     |
|   71|         0|   542|       1| Obs     |

The relevant columns are:

-   `age` and `obstruct`: These are the features we want to regress on.
-   `time`: Time to event.
-   `status`: Event status (1 for event; 0 for non-event).
-   `rx`: Different treatment groups.

Then to run `get_cox_res`:

``` r
endpoint <- "time"
endpoint.code <- "status"
 
features <- c("age", "obstruct")
cox.res.df <- get_cox_res(colon, endpoint, endpoint.code, features)
kable(cox.res.df)
```

| term     |   estimate|  std.error|   statistic|    p.value|   conf.low|  conf.high|
|:---------|----------:|----------:|-----------:|----------:|----------:|----------:|
| age      |  0.9983432|  0.0028040|  -0.5913434|  0.5542904|  0.9928717|   1.003845|
| obstruct |  1.2677379|  0.0808045|   2.9359039|  0.0033258|  1.0820531|   1.485287|

This runs a multivariate cox regression on the entire set of data. We can plot the results using `plot_cox_res`:

``` r
plot_cox_res(cox.res.df)
```

![](README-images/get_cox_res_example-1.png)

This gives us a forest plot with the hazard ratio and confidence evidence for each feature. If we are interested in running cox regression within each treatment group, we can make use of the `group` parameter.

``` r
group <- "rx"
cox.res.df <- get_cox_res(colon, endpoint, endpoint.code, features, group)
kable(cox.res.df)
```

| rx      | term     |   estimate|  std.error|   statistic|    p.value|   conf.low|  conf.high|
|:--------|:---------|----------:|----------:|-----------:|----------:|----------:|----------:|
| Obs     | age      |  1.0026174|  0.0046032|   0.5678581|  0.5701313|  0.9936124|  1.0117040|
| Obs     | obstruct |  1.2123725|  0.1319705|   1.4592591|  0.1444938|  0.9360576|  1.5702528|
| Lev     | age      |  1.0042268|  0.0048754|   0.8651343|  0.3869651|  0.9946764|  1.0138689|
| Lev     | obstruct |  1.4151910|  0.1293943|   2.6837694|  0.0072797|  1.0981822|  1.8237097|
| Lev+5FU | age      |  0.9869403|  0.0051866|  -2.5345800|  0.0112582|  0.9769584|  0.9970242|
| Lev+5FU | obstruct |  1.0844978|  0.1677669|   0.4835103|  0.6287334|  0.7805940|  1.5067186|

Notice how the output data.frame now has cox regression results for each treatment group (i.e. Obs, Lev, Lev+5FU). We can also plot these results very easily:

``` r
plot_cox_res(cox.res.df, group = group)
```

![](README-images/get_cox_res_group_example-1.png)

This will facet the groups so that we can visualize the cox regression results for each treatment group.
