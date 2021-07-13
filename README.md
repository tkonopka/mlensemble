<!-- README.md is generated from README.Rmd. Do not edit manually -->

# mlensemble

Ensembles of machine learning models

## Synopsis

Package `mlensemble` provides a framework for managing ensembles of
machine learning models. Importantly, the framework does not train or
evaluate individual models. It only provides an interface for putting
existing models together into ensembles. The framework is agnostic to
the implementation details of individual models. This approach allows
ensembles to have heterogeneous components such as models trained, for
example, on different training datasets or with different feature sets.
At the same time, the framework allows for ensemble calibration. This
offers opportunities to tune how individual models come together to form
integrated predictions.

## Installation

The package can be installed through github.

``` r
library(devtools)
install_github("tkonopka/mlenesemble")
```

## Minimal example

Consider a dataset with three variables `x1`, `x2`, and `y`, which are
all dependent on a hidden variable `x`.

``` r
x <- seq(0, 10, length=21)
d <- data.frame(
  x1 = x + rnorm(length(x)),
  x2 = x + rnorm(length(x)),
  y = x + rnorm(length(x))
)
```

Consider the task of predicting `y` given `x1` and `x2`.

With the `mlensemble` package, this problem can be addressed by first
training models using base R, and then combining them into an ensemble.

``` r
# train models using base R
lm_x1 <- lm(y ~ x1, data=d)
lm_x2 <- lm(y ~ x2, data=d)
# create an ensemble of two models
library(mlensemble)
ensemble <- ml_model(lm_x1) + ml_model(lm_x2)
```

The ensemble can be used to predict `y` on new data.

``` r
test_d <- data.frame(x1 = c(1, 2), x2 = c(1, 2))
predict(ensemble, test_d)
```

    ## Warning in predict.ml_ensemble(ensemble, test_d): ml_ensemble is not calibrated
    ## - using equal model weights

    ##        1        2 
    ## 1.467800 2.367683

By default, predictions from individual models are pooled together by
averaging. However, the package offers a means to calibrate the ensemble
and thus pool predictions in a more principled manner. The package also
supports attaching custom functions to pre-process input data or
post-process outputs. See the package vignette for details.

The package also provides summaries in plain-language.

``` r
summary(ensemble)
```

    ## Ensemble 'ml_ensemble' consists of two models: 'lm_x1', 'lm_x2'.
    ## 
    ## Model 'lm_x1' is of class 'lm'. It uses one feature ('x1'). It does not
    ## apply any pre-processing steps. It does not apply any post-processing
    ## steps.
    ## 
    ## Model 'lm_x2' is of class 'lm'. It uses one feature ('x2'). It does not
    ## apply any pre-processing steps. It does not apply any post-processing
    ## steps.
    ## 
    ## Ensemble 'ml_ensemble' is not calibrated. It does not apply any
    ## pre-processing steps. It does not apply any post-processing steps.
