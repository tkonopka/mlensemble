<!-- README.md is generated from README.Rmd. Do not edit manually -->

# mlensemble

Ensembles of machine learning models

## Synopsis

Package `mlensemble` provides a framework for managing ensembles of
machine learning models in R. Importantly, the framework does not train
or evaluate individual models. It only provides an interface for putting
existing models together into ensembles.

The framework is agnostic to the implementation details of individual
models. This approach allows ensembles to have heterogeneous components,
e.g. models trained on different training datasets or with different
feature sets. At the same time, the framework allows for ensemble
calibration. Calibration tunes how individual models come together to
form integrated predictions. This offers a means to adjust ensembles to
new datasets without the need to retrain the underlying models.

## Features

The package provides the following features:

  - creation of ensembles via composition of models using the `+`
    operator
  - support for regression and multi-class classification;
    classification models must provide output in the form of
    probabilities for each class
  - support for custom predictors implemented as an R function
  - support for heterogeneous models, including models using different
    sets of input features and models trained on different datasets
  - tested with models created by `lm`, `glm`, and `xgboost` (library
    [xgboost](https://cran.r-project.org/web/packages/xgboost/index.html))
  - should, in principle, be compatible with any R model that supports
    the `predict` method
  - harmonization of class labels in the context of multi-class
    classification (e.g. when integrating models using different sets of
    labels)
  - attaching custom pre-processing and post-processing functions
  - calibration of ensembles using calibration datasets
  - plain-language descriptions of individual models and model ensembles

It is important to note that the package does not train new models. It
also does not compare the performance of competing models. For these
tasks, see other machine learning frameworks in R, for example
[caret](https://cran.r-project.org/web/packages/caret/) or
[mlr3](https://cran.r-project.org/web/packages/mlr3/index.html).

## Installation

The package can be installed through github.

``` r
library(devtools)
install_github("tkonopka/mlenesemble")
```

## Minimal example

Consider a training dataset with three variables `x1`, `x2`, and `y`,
which are dependent on a hidden variable `x`.

``` r
set.seed(12345)
x <- seq(0, 10, by=0.5)
data_train <- data.frame(
  x1 = x + rnorm(length(x)),
  x2 = x + rnorm(length(x)),
  y = x + rnorm(length(x))
)
```

Consider the task of predicting `y` given `x1` and `x2`. With
`mlensemble`, this problem can be addressed by training models using
base R, and then combining them into an ensemble.

``` r
lm_x1 <- lm(y ~ x1, data=data_train)
lm_x2 <- lm(y ~ x2, data=data_train)
library(mlensemble)
ensemble <- ml_model(lm_x1) + ml_model(lm_x2)
# prediction on test data (emits a warning)
data_test <- data.frame(x1 = c(1, 10), x2 = c(1, 10))
predict(ensemble, data_test)
```

    ## Warning in predict.ml_ensemble(ensemble, data_test): ml_ensemble is not
    ## calibrated - using equal model weights

    ##        1        2 
    ## 1.766869 8.985595

By default, the ensemble pools predictions from individual models by
averaging and using equal weights for all models. However, the package
can calibrate ensembles and combine predictions in a more principled
manner. This is especially interesting when new data are expected to
have slightly different properties than the training data.

``` r
# calibration using data with noise in x2
x_calib <- seq(1, 10)
data_calib <- data.frame(
  x1 = x_calib + rnorm(length(x_calib)),
  x2 = x_calib + rnorm(length(x_calib), 200, 10),
  y = x_calib + rnorm(length(x_calib))
)
ensemble_calibrated <- calibrate(ensemble, data_calib, data_calib$y)
# new data with noise in x2
data_noisy <- data.frame(x1=c(1, 10), x2=c(300, 300))
# predictions by the uncalibrated ensemble are affected by extreme values of x2
predict(ensemble, data_noisy)
```

    ## Warning in predict.ml_ensemble(ensemble, data_noisy): ml_ensemble is not
    ## calibrated - using equal model weights

    ##        1        2 
    ## 112.5373 116.4218

``` r
# predictions by the calibrated ensemble are less affected by x2
predict(ensemble_calibrated, data_noisy)
```

    ##         1         2 
    ##  5.988938 12.977685

The package provides summaries in plain language.

``` r
summary(ensemble_calibrated)
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
    ## Ensemble 'ml_ensemble' is calibrated with a dataset of 10 items. It
    ## does not apply any pre-processing steps. It does not apply any
    ## post-processing steps.

As the summary suggests, the package supports attaching custom functions
to pre-process input data or post-process outputs. See the package
vignette for details.

## Contributing

Contributions are welcome. Please raise an issue in the github
repository.
