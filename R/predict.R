# using ml_model and ml_ensemble to create predictions


#' cut a matrix, data.frame, or data table to certains columns
#'
#' @param x matrix, data.frame, or data.table
#' @param col_names character, vector with feature names to keep
#'
#' @return object similar to x, cut to certain columns only
cut_to_features <- function(x, col_names) {
  if (is(x, "data.table")) {
    result <- x[, col_names, with=FALSE]
  } else {
    result <- x[, col_names, drop=FALSE]
  }
  result
}


#' prepare a data object so that it includes all required features
#'
#' @param newdata object with data to operate on, e.g. matrix or data.frame
#' @param feature_names character, vector with feature names
#'
#' @return object with data holding expected features; some features in newdata
#' may be omitted, and other features added with NA values
prepare_newdata <- function(newdata, feature_names) {
  if (!is(newdata, "data.frame") & !is(newdata, "matrix")) {
    return(newdata)
  }
  keep_features <- intersect(colnames(newdata), feature_names)
  result <- cut_to_features(newdata, keep_features)
  missing_features <- setdiff(feature_names, colnames(newdata))
  n_missing <- length(missing_features)
  if (n_missing) {
    missing_data <- matrix(as.integer(NA), nrow=nrow(newdata), ncol=n_missing)
    colnames(missing_data) <- missing_features
    result <- cut_to_features(cbind(result, missing_data), feature_names)
  }
  result
}


#' use an ml_model to predict output for a new dataset
#'
#' @export
#' @param object object of class ml_model
#' @param data object with data to operate on, e.g. matrix or data frame
#' @param type character, code passed to predict()
#' @param ... other arguments passed to predict()
#'
#' @return prediction of the model "object" on a dataset "data"
predict.ml_model <- function(object, newdata, type="response", ...) {
  result <- apply_hooks(object$hooks, newdata, type="pre")
  result <- predict(object$model,
                    prepare_newdata(result, object$feature_names),
                    type=type, ...)
  if (length(result) > nrow(newdata)) {
    result <- matrix(result, byrow=TRUE, nrow=nrow(newdata))
    colnames(result) <- object$label_names
    if (is.null(object$label_names)) {
      colnames(result) <- paste0("label_", -1 + seq_len(ncol(result)))
    }
  }
  apply_hooks(object$hooks, result, type="post")
}


#' use an ensemble of models to predict on new data
#'
#' @param ensemble object of class ml_ensemble
#' @param newdata object with data
#' @param ... other arguments passed to predict
#'
#' @return list of raw predictions
raw_predict_ensemble <- function(ensemble, newdata, ...) {
  result <- lapply(ensemble$models,
                   function(z) {
                     predict(z, newdata, ...)
                   })
  model_names <- vapply(ensemble$models, function(z) { z$model_name },
                        character(1))
  names(result) <- model_names
  result
}


#' use an ensemble of models (ml_ensembl) to predict output for a new dataset
#'
#' @export
#' @param ensemble object of class ml_ensemble
#' @param newdata object with data to operate on, e.g. matrix or data frame as
#' appropriate for the ensemble of models
#' @param type character, passed to predict()
#' @param ... other arguments, passed to predict()
#'
#' @return object of class ml_ensemble
predict.ml_ensemble <- function(ensemble, newdata, type="response", ...) {
  if (length(ensemble$models)==0) {
    stop("ml_ensemble is empty")
  }

  # produce predictions from the individual models
  result <- apply_hooks(ensemble$hooks, newdata, type="pre")
  result <- raw_predict_ensemble(ensemble, result)

  # compute reduction
  if (identical(ensemble$calibration, NA) & length(result)>1) {
    warning("ml_ensemble is not calibrated - using equal model weights")
  }
  reduce_fun <- reduce_vector
  if (is(result[[1]], "matrix")) {
    reduce_fun <- reduce_matrix
  }
  result <- reduce_fun(result, ensemble$calibration)

  if (is(result, "matrix")) {
    rownames(result) <- rownames(newdata)
  } else {
    names(result) <- rownames(newdata)
  }
  apply_hooks(ensemble$hooks, result, type="post")
}

