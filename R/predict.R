# using ml_model and ml_ensemble to create predictions


#' prepare a data object so that it includes all required features
#'
#' @param data object with data to operate on, e.g. matrix or data.frame
#' @param feature_names character, vector with feature names
#'
#' @return object with data holding expected features; some features in data
#' may be omitted, and other features added with NA values
prepare_data <- function(data, feature_names) {
  if (!is(data, "data.frame") & !is(data, "matrix")) {
    return(data)
  }
  keep_features <- intersect(colnames(data), feature_names)
  result <- subset(data, select=keep_features)
  missing_features <- setdiff(feature_names, colnames(data))
  n_missing <- length(missing_features)
  if (n_missing) {
    missing_data <- matrix(as.integer(NA), nrow=nrow(data), ncol=n_missing)
    colnames(missing_data) <- missing_features
    result <- subset(cbind(result, missing_data), select=feature_names)
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
predict.ml_model <- function(object, data, type="response", ...) {
  prepped_data <- apply_hooks(object$hooks, data, type="pre")
  prepped_data <- prepare_data(prepped_data, object$feature_names)
  if (is(object$model, "function")) {
    result <- object$model(prepped_data, type=type, ...)
  } else {
    result <- predict(object$model, prepped_data, type=type, ...)
  }
  if (length(result) > nrow(data)) {
    if (!is(result, "matrix")) {
      result <- matrix(result, byrow=TRUE, nrow=nrow(data))
    }
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
#' @param data object with data
#' @param ... other arguments passed to predict
#'
#' @return list of raw predictions
raw_predict_ensemble <- function(ensemble, data, ...) {
  result <- lapply(ensemble$models,
                   function(z) {
                     predict(z, data, ...)
                   })
  names(result) <- vapply(ensemble$models, function(z) { z$model_name },
                          character(1))
  result
}


#' use an ensemble of models (ml_ensembl) to predict output for a new dataset
#'
#' @export
#' @param object object of class ml_ensemble
#' @param data object with data to operate on, e.g. matrix or data frame as
#' appropriate for the ensemble of models
#' @param type character, passed to predict()
#' @param ... other arguments, passed to predict()
#'
#' @return object of class ml_ensemble
predict.ml_ensemble <- function(object, data, type="response", ...) {
  if (length(object$models)==0) {
    stop("ml_ensemble is empty")
  }

  # produce predictions from the individual models
  result <- apply_hooks(object$hooks, data, type="pre")
  result <- raw_predict_ensemble(object, result)

  # compute reduction
  if (identical(object$calibration, NA) & length(result)>1) {
    warning("ml_ensemble is not calibrated - using equal model weights")
  }
  reduce_fun <- reduce_vector
  if (is(result[[1]], "matrix")) {
    reduce_fun <- reduce_matrix
  }
  result <- reduce_fun(result, object$calibration)

  if (is(result, "matrix")) {
    rownames(result) <- rownames(data)
  } else {
    names(result) <- rownames(data)
  }
  apply_hooks(object$hooks, result, type="post")
}

