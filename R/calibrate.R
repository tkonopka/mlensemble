# adjusting an ensemble of models using calibration data


#' redefine weights assigned to models in an ensemble
#'
#' @export
#' @param ensemble object of class 'ml_ensemble'
#' @param data calibration data, e.g. matrix or data frame as
#' appropriate for the model ensemble
#' @param label vector with expected predictions, e.g. numeric values for
#' regression, or integer/character values for classification
#'
#' @return object of class ml_ensemble
#'
#' @examples
#'
calibrate <- function(ensemble, data, label) {
  # get predictions from each model
  raw <- raw_predict_ensemble(ensemble, data)
  raw <- standardize_raw(raw)
  n <- ncol(raw[[1]])
  calibration_family <- gaussian
  if (n>1) {
    calibration_family <- binomial
  }
  label <- label_matrix(label, n>1)
  # build models for each label
  # (one model for regression, many models for multi-class classification)
  result <- lapply(colnames(label), function(j) {
    calibrate_one(raw, label, j, calibration_family)
  })
  ensemble$calibration <- setNames(result, colnames(label))
  ensemble
}


#' build one calibration model
#'
#' @param raw list of matrices with predictions
#' @param label matrix with expected predictions
#' @param j integer, column to consider
#' @param family description of error distribution
#'
#' @return glm model
calibrate_one <- function(raw, label, j, family) {
  d <- data.frame(matrix(0, ncol=length(raw)+1, nrow=nrow(label)))
  colnames(d) <- c(".output", names(raw))
  d$.output <- label[,j]
  for (model_name in names(raw)) {
    if (j %in% colnames(raw[[model_name]])) {
      d[[model_name]] <- raw[[model_name]][,j]
    } else {
      d[[model_name]] <- NULL
    }
  }
  d_features <- intersect(names(raw), colnames(d))
  f <- paste0(".output ~ ", paste0(d_features, collapse="+"))
  glm(as.formula(f), data=d, family=family, model=FALSE)
}


#' standardize predictions into matrix form
#'
#' @param raw list of predictions as vectors or matrices
#'
#' @return list of predictions, always as matrices
standardize_raw <- function(raw) {
  lapply(raw, function(z) {
    if (!is(z, "matrix")) {
      return(matrix(z, ncol=1, dimnames=list(NULL, "output")))
    }
    z
  })
}


#' standardize labels into matrix form
#'
#' @param label vector with values
#' @param one_hot logical, determines if label matrix has one column
#' with values, or a matrix using one-hot encoding
#'
#' @return matrix with one row per prediction, one column per label class
label_matrix <- function(label, one_hot) {
  if (!one_hot) {
    return(matrix(label, ncol=1, dimnames=list(NULL, "output")))
  }
  label_names <- all_labels <- unique(label)
  if (is(label, "integer") | is(label, "numeric")) {
    label_names <- paste0("label_", as.integer(all_labels))
  }
  result <- matrix(0, ncol=length(all_labels), nrow=length(label),
                   dimnames=list(NULL, label_names))
  for (i in seq_along(all_labels)) {
    result[label==all_labels[i], label_names[i]] <- 1
  }
  result
}

