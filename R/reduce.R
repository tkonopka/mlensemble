# functions to reduce a list of predictions into a single result


#' use an ml_model to predict output for a new dataset
#'
#' @keywords internal
#' @noRd
#' @param object object of class ml_model
#' @param data object with data to operate on, e.g. matrix or data frame
#'
#' @return prediction of the model "object" on a dataset "data"
reduce_mean <- function(x, weight=NULL) {
  if (is.null(weight)) {
    weight <- setNames(rep(1, length(x)), names(x))
  }
  stopifnot(identical(names(x), names(weight)))
  result <- x[[1]] * weight[1]
  if (length(x)>1) {
    for (i in seq(2, length(x))) {
      result <- result + (x[[i]]*weight[[i]])
    }
  }
  result / sum(weight)
}


#' reduce a set of vector predictions using a calibration
#'
#' @keywords internal
#' @noRd
#' @param x list of vectors
#' @param calibration list with one model to
#'
#' @return vector with reduced predictions
reduce_vector <- function(x, calibration=NA) {
  # process vectors as one-column matrices
  result <- lapply(x, function(z) {
    matrix(z, ncol=1, dimnames=list(NULL, "output"))
  })
  reduce_matrix(result, calibration, row_normalize=FALSE)[, 1]
}


#' reduce predictions for one class using a glm calibration model
#'
#' @keywords internal
#' @noRd
#' @param x list of matrices
#' @param j integer, class index
#' @param adjusting_model glm model to reduce predictions from many models
#'
#' @return vector with predictions
reduce_matrix_j <- function(x, j, model=NA) {
  # a NULL model can occur when a label is calibrated away
  if (is.null(model)) {
    return(0)
  }
  d <- list()
  for (model_name in names(x)) {
    if (j %in% colnames(x[[model_name]])) {
      d[[model_name]] <- x[[model_name]][, j]
    }
  }
  d <- as.data.frame(d)
  # a model can be NA if calibration was not run at all
  if (identical(model, NA)) {
    return(apply(as.matrix(d), 1, mean, na.rm=TRUE))
  }
  predict(model, newdata=d, type="response")
}


#' reduce a set of predictions using list of glm calibration models
#'
#' @keywords internal
#' @noRd
#' @param x list of matrices, each matrix holding predictions from one model
#' @param calibration list with glm models
#' @param row_normalize logical, determines if rows in the matrix are
#' normalized to sum to one; leave TRUE for multiclass classification and
#' set FALSE for regression
#'
#' @return matrix with reduced predictions
reduce_matrix <- function(x, calibration=NA, row_normalize=TRUE) {
  n <- nrow(x[[1]])
  all_labels <- unique(unlist(lapply(x, function(z) { colnames(z) })))
  n_labels <- length(all_labels)
  result <- matrix(0, ncol=n_labels, nrow=n,
                   dimnames=list(NULL, all_labels))
  for (j in all_labels) {
    j_model <- NA
    if (!identical(calibration, NA)) {
      j_model <- calibration[[j]]
    }
    result[, j] <- reduce_matrix_j(x, j, j_model)
  }
  if (row_normalize) {
    result <- result / apply(result, 1, sum)
  }
  result
}

