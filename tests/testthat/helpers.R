# helper functions used in the test suite


#' call the most probably class/label from xgboost softprob
#'
#' @param x matrix with class labels in columns
call_softmax <- function(x) {
  label_index <- apply(x, 1, which.max)
  if (is.null(colnames(x))) {
    return(as.integer(label_index))
  }
  colnames(x)[label_index]
}


#' compute root-mean-square-error
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return root mean square error
rmse <- function(x, y) {
  sqrt(sum((x-y)**2)/length(y))
}


#' row-normalize a matrix
#' (can be used as a hook)
#'
#' @param x matrix
#'
#' @return matrix
rowsum_norm <- function(x) {
  x / apply(x, 1, sum)
}


#' binary classification into negative and non-negative based on one feature
#'
#' @param x matrix with at least one numeric column
#' @param ... other arguments (not used)
#'
#' @return matrix with two columns
predict_negative_or_positive <- function(x, ...) {
  result <- matrix(0, ncol=2, nrow=nrow(x))
  x_sign <- sign(x[,1])
  result[x_sign<0, 1] <- 1
  result[x_sign>0, 2] <- 1
  result[x_sign==0, c(1, 2)] <- c(0.5, 0.5)
  result
}

