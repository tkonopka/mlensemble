# convenience function: print


#' Display a simple summary of an ml_model object
#'
#' @export
#' @param x ml_model
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=1:2))
#' model = ml_model(lm_1)
#' print(model)
#'
print.ml_model <- function(x, ...) {
  out <- paste0("ml_model: ", x$model_name)
  message(paste(out, collapse="\n"))
  invisible(x)
}


#' Display a simple summary of an ml_ensemble object
#'
#' @export
#' @param x ml_ensemble
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(Y~x, data=data.frame(x=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#' print(ensemble)
#'
print.ml_ensemble <- function(x, ...) {
  out <- c(paste0("ml_ensemble: ", x$model_name),
           "",
           paste("number of models: ", length(x$models)))
  out_names <- vapply(x$models, function(z) { z$model_name}, character(1))
  out <- c(out, "model names:", paste0("  ", out_names))
  message(paste(out, collapse="\n"))
  invisible(x)
}

