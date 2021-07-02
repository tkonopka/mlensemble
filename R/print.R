# convenience function: print

#' create character strings to describe an ml_hook object
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_hook
#' @param indent integer, number of spaces to indent
#'
#' @return character vector
str.ml_hook <- function(x, in_list=FALSE, indent=0) {
  result <- c(paste0("  name: ", x$hook_name),
              paste0("  type: ", x$hook_type),
              paste0("  order: ", x$hook_order))
  if (in_list) {
    result[1] <- paste0("- name: ", x$hook_name)
  } else {
    result <- c("ml_hook:", result)
  }
  indent_spaces <- paste0(rep(" ", indent), collapse="")
  paste0(indent_spaces, result)
}


#' Display a simple summary of an ml_hook object
#'
#' @export
#' @param x object of class ml_hook
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' # hook function for normalizing matrices so that rows sum to unity
#' my_fun = function(x) {
#'   x / apply(x, 1, sum)
#' }
#' my_hook = ml_hook(my_fun)
#' my_hook
#'
print.ml_hook <- function(x, ...) {
  out <- str.ml_hook(x, in_list=FALSE, indent=0)
  message(paste(out, collapse="\n"))
  invisible(x)
}


#' Display a simple summary of a list of ml_hook objects
#'
#' @export
#' @param x object of class ml_hooks
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' my_fun = function(x) {
#'   x / apply(x, 1, sum)
#' }
#' my_hooks = ml_hooks() + ml_hook(my_fun)
#' my_hooks
#'
print.ml_hooks <- function(x, ...) {
  out <- lapply(x, str.ml_hook, in_list=TRUE, indent=0)
  out <- c("ml_hooks:", unlist(out))
  message(paste(out, collapse="\n"))
  invisible(x)
}


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

