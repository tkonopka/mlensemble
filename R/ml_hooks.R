# hooks (functions) for ml_model objects


#' define a list of ml_hook objects
#'
#' @export
#' @param x list of ml_hook objects
#'
#' @return a list-like object
#'
#' @examples
#'
#' # empty collection of hooks
#' my_hooks <- ml_hooks()
#' my_hooks
#'
#' # prep: hook functions
#' hook_fun_1 <- function(x) { x**2 }
#' hook_fun_2 <- function(x) { sqrt(2) }
#' # list of hook objects, and object containing the list of hooks
#' hook_list <- list(ml_hook(hook_fun_1), ml_hook(hook_fun_2))
#' my_hooks <- ml_hooks(hook_list)
#' my_hooks
#'
ml_hooks <- function(x=list()) {
  result <- x
  class(result) <- "ml_hooks"
  result
}


#' extract hooks of a certain type from an ml_hooks list
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_hooks
#' @param type character, type of hook to extract
#'
#' @return list with ordered hooks
get_hooks <- function(x, type=c("pre", "post")) {
  type <- match.arg(type)
  hook_order <- vapply(x,
                       function(z) {
                         if (z$type!=type) return(as.numeric(NA))
                         as.numeric(z$order)
                       }, numeric(1))
  hook_rank <- sort.list(sort.list(hook_order))
  n_hooks <- sum(!is.na(hook_order))
  if (n_hooks==0) {
    result <- list()
  } else {
    result <- new("list", n_hooks)
    for (i in seq_len(length(x))[!is.na(hook_order)]) {
      result[[hook_rank[i]]] <- x[[i]]
    }
  }
  ml_hooks(result)
}


#' apply a list of hooks (in order) on data
#'
#' @keywords internal
#' @noRd
#' @param hooks object of class ml_hooks, or a list of ml_hook
#' @param data data object
#' @param type character, type of hook to apply
#'
#' @return result of applying hook functions onto data
apply_hooks <- function(hooks, data, type=c("pre", "post")) {
  # quick exit if there are no hook
  if (length(hooks)==0) return(data)
  # get a subset of hooks, and apply them sequentially
  result <- data
  selected_hooks <- get_hooks(hooks, type=type)
  for (i in seq_along(selected_hooks)) {
    result <- selected_hooks[[i]]$fun(result)
  }
  result
}
