# hooks (functions) for ml_model objects


#' define one hook for a ml_model object
#'
#' @export
#' @param f function
#' @param name character, a name/identifier for the hook
#' @param type character, vector of feature names. If NULL,
#'    feature names will be guessed from the model object
#' @param order numeric, determines the order/priority of a hook when it is
#' used within a collection of many other hooks
#'
#' @return the same object with a new class 'ml_model'
#'
#' @examples
#'
#' # define a hook function that replaces NA of "x" in a data frame
#' fill_NA <- function(d) {
#'   d[is.na(d$x), "x"] <- mean(d$x, na.rm=TRUE)
#'   d
#' }
#' # define a hook using fill_NA as a pre-processing operation
#' hook <- ml_hook(fill_NA, type="pre")
#' hook
#'
ml_hook <- function(f, name=NULL, type=c("pre", "post"), order=1) {
  hook_type <- match.arg(type)
  hook_name <- name
  if (is.null(name)) {
    hook_name <- deparse(substitute(f))
  }
  name_check <- unlist(strsplit(hook_name, "\\s"))
  if (length(name_check) > 1) {
    stop(paste0("invalid hook name: ", paste(hook_name, collapse=" ")))
  }

  result <- list(
    fun=f,
    name=hook_name,
    type=hook_type,
    order=as.numeric(order)
  )
  class(result) <- "ml_hook"
  result
}

