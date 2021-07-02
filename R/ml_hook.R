# hooks (functions) for ml_model objects


#' define one hook for a ml_model object
#'
#' @export
#' @param f function
#' @param hook_name character, a name/identifier for the hook
#' @param hook_type character, vector of feature names. If NULL,
#'    feature names will be guessed from the model object
#'
#' @return the same object with a new class 'ml_model'
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=1:2))
#'
#' # default constructor
#' ml_model(lm_1)
#'
#' # use a custom name
#' ml_model(lm_1, model_name="linear_model_1")
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
    hook_fun=f,
    hook_name=hook_name,
    hook_type=hook_type,
    hook_order=as.numeric(order)
  )
  class(result) <- "ml_hook"
  result
}

