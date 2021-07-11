# creating model ensembles by constructors and composition


#' define an object as an ml_model type
#'
#' @export
#' @param model object with a machine learning model (e.g. lm, glm, xgboost)
#' @param name character, a unique name for the model
#' @param feature_names character, vector of feature names. If NULL,
#'    feature names will be guessed from the model object
#' @param label_names character, vector of labels for multi-class
#' classification.
#' @param hooks objects of class ml_hooks, or a list of ml_hook objects
#' @param description character, used in construction of plain-language
#' summaries of the model; recommended to consist of a single sentence fragment
#' starting with a verb, e.g. 'is a linear regression'.
#'
#' @return an object holding the model and book-keeping metadata
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=1:2))
#'
#' # default constructor
#' ml_model(lm_1)
#'
#' # use a custom name
#' ml_model(lm_1, name="linear_model_1")
#'
ml_model <- function(model, name=NULL,
                     feature_names=NULL, label_names=NULL,
                     hooks=list(), description=NA) {
  if (is.null(name)) {
    model_name <- trim_model_name(deparse(substitute(model)))
  } else {
    model_name <- trim_model_name(name)
  }
  if (is.null(feature_names)) {
    feature_names <- guess_feature_names(model)
  }
  result <- list(
    model=model,
    name=model_name,
    feature_names=feature_names,
    label_names=label_names,
    hooks=ml_hooks(hooks),
    description=description
  )
  class(result) <- "ml_model"
  result
}


#' trim a model name into alphanumeric, underscore, and dot
#'
#' @param x character
#'
#' @return character, model name, suitable as a data frame column
trim_model_name <- function(x) {
  if (is.null(x)) return (x)
  result <- unlist(strsplit(x, "\\(| |\\)"))[1]
  result <- unlist(strsplit(result, ""))
  alphanumeric <- c(letters, LETTERS, as.character(0:9), ".", "_")
  result <- result[result %in% alphanumeric]
  paste(result, collapse="")
}


#' guess a list of features names from a machine-learning model
#'
#' @keywords internal
#' @noRd
#' @param model object with a machine-learning model
#'
#' @return character vector
guess_feature_names <- function(model) {
  result <- NA
  if (is(model, "xgb.Booster")) {
    result <- model$feature_names
  }
  if (is(model, "lm")) {
    result <- setdiff(names(model$coefficients), "(Intercept)")
  }
  if (any(is.na(result))) {
    stop("could not determine feature names")
  }
  result
}
