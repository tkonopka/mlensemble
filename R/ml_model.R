# creating model ensembles by constructors and composition


#' define an object as an ml_model type
#'
#' @export
#' @param model object with a machine learning model (e.g. lm, glm, xgboost)
#' @param model_name character
#' @param feature_names character, vector of feature names. If NULL,
#'    feature names will be guessed from the model object
#' @param label_names character, vector of labels for multi-class
#' classification.
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
ml_model <- function(model, model_name=NULL,
                     feature_names=NULL, label_names=NULL) {
  if (is.null(model_name)) {
    model_name <- deparse(substitute(model))
  }
  if (is.null(feature_names)) {
    feature_names <- guess_feature_names(model)
  }
  result <- list(
    model=model,
    model_name=model_name,
    feature_names=feature_names,
    label_names=label_names
  )
  class(result) <- "ml_model"
  result
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
