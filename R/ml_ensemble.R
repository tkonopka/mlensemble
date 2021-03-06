# creating model ensembles by constructors and composition


#' define an ensemble of ml_model objects
#'
#' @export
#' @param name character, a unique name for the model
#' @param hooks object of class ml_hooks, or a list of ml_hook objects
#' @param description character, used in construction of plain-language
#' summaries of the ensemble; recommended to consist of a single sentence
#' fragment starting with a verb, e.g. 'integrates predictions'.
#'
#' @return object of class ml_ensemble
#'
#' @examples
#'
#' #empty ensemble
#' ensemble = ml_ensemble()
#'
#' # ensemble with several models
#' lm_1 = lm(y ~ x1, data=data.frame(x1=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(y ~ x2, data=data.frame(x2=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#'
ml_ensemble <- function(name=NULL, hooks=list(), description=NA) {
  model_name <- trim_model_name(name)
  if (is.null(name)) {
    model_name <- "ml_ensemble"
  }
  result <- list(
    models=list(),
    name=model_name,
    calibration=NA,
    hooks=ml_hooks(hooks),
    description=description
  )
  class(result) <- c("ml_ensemble", "ml_model")
  result
}


#' add a model into an existing ensemble
#'
#' @keywords internal
#' @noRd
#' @param ensemble object of class ml_ensemble
#' @param model object of class ml_model
#'
#' @return object of class ml_ensemble that incorporates the new model;
#' note that any calibration data will be erased in the new ensemble
add_ml_model <- function(ensemble, model) {
  existing_names <- vapply(ensemble$models, str_name, character(1))
  model_name <- str_name(model)
  if (model_name %in% existing_names) {
    stop(paste0("cannot add model to ensemble: model name already exists '",
                model_name, "'"))
  }
  result <- ensemble
  result$models[[length(ensemble$models)+1]] <- model
  result$calibration <- NA
  result
}


#' create an ensemble from two models
#'
#' @export
#' @method + ml_model
#' @param m1 model object
#' @param m2 model object
#'
#' @return a model ensemble
#'
#' lm_1 = lm(y ~ x1, data=data.frame(x1=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(y ~ x2, data=data.frame(x2=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#'
"+.ml_model" <- function(m1, m2) {
  is_ensemble1 <- is(m1, "ml_ensemble")
  if (is_ensemble1) {
    result <- add_ml_model(m1, m2)
  } else {
    result <- add_ml_model(add_ml_model(ml_ensemble(), m1), m2)
  }
  result
}

