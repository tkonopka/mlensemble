# creating model ensembles by constructors and composition


#' define an ensemble of ml_model objects
#'
#' @export
#' @param model_name character, a name
#'
#' @return object of class ml_ensemble
#'
#' @examples
#'
#' #empty ensemble
#' ensemble = ml_ensemble()
#'
#' # ensemble with several models
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(Y~x, data=data.frame(x=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#'
ml_ensemble <- function(model_name=NULL) {
  if (is.null(model_name)) {
    model_name <- "ml-ensemble"
  }
  result <- list(models=list(), model_name=model_name, calibration=NA)
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
#' @return object of class ml_ensemble that incorporates the new model
add_ml_model <- function(ensemble, model) {
  n <- length(ensemble$models)
  result <- ensemble
  result$models[[n+1]] <- model
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
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(Y~x, data=data.frame(x=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#'
"+.ml_model" <- function(m1, m2) {
  is_ensemble1 <- is(m1, "ml_ensemble")
  is_ensemble2 <- is(m2, "ml_emsemble")
  if ((is_ensemble1 & is_ensemble2) | (!is_ensemble1 & !is_ensemble2)) {
    # two model ensembles, or two single models
    result <- add_ml_model(add_ml_model(ml_ensemble(), m1), m2)
  } else if (is_ensemble1 & !is_ensemble2){
    result <- add_ml_model(m1, m2)
  } else if (!is_ensemble1 & is_ensemble2) {
    result <- add_ml_model(m2, m1)
  }
  result
}

