# convenience function: summary


#' clean a string (remove double spaces, double dots, ...)
#'
#' @keywords internal
#' @noRd
#' @param x character
#'
#' @return character
clean_str <- function(x) {
  result <- gsub("  ", " ", x)
  result <- gsub("\\.\\.", "\\.", result)
  while (!identical(result, x)) {
    x <- result
    result <- gsub("  ", " ", result)
    result <- gsub("\\.\\.", "\\.", result)
  }
  result
}


#' formulate a word-like summary like "one feature", "two hooks", "13 models"
#'
#' @keywords internal
#' @noRd
#' @param x character, vector of items
#' @param noun character, noun uses in string description
#'
#' @return character string
str_count <- function(x, noun=c("feature", "model", "item",
                                "pre-processing step",
                                "post-processing step")) {
  noun <- match.arg(noun)
  n <- length(x)
  if (n == 1) {
    result <- paste0("one ", noun)
  }  else if (n == 0 | n < 9) {
    numbers <- c("no", "one", "two", "three", "four", "five",
                 "six", "seven", "eight", "nine")
    result <- paste0(numbers[n+1], " ", noun, "s")
  } else {
    result <- paste0(n, " ", noun, "s")
  }
  result
}


#' create a list of items in quotes 'a', 'b', 'c'
#'
#' @keywords internal
#' @noRd
#' @param x character vector
#'
#' @return character string
str_quoted_list <- function(x) {
  paste(paste0("'", x, "'"), collapse=", ")
}


#' get a name of an object
#'
#' @keywords internal
#' @noRd
#' @param z list-like object
#'
#' @return character
str_name <- function(z) {
  z$name
}


#' create a string summary of hooks
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_hooks
#'
#' @return character string with a description or pre- and post-processing
str_ml_hooks <- function(x) {
  pre <- get_hooks(x, type="pre")
  post <- get_hooks(x, type="post")
  pre_names <- vapply(pre, str_name, character(1))
  post_names <- vapply(post, str_name, character(1))
  if (length(pre_names)==0) {
    result_pre <- "It does not apply any pre-processing steps. "
  } else {
    result_pre <- paste0("It applies ", str_count(pre_names, "pre"),
                         " (", str_quoted_list(pre_names),"). ")
  }
  if (length(post_names)==0) {
    result_post <- "It does not apply any post-processing steps. "
  } else {
    result_post <- paste0("It applies ", str_count(post_names, "post"),
                          " (", str_quoted_list(post_names), "). ")
  }
  clean_str(paste(result_pre, result_post))
}


#' create a summary of an ml_model object as a string
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_model
#'
#' @return character
str_ml_model <- function(x) {
  prefix <- paste0("Model '", x$name, "' ")
  if (is(x, "ml_ensemble")) {
    model_names <- vapply(x$models, str_name, character(1))
    if (!is.na(x$description)) {
      description <- paste0(prefix, x$description, ". ",
                            "The ensemble consists of ")
    } else {
      description <- paste0(prefix, "is an ensemble of ")
    }
    description <- paste0(description,
                          str_count(model_names, "model"),
                          " (", str_quoted_list(model_names), "). ")
  } else {
    if (!is.na(x$description)) {
      description <- paste0(prefix, x$description, ". ")
    } else {
      description <- paste0(prefix, " is of class '", class(x$model)[1], "'. ")
    }
    f_names <- x$feature_names
    description <- paste0(description,
                          "It uses ", str_count(f_names, "feature"),
                          " (", str_quoted_list(f_names), "). ")
  }
  clean_str(paste(description, str_ml_hooks(x$hooks)))
}


#' create a summary (introduction) of an ml_ensemble object as a string
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_ensemble
#'
#' @return character
str_ml_ensemble_1 <- function(x) {
  prefix <- paste0("Ensemble '", x$name, "' ")
  model_names <- lapply(x$models, str_name)
  content <- paste0("consists of ", str_count(model_names, "model"),
                    ": ", str_quoted_list(model_names), ". ")
  if (!is.na(x$description)) {
    description <- paste0(prefix, x$description, ". The ensemble ")
  } else {
    description <- prefix
  }
  clean_str(paste(description, content))
}


#' create a summary (conclusion) of an ml_ensemble object as a string
#'
#' @keywords internal
#' @noRd
#' @param x object of class ml_ensemble
#'
#' @return character
str_ml_ensemble_2 <- function(x) {
  prefix <- paste0("Ensemble '", x$name, "' ")
  if (!identical(x$calibration, NA)) {
    calib_values <- x$calibration[[1]]$fitted.value
    calibration <- paste0(prefix, "is calibrated with a dataset of ",
                          str_count(calib_values, "item"), ". ")
  } else {
    calibration <- paste0(prefix, "is not calibrated. ")
  }
  clean_str(paste(calibration, str_ml_hooks(x$hooks)))
}


#' format a long string into a paragraph with words wrapped around lines
#'
#' @keywords internal
#' @noRd
#' @param x character string
#' @param line_len integer, number of characters on a single line
#'
#' @return character vector
wrapped_lines <- function(x, line_len=60, ...) {
  words <- unlist(strsplit(paste(x, collapse=" "), " "))
  words_len <- nchar(words)
  result <- split(words, floor(cumsum(words_len) / line_len))
  result <- vapply(result, paste, collapse=" ", character(1))
  as.character(result)
}


#' Display a description of an ml_model object
#'
#' @export
#' @method summary ml_model
#' @param object ml_model
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=1:2))
#' model = ml_model(lm_1)
#' summary(model)
#'
summary.ml_model <- function(object, ...) {
  result <- str_ml_model(object)
  message(paste(wrapped_lines(result, ...), collapse="\n"))
  invisible(result)
}


#' Display a summary of an ml_ensemble object
#'
#' @export
#' @method summary ml_ensemble
#' @param object ml_ensemble
#' @param ... other parameters (not used)
#'
#' @examples
#'
#' lm_1 = lm(y~x, data=data.frame(x=1:2, y=-0.2 + 1:2))
#' lm_2 = lm(y~x, data=data.frame(x=1:2, y=+0.2 + 1:2))
#' ensemble = ml_model(lm_1) + ml_model(lm_2)
#' summary(ensemble)
#'
summary.ml_ensemble <- function(object, ...) {
  # construct a description with several paragraphs (intro, models, conclusion)
  result <- str_ml_ensemble_1(object)
  for (i in seq_along(object$models)) {
    result[[length(result)+1]] <- str_ml_model(object$models[[i]])
  }
  result[[length(result)+1]] <- str_ml_ensemble_2(object)
  # format descriptions for display
  out <- vapply(result, function(z) {
    paste(wrapped_lines(z, ...), collapse="\n")
  }, character(1))
  message(paste(out, collapse="\n\n"))
  invisible(result)
}

