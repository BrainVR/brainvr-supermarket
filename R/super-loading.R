#' Loads supermarket experiment data from folder
#'
#' @param folder folder with one or more supermarket experiments
#' @param language language of the items in the experiment log.
#' Only important if you are not logging item codes
#' See language options in [item_translations]. Default is "CZ".
#' @param ... other parameters being set to brainvr.reader::load_experiments.
#' typically this is `override`, and `save`. If `timestamp` is defined, then
#' the function `brainvr.reader::load_experiment` is used instead
#'
#' @return list with loaded experiments
#' @export
#'
#' @examples
load_supermarket_experiments <- function(folder, language = "CZ", ...) {
  exps <- load_experiments(folder, ...)
  exps <- sapply(exps, preprocess_supermarket, language, simplify = FALSE)
  exps <- exps[!sapply(exps, is.null)]
  return(exps)
}

#' @describeIn load_supermarket_experiments Loads a single supermarket experiment
#' given the passed timestamp. Throws error when not found. Use only if you know your
#' timestamps
#' @export
load_supermarket_experiment <- function(folder, timestamp, language = "CZ", ...){
  exp <- load_experiment(folder, exp_timestamp = timestamp, ...)
  if(is.null(exp)) return(NULL)
  exp <- preprocess_supermarket(exp, language)
  return(exp)
}
#' Loads .json settings file
#'
#' @param filepath path to the .json settings file. In newer versions,
#' settings is usually already included in the header, but in older versions
#' it needs to be loaded sparately.
#' @import jsonlite
#'
#' @return list with loaded settings
#' @export
#'
#' @examples
load_supermarket_settings <- function(filepath) {
  settings <- jsonlite::read_json(filepath)
  return(settings)
}

#' Loads taskslist form .json file
#'
#' @param filepath path to the .json tasklist file. In newer logging versions,
#' this is already included in the header, but in older versions it
#' needs to be loaded separately
#' @param language language of the tasklist.  Only important if you are
#' not logging item codes.
#' See language options in [item_translations]. Default is "CZ".
#'
#' @return data.frame with supermarket task progression
#' @export
#'
#' @examples
load_supermarket_takslist <- function(filepath, language = "CZ") {
  settings <- jsonlite::read_json(filepath)
  df <- json_tasklist_to_data_frame(settings)
  df$item <- convert_name_to_item_code(df$item, language)
  return(df)
}

#' HELPERS -------
#' @noRd
json_tasklist_to_data_frame <- function(tasklist) {
  df <- data.frame(
    trial = numeric(0),
    n_items = numeric(0),
    order = numeric(0),
    item = character(0)
  )
  for (i in seq_len(length(tasklist$tasks))) {
    task <- tasklist$tasks[[i]]$task
    n <- length(task)
    df_small <- data.frame(
      trial = rep(i, n),
      n_items = rep(n, n),
      order = 1:n,
      item = unlist(task),
      stringsAsFactors = FALSE
    )
    df <- rbind(df, df_small)
  }
  return(df)
}
