#' Loads supermarket experiment data from folder
#'
#' @param folder folder with one or more supermarket experiments
#'
#' @return list with loaded experiments
#' @export
#'
#' @examples
load_supermarket_experiments <- function(folder){
  exps <- load_experiments(folder)
  message("Loaded ", length(exps), " from folder ", folder)
  for(i in 1:length(exps)){
    exps[[i]]$data$position <- add_area_boundaries(exps[[i]]$data$position, AREA_BOUNDARIES)
    class(exps[[i]]) <- append(class(exps[[i]]), "supermarket")
  }
  # Do some preprocessing
  return(exps)
}

#' Loads .json settings file
#'
#' @param filepath path to the .json settings file. In newer versions, settings is usually already included in the header,
#' but in older versions it needs to be loaded sparately.
#' @import jsonlite
#'
#' @return list with loaded settings
#' @export
#'
#' @examples
load_supermarket_settings <- function(filepath){
  settings <- jsonlite::read_json(filepath)
  return(settings)
}

#' Loads taskslist form .json file
#'
#' @param filepath path to the .json tasklist file. In newer logging versions, this is already included in the header,
#' but in older versions it needs to be loaded separately
#'
#' @return data.frame with supermarket task progression
#' @export
#'
#' @examples
load_supermarket_takslist <- function(filepath){
  settings <- jsonlite::read_json(filepath)
  df <- json_tasklist_to_data_frame(settings)
  return(df)
}

json_tasklist_to_data_frame <- function(tasklist){
  df <- data.frame(trial = numeric(0), n_items = numeric(0), order = numeric(0), item = character(0))
  for (i in 1:length(tasklist$tasks)) {
    task <- tasklist$tasks[[i]]$task
    n <- length(task)
    df_small <- data.frame(trial  = rep(i, n), n_items = rep(n, n), order = 1:n, item = unlist(task), stringsAsFactors = F)
    df <- rbind(df, df_small)
  }
  return(df)
}
