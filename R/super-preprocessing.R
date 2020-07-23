#' @noRd
preprocess_supermarket <- function(obj, language){
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_BOUNDARIES)
  #' NECESSARY TO come before expeirment because of the "has_item_codes" function
  #' The has_item_codes is currently deduced form experiment log, which is processed
  #' and thus adds them - results then thinks that codes are present and wouldn't process
  #' properly
  obj <- preprocess_supermarket_experiment(obj, language)
  obj <- preprocess_supermarket_results(obj, language)
  class(obj) <- append("supermarket", class(obj))
  return(obj)
}

preprocess_supermarket_experiment <- function(obj, language = "CZ"){
  exp <- get_experiment_log(obj)
  if("Test_cycle" %in% colnames(exp)){
    colnames(exp) <- c("Time", "TestCycle", "TaskItems", "PlayerPosition",
      "ObjectName", "TaskOrder", "Action", "RightWrong", "Trajectory")
  }
  exp <- convert_action(exp)
  exp <- convert_rightwrong(exp)
  if(!(grepl("ITEM", exp$ObjectName[1]))){
    exp$ObjectName <- convert_name_to_item_code(exp$ObjectName, language)
  }
  obj$data$experiment_log$data <- exp
  return(obj)
}


preprocess_supermarket_results <- function(obj, language){
  res <- get_results_log(obj)
  res[,ncol(res)] <- NULL # last column is empty
  # VERSION 4 has suddenly renamed columns. Need to standardize them
  if("Time_finished" %in% colnames(res)){
    colnames(res) <- c("TimeFinished", "TimeStarted", "TaskItems",
                       "ItemsCollected", "AdditionalItems", "MissingItems",
                       "TaskTime", "TaskTrajectory", "AdditionalItemsList",
                       "MissingItemsList")
  }
  if(!is.null(res)){
    if(!("TestCycle" %in% colnames(res)) & nrow(res) > 0) {
      res$TestCycle <- 1:nrow(res)
    }
    res$MissingItemsList <- gsub("[()]","", res$MissingItemsList)
    res$AdditionalItemsList <- gsub("[()]","", res$AdditionalItemsList)
    if(!has_item_codes(obj)){
      res$MissingItemsList <- convert_strings_to_item_codes(res$MissingItemsList, language)
      res$AdditionalItemsList <- convert_strings_to_item_codes(res$AdditionalItemsList, language)
    }
    obj$data$results_log$data <- res
  }
  return(obj)
}

#' Checks if the items in the data are coded with codes or with czech names
#'
#' @param obj
#'
#' @return logical value
has_item_codes <- function(obj){
  exp_log <- get_experiment_log(obj)
  if(nrow(exp_log) < 1) stop("Test log has no information")
  return(grepl("ITEM", exp_log$ObjectName[1]))
}

#' Converts actions written in Czech to english counterparts
#'
#' @param exp experiment_log
#'
#' @return modified experiment log
convert_action <- function(exp){
  if(any(names(term_translation$Action) %in% exp$Action)){
    exp$Action <- term_translation$Action[exp$Action]
  }
  return(exp)
}

#' Converts RightWrong written in Czech to english counterparts.
#' Then it converts the column to logical
#'
#' @param exp experiment_log
#'
#' @return modified experiment log
convert_rightwrong <- function(exp){
  if(any(names(term_translation$RightWrong) %in% exp$RightWrong)){
    exp$RightWrong <- term_translation$RightWrong[exp$RightWrong]
  }
  exp$RightWrong <- exp$RightWrong == 1
  return(exp)
}
#' Converts existing
#'
#' @param items
#' @param language what language to use for conversions. e.g. "CZR".
#' This language needs to figure out in the intrm_translations.rda file
#'
#' @return vector of converted values
convert_name_to_item_code <- function(items, language){
  #' The [1] is there because some of the items have two IDs
  #' in the item_translations data
  codes <- sapply(items, function(x){
    item_translations$ID[item_translations[[language]] == x][1]
    }, simplify = TRUE)
  return(codes)
}

#' Used on MissingItemsList and AdditionalItemsList to just replace
#' names with proper CODES takes strings like "potato chips, apple" and
#' returns "ITEM_CHIPS, ITEM_APPLE"
#' @noRd
convert_strings_to_item_codes <- function(strings, language){
  convert_string <- function(x){
    paste(convert_name_to_item_code(strsplit(x, ",")[[1]], language), collapse=",")
  }
  res <- sapply(strings, convert_string)
  return(res)
}
