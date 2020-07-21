#' @noRd
preprocess_supermarket <- function(obj, language){
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_BOUNDARIES)
  #' NECESSARY TO come before expeirment because of the "has_item_codes" function
  #' The has_item_codes is currently deduced form experiment log, which is processed
  #' and thus adds them - results then thinks that codes are present and wouldn't process
  #' properly
  obj <- preprocess_supermarket_results(obj, language)
  obj <- preprocess_supermarket_experiment(obj, language)
  class(obj) <- append("supermarket", class(obj))
  return(obj)
}

preprocess_supermarket_experiment <- function(obj, language = "CZ"){
  exp <- get_experiment_log(obj)
  exp$RightWrong <- exp$RightWrong == 1
  if(!has_item_codes(obj)){
    exp$ObjectName <- convert_name_to_item_code(exp$ObjectName, language)
  }
  obj$data$experiment_log$data <- exp
  return(obj)
}

preprocess_supermarket_results <- function(obj, language){
  res <- get_results_log(obj)
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

has_item_codes <- function(obj){
  exp_log <- get_experiment_log(obj)
  if(nrow(exp_log) < 1) stop("Experiment log has no information")
  return(grepl("ITEM", exp_log$ObjectName[1]))
}

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
  res <- sapply(strings,
                function(x){
                  paste(convert_name_to_item_code(strsplit(x, ",")[[1]], language),
                        collapse=",")
                })
  return(res)
}
