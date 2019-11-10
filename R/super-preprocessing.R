preprocess_supermarket <- function(obj){
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_BOUNDARIES)
  obj <- preprocess_supermarket_experiment(obj)
  obj <- preprocess_supermarket_results(obj)
  class(obj) <- append("supermarket", class(obj))
  return(obj)
}

preprocess_supermarket_experiment <- function(obj){
  exp <- get_experiment_log(obj)
  exp$RightWrong <- exp$RightWrong == 1
  obj$data$experiment_log$data <- exp
  return(obj)
}

preprocess_supermarket_results <- function(obj){
  res <- get_results_log(obj)
  if(!is.null(res)){
    obj$data$results_log$data <- res
  }
  return(obj)
}
