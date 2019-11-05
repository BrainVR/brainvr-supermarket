#' Returns position data for picking up a specific object in given trial
#'
#' @param obj loaded supermarket data
#' @param object_name object name as it appears in the experiment log
#' @param i_task if there are more objects of the same name picked up, this defines which cycle you are interested in
#'
#' @return navr object with the object path (from previous item to the current item)
#' @export
#'
#' @examples
get_position_pickup <- function(obj, object_name = NULL, i_cycle = NULL){
  exp_log <- get_experiment_log(obj)
  exp_log$pickup_order <- 1:nrow(exp_log) #for time getting
  obj_log <- exp_log[exp_log$ObjectName == object_name, ]
  if(nrow(obj_log) > 1){
    if(is.null(i_cycle)){
      warning("There were ", nrow(obj_log), "items ", object_name, " picked up. Specify i_cycle to select one")
      return(NULL)
    }
    obj_log <- obj_log[obj_log$TestCycle == i_cycle, ]
    if(nrow(obj_log)!=1){
      warning("There were ", nrow(obj_log), "items ", object_name, " picked up in the cycle. Author needs to fix this function." )
      return(NULL)
    }
  }
  # TODO - this needs to reflect tasks whcih are first in each cycle, not jsut first task.- This information is in the result log
  if(obj_log$pickup_order == 1){
    start_time <- get_log(obj)$timestamp[1]
  } else {
    start_time <- exp_log$Time[obj_log$pickup_order-1] #previous item pickup time
  }
  end_time <- obj_log$Time
  pos <- get_position_timewindow(obj, start_time, end_time)
  return(pos)
}

#' Returns position during trial
#'
#' @param obj supermarket object
#' @param i_trial
#'
#' @return navr position object
#' @export
#'
#' @examples
get_position_trial <- function(obj, i_trial){
  exp_log <- get_experiment_log(obj)
  i_rows <- which(exp_log$TestCycle == i_trial)
  if(i_trial == 1){
    start_time <- get_log(obj)$timestamp[1]
  } else {
    start_time <- exp_log$Time[i_rows[1] - 1]
  }
  end_time <- exp_log$Time[tail(i_rows, 1)]
  pos <- get_position_timewindow(obj, start_time, end_time)
  return(pos)
}
