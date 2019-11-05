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
get_pickup_position <- function(obj, object_name = NULL, i_cycle = NULL){
  exp_log <- get_experiment_log(obj)
  exp_log$pickup_order <- 1:nrow(exp_log) #for time getting
  exp_log$trial_pickup_order <- unlist(sapply(rle(exp_log$TestCycle)$lengths, function(x){1:x}))
  obj_log <- exp_log[exp_log$ObjectName == object_name, ]
  if(nrow(obj_log) > 1){
    if(is.null(i_cycle)){
      warning("There were ", nrow(obj_log), "items ", object_name, " picked up. Specify i_cycle to select one")
      return(NULL)
    }
    obj_log <- obj_log[obj_log$TestCycle == i_cycle, ]
    if(nrow(obj_log) != 1){
      warning("There were ", nrow(obj_log), "items ", object_name, " picked up in the cycle. Author needs to fix this function." )
      return(NULL)
    }
  }
  if(obj_log$trial_pickup_order == 1){
    #if it is the first item, it considers start the TimeStarted column from results log
    start_time <- obj$data$results_log$data$TimeStarted[obj_log$TestCycle]
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
#' @importFrom brainvr.reader get_trial_times
#' @return navr position object
#' @export
#'
#' @examples
get_trial_position.supermarket <- function(obj, i_trial){
  timewindow <- get_trial_times.supermarket(obj, i_trial)
  pos <- get_position_timewindow(obj, timewindow$start, timewindow$end)
  return(pos)
}

#' Returns trial times
#'
#' @param obj
#' @param i_trial
#'
#' @importFrom brainvr.reader get_trial_times
#'
#' @return
#' @export
#'
#' @examples
get_trial_times.supermarket <- function(obj, i_trial){
  results_trial <- obj$data$results_log[i_trial, ]
  return(list(waitingToStart = results_trial$TimeStarted,
              start = results_trial$TimeStarted,
              end = results_trial$TimeFinished))
}
