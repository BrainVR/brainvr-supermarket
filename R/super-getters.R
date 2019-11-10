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
  results_trial <- obj$data$results_log$data[i_trial, ]
  return(list(waitingToStart = results_trial$TimeStarted,
              start = results_trial$TimeStarted,
              end = results_trial$TimeFinished))
}

#' Returns list of correct items from a trial
#'
#' @description this function became necessary when the entire structure of the task changed and
#' suddenly the list of items is no longer serialised and is generated but never noted what those
#' items are.
#'
#' This function requires both actions and results log to be present because it needs both to
#' "deduce" which items were supposed to be collected. In case the tasklist is present, the items
#' are collected from the tasklist instead
#'
#' @param obj
#' @param i_trial
#'
#' @return
#' @export
#'
#' @examples
get_trial_wanted_items <- function(obj, i_trial){
  if(!is.null(obj$tasklist)) return(obj$tasklist$item[obj$tasklist$trial == i_trial])
  ## needs both experiment_log and results log
  exp_log <- get_experiment_log(obj)
  res_log <- get_results_log(obj)
  if(any(is.null(exp_log), is.null(res_log))){
    warning("cannot return wanted items without both experiment and results logs")
    return(NULL)
  }
  collected_correct_items <- exp_log[exp_log$TestCycle == i_trial & exp_log$RightWrong, "ObjectName" ]
  non_collected_items <- get_trial_missing_items(obj, i_trial)
  wanted_items <- c(collected_correct_items, non_collected_items)
  return(wanted_items)
}

get_trial_missing_items <- function(obj, i_trial){
  exp_log <- get_experiment_log(obj)
  res_log <- get_results_log(obj)
  task_items <- exp_log[exp_log$TestCycle == i_trial, "TaskItems"][1]
  #' THIS IS IMPORTANT - There is an error in the results log where the MissingItemsList and
  #' Additional ItemsList are flipped. So that is why this is here
  missing_items <- res_log[res_log$TaskItems == task_items, "AdditionalItemsList", ]
  missing_items <- gsub("[()]","", missing_items)
  missing_items <- strsplit(missing_items, ",")[[1]]
  return(missing_items)
}
