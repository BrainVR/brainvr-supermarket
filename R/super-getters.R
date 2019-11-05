get_position_data_object <- function(obj, object_name = NULL, i_cycle = NULL, i_task = NULL){
  exp_log <- get_experiment_log(obj)
  if(!is.null(object_name)){
    i_row <- which(exp_log$ObjectName == object_name)
  } else {
    i_row <- which(exp_log$TestCycle == i_cycle)[i_task]
  }
  if(i_row == 1){
    start_time <- get_log(obj)$timestamp[1]
  } else {
    start_time <- exp_log$Time[i_row-1]
  }
  end_time <- exp_log$Time[i_row]
  pos <- get_position_timewindow(obj, start_time, end_time)
  return(pos)
}

get_task_position <- function(obj, task_items = NULL){
  exp_log <- get_experiment_log(obj)
  i_rows <- which(exp_log$TaskItems == task_items)
  if(task_items == 3){
    start_time <- get_log(obj)$timestamp[1]
  } else {
    start_time <- exp_log$Time[i_rows[1] - 1]
  }
  end_time <- exp_log$Time[tail(i_rows, 1)]
  pos <- get_position_timewindow(obj, start_time, end_time)
  return(pos)
}
