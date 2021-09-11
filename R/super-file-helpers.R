#' Clears out all test logs from given folder
#'
#' @param folder folder which to clean test logs from
#'
#' @export
#'
#' @examples
clear_test_logs <- function(folder){
  # find all test logs
  pth_test <- list.files(folder, pattern = "_test_",
             full.names = TRUE)
  # check if empty
  # remove
  unlink(pth_test)
}

#' Convets action logs to test logs. The outcome is the version 5
#' of the supermarket data due these files originating in the pre version 5 era
#'
#' @param folder folder to edit
#'
#' @export
#'
#' @examples
rename_action_to_test_logs <- function(folder){
  # find all actions logs

  pth_actions <- list.files(folder, pattern = "_actions_",
             full.names = TRUE)
  if(length(pth_actions) <= 0) return()
  for(pth in pth_actions){
    newName <- gsub("supermarket_actions", "test_supermarket-actions", pth)
    file.rename(pth, newName)
  }
}
