#' unlike setdiff and intersect, returns proper count for differences
#' @noRd
setdiff_unique <- function(wanted, received){
  # https://stackoverflow.com/questions/52941312/set-difference-between-two-vectors-with-duplicate-values
  res <- wanted[-match( make.unique(as.character(received)), make.unique(as.character(wanted)), nomatch=0)]
  return(res)
}
#' @noRd
intersect_unique <- function(wanted, received){
  res <- wanted[match( make.unique(as.character(received)), make.unique(as.character(wanted)), nomatch=0)]
  return(res)
}
