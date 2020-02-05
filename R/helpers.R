#' unlike setdiff and intersect, returns proper count for differences
#' @noRd
setdiff_unique <- function(wanted, received, nomatch = wanted){
  # https://stackoverflow.com/questions/52941312/set-difference-between-two-vectors-with-duplicate-values
  i_matching <- (-match(make.unique(as.character(received)), make.unique(as.character(wanted)), nomatch = 0))
  if(all(i_matching == 0)){
    res <- nomatch
  } else {
    res <- wanted[i_matching]
  }
  return(res)
}
#' @noRd
intersect_unique <- function(wanted, received){
  res <- wanted[match( make.unique(as.character(received)), make.unique(as.character(wanted)), nomatch = 0)]
  return(res)
}
