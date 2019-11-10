#' Item categories encoding table
#'
#' This dataframe serves the purpose to convert between language versions
#' as well as codument categorization of individual objects
#'
#' Language options at this point include "CZ" and "EN"
#'
#' @format A data.frame with 212 items and 5 variables:
#' \describe {
#'    \item{ID}{ID of the item. In english, but ultimately convertable}
#'    \item{CZ}{Czech name of the item. UTF-8 encoded}
#'    \item{EN}{English common name of the item. uTF-8 encoded}
#'    \item{CZR}{}
#'    \item{Category}{name of the category ofg the item in english}
#' }
"item_categories"
