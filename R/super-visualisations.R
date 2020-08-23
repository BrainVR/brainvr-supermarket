#' Plots position data and adds background
#'
#' @param obj navr object filtered from the supermarket data position
#' @param background do you want to add background to the path
#' @param background_type if you want installed background (categories, plain)
#' @param custom_background path to your custom background
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_supermarket_path <- function(obj, background = FALSE, background_type = "categories",
                                  custom_background = NULL, ...) {
  g <- ggplot() +
    geom_navr_limits(obj) ## this is just to set the generic
  if (background) {
    if (background_type == "categories") background_path <- BACKGROUND_CATEGORIES_PATH
    # if (background_type == "plain") background_path <- PLAIN_CATEGORIES_PATH
    if (!is.null(custom_background)) background_path <- custom_background
    g <- g + geom_navr_background(background_path,
                                  xlim = AREA_BOUNDARIES$x,
                                  ylim = AREA_BOUNDARIES$y)
  }
  g <- g + geom_navr_path(obj, ...) + theme_void()
  return(g)
}
