##' Calculates results for each tríal
#'
#' @param obj supermarket experiment loaded with load_supermarket_experiment. Needs to have tasklist attached
#' @param i_trial which trial number as it stands in the tasklist and "TestCycle"
#'
#' @return
#' @export
#'
#' @examples
supermarket_performance_trial <- function(obj, i_trial){
  ## DO trial number validations
  exp_data <- obj$data$experiment_log$data
  wanted_items <- get_trial_wanted_items(obj, i_trial)
  collected_items <- exp_data$ObjectName[exp_data$TestCycle == i_trial & exp_data$Action == "pickup"]
  dropped_items <- exp_data$ObjectName[exp_data$TestCycle == i_trial & exp_data$Action == "drop"]
  # removes dropped items from collected items
  if(length(dropped_items) > 0) collected_items <- setdiff_unique(collected_items, dropped_items)

  ls <- list(trial = i_trial, n_items = length(wanted_items))
  ls_items <- item_results(wanted_items, collected_items)
  ls <- c(ls, ls_items)

  # Calculates category results
  ls_categories <- category_results(wanted_items, collected_items)
  ls <- c(ls, ls_categories)

  # calculates how many extra items were requested in previous trials
  ls_items_previous <- results_from_previous(ls$extra_items, i_trial, obj)
  ls <- c(ls, ls_items_previous)
  return(ls)
}

#' Calculates results for each tríal
#'
#' @param obj supermarket experiment loaded with load_supermarket_experiment. Needs to have tasklist attached
#'
#' @return
#' @export
#'
#' @examples
supermarket_performance_all <- function(obj){
  df_results <- create_results_table()
  exp_data <- obj$data$experiment_log$data
  for(i_trial in unique(exp_data$TestCycle)){
    results <- supermarket_performance_trial(obj, i_trial)
    df_results <- rbind(df_results, results, stringsAsFactors = F)
  }
  return(df_results)
}

### HELPERS -----
convert_items_to_categories <- function(items){
  categories <- unlist(sapply(items, item_category))
  return(categories)
}

# Calculates item results
item_results <- function(wanted_items, collected_items){
  ls <- list()
  ## This DOESN'T work if the same object can be picked up multiple times
  ls$correct_items <- intersect_unique(wanted_items, collected_items)
  ls$missing_items <- setdiff_unique(wanted_items, collected_items, nomatch = wanted_items)
  ls$extra_items <- setdiff_unique(collected_items, wanted_items, nomatch = collected_items)
  ls <- add_field_lengths(ls, c("missing_items", "correct_items", "extra_items"))
  ls <- collapse_fields(ls, c("missing_items", "correct_items", "extra_items"))
  return(ls)
}
# Calculates item results but for category fields
category_results <- function(wanted_items, collected_items){
  ls <- list()

  wanted_categories <- convert_items_to_categories(wanted_items)
  collected_categories <- convert_items_to_categories(collected_items)

  wanted_counts <- reshape2::melt(table(wanted_categories), factorsAsStrings = TRUE) #potentially strings as factors issue
  wanted_counts$wanted_categories <- as.character(wanted_counts$wanted_categories)
  collected_counts <- reshape2::melt(table(collected_categories), factorsAsStrings = TRUE)
  collected_counts$collected_categories <- as.character(collected_counts$collected_categories)

  df_comparing <- merge(wanted_counts, collected_counts,
                        by.x = "wanted_categories", by.y = "collected_categories",
                        all = TRUE)
  colnames(df_comparing) <- c("category", "wanted", "collected")
  df_comparing[is.na(df_comparing)] <- 0
  df_comparing$difference <- df_comparing$wanted - df_comparing$collected

  missing <- df_comparing$difference > 0
  extra <- df_comparing$difference < 0
  correct <- df_comparing$difference >= 0
  ls$missing_categories <- rep(as.character(df_comparing$category[missing]), df_comparing$difference[missing])
  ls$extra_categories <- rep(as.character(df_comparing$category[extra]), -df_comparing$difference[extra])
  ls$correct_categories <-  rep(as.character(df_comparing$category[correct]), df_comparing$collected[correct])
  ls <- add_field_lengths(ls, c("missing_categories", "extra_categories", "correct_categories"))
  ls <- collapse_fields(ls, c("missing_categories", "extra_categories", "correct_categories"))
  return(ls)
}

results_from_previous <- function(extra_items, i_trial, obj){
  if(i_trial == 1) {
    ls <- list(last_trial_items = character(0), all_previous_items = character(0))
  } else {
    all_previous_wanted_items <- get_trial_wanted_items(obj, 1:i_trial-1)
    last_trial_wanted_items <- get_trial_wanted_items(obj, i_trial - 1)
    ls <- list()
    ls$last_trial_items <- intersect_unique(extra_items, last_trial_wanted_items)
    ls$all_previous_items <- intersect_unique(extra_items, all_previous_wanted_items)
  }
  ls <- add_field_lengths(ls, c('last_trial_items', "all_previous_items"))
  ls <- collapse_fields(ls, c('last_trial_items', "all_previous_items"))
  return(ls)
}

# Calculates lengths of vectors and saves to new fields
add_field_lengths <- function(ls, fields){
  for(field in fields){
    field_name <- paste("n", field, sep = "_")
    ls[[field_name]] <- length(ls[[field]])
  }
  return(ls)
}

#collapses collected items or categories into a single vector
collapse_fields <- function(ls, fields){
  for(field in fields){
    ls[[field]] <- paste0(ls[[field]], collapse = ",")
  }
  return(ls)
}

# Returns item category from a lookup table
item_category <- function(item_code){
  return(item_categories$Category[item_categories$ID == item_code])
}

create_results_table <- function(){
  df_results <- data.frame(trial = numeric,
                           n_items = numeric(0),
                           n_correct_items = numeric(0),
                           n_missing_items = numeric(0),
                           n_extra_items = numeric(0),
                           correct_items = character(0),
                           missing_items = character(0),
                           extra_items = character(0),
                           correct_categories = character(0),
                           n_correct_categories = numeric(0),
                           extra_categories = character(0),
                           n_extra_categories = numeric(0),
                           missing_categories = character(0),
                           n_missing_categories = numeric(0),
                           n_last_trial_items = numeric(0),
                           last_trial_items = character(0),
                           n_all_previous_items = numeric(0),
                           all_previous_items = character(0),
                           stringsAsFactors = F)
  return(df_results)
}
