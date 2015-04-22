# Clean spaces and special characters from strings
clean_string <- function(x) {
  gsub(" |&|'|-|\\.", "", x)
}

# Delete file and give error if unsuccesfull
stop_ifnot_delete <- function(x) {
  # Error if file cannot be removed
  suppressWarnings(did.remove <- file.remove(x))
  if (!did.remove)
    stop("Unable to delete file: ", x, call. = FALSE)
}

# Regroup with characters
group_by_char <- function(x, vars) {
  dots <- vars %>%
    as.list %>%
    lapply(as.symbol)
  group_by_(x, .dots = dots)
}

#' Get list of valid columns
#'
#' List of valid columns accepted in \code{\link{query_master}}, \code{\link{sum_master}} and related functions.
#'
#' @seealso \code{\link{query_master}}, \code{\link{sum_master}}
#'
#' @export
valid_columns <- function() c("collection", "property", "name", "parent", "category", "region", "zone",
                              "period_type_id", "band", "sample", "timeslice", "time")


#' Test if elements in sample column are statistics
#'
#' In stochastic simulations, PLEXOS will return sample results and their statistics together. This function
#' makes it easy to separate them with a filter.
#'
#' @param x Vector of sample values from an rplexos query
#'
#' @examples
#' \dontrun{db <- plexos_open()}
#' \dontrun{res <- query_month(db, "Generator", "Generation")}
#' \dontrun{res %>% filter(sample_stats(sample))    # To obtain statistics}
#' \dontrun{res %>% filter(!sample_stats(sample))   # To obtain sample results}
#'
#' @export
is_sample_stats <- function(x)
  x %in% c("Max", "Min", "Mean", "StDev")

#' Get list of folders in the working directory
#'
#' List of existing folders in the working directory. This function is used when the wildcard symbol (\code{"*"})
#' is provided to the \code{\link{process_folder}} and \code{\link{plexos_open}} functions.
#'
#' @seealso \code{\link{setwd}}, \code{\link{process_folder}}, \code{\link{plexos_open}}
#'
#' @export
list_folders <- function() {
  f <- dir()
  f[file.info(f)$isdir]
}


# *** assert_that validation functions ***

# Check that columns are valid
are_columns <- function(col) all(col %in% valid_columns())

on_failure(are_columns) <- function(call, env) {
  paste0("Incorrect column parameter. Use valid_columns() to get the full list.")
}

# Check that names are valid columns
names_are_columns <- function(x) are_columns(names(x))

on_failure(names_are_columns) <- function(call, env) {
  paste0("The names in ", deparse(call$x), " must correspond to correct columns. Use valid_columns() to get the full list.")
}

# Check that names are valid columns
time_not_a_name <- function(x) !"time" %in% (names(x))

on_failure(time_not_a_name) <- function(call, env) {
  paste0("time should not be an entry in ", deparse(call$x), ". Use time.range instead.")
}

# Check that object is valid
is.rplexos <- function(x) inherits(x, "rplexos")

on_failure(is.rplexos) <- function(call, env) {
  paste0(eval(call$x, env), " is not a valid database object. 'db' should be created with plexos_open().")
}

# Check date range inputs
correct_date <- function(x) all(!is.na(x))

on_failure(correct_date) <- function(call, env) {
  paste0("Could not convert time.range. Use 'ymdhms' or 'ymd' formats")
}

# Check phase inputs
correct_phase <- function(x) x %in% 1:4

on_failure(correct_phase) <- function(call, env) {
  paste0("'phase' must be one of: 1 (LT), 2 (PASA), 3 (MT) or 4 (ST)")
}

# Check time
correct_time <- function(x) x %in% c("interval", "day", "week", "month", "year")

on_failure(correct_time) <- function(call, env) {
    paste0("'time' must be one of: interval, day, week, month or year")
}

# Check that a vector of characters are folder names
is_folder <- function(x) {
  if (length(x) == 1L) {
    if(identical(x, "*")) {
      return(TRUE)
    }
  }
  all(file.exists(x)) & all(file.info(x)$isdir, na.rm = FALSE)
}

on_failure(is_folder) <- function(call, env) {
  paste0("'folders' must be a vector of existing folders or the wildcard \"*\"")
}
