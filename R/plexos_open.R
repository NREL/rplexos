#' Open all PLEXOS databases
#' 
#' The default for \code{folders} is the working directory. If the wildcard \code{"*"} is provided, all
#' the folders in the working directory will be processed (the list of folders if provided by
#' the \code{\link{list_folders}} function).
#' 
#' @param folders character. Folder(s) where the data is located (each folder represents a scenario)
#' @param names character. Scenario names
#'
#' @seealso \code{\link{plexos_close}}
#' @seealso \code{\link{query_master}}
#' 
#' @export
plexos_open <- function(folders = ".", names = folders) {
  # Check inputs
  assert_that(is.character(folders), is.character(names), is_folder(folders))
  
  # Check for wildcard
  if (length(folders) == 1) {
    if (folders == "*") {
      folders <- list_folders()
      names <- folders
    }
  }
  
  # Check that folder and names have the same length
  assert_that(length(folders) == length(names))
  
  # Change default scenario name to something better than '.'
  if (length(folders) == 1) {
    if ((folders == ".") & (names == ".")) {
      names <- "default"
    }
  }
  
  # Function to list PLEXOS files in each folder
  plexos_list_files <- function(df) {
    filename <- list.files(df$folder %>% as.character,
                           pattern = ".db$", full.names = TRUE)
    filename <- filename[!grepl("temp.db$", filename)]
    
    if (length(filename) == 0L)
      return(data.frame())

    data.frame(folder = df$folder,
               scenario = df$scenario,
               filename,
               stringsAsFactors = FALSE)
  }
  
  # Get database file names
  df <- data.frame(folder = folders,
                   scenario = factor(names, levels = names)) %>%
        rowwise() %>%
        do(plexos_list_files(.))
  
  # Error if all folders were empty
  if (nrow(df) == 0L)
    stop("No databases found in the list of folders.\n",
         "Did you forget to use process_folder()?",
         call. = FALSE)
  
  # Check for folders without databases
  folder.missing <- setdiff(folders, df$folder)
  if (length(folder.missing) > 0L) {
    warning("No databases found in folder",
            ifelse(length(folder.missing) == 1L, ": ", "s: "),
            paste(folder.missing, collapse = ", "),
            call. = FALSE)
  }
  
  # Open databases
  out <- df %>%
    ungroup() %>%
    mutate(position = 1:n()) %>%
    group_by(scenario, position, filename) %>%
    do(db = src_sqlite(as.character(.$filename)))
  
  # Add rplexos class to object
  class(out) <- c("rplexos", class(out))
  
  # Check the version of rplexos
  conf <- query_config(out)
  this.vers <- packageVersion("rplexos") %>% as.character
  if (!"rplexos" %in% names(conf)) {
    # rplexos is not even an entry in the config table
    warning("File(s) processed with an old version of rplexos. ",
            "Rerun process_folder() to avoid problems.",
            call. = FALSE)
  } else {
    # Compare
    comp <- integer(length(conf$rplexos))
    for (i in 1:length(conf$rplexos))
      comp[i] <- compareVersion(this.vers, conf$rplexos[i])
    
    if (any(comp == -1)) {
      warning("File(s) processed with a newer version of rplexos. ",
              "Update rplexos or rerun process_folder() to avoid problems.",
              call. = FALSE)
    } else if (any(comp == 1)) {
      warning("File(s) processed with an old version of rplexos. ",
              "Rerun process_folder() to avoid problems.",
              call. = FALSE)
    }
  }
  
  out
}

#' Close all PLEXOS databases
#'
#' Close all the open connections to PLEXOS SQLite databases. This function
#' completely erases the provided object.
#'
#' @param db PLEXOS database object
#'
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#'
#' @export
plexos_close <- function(db) {
  assert_that(is.rplexos(db))

  # For each database, close the connection
  db %>%
    do(result = dbDisconnect(.$db$con))
  
  # Remove object from memory
  rm(list = deparse(substitute(db)), envir = sys.frame(-1))
  
  TRUE
}

# Create custom summary for rplexos objects
#' @export
#' @method summary rplexos
summary.rplexos <- function(object, ...) {
  info <- object %>%
    group_by(position, scenario, filename) %>%
    summarise(tables = length(src_tbls(db[[1]]))) %>%
    as.data.frame
  
  print(info, row.names = FALSE)
}

# Create custom visualization for rplexos objects
#' @export
#' @importFrom reshape2 dcast
print.rplexos <- function(x, ...) {
  cat("Structure:\n")
  summary(x)
  
  cat("\nTables:\n")
  info <- x %>%
    group_by(position) %>%
    do(data.frame(table = src_tbls(.$db[[1]])))
  
  print(dcast(info, table ~ position, fun.aggregate = length, value.var = "table"),
        row.names = FALSE)
}

# Custom ungroup method, to preserve 'rplexos' class
ungroup.rplexos <- function(x) {
  class(x) <- setdiff(class(x), c("grouped_df", "rowwise_df"))
  x
}

# Avoid group_by_.rowwise_df warning
group_by_.rplexos <- function(.data, ...) {
  class(.data) <- setdiff(class(.data), c("rplexos", "rowwise_df"))
  group_by_(.data, ...)
}
