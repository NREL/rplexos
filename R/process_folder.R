#' Convert PLEXOS files to SQLite databases
#'
#' Functions to process PLEXOS solution and input files and dump them into SQLite databases
#' that are easier to read.
#'
#' \code{process_solution} converts a single zipped PLEXOS solution into a SQLite database.
#' The resulting database file has a simplified structure so data can be retrieved easily.
#' The temporary database contains the original data structure in the PLEXOS solution file.
#'
#' \code{process_input} converts a single PLEXOS input file (in XML form) solution into a
#' SQLite database. The database includes the raw tables and a series of views that better
#' organize the data.
#'
#' \code{process_folders} is used to process one or more folders. It searches each folder for
#' PLEXOS solution or input files and applies \code{process_solution} or
#' \code{process_input} to each one of them.
#'
#' The default folder is the working directory. If the wildcard \code{"*"} is provided, all
#' the folders in the working directory will be processed (the list of folders if provided by
#' the \code{\link{list_folders}} function).
#'
#' Do not rename the SQLite databases created with these functions. Other code expects
#' those filenames to remain unchanged.
#' 
#' @param folders Folder(s) to process (See details)
#' @param file Single PLEXOS solution or input file to process
#' @param keep.temp Should temporary databases be preserved?
#'
#' @examples
#' \dontrun{process_folder()}
#' \dontrun{process_folder("HiWind")}
#' \dontrun{process_solution("HiWind/Model WWSIS_c_RT_CoreB_M01_SC3 Solution.zip")}
#' \dontrun{process_input("WWSIS model.xml")}
#'
#' @export
process_folder <- function(folders = ".", keep.temp = FALSE) {
  # Check inputs
  assert_that(is.character(folders), is.flag(keep.temp), is_folder(folders))
  
  # Check for wildcard
  if (length(folders) == 1L) {
    if (folders == "*") {
      folders <- list_folders()
    }
  }
  
  # Check that folders exist
  are.dirs <- file.info(folders)$isdir
  are.dirs[is.na(are.dirs)] <- FALSE
  if(!all(are.dirs)) {
    not.dirs <- folders[!are.dirs]
    stop(paste(not.dirs, collapse = ", "), " are not valid folders", call. = FALSE)
  }
  
  # Iterate through list of folders
  for (folder in folders) {
    message("Folder: '", folder, "'")
    
    # Read list of potential PLEXOS solution and input files
    zip.files <- list.files(folder, ".zip$|.ZIP$", full.names = TRUE)
    xml.files <- list.files(folder, ".xml$|.XML$", full.names = TRUE)
    
    # If no ZIP or XML files found, skip to the next folder
    if (length(zip.files) + length(xml.files) == 0L) {
      message("  - No solutions found, skipping folder")
      next
    }
    
    # Process each file
    vapply(zip.files, process_solution, "", keep.temp)
    vapply(xml.files, process_input, "")
    
    message("  - Finished folder!")
  }
  
  invisible(TRUE)
}
