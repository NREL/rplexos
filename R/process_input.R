#' @rdname process_folder
#' 
#' @useDynLib rplexos
#' @export
process_input <- function(file) {
  # Check that inputs are valid
  assert_that(is.string(file))
  
  # Check that file exists
  if (!file.exists(file)) {
    warning(file, " does not exist and was ignored.", call. = FALSE, immediate. = TRUE)
    return(invisible(""))
  }

  # Database name will match that of the zip file
  db.name <- gsub(".xml|.XML", "-input.db", file)
  
  # Delete old file, if possible
  if (file.exists(db.name)) {
    stop_ifnot_delete(db.name)
  }
  
  # Read content from the XML file
  read.con <- file(file, open = "r")
  xml.content.temp <- NULL
  try(xml.content.temp <- readLines(read.con, warn = FALSE))
  if (is.null(xml.content.temp)) {
    stop("Error reading XML file into memory", call. = FALSE)
  }
  xml.content <- paste(xml.content.temp, collapse = " ")
  close(read.con)
  
  # Check that XML is a valid PLEXOS file
  plexos.check <- grep("MasterDataSet", xml.content)
  if (length(plexos.check) == 0L) {
    rplexos_message("Invalid XML content in ", file)
    warning(file, " is not a PLEXOS input file and was ignored.", call. = FALSE, immediate. = TRUE)
    return(invisible(""))
  }
  
  # Create an empty database and add the XML information
  message("  - Input: '", file, "'")
  
  # Open connection to SQLite for R
  dbf <- src_sqlite(db.name, create = TRUE)
  
  # Add basic XML structure and delete cached XML file
  new_database(dbf, xml.content, is.solution = FALSE)
  rm(xml.content)
  
  # Close database connections
  dbDisconnect(dbf$con)
  
  # Message that file processing is done
  rplexos_message("Finished processing file ", file, "\n")
  
  # Return the name of the database that was created
  invisible(db.name)
}
