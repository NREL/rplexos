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
  
  # Add a few tables that will be useful later on
  add_extra_tables_input(dbf)
  
  # Close database connections
  dbDisconnect(dbf$con)
  
  # Message that file processing is done
  rplexos_message("Finished processing file ", file, "\n")
  
  # Return the name of the database that was created
  invisible(db.name)
}

# Add a few tables that will be useful later on
add_extra_tables_input <- function(db) {
  rplexos_message("Adding extra tables to the database")
  
  # View with class and class_group
  sql <- "CREATE VIEW [class] AS
          SELECT c.class_id class_id,
                 c.name,
                 g.name class_group,
                 c.is_enabled,
                 c.description
          FROM t_class c
          INNER JOIN t_class_group g
            ON c.class_group_id = g.class_group_id"
  dbGetQuery(db$con, sql)
  
  # View with object, category, class, class_group
  sql <- "CREATE VIEW [object] AS
          SELECT o.object_id,
                 o.name name,
                 o.description,
                 cat.name category,
                 c.class_group,
                 c.name class
          FROM t_object o
          JOIN [class] c
            ON o.class_id = c.class_id
          JOIN t_category cat
            ON o.category_id = cat.category_id"
  dbGetQuery(db$con, sql)
  
  # View with property
  sql <- "CREATE VIEW [property] AS
          SELECT p.property_id,
                 p.name,
                 p.default_value,
                 p.period_type_id,
                 p.is_enabled,
                 p.is_dynamic,
                 p.is_multi_band,
                 p.max_band_id,
                 pg.name property_group,
                 c.name collection,
                 u.value unit,
                 p.description
          FROM t_property p
          JOIN t_property_group pg
            ON p.property_group_id = pg.property_group_id
          JOIN t_collection c
            ON c.collection_id = p.collection_id
         JOIN t_unit u
           ON u.unit_id = p.unit_id"
  dbGetQuery(db$con, sql)
  
  0
}