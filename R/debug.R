# Custom function to print debug messages
rplexos_message <- function(...) {
  if (is_debug_rplexos()) {
    message("*** rplexos debug: ", ...)
  }
}

#' Enable or disable debug mode
#'
#' The debug mode will print progress on screen and additional information to 
#' help diagnose problems.
#'
#' @export
start_debug_rplexos <- function() {
  options(rplexos.debug = TRUE)
  check_debug_rplexos()
}

#' @rdname start_debug_rplexos
#' @export
stop_debug_rplexos <- function() {
  options(rplexos.debug = FALSE)
  check_debug_rplexos()
}

#' @rdname start_debug_rplexos
#' @export
check_debug_rplexos <- function() {
  out <- is_debug_rplexos()
  
  if (out) {
    cat("rplexos debug mode is enabled\n")
  } else {
    cat("rplexos debug mode is disabled\n")
  }
  
  invisible(out)
}

#' @rdname start_debug_rplexos
#' @export
is_debug_rplexos <- function() {
  getOption("rplexos.debug", FALSE)
}
