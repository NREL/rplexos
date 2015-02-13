.onLoad <- function(libname, pkgname) {
  # Add a couple of default options to determine tiebreaks and debug mode
  op <- options()
  op.rplexos <- list(
    rplexos.tiebreak = "last",
    rplexos.debug = FALSE
  )
  toset <- !(names(op.rplexos) %in% names(op))
  if(any(toset)) options(op.rplexos[toset])

  invisible()
}
