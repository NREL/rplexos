.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rplexos <- list(
    rplexos.tiebreak = "last",
    rplexos.debug = FALSE
  )
  toset <- !(names(op.rplexos) %in% names(op))
  if(any(toset)) options(op.rplexos[toset])

  invisible()
}
