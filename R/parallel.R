# Variable to hold cluster info and function to do parallelization
.rplexos.cluster <- NULL
`%dp%` <- foreach::`%do%`

#' Enable or disable parallel queries
#'
#' Multiple solutions can be queried in parallel to improve performace.
#'
#' The \code{start_parallel_rplexos} allows the user to set the number of cores
#' to use when querying in parallel.
#'
#' If the number of cores is set to 1 (the default), parallel queries are disables.
#'
#' \code{check_parallel_plexos} shows whether parallel queries are currently enabled
#' and the number of cores being used.
#'
#' @param ncores Number of cores to use (defaults to 1)
#'
#' @examples
#' \dontrun{start_parallel_rplexos(3)}
#' @export
start_parallel_rplexos <- function(ncores = 1) {
  # Check inputs
  assert_that(is.count(ncores), ncores >= 1)
  
  # If one cluster is selected, turn of parallel capabilities
  if (ncores == 1) {
    if (!is.null(.rplexos.cluster)) {
      parallel::stopCluster(.rplexos.cluster)
      .rplexos.cluster <<- NULL
      `%dp%` <<- foreach::`%do%`
    }
    
    return(invisible(1))
  }
  
  # Make sure you don't start more cores that available
  max.cores <- parallel::detectCores()
  if(ncores > max.cores)
    ncores <- max.cores
  
  # Create cluster with desired number of cores
  .rplexos.cluster <<- parallel::makeCluster(ncores)
  
  # Register cluster
  doParallel::registerDoParallel(.rplexos.cluster)
  `%dp%` <<- foreach::`%dopar%`
  
  invisible(ncores)
}


#' @rdname start_parallel_rplexos
#' @export
stop_parallel_rplexos <- function() {
  start_parallel_rplexos(1)
}

#' @rdname start_parallel_rplexos
#' @export
check_parallel_rplexos <- function() {
  if(is.null(.rplexos.cluster)) {
    cat("Parallel queries are disabled")
    return(invisible(1))
  }
  
  n.cluster <- foreach::getDoParWorkers()
  cat("Parallel queries enabled with", n.cluster, "threads")
    
  return(invisible(n.cluster))
}
