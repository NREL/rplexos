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
#' @param silent Print status of parallel queries at the end
#'
#' @examples
#' \dontrun{start_parallel_rplexos(3)}
#' @export
start_parallel_rplexos <- function(ncores = 1, silent = FALSE) {
  # Check inputs
  assert_that(is.count(ncores), ncores >= 1)
  cluster <- get("cluster", rplexos_globals)
  
  # If one cluster is selected, turn of parallel capabilities
  if (ncores == 1) {
    if (!is.null(cluster)) {
      parallel::stopCluster(cluster)
      assign("cluster", NULL, rplexos_globals)
    }
  } else {
    # Make sure you don't start more cores that available
    max.cores <- parallel::detectCores()
    if(ncores > (max.cores - 1))
      ncores <- max.cores - 1
    
    # Create cluster with desired number of cores
    cluster <- parallel::makeCluster(ncores)
    assign("cluster", cluster, rplexos_globals)
    
    # Register cluster
    doParallel::registerDoParallel(cluster)
  }
  
  if (!silent)
    check_parallel_rplexos()
  
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
  cluster <- get("cluster", rplexos_globals)
  
  if(is.null(cluster)) {
    n.cluster <- 1
    cat("Parallel queries are disabled\n")
  } else {
    n.cluster <- foreach::getDoParWorkers()
    cat("Parallel queries enabled with", n.cluster, "threads\n")
  }
  
  return(invisible(n.cluster))
}

# Function to do parallel calculations, if needed
select_do <- function() {
  cluster <- get("cluster", rplexos_globals)
  
  if (is.null(cluster)) {
    out <- foreach::`%do%`
  } else {
    out <- foreach::`%dopar%`
  }
  
  out
}
