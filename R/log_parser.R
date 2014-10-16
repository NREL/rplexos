# This file contains the functions that parse the log file

# Take log and return processed results
plexos_log_parser <- function(txt) {
  list(log_info  = log_info(txt),
       log_steps = log_steps(txt))
}

# Look for a phase in the log output and extract time (and infeasibilities)
#' @importFrom stringi stri_extract_first_regex stri_opts_regex
get_time <- function(pattern, txt, add.inf = FALSE) {
  dotall <- stri_opts_regex(dotall = TRUE)
  
  if (add.inf) {
    pat <- paste0(pattern, ".*?(?:\n|\r\n)", ".*?",
                  "Infeasibilities", ".*?(?:\n|\r\n)")
  } else {
    pat <- paste0(pattern, ".*?(?:\n|\\r\n)")
  }
  
  chunk <- stri_extract_first_regex(txt, pat, dotall)
  
  data.frame(phase        = gsub(" Completed", "", pattern),
             time         = extract_number(chunk, pattern),
             rel_gap_perc = extract_number(chunk, "Relative Gap"),
             infeas       = extract_number(chunk, "Infeasibilities"),
             stringsAsFactors = FALSE)
}

# Find the line of text that includes 'pattern' and extract the number
#' @importFrom stringi stri_extract_first_regex stri_detect stri_split_fixed
extract_number <- function(txt, pattern) {
  pat <- paste0(pattern, ".*")
  line <- stri_extract_first_regex(txt, pat)
  line.sp <- stri_split_fixed(line, " ")[[1]]
  line.sp[stri_detect(line.sp, regex = "[0-9]")]
}

# Get summary for each step
log_info <- function(txt) {
  rbind_list(get_time("Primary Compilation Completed", txt),
             get_time("Secondary Compilation Completed", txt),
             get_time("PASA Completed", txt, TRUE),
             get_time("MT Schedule Completed", txt, TRUE),
             get_time("ST Schedule Completed", txt, TRUE))

}

# Get duration of steps
#' @importFrom stringi stri_opts_regex stri_extract_all_regex stri_replace_all_regex stri_split_regex
log_steps <- function(txt) {
  dotall <- stri_opts_regex(dotall = TRUE)
  caseins <- stri_opts_regex(case_insensitive = TRUE)
  
  steps <- stri_extract_all_regex(txt,
                                  "Completed .*? Step +[0-9]+ of [0-9]+.*?(?:\n|\r\n)",
                                  dotall)
  steps2 <- stri_replace_all_regex(steps[[1]], "Completed |\r\n|\n", "")
  steps3 <- stri_split_regex(steps2, " step| of |time: |elapsed: ", n_max = 5, opts_regex = caseins)

  steps4 <- do.call("rbind", steps3)
  steps4 <- data.frame(steps4, stringsAsFactors = FALSE)
  names(steps4) <- c("phase", "step", "total_step", "time", "elapsed")

  steps4$step       <- as.numeric(steps4$step)
  steps4$total_step <- as.numeric(steps4$total_step)
  steps4$time       <- sub(" .$|. $|.$", "", steps4$time) %>% to_seconds
  steps4$elapsed    <- sub(" .$|. $|.$", "", steps4$elapsed) %>% to_seconds
  steps4
}

# Convert an HH:MM:SS to seconds
#' @importFrom stringi stri_split_fixed
to_seconds <- function(x) {
  x2 <- stri_split_fixed(x, ":", 3) %>%
    do.call("rbind", .) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    sapply(as.numeric)
  3600 * x2[, 1] + 60 * x2[, 2] + x2[, 3]
}
