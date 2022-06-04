error_check_alternate_df <- function(alternate_df){
  if (!is.null(alternate_df)){
    stopifnot(is.data.frame(alternate_df) | is.matrix(alternate_df))
  }
}


error_check_rules <- function(...){
  rules <- list(...)

  for (i in seq_along(rules)){
    if (!is.logical(rules[[i]])){
      stop(paste(names(rules)[i], "must be either TRUE or FALSE"), call. = FALSE)
    }
  }
}
