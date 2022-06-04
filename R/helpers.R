## Infix operator (null-default)
`%||%` <- purrr::`%||%`


#' re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

## From tidyverse package
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

autoscore_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])
  crayon::italic(paste0(version, collapse = "."))
}

search_conflicts <- function(path = search()){
  ## Search for conflicts
  confs <- conflicts(path,TRUE)
  ## Grab those with the autoscore package
  autoscore_conflicts <- confs$`package:autoscore`

  ## Find which packages have those functions that are conflicted
  if (length(autoscore_conflicts) != 0){
    other_conflicts <- list()
    for (i in autoscore_conflicts){
      other_conflicts[[i]] <- lapply(confs, function(x) any(grepl(i, x))) %>%
        do.call("rbind", .) %>%
        data.frame %>%
        setNames(c("conflicted")) %>%
        tibble::rownames_to_column() %>%
        .[.$conflicted == TRUE &
            .$rowname != "package:autoscore",]
    }
  } else {
    other_conflicts <- data.frame()
  }
  other_conflicts
}
