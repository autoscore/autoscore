#' compound_fixer
#'
#' This function uses a default list of compound words (common to the SIT) and replaces the response where appropriate to match the right usage.
#'
#' @param response the response variable
#' @param comp a named vector of compounds (e.g., \code{c("rose bush" = "rosebush")} where the response is "rosebush" and the desired change is "rose bush").
#'
#' @import stringr
#'
#' @examples
#'
#' response = c("rose bush", "junkyard", "door man", "cracker jack")
#' compound_fixer(
#'   response,
#'   comp = c(
#'     "junk yard" = "junkyard",
#'     "crackerjack" = "cracker jack"
#' ))
#'
#' @export
compound_fixer <- function(response, comp){
  # do custom list first
  if (!is.null(comp)){
    for (i in seq_along(comp)){
      call1 <- call("str_replace", quote(response), unname(comp[i]), names(comp[i]))
      response <- eval(call1)
    }
  }

  response
}

