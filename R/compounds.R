#' compound_fixer
#'
#' This function uses a default list of compound words (common to the SIT) and replaces the response where appropriate to match the right usage.
#'
#' @param response the response variable
#' @param comp a named vector of compounds (e.g., \code{c("rose bush" = "rosebush")} where the response is "rose bush" and the desired change is "rosebush"). If \code{comp =NULL} then a default list is used only.
#' @param default should the default list be used? (default = TRUE)
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
compound_fixer <- function(response, comp = NULL, default = TRUE){
  # do custom list first
  if (!is.null(comp)){
    for (i in seq_along(comp)){
      call1 <- call("str_replace", quote(response), unname(comp[i]), names(comp[i]))
      response <- eval(call1)
    }
  }

  # default list
  if (isTRUE(default)){
    response <- stringr::str_replace(response, "rose bush", "rosebush") %>%
      stringr::str_replace("inkstain", "ink stain") %>%
      stringr::str_replace("sail boat", "sailboat") %>%
      stringr::str_replace("cook books", "cookbooks") %>%
      stringr::str_replace("drift wood", "driftwood") %>%
      stringr::str_replace("business men", "businessmen") %>%
      stringr::str_replace("business man", "businessman") %>%
      stringr::str_replace("rail road", "railroad") %>%
      stringr::str_replace("door man", "doorman") %>%
      stringr::str_replace("high way", "highway") %>%
      stringr::str_replace("white capped", "whitecapped") %>%
      stringr::str_replace("White capped", "Whitecapped") %>%
      stringr::str_replace("may be", "maybe") %>%
      stringr::str_replace("every day", "everyday")  %>%
      stringr::str_replace("horse back", "horseback") %>%
      stringr::str_replace("where ever", "wherever") %>%
      stringr::str_replace("wall paper", "wallpaper") %>%
      stringr::str_replace("out look", "outlook") %>%
      stringr::str_replace("thread bare", "threadbare")
  }

  response
}

