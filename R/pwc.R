#' Percent Words Correct
#'
#' Obtain the percent words correct from a data.frame produced by `autoscore()`.
#'
#' @param data data.frame containing the target, response, and autoscore columns as produced by the function `autoscore()`.
#'
#' @return data.frame of the percent correct for each line in `data`
#'
#'
#' @importFrom dplyr pull
#' @importFrom dplyr rename_all
#' @import stringr
#' @importFrom dplyr mutate
#'
#' @export
pwc <- function(data){
  target_words <- data %>%
    dplyr::rename_all(tolower) %>%
    dplyr::pull(target) %>%
    stringr::str_split(pattern = " ") %>%
    purrr::map(~stringr::str_remove_all(.x, pattern = "[[:punct:]]")) %>%
    purrr::map_dbl(~length(.x))
  correct_words <- data %>%
    dplyr::pull(autoscore)

  data %>%
    dplyr::mutate(pwc = (correct_words / target_words) * 100)
}


