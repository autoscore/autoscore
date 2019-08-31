#' Percent Words Correct
#'
#' Obtain the percent words correct from a data.frame produced by `autoscore()`.
#'
#' @param data data.frame containing the target, response, and autoscore columns as produced by the function `autoscore()`.
#' @param id the id variable(s) to summarize the PWC for
#'
#' @return data.frame of the percent correct for each line in `data`
#'
#' @import rlang
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @import dplyr
#' @import stringr
#'
#' @export
pwc <- function(data, id){
  id <- rlang::enquo(id)

  data %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(target = stringr::str_split(target, pattern = " ") %>%
                    purrr::map(~stringr::str_remove_all(.x, pattern = "[[:punct:]]")) %>%
                    purrr::map_dbl(~length(.x))) %>%
    dplyr::group_by(!! id) %>%
    dplyr::summarise(pwc = sum(autoscore) / sum(target) * 100)
}

