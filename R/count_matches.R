# Count matches after all adjustments
count_matches <- function(d) {
  d %>%
    dplyr::mutate(count_target = purrr::map(diff_target,
                                            ~ifelse(.x, 1, NA)) %>%
                    purrr::map(~.x[complete.cases(.x)]) %>%
                    purrr::map(~length(.x)) %>% unlist) %>%
    dplyr::mutate(count_response = purrr::map(diff_response,
                                              ~ifelse(.x, 1, NA)) %>%
                    purrr::map(~.x[complete.cases(.x)]) %>%
                    purrr::map(~length(.x)) %>% unlist)
}
