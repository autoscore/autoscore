# deal with repeated words in targets
rep_word_fun <- function(data){
  data$rep_word_target = double_word_detect(data$target)

  if (all(is.na(data$rep_word_target))){
    data$rep_word <- 0
    data <- select(data, -rep_word_target)
    return(data)
  }

  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(
    data,
    doubled = dplyr::case_when(
      ! is.na(rep_word_target) ~ stringr::str_extract_all(response, full_word(rep_word_target))
    ))
  data <- dplyr::mutate(data, double_length = length(doubled))
  data <- dplyr::ungroup(data)
  data <- dplyr::mutate(
    data,
    rep_word = case_when(
      double_length == 1 & ! is.na(rep_word_target) ~ -1,
      double_length != 1 | is.na(rep_word_target) ~ 0
    ))
  data <- dplyr::select(data, -doubled, -double_length, -rep_word_target)
}
