# deal with repeated words in targets
rep_word_fun <- function(data){
  data$rep_word_target <- double_word_detect(data$target)
  data$rep_word_response <- double_word_detect(data$response)

  if (all(is.na(data$rep_word_target))){
    data$rep_word <- 0
    data <- select(data, -rep_word_target)
    return(data)
  }

  data$rep_word_target_n <- stringr::str_extract_all(data$target, full_word(data$rep_word_target))
  data$rep_word_response_n <- stringr::str_extract_all(data$response, full_word(data$rep_word_response))

  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(
    data,
    doubled = list(dplyr::case_when(
      ! is.na(rep_word_target) ~ rep_word_target2(rep_word_target, response)
    )))
  data <- dplyr::ungroup(data)

  data$double_length <- purrr::map_dbl(data$doubled, length)
  data$length_rep_target <- lengths(str_split(data$rep_word_target, ", "))
  data$rep_word <- ifelse(data$double_length == 1 & ! is.na(data$rep_word_target), -1, 0)
  data$rep_word <- ifelse(data$double_length > 1 & data$length_rep_target > 1, -data$length_rep_target, data$rep_word)
  data$rep_word <- dplyr::case_when(
    data$rep_word_target == data$rep_word_response ~ 0,
    TRUE ~ data$rep_word
  )
  diff <- lengths(data$rep_word_target_n) - data$double_length
  data$rep_word <- ifelse(diff > 0 & lengths(data$rep_word_target_n) > 2 & data$double_length > 0, data$rep_word - 1, data$rep_word)
  data <- dplyr::select(data, -doubled, -double_length, -rep_word_target, -rep_word_target_n, -rep_word_response_n)
  data
}


rep_word_target2 <- function(rep_word_target, response){
  x = full_word(rep_word_target, TRUE)
  m = c()
  for (i in x){
    m[i] = stringr::str_extract_all(response, i)
  }
  unlist(m)
}
