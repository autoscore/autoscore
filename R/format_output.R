# To format the output in a nice table
format_output <- function(final_table, output, original_data){
  original_data <- original_data %>%
    dplyr::rename_all(tolower)

  if (isTRUE("human" %in% names(final_table))){

    orig_d2 <- original_data %>%
      dplyr::select(-id, -target, -response, -human)

    ft <- final_table %>%
      dplyr::select(human, count_target) %>%
      dplyr::mutate(equal = human == count_target)
    ft <- cbind(original_data$id, original_data$target, original_data$response,
                ft, orig_d2) %>%
      stats::setNames(c("id", "target", "response", "human", "autoscore", "equal",
                        names(orig_d2)))
  } else {

    orig_d2 <- original_data %>%
      dplyr::select(-id, -target, -response)

    ft <- final_table %>%
      dplyr::select(count_target)
    ft <- cbind(original_data$id, original_data$target, original_data$response,
                ft, orig_d2) %>%
      stats::setNames(c("id", "target", "response", "autoscore",
                        names(orig_d2)))
  }

  if (output == "text"){
    ft
  }
}
