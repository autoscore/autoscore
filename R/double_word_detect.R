#' double_word_detect
#'
#' This function marks the sentences with words that are repeated.
#'
#' @param target the target variable
#'
#' @import stringr
#'
#' @export
double_word_detect <- function(target){

  targ = stringr::str_remove_all(target, "[[:punct:]]")
  targ = stringr::str_split(targ, " ")

  sapply(targ, function(x){
    count = vector(length = length(x))
    if (length(x) <= 1) return(NA)
    for (i in 1:(length(x)-1)){
      for (j in seq(up_to(i+1, length(x)), length(x))){
        if (x[i] == x[j]){
          count[i] = TRUE
          x[j] = i
        }
      }
    }
    if (sum(count) > 0)
      paste(unique(x[count]), collapse = ", ")
    else
      NA
  })
}

up_to <- function(x, top){
  if (x > top)
    top
  else
    x
}


