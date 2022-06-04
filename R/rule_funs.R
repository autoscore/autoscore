# Functions for calculating single rules

plurals <- function(x, suf = "es", plural_rule, plural_add_rule){
  if (isTRUE(plural_rule) | isTRUE(plural_add_rule)){
    paste0(x, suf)
  } else {
    x
  }
}

tenses <- function(x, suf = "ed", tense_rule, tense_add_rule){
  if (isTRUE(tense_rule) | isTRUE(tense_add_rule)){
    paste0(x, suf)
  } else {
    x
  }
}

is_number_in_text = function(x){
  stringr::str_detect(x, "\\d+")
}

numbers_fun <- function(x, use = TRUE){
  purrr::map_chr(x, function(y) {
    num = is_number_in_text(y)
    if (isTRUE(use) & isTRUE(num)){
      english::words(as.integer(y))
    } else {
      y
    }
  })
}

suffix_fun <- function(chr, use = TRUE){
  if (isTRUE(use)){
    tm::stemDocument(chr)
  } else {
    chr
  }
}

a_the_fun <- function(chr, use = TRUE){
  if (isTRUE(use)){
    nam = names(chr)
    chr = stringr::str_replace(chr, pattern = "^a$", replacement = "the")
    names(chr) = chr
    chr
  } else {
    chr
  }
}

double_letter_fun <- function(chr, use = FALSE){
  if (isTRUE(use)){
    nam = names(chr)
    chr = stringr::str_replace_all(chr, pattern = "([[:alpha:]])\\1+", replacement = "\\1")
    names(chr) = chr
    chr
  } else {
    chr
  }
}
