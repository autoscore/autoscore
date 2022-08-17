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
  stringr::str_detect(x, "\\d+") & !stringr::str_detect(x, "[A-Za-z]")
}

numbers_fun <- function(x){
  purrr::map_chr(x, function(y) {
    num = is_number_in_text(y)
    if (isTRUE(num)){
      english::words(as.integer(y))
    } else {
      y
    }
  })
}

extract_numbers_fun <- function(x){
  stringr::str_extract_all(x, "[0-9]+")
}

replace_numbers <- function(x, use = TRUE){
  nums = extract_numbers_fun(x)
  nums_words = purrr::map(nums, ~numbers_fun(.x))
  if (length(nums) == 0) return(x)

  for (i in seq_along(nums)){
    for (j in seq_along(nums_words[[i]])){
      x[i] = stringr::str_replace(x[i], nums[[i]][[j]], nums_words[[i]][[j]])
    }
  }
  x
}

full_word <- function(x, sep = FALSE){
  if (isTRUE(sep)){
    x = str_split(x, ", ")[[1]]
  }
  paste0("\\b", x, "\\b")
}

contractions_fun <- function(x, contraction_list){
  for (i in 1:nrow(contraction_list)){
    x = stringr::str_replace(
      x,
      full_word(contraction_list$contraction[i]),
      contraction_list$replacement[i]
    )
  }
  x
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

pasttense_plurals_fun <- function(x, y, tense_rule, tense_add_rule, plural_rule, plural_add_rule, root_word_rule){

  if (isTRUE(tense_rule) & isTRUE(plural_rule)){
    ed1 <- match_fun(paste0(x, "ed"), y, root_word_rule)
    ed2 <- match_fun(paste0(x, "d"), y, root_word_rule)
    ed3 <- match_fun(x, paste0(y, "ed"), root_word_rule)
    ed4 <- match_fun(x, paste0(y, "d"), root_word_rule)
    es1 <- match_fun(paste0(x, "es"), y, root_word_rule)
    es2 <- match_fun(paste0(x, "s"), y, root_word_rule)
    es3 <- match_fun(x, paste0(y, "es"), root_word_rule)
    es4 <- match_fun(x, paste0(y, "s"), root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(ed1, ed2, ed3, ed4, es1, es2, es3, es4, reg)) %>% unique %>% as.numeric

  } else if (isTRUE(plural_rule)) {
    es1 <- match_fun(paste0(x, "es"), y, root_word_rule)
    es2 <- match_fun(paste0(x, "s"), y, root_word_rule)
    es3 <- match_fun(x, paste0(y, "es"), root_word_rule)
    es4 <- match_fun(x, paste0(y, "s"), root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(es1, es2, es3, es4, reg)) %>% unique %>% as.numeric

  } else if (isTRUE(tense_rule)) {
    ed1 <- match_fun(paste0(x, "ed"), y, root_word_rule)
    ed2 <- match_fun(paste0(x, "d"), y, root_word_rule)
    ed3 <- match_fun(x, paste0(y, "ed"), root_word_rule)
    ed4 <- match_fun(x, paste0(y, "d"), root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(ed1, ed2, ed3, ed4, reg)) %>% unique %>% as.numeric

  } else if (isTRUE(tense_add_rule) & isTRUE(plural_add_rule)){
    ed1 <- match_fun(paste0(x, "ed"), y, root_word_rule)
    ed2 <- match_fun(paste0(x, "d"), y, root_word_rule)
    es1 <- match_fun(paste0(x, "es"), y, root_word_rule)
    es2 <- match_fun(paste0(x, "s"), y, root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(ed1, ed2, es1, es2, reg)) %>% unique %>% as.numeric

  } else if (isTRUE(tense_add_rule)) {
    ed1 <- match_fun(paste0(x, "ed"), y, root_word_rule)
    ed2 <- match_fun(paste0(x, "d"), y, root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(ed1, ed2, reg)) %>% unique %>% as.numeric

  } else if (isTRUE(plural_add_rule)){
    es1 <- match_fun(paste0(x, "es"), y, root_word_rule)
    es2 <- match_fun(paste0(x, "s"), y, root_word_rule)
    reg <- match_fun(x, y, root_word_rule)
    na.omit(c(es1, es2, reg)) %>% unique %>% as.numeric

  } else {
    match(x, y)
  }
}
