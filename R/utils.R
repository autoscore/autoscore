## Utilities for the package

## Read in and select the appropriate columns for the analysis
select_cols <- function(d){
  d <- d %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(id, target, response, contains("human"))

  if (ncol(d) > 4){
    stop("More than 4 columns were selected containing 'id', 'target', 'response', or 'human'", call. = FALSE)}
  else if (ncol(d) < 3){
    stop(paste0("Less than 3 columns were selected containing 'id', 'target', 'response', or 'human'\n",
                " - One column should be named 'id', another named 'target', another named 'response', and",
                "optionally one named 'human'. Check your CSV to upload again."), call. = FALSE)}
  d
}

split_clean <- function(d){
  select_cols(d) %>%
    dplyr::mutate(target = furniture::washer(target, is.na, value = ""),
                  response = furniture::washer(response, is.na, value = "")) %>%
    dplyr::mutate(target = stringr::str_to_lower(target) %>%
                    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
                    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
                    stringr::str_replace_all(pattern = "2", replacement = "two") %>%
                    stringr::str_replace_all(pattern = "4", replacement = "four") %>%
                    stringr::str_trim() %>%
                    stringr::str_split(pattern = " "),
                  response = stringr::str_to_lower(response) %>%
                    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
                    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
                    stringr::str_replace_all(pattern = "2", replacement = "two") %>%
                    stringr::str_replace_all(pattern = "4", replacement = "four") %>%
                    stringr::str_trim() %>%
                    stringr::str_split(pattern = " "))
}




within_the_alternate_loop <- function(.x, .a,
                                      plural_rule, plural_add_rule,
                                      tense_rule, tense_add_rule){

  names(.x) = .x

  replace = .a %>%
    dplyr::mutate(in_it1 = alternate_string %in% .x,
                  in_it2 = plurals(alternate_string, "es", plural_rule, plural_add_rule) %in% .x,
                  in_it3 = plurals(alternate_string, "s", plural_rule, plural_add_rule) %in% .x,
                  in_it4 = tenses(alternate_string, "ed", tense_rule, tense_add_rule) %in% .x,
                  in_it5 = tenses(alternate_string, "d", tense_rule, tense_add_rule) %in% .x) %>%
    dplyr::filter(in_it1 | in_it2 | in_it3 | in_it4 | in_it5) %>%
    dplyr::mutate(in_it = in_it1 | in_it2 | in_it3 | in_it4 | in_it5,
                  which_rule = dplyr::case_when(in_it1 ~ "none",
                                                in_it2 ~ "es",
                                                in_it3 ~ "s",
                                                in_it4 ~ "ed",
                                                in_it5 ~ "d"))

  if (nrow(replace) > 0){
    for (i in 1:nrow(replace)){
      what_to_replace = switch(replace$which_rule[[i]],
                               "none" = replace$alternate_string[[i]],
                               "es" = plurals(replace$alternate_string[[i]], "es", plural_rule, plural_add_rule),
                               "s"  = plurals(replace$alternate_string[[i]], "s", plural_rule, plural_add_rule),
                               "ed" = tenses(replace$alternate_string[[i]], "ed", tense_rule, tense_add_rule),
                               "d"  = tenses(replace$alternate_string[[i]], "d", tense_rule, tense_add_rule))
      .x[what_to_replace] <- paste0(replace$target[[i]],
                                    ifelse(replace$which_rule[[i]] == "none", "", replace$which_rule[[i]]))
    }
  }

  .x
}


## Acceptable Spelling List
alternate_fun <- function(d, alternate_df,
                          plural_rule, plural_add_rule,
                          tense_rule, tense_add_rule){

  if (is.null(alternate_df))
    return(d)

  ## Need to make it possible to use other rules within this one
  alternate_df <- alternate_df %>%
    dplyr::mutate(rowname = row_number(target)) %>%
    dplyr::mutate(alternate_string = stringr::str_split(acceptable, pattern = ", "))

  .a <- alternate_df %>%
    tidyr::unnest(., cols = dplyr::everything()) %>%
    dplyr::distinct(.)

  d %>%
    ## See if there are matches in the target (per line)
    dplyr::mutate(target = purrr::map(target, ~{
      within_the_alternate_loop(.x, .a,
                                plural_rule, plural_add_rule,
                                tense_rule, tense_add_rule)

    })) %>%
    dplyr::mutate(response = purrr::map(response, ~{
      within_the_alternate_loop(.x, .a,
                                plural_rule, plural_add_rule,
                                tense_rule, tense_add_rule)

    }))
}



match_fun <- function(x, y, root_word_rule) {

  ## depending on root_word_rule should pmatch or match be used
  if (isTRUE(root_word_rule)){

    val <- vector('list', length = length(y))
    for (i in seq_along(y)){

      val[[i]] <- startsWith(y[i], x)

      if (isTRUE(sum(val[[i]]) > 1L)){

        ## Only keep first true
        val[[i]][-which.max(val[[i]])] <- FALSE
        ## change the first match to something unmatchable to avoid double matches
        word <- unique(x[val[[i]]])
        y[y == word][1] <- "lskdjf"
        x[val[[i]]] <- "qwyk"

      }
    }

  } else {

    val <- vector('list', length = length(y))
    for (i in seq_along(y)){

      val[[i]] <- x %in% y[i]

      if (isTRUE(sum(val[[i]]) > 1L)){

        ## Only keep first true
        val[[i]][-which.max(val[[i]])] <- FALSE
        ## change the first match to something unmatchable to avoid double matches
        word <- unique(x[val[[i]]])
        y[y == word][1] <- "lskdjf"
        x[val[[i]]] <- "qwyk"

      }
    }
  }

  val %>%
    do.call("rbind", .) %>%
    data.frame %>%
    lapply(any) %>%
    unlist %>%
    which

}


## Main work horse function
match_position_basic <- function(d, alternate_df,
                                 plural_rule, plural_add_rule,
                                 tense_rule, tense_add_rule,
                                 a_the_rule, root_word_rule,
                                 suffix_rule, double_letter_rule,
                                 number_text_rule){

  if (isTRUE(suffix_rule)){
    tense_rule <- FALSE
    plural_rule <- FALSE
    tense_add_rule <- FALSE
    plural_add_rule <- FALSE
  }

  ## alternate_spell_rule
  d <- alternate_fun(d, alternate_df,
                     plural_rule, plural_add_rule,
                     tense_rule, tense_add_rule)

  d <- d %>%
    dplyr::mutate(target = purrr::map(target, ~{
      double_letter_fun(.x, double_letter_rule) %>%
        a_the_fun(a_the_rule) %>%
        suffix_fun(suffix_rule) %>%
        numbers_fun(number_text_rule)
    })) %>%
    dplyr::mutate(response = purrr::map(response, ~{
      double_letter_fun(.x, double_letter_rule) %>%
        a_the_fun(a_the_rule) %>%
        suffix_fun(suffix_rule) %>%
        numbers_fun(number_text_rule)
    })) %>%
    dplyr::mutate(diff_target_pre = purrr::map2(target, response, ~{
      pasttense_plurals_fun(.x, .y, tense_rule, tense_add_rule, plural_rule, plural_add_rule, root_word_rule)
    })) %>%
    dplyr::mutate(diff_response_pre = purrr::map2(response, target, ~{
      pasttense_plurals_fun(.x, .y, tense_rule, tense_add_rule, plural_rule, plural_add_rule, root_word_rule)
    }))

  d %>%
    dplyr::mutate(diff_target = purrr::map(diff_target_pre, ~.x > 0)) %>%
    dplyr::mutate(diff_response = purrr::map(diff_response_pre, ~.x > 0))
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








