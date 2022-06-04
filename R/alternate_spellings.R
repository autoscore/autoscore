# Functions for working with alternate spellings list
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

  tolower(.x)
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
