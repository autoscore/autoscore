## Main work horse function
match_position_basic <- function(d, alternate_df,
                                 plural_rule, plural_add_rule,
                                 tense_rule, tense_add_rule,
                                 a_the_rule, root_word_rule,
                                 suffix_rule, double_letter_rule,
                                 number_text_rule, contraction_list){

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
