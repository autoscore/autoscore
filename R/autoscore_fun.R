#' autoscore
#'
#' This function specifically takes a data frame with target words and response words and calculates the number of matches based on a number of scoring rules.
#'
#' @param .data The data.frame (or tbl_df) to be used to autoscore
#' @param acceptable_df A user-provided `data.frame` of original and alternate spellings for words in the target/response lists (this is the \code{acceptable_spell_rule} and is in addition to built-in homophone list that can be seen with \code{data(homophones)})
#' @param plural_rule if target or response added or subtracted -s and -es at the end of the word,  count as correct (default = `FALSE`)
#' @param plural_add_rule only if response has an additional -s or -es (not missing either at the end of the word) to be counted right. Differs from \code{plural_rule} since this can only be added to the end of the response word, not missing from it.
#' @param tense_rule if target or response added or subtracted -d and -ed at the end of the word,  count as correct (default = `FALSE`)
#' @param tense_add_rule only if response has an additional -d or -ed (not missing either at the end of the word) to be counted right. Differs from \code{tense_rule} since this can only be added to the end of the response word, not missing from it.
#' @param a_the_rule should "a" and "the" be considered the same? (default = `FALSE`)
#' @param root_word_rule should a word that contains the target word at the beginning of the reponse word be considered correct (default = `FALSE` because does "partial" matching which can bring in some unexpected results)
#' @param double_letter_rule should double letters within a word (the t in 'attack') be considered the same as if there is only one of that latter ('atack'); some of these will be in the common_misspell_rule; default = `FALSE`
#' @param suffix_rule should the words be stemmed (all suffix characters removed)? (default = `FALSE`); if `TRUE`, plural_rule and tense_rule are `FALSE`
#' @param number_text_rule should the numbers (e.g., 1, 2, 3) be used in word form (e.g., one, two, three) in the scoring? (default = `FALSE`)
#' @param contractions_df list of contractions to change (e.g., "She'll" changed to "She will"). The built-in list of contractions can be seen with \code{data(contractions_df)}.
#' @param compound_rule named vector of `c("correct" = "original")` form. For instance, "junkyard" is the target form but some responses were "junk yard". To fix, we can include a vector like `c("junkyard" = "junk yard")`.
#' @param output the output type for the autoscore table; current options are "text" (provides a cleaned data set) and "none" (which provides all data); others to follow soon
#'
#' @examples
#'
#' library(tidyverse)
#' library(autoscore)
#'
#' data("example_data")
#'
#' ## Using all the defaults
#' autoscore(example_data)
#'
#' ## Using the default acceptable spellings list
#' example_data %>%
#'   autoscore::autoscore(acceptable_df = autoscore::acceptable_spellings)
#'
#' ## Changing some of the rules
#'
#' example_data %>%
#'   autoscore::autoscore(acceptable_df = autoscore::acceptable_spellings,
#'                        plural_rule = FALSE,
#'                        tense_rule = FALSE)
#'
#' comp = c("junkyard" = "junk yard")
#' example_data %>%
#'   autoscore::autoscore(compound_rule = comp)
#'
#' @import dplyr
#' @import tibble
#' @importFrom english words
#' @importFrom furniture washer
#' @importFrom stats setNames
#' @importFrom stats na.omit
#' @importFrom purrr map_dbl
#'
#' @export
autoscore <- function(.data,
                      acceptable_df = NULL,
                      plural_rule = FALSE,
                      plural_add_rule = FALSE,
                      tense_rule = FALSE,
                      tense_add_rule = FALSE,
                      a_the_rule = FALSE,
                      root_word_rule = FALSE,
                      double_letter_rule = FALSE,
                      suffix_rule = FALSE,
                      number_text_rule = FALSE,
                      contractions_df = NULL,
                      compound_rule = NULL,
                      output = "text") {

  error_check_rules(suffix_rule, plural_rule, plural_add_rule, tense_rule,
                    tense_add_rule, a_the_rule, root_word_rule,
                    double_letter_rule, number_text_rule)
  error_check_alternate_df(acceptable_df)

  counts <- split_clean(.data,
                        contractions_df = contractions_df,
                        number_text_rule = number_text_rule,
                        compound_rule = compound_rule) %>%
    match_position_basic(alternate_df = acceptable_df,
                         plural_rule = plural_rule,
                         plural_add_rule = plural_add_rule,
                         tense_rule = tense_rule,
                         tense_add_rule = tense_add_rule,
                         a_the_rule = a_the_rule,
                         root_word_rule = root_word_rule,
                         suffix_rule = suffix_rule,
                         double_letter_rule = double_letter_rule) %>%
    count_matches() %>%
    dplyr::mutate(count_target = count_target + rep_word) %>%
    dplyr::select(-rep_word)

  if (output == "none"){
    counts
  } else {
    format_output(counts, output = output, .data)
  }
}
