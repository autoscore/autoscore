
df <- autoscore::example_data
acceptable_df <- tibble::tibble(
  target = c("model",
             "treason",
             "duck"),
  acceptable= c("modal, moddel",
                          "treeson",
                          "dock")
)

testthat::expect_s3_class(autoscore::autoscore(df), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               acceptable_df = acceptable_df), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               acceptable_df = acceptable_df,
                                               root_word_rule = TRUE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               root_word_rule = FALSE,
                                               double_letter_rule = FALSE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               suffix_rule = TRUE,
                                               double_letter_rule = FALSE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               suffix_rule = TRUE,
                                               tense_rule = TRUE,
                                               double_letter_rule = FALSE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               suffix_rule = TRUE,
                                               plural_rule = TRUE,
                                               double_letter_rule = FALSE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               suffix_rule = TRUE,
                                               tense_rule = FALSE,
                                               tense_add_rule = TRUE,
                                               double_letter_rule = FALSE), "data.frame")
testthat::expect_s3_class(autoscore::autoscore(df,
                                               suffix_rule = TRUE,
                                               plural_rule = FALSE,
                                               plural_add_rule = TRUE,
                                               double_letter_rule = FALSE), "data.frame")

alternate_df <- tibble::tibble(
  target = c("beat",
             "treeson"),
  acceptable = c("beet, baet",
                 "treason"))

d <- tibble::tribble(
  ~id, ~target, ~response, ~human,
  1, "the coin ate it", "a coins for it", 3,
  2, "beat the clock", "beets the clock", 3,
  3, "beated it", "beet it", 2,
  4, "beets the clock", "beat the clock", 3,
  5, "beeted the clock", "beet the clock", 3,
  6, "junkyard", "junk yard", 0,
  7, "junk yard", "junkyard", 1,
  8, "The matches are on the shelf", "23  the matches are on the shelf", 6,
  9, "The puppy played with a ball", "1  x", 0,
  10, "One two three", "1 2 3", 3,
  11, "She will each be", "She'll be", 3
)

contractions_list = tibble::tibble(
  contraction = "She'll",
  replacement = "She will"
)

autoscored <- autoscore::autoscore(d, alternate_df,
                                   plural_rule = TRUE,
                                   tense_rule = TRUE,
                                   root_word_rule = TRUE,
                                   a_the_rule = TRUE,
                                   number_text_rule = TRUE,
                                   contractions_df = contractions_list,
                                   output = "text")

testthat::expect_s3_class(autoscored,
                          "data.frame")
testthat::expect_equal(autoscored$equal, rep(TRUE, 11))

autoscored2 <- autoscore::autoscore(d, alternate_df,
                                   plural_rule = TRUE,
                                   tense_rule = TRUE,
                                   root_word_rule = FALSE,
                                   a_the_rule = TRUE,
                                   output = "text")

testthat::expect_s3_class(autoscored2,
                          "data.frame")
testthat::expect_equal(autoscored2$equal, c(rep(TRUE, 6), FALSE, TRUE, TRUE, FALSE, FALSE))

testthat::expect_equal({
  target = c(
    "we will get to know these players as we go along",
    "according to the rules, you shouldn't end any sentence with a preposition",
    "it will only work if it will do work",
    "nope"
  )
  double_word_detect(target)
  },
  c("we", NA, "it, will, work", NA)
)

testthat::expect_equal({
  d <- tibble::tribble(
    ~id, ~target, ~response, ~human,
    1, "The score was the goal", "The score was a goal", 4,
    2, "The score was the goal", "The score was the goal", 5,
    3, "The score was a goal", "The score was a goal", 5,
    4, "The score was a goal", "The score was the goal", 4,
    5, "The score was the goal", "The the score was the goal", 5,
    6, "The they score was the goal", "The score was the goal", 5,
    7, "A patient seriously hurt his ankle in a motorcycle accident", "My patient seriously hurt his ankle in a motorcycle accident", 9,
    8, "There were seven hundred things", "There were 700 stuff", 4,
    9, "One two three", "1 2 3", 3,
    10, "Junkyard is far", "Junk yard is far", 3
  )
  comp <- c("junkyard" = "junk yard")
  autoscore(d, number_text_rule = TRUE, compound_rule = comp) %>%
    dplyr::pull(equal)
  },
  rep(TRUE, 10)
)



