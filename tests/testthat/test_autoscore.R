
df <- autoscore::example_data
acceptable_df <- tibble::data_frame(
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

alternate_df <- tibble::data_frame(
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
  7, "junk yard", "junkyard", 1
)

autoscored <- autoscore::autoscore(d, alternate_df,
                                   plural_rule = TRUE,
                                   tense_rule = TRUE,
                                   root_word_rule = TRUE,
                                   a_the_rule = TRUE,
                                   output = "text")

testthat::expect_s3_class(autoscored,
                          "data.frame")
testthat::expect_equal(autoscored$equal, rep(TRUE, 7))


