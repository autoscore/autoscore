#' AutoScore
#'
#' The purpose of `autoscore` is to automatically score word identification in speech perception research,
#' such as studies involving listener understanding of speech in background noise or disordered speech.
#' The program uses a flexible number of rules that determine whether a response set of words (i.e., listener
#' transcriptions) match a target set of words (i.e., speech corpus). At the most basic level, Autoscore
#' counts words in the listener transcript as correct if they match the words in the target phrase exactly
#' (regardless of word order), or match a homophone or common misspelling of the target word. Individual
#' rules can be applied or removed, depending on the needs of researcher and the scoring rules of the research
#' lab. Examples of rules available in Autoscore include the ability to count as correct substitutions of
#' articles (A for The) or differences in plural or tense (adding -s or -ed to a word). Additional rules
#' can be added by the researcher as needed.
#'
#' @docType package
#' @name autoscore
NULL
