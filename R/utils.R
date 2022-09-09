## Utilities for the package

# Read in and select the appropriate columns for the analysis
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

# Get data ready to have rules applied to it
split_clean <- function(d, contractions_df, number_text_rule, compound_rule){
  d <- select_cols(d)
  d$target <- stringr::str_to_lower(d$target)
  d$response <- stringr::str_to_lower(d$response)

  if (!is.null(contractions_df)){
    contractions_df$contraction <- stringr::str_to_lower(contractions_df$contraction)
    contractions_df$replacement <- stringr::str_to_lower(contractions_df$replacement)
    contractions_df = unique(contractions_df)
    d$response <- contractions_fun(d$response, contractions_df)
    d$target <- contractions_fun(d$target, contractions_df)
  }

  # remove "x" or "xx"
  d$response <- stringr::str_remove_all(d$response, full_word("x"))
  d$response <- stringr::str_remove_all(d$response, full_word("xx"))

  # twenties, thirties, etc.
  if (isTRUE(number_text_rule))
    d$response <- numbers_with_s(d$response)

  # remove NAs
  d$target <- furniture::washer(d$target, is.na, value = "")
  d$response <- furniture::washer(d$response, is.na, value = "")

  # clean up
  d$target <- d$target %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    stringr::str_trim() %>%
    stringr::str_squish()
  d$response <- d$response %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    stringr::str_trim() %>%
    stringr::str_squish()

  if (isTRUE(number_text_rule))
    d$response <- if_else(extract_numbers_fun(d$response) == "", d$response, replace_numbers(d$response))

  if (!is.null(compound_rule))
    d$response <- compound_fixer(d$response, comp = compound_rule)

  # double word
  d <- rep_word_fun(d)

  d$target <- stringr::str_split(d$target, pattern = " ")
  d$response <- stringr::str_split(d$response, pattern = " ")

  d
}


# Function to match words
match_fun <- function(x, y, root_word_rule) {

  # depending on root_word_rule should pmatch or match be used
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










