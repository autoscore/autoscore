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
split_clean <- function(d, contractions_df){
  d <- select_cols(d)

  if (!is.null(contractions_df)){
    contractions_df$contraction <- stringr::str_to_lower(contractions_df$contraction)
    contractions_df$replacement <- stringr::str_to_lower(contractions_df$replacement)
    contractions_df = unique(contractions_df)
    d$response <- contractions_fun(stringr::str_to_lower(d$response), contractions_df)
    d$target <- contractions_fun(stringr::str_to_lower(d$target), contractions_df)
  }

  d$target <- furniture::washer(d$target, is.na, value = "")
  d$response <- furniture::washer(d$response, is.na, value = "")
  d$target <- stringr::str_to_lower(d$target) %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    stringr::str_replace_all(pattern = "2", replacement = "two") %>%
    stringr::str_replace_all(pattern = "4", replacement = "four") %>%
    stringr::str_trim() %>%
    stringr::str_split(pattern = " ")
  d$response <- stringr::str_to_lower(d$response) %>%
    stringr::str_replace_all(pattern = "-", replacement = " ") %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
    stringr::str_replace_all(pattern = "2", replacement = "two") %>%
    stringr::str_replace_all(pattern = "4", replacement = "four") %>%
    stringr::str_trim() %>%
    stringr::str_split(pattern = " ")

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










