<!-- README.md is generated from README.Rmd. Please edit that file -->

    ## ℹ Loading autoscore

    ## Warning: package 'testthat' was built under R version 4.1.2

    ## ── autoscore 0.5.0 ─────────────────────────────────────────────────────────────
    ## ✔ autoscore attached
    ## ✔ No potential conflicts found

<!-- badges: start -->

[![R-CMD-check](https://github.com/autoscore/autoscore/workflows/R-CMD-check/badge.svg)](https://github.com/autoscore/autoscore/actions)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
<!-- badges: end -->

# `autoscore` <img src="man/figures/autoscore_logo.png" align="right" width="30%" height="30%" />

> R Package: `0.5.0` <br> Shiny App:
> [autoscore.usu.edu](http://autoscore.usu.edu)

*Authors:*

-   *Tyson S. Barrett*
-   *Stephanie A. Borrie*
-   *Sarah E. Yoho*
-   *Kaila Stipancic*

The purpose of `autoscore` is to automatically score word identification
in speech perception research, such as studies involving listener
understanding of speech in background noise or disordered speech. The
article first presenting the program has been cited in 20 peer-reviewed
publications as of June 2022 ([see Google
Scholar](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=1603911154801242073)).

The program uses a flexible number of rules that determine whether a
response set of words (i.e., listener transcriptions) match a target set
of words (i.e., speech corpus). At the most basic level, Autoscore
counts words in the listener transcript as correct if they match the
words in the target phrase exactly (regardless of word order).
Individual rules can be applied or removed, depending on the needs of
researcher and the scoring rules of the research lab. Examples of rules
available in Autoscore include the ability to count as correct
substitutions of articles (“a” for “the”) or differences in plural or
tense (adding -s or -ed to a word). Additional rules can be added by the
researcher as needed.

The rule options are categorized into either spelling rules or grammar
rules.

#### Spelling Rules

1.  `acceptable_spell_rule`: Response word counted correct if it is a
    homophone or common misspelling of the target word, according to a
    preloaded default acceptable spelling list (contains over 300 common
    acceptable spellings). User can also download the default acceptable
    spelling list, add/remove items, and upload for automation. Response
    word counted correct if is on the acceptable spelling list. This
    rule is activated by providing the acceptable spelling list. Default
    is `FALSE`.
2.  `root_word_rule`: Response word counted correct if the target word
    (e.g. ‘day’) is embedded at either the beginning (e.g. ‘daybreak’)
    of the target word. Default is `FALSE`.
3.  `double_letter_rule`: Response word counted correct if it omitted a
    double letter within a word (e.g. ‘atack’ matches ‘attack’) or added
    an unnecessary double letter (e.g. ‘occassion’ matches ‘occasion’).
    Default is `FALSE`.
4.  `number_text_rule`: Response word counted correct if using actual
    numbers (e.g. 1, 2, 100) instead of the spelled out version
    (e.g. one, two, one hundred). Default is `FALSE`.
5.  `contractions_rule`: Response word counted correct if using the
    contraction of the target (e.g., target is “she will” and the
    response is “she’ll”). This rule is activated by providing
    contractions to use (there is a default list provided). Default is
    `FALSE`.
6.  `compound_rule`: Response word counted correct if matching a
    specified compound. This rule is activated by providing a named
    vector (e.g., `c("junkyard" = "junk yard")` where “junkyard” is the
    target but some responses were “junk yard”).

#### Grammar Rules

1.  `tense_rule`: Response word counted correct if it differs from the
    target word only by tense; Default is `FALSE`.
2.  `tense_add_rule`: Response word counted correct if it only adds a
    tense as compared to the target word; Default is `FALSE`. It is also
    referred to as “Tense+ Rule.”
3.  `plural_rule`: Response word counted correct if it differs from the
    target word only by plurality; Default is `FALSE`.
4.  `plural_add_rule`: Response word counted correct if it only adds a
    plural as compared to the target word; Default is `FALSE`. It is
    also referred to as “Plural+ Rule.”
5.  `a_the_rule`: Substitutions between “a” and “the” to be scored as
    correct; Default is `FALSE`.

## Design

The API of the `R` package is simple. A single call to `autoscore()`
with the formatted data will run everything for you. This function is a
composite of several sub-functions that do various jobs:

-   `select_cols()` – The first function which takes the data and gets
    it in the right format for analysis.
-   `split_clean()` – Using the cleaned data from `select_cols()`, this
    uses `stringr` to turn the phrases into individual words.
-   `alternate_fun()` – If a data.frame of alternate spellings is
    provided, this function will find and normalize all alternate
    spellings to match the original spelling as defined by the
    researcher.
-   `match_position_basic()` – This function is the workhorse of the
    package. It takes the cleaned data and does three main things: 1)
    applies all the rules except for the `position_rule`, 2) finds the
    matches between the responses and the targets, and 3) reports how
    far away the matches are from each other.
-   `count_matches()` – Finally, this function takes the information
    from `match_position_basic()` and counts the number of matches based
    on the `position_rule`.

Beyond the main analysis when using `autoscore()`, we can also call
`pwc()` to get the percent words correct (based on the number of target
words) for each id.

## Use of the Online Tool

Visit [autoscore.usu.edu](http://autoscore.usu.edu) to use the online
tool. Instructions for its use are found there.

<img src="man/figures/online_autoscore_snapshot.png" align="center" width="70%" height="70%" />

## Use of the R Package

To install the package use the developmental version as it is not yet on
CRAN.

``` r
remotes::install_github("autoscore/autoscore")
```

An example of the use of `autoscore` is below. We will use the example
data set provided in the package.

``` r
library(tidyverse)
library(autoscore)

data("example_data")
example_data
#> # A tibble: 40 × 4
#>       Id Target                      Response                    human
#>    <dbl> <chr>                       <chr>                       <dbl>
#>  1     1 mate denotes a judgement    made the dinner in it           1
#>  2     1 rampant boasting captain    rubbed against the captain      1
#>  3     1 resting older earring       resting alert hearing           1
#>  4     1 bolder ground from justice  boulder down from dresses       2
#>  5     1 remove and name for stake   remember the name for steak     3
#>  6     1 done with finest handle     dinner finished handle          1
#>  7     1 support with dock and cheer she put the duck in chair       1
#>  8     1 or spent sincere aside      earth bent spent her aside      2
#>  9     1 account for who could knock i can for hookah knock          2
#> 10     1 connect the beer device     connected beard kindle bus      1
#> # … with 30 more rows
```

First, let’s use all the defaults and look at the first 10 rows of the
output.

``` r
example_data %>%
  autoscore() %>%   ## using all the defaults
  as_tibble()       ## to shorted output
#> # A tibble: 40 × 6
#>       id target                      response              human autoscore equal
#>    <dbl> <chr>                       <chr>                 <dbl>     <dbl> <lgl>
#>  1     1 mate denotes a judgement    made the dinner in it     1         0 FALSE
#>  2     1 rampant boasting captain    rubbed against the c…     1         1 TRUE 
#>  3     1 resting older earring       resting alert hearing     1         1 TRUE 
#>  4     1 bolder ground from justice  boulder down from dr…     2         1 FALSE
#>  5     1 remove and name for stake   remember the name fo…     3         2 FALSE
#>  6     1 done with finest handle     dinner finished hand…     1         1 TRUE 
#>  7     1 support with dock and cheer she put the duck in …     1         0 FALSE
#>  8     1 or spent sincere aside      earth bent spent her…     2         2 TRUE 
#>  9     1 account for who could knock i can for hookah kno…     2         2 TRUE 
#> 10     1 connect the beer device     connected beard kind…     1         0 FALSE
#> # … with 30 more rows
```

Next, let’s change some of the rules.

``` r
example_data %>%
  autoscore(plural_rule = FALSE, tense_rule = FALSE) %>%
  as_tibble()
#> # A tibble: 40 × 6
#>       id target                      response              human autoscore equal
#>    <dbl> <chr>                       <chr>                 <dbl>     <dbl> <lgl>
#>  1     1 mate denotes a judgement    made the dinner in it     1         0 FALSE
#>  2     1 rampant boasting captain    rubbed against the c…     1         1 TRUE 
#>  3     1 resting older earring       resting alert hearing     1         1 TRUE 
#>  4     1 bolder ground from justice  boulder down from dr…     2         1 FALSE
#>  5     1 remove and name for stake   remember the name fo…     3         2 FALSE
#>  6     1 done with finest handle     dinner finished hand…     1         1 TRUE 
#>  7     1 support with dock and cheer she put the duck in …     1         0 FALSE
#>  8     1 or spent sincere aside      earth bent spent her…     2         2 TRUE 
#>  9     1 account for who could knock i can for hookah kno…     2         2 TRUE 
#> 10     1 connect the beer device     connected beard kind…     1         0 FALSE
#> # … with 30 more rows
```

We can also change the output type to “none” to get all the data from
the computation.

``` r
example_data %>%
  autoscore(output = "none")
#> # A tibble: 40 × 10
#>       id target    response  human diff_target_pre diff_response_pre diff_target
#>    <dbl> <list>    <list>    <dbl> <list>          <list>            <list>     
#>  1     1 <chr [4]> <chr [5]>     1 <int [4]>       <int [5]>         <lgl [4]>  
#>  2     1 <chr [3]> <chr [4]>     1 <int [3]>       <int [4]>         <lgl [3]>  
#>  3     1 <chr [3]> <chr [3]>     1 <int [3]>       <int [3]>         <lgl [3]>  
#>  4     1 <chr [4]> <chr [4]>     2 <int [4]>       <int [4]>         <lgl [4]>  
#>  5     1 <chr [5]> <chr [5]>     3 <int [5]>       <int [5]>         <lgl [5]>  
#>  6     1 <chr [4]> <chr [3]>     1 <int [4]>       <int [3]>         <lgl [4]>  
#>  7     1 <chr [5]> <chr [6]>     1 <int [5]>       <int [6]>         <lgl [5]>  
#>  8     1 <chr [4]> <chr [5]>     2 <int [4]>       <int [5]>         <lgl [4]>  
#>  9     1 <chr [5]> <chr [5]>     2 <int [5]>       <int [5]>         <lgl [5]>  
#> 10     1 <chr [4]> <chr [4]>     1 <int [4]>       <int [4]>         <lgl [4]>  
#> # … with 30 more rows, and 3 more variables: diff_response <list>,
#> #   count_target <dbl>, count_response <int>
```

To use the acceptable spelling rule, let’s use the default provided in
`autoscore`. . In the data frame below, the `target` spellings are the
generally accepted spellings that are in the target list of words while
the `acceptable` column are those that should also be counted as
correct.

``` r
autoscore::acceptable_spellings
#> # A tibble: 257 × 2
#>    target   acceptable       
#>    <chr>    <chr>            
#>  1 absent   abcent           
#>  2 achieved achieved, achived
#>  3 acid     asid             
#>  4 advance  advanse          
#>  5 again    agin             
#>  6 alone    aloan            
#>  7 along    a long           
#>  8 among    amung            
#>  9 ancient  ansient          
#> 10 ancient  anceint          
#> # … with 247 more rows
```

Using this, we can provide it to the `autoscore()` function with the
`acceptable_df` argument.

``` r
example_data %>%
  autoscore::autoscore(acceptable_df = autoscore::acceptable_spellings) %>%
  as_tibble()
#> # A tibble: 40 × 6
#>       id target                      response              human autoscore equal
#>    <dbl> <chr>                       <chr>                 <dbl>     <dbl> <lgl>
#>  1     1 mate denotes a judgement    made the dinner in it     1         0 FALSE
#>  2     1 rampant boasting captain    rubbed against the c…     1         1 TRUE 
#>  3     1 resting older earring       resting alert hearing     1         1 TRUE 
#>  4     1 bolder ground from justice  boulder down from dr…     2         2 TRUE 
#>  5     1 remove and name for stake   remember the name fo…     3         3 TRUE 
#>  6     1 done with finest handle     dinner finished hand…     1         1 TRUE 
#>  7     1 support with dock and cheer she put the duck in …     1         0 FALSE
#>  8     1 or spent sincere aside      earth bent spent her…     2         2 TRUE 
#>  9     1 account for who could knock i can for hookah kno…     2         2 TRUE 
#> 10     1 connect the beer device     connected beard kind…     1         0 FALSE
#> # … with 30 more rows
```

In each of these examples, it is clear that the human and “autoscore”
agree the majority of the time. The times that they disagree, it is
usually predictably a human error or a subjective judgement that the
researcher will have to consider (for example by including alternate
spellings of words as we just demonstrated).

Finally, we can use the `pwc()` function to calculate the percent words
correct using the output from `autoscore()`. We provide the `id`
variable so that `pwc()` provides a value for each individual.

``` r
example_data %>%
  autoscore() %>% 
  pwc(id)
#> # A tibble: 2 × 2
#>      id   pwc
#>   <dbl> <dbl>
#> 1     1  32.7
#> 2     2  20.8
```

As of `autoscore 0.5.0` two new rules can also be used:
`number_text_rule` (changes numbers like 1 or 2 to “one” or “two”) and
`contractions_df` (which, when applied, adjusts for contraction words).
There is also a new function called `compound_fixer()` that allows you
to adjust compound words the way you want. The example below shows all
three of these new features together. To start we make a fake data set
with some good examples of things we want to fix.

``` r
small_example <- tibble::tribble(
  ~id, ~target, ~response, ~human,
  1, "the coin ate it", "a coins for it", 1,
  2, "beat the clock", "beets the clock", 2,
  3, "beated it", "beet it", 1,
  4, "beets the clock", "beat the clock", 2,
  5, "beeted the clock", "beet the clock", 2,
  6, "junkyard", "junk yard", 1,
  7, "Breakfast is great", "break fast is great", 3,
  8, "The matches are on the shelf", "23  the matches are on the shelf", 6,
  9, "The puppy played with a ball", "1  x", 0,
  10, "One two three", "1 2 3", 3,
  11, "Hide one thing", "hide 1", 2,
  12, "She will eat", "She'll eat", 3
)
```

Then, using this data, we apply the compound fixer function and the
number and contraction rules. For the `compound_fixer()`, we want “junk
yard” to become “junkyard” and “break fast” to become “breakfast”. These
words are not in the default list so we add them with the `comp`
argument. The `human` column is the correct number so we can compare
autoscore’s results with it.

``` r
small_example %>% 
  dplyr::mutate(response = compound_fixer(response, comp = c("junkyard" = "junk yard", "breakfast" = "break fast"))) %>% 
  autoscore(number_text_rule = TRUE,
            contractions_df = autoscore::contractions_df) 
#>    id                       target                         response human
#> 1   1              the coin ate it                   a coins for it     1
#> 2   2               beat the clock                  beets the clock     2
#> 3   3                    beated it                          beet it     1
#> 4   4              beets the clock                   beat the clock     2
#> 5   5             beeted the clock                   beet the clock     2
#> 6   6                     junkyard                         junkyard     1
#> 7   7           Breakfast is great               breakfast is great     3
#> 8   8 The matches are on the shelf 23  the matches are on the shelf     6
#> 9   9 The puppy played with a ball                             1  x     0
#> 10 10                One two three                            1 2 3     3
#> 11 11               Hide one thing                           hide 1     2
#> 12 12                 She will eat                       She'll eat     3
#>    autoscore equal
#> 1          1  TRUE
#> 2          2  TRUE
#> 3          1  TRUE
#> 4          2  TRUE
#> 5          2  TRUE
#> 6          1  TRUE
#> 7          3  TRUE
#> 8          6  TRUE
#> 9          0  TRUE
#> 10         3  TRUE
#> 11         2  TRUE
#> 12         3  TRUE
```

### Learn More

For more information, contact <autoscorehelp@gmail.com>.
