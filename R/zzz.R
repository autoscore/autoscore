.onAttach <- function(libname = find.package("autoscore"), pkgname = "autoscore") {

  ## get potential conflicts and filter out unncessary ones
  confs <- search_conflicts() %>%
    do.call("rbind", .) %>%
    .[(!grepl("%>%", rownames(.))) &
        (!grepl("%||%", rownames(.))) &
        (.$rowname != ".GlobalEnv"),] %>%
    data.frame
  if (dim(confs)[1] == 0){
    confs_msg <- text_col(paste0(crayon::green(cli::symbol$tick), " No potential conflicts found"))
    helper_msg <- ""
  } else {
    confs_msg <- text_col(paste0(crayon::yellow(cli::symbol$cross),
                                 " The autoscore::", rownames(confs), "() function has the same name as ",
                                 gsub("package:", "", confs$rowname), "::", rownames(confs), " (",
                                 sapply(paste0(gsub("package:", "", confs$rowname), "::", rownames(confs)),
                                        function(x) class(eval(parse(text = x)))[1]), ")\n"))
    helper_msg <- text_col(crayon::italic("   Consider using `autoscore::` for each function call."))
  }

  packageStartupMessage(text_col(cli::rule(left = paste0("autoscore ", autoscore_version("autoscore")),
                                           right = "learn more at tysonbarrett.com")),
                        text_col(paste0("\n", crayon::green(cli::symbol$tick), " autoscore attached\n")),
                        confs_msg,
                        helper_msg)

}

.onLoad <- function(libname = find.package("autoscore"), pkgname = "autoscore"){
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", "target", "response", "acceptable", "alternate", "acceptable_response", "diff_target",
                             "diff_response", "human", "count_target", "homophone", "diff_target_pre",
                             "diff_response_pre", "count_target", "homophone_target", "homophone_response",
                             "alternate_string", "in_it1", "in_it2", "in_it3", "in_it4", "in_it5", "plural_add_rule",
                             "plural_rule", "tense_add_rule", "tense_rule"))
  }
  invisible()
}
