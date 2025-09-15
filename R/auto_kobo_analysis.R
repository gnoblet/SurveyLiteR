#' @title Auto survey from a Kobo tool analysis
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed survey: df[1+, (name = character, type = character, ...)]
#'   A survey sheet.
#' @typed choices: df[1+, (label = character, name = character, ...)]
#'   A choices sheet.
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column. (default: " -/- ")
#' @typed na_rm: logical[1]
#'   Whether NAs in 'var' should be removed. (default: TRUE)
#' @typed bind: logical[1]
#'   Whether to output a data.table or a list. (default: TRUE for data.table)
#'
#' @section Automation:
#'
#' [auto_kobo_analysis] performs a very basic automated analysis using the provided Kobo tool.
#'
#' @inheritParams kobo_select_multiple_all
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A survey-summarized-median data frame
#'
#' @export
#'
auto_kobo_analysis <- function(
  design,
  survey,
  choices,
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  choices_sep = "/",
  label_survey = TRUE,
  bind = TRUE
) {
  num_median_an <- kobo_median_all(
    design = design,
    survey = survey,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )
  num_mean_an <- kobo_mean_all(
    design = design,
    survey = survey,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  select_ones_an <- kobo_select_one_all(
    design = design,
    survey = survey,
    choices = choices,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  select_multiples_an <- kobo_select_multiple_all(
    design = design,
    survey = survey,
    choices = choices,
    group = group,
    group_key_sep = group_key_sep,
    choices_sep = choices_sep,
    label_survey = label_survey,
    label_choices = TRUE,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  an <- list(num_median_an, num_mean_an, select_ones_an, select_multiples_an)

  if (!bind) {
    return(an)
  } else {
    an <- data.table::rbindlist(an, fill = TRUE)

    return(an)
  }
}
