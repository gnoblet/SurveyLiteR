#' @title Survey mean
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate means from.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column. (default: " -/- ")
#' @typed na_rm: logical[1]
#'   Whether NAs in 'var' should be removed. (default: TRUE)
#' @typed ak: logical[1]
#'   Whether to add an analysis key column. (default: TRUE)
#' @typed ak_overall_sep: character[1]
#'   The overall separator between items, e.g. between the type of analysis and the variables information. (default: " @/@ ")
#' @typed ak_main_sep: character[1]
#'   The main separator between variables, e.g. between the two grouping columns. (default: " -/- ")
#' @typed ak_var_to_value_sep: character[1]
#'   The separator between the variable and its value. (default: " %/% ")
#' @param ... Other parameters to pass to `srvyr::survey_mean()`.
#'
#' @inheritParams srvyr::survey_mean
#'
#' @family survey analysis functions
#'
#' @typedreturn
#'   A survey-summarized-mean data frame
#'
#' @export
svy_mean <- function(
  design,
  vars,
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  ak = TRUE,
  ak_overall_sep = " @/@ ",
  ak_main_sep = " -/- ",
  ak_var_to_value_sep = " %/% ",
  ...
) {
  #------ Checks

  # Validate common svy_ function parameters
  svy_param_check(
    design = design,
    vars = vars,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    separators = c(ak_overall_sep, ak_main_sep, ak_var_to_value_sep)
  )

  #------ Body

  analysis <- svy_make(
    design = design,
    vars = vars,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE,
    var_column_name = "var",
    add_var_value = TRUE,
    ...
  )

  # if analysis is empty df, return
  if (nrow(analysis) == 0) {
    return(analysis)
  }

  # Add the analysis key
  if (ak) {
    analysis <- add_analysis_key(
      results = analysis,
      group_key_sep = group_key_sep,
      overall_sep = ak_overall_sep,
      main_sep = ak_main_sep,
      var_to_value_sep = ak_var_to_value_sep
    )
  }

  return(analysis)
}
