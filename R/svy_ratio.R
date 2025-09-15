#' @title Survey ratio
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed nums: character[1+]
#'   The numerator variables.
#' @typed denoms: character[1+]
#'   The denominator variables.
#' @typed ratio_key_sep: character[1]
#'   A separator for the ratio key. (default: " -/- ")
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column.
#' @typed na_rm: logical[1]
#'   Whether to remove any line that as an NA in `num` or `denom` Default to TRUE.
#' @typed ak: logical[1]
#'   Whether to add an analysis key column. Default to TRUE.
#' @typed ak_overall_sep: character[1]
#'   The overall separator between items, e.g. between the type of analysis and the variables information.
#' @typed ak_main_sep: character[1]
#'   The main separator between variables, e.g. between the two grouping columns.
#' @typed ak_var_to_value_sep: character[1]
#'   The separator between the variable and its value.
#' @param ... Parameters to pass to `srvyr::survey_ratio()`.
#'
#' @inheritParams srvyr::survey_ratio
#'
#' @family survey analysis functions
#'
#' @typedreturn
#'   A survey-summarized-ratio data frame
#'
#' @export
svy_ratio <- function(
  design,
  nums,
  denoms,
  ratio_key_sep = " -/- ",
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
    nums = nums,
    denoms = denoms,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    separators = c(
      ratio_key_sep,
      ak_overall_sep,
      ak_main_sep,
      ak_var_to_value_sep
    )
  )

  #------ Body

  # set to NULL
  var_value <- NULL

  analysis <- svy_make_ratio(
    design = design,
    nums = nums,
    denoms = denoms,
    ratio_key_sep = ratio_key_sep,
    group = group,

    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    ...
  )

  # if analysis is empty df, return
  if (nrow(analysis) == 0) {
    return(analysis)
  }

  analysis[, var_value := NA_character_]
  set_order_before(analysis, "var_value", "var")

  # Add the analysis key
  if (ak) {
    analysis <- add_analysis_key(
      results = analysis,
      group_key_sep = group_key_sep,
      var_key_sep = ratio_key_sep,
      overall_sep = ak_overall_sep,
      main_sep = ak_main_sep,
      var_to_value_sep = ak_var_to_value_sep
    )
  }

  return(analysis)
}
