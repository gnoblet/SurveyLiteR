#' @title Survey analysis
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed analysis:
#'   Analysis type. See details for more, well, details.
#' @typed vars: character[1+]
#'   Variables to calculate from.
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column.
#' @typed na_rm: logical[1]
#'   Whether NAs in 'var' should be removed. (default: TRUE)
#' @param ... Other parameters to pass to the used `srvyr` method.
#'
#' @section Specificity per type of analysis:
#'
#' The possible analysis type are: mean for [svy_mean()], median for [svy_median()], interact for [svy_interact()], proportion for [svy_proportion()], ratio for [svy_ratio()], or quantile [svy_quantile()].
#'
#' * If `analysis` is "ratio": `vars` takes a named vector. Names will be passed to `nums` in [svy_ratio()] and values to `denoms`.
#' * If `analysis` is "interact": `vars` is the equivalent of `interact`.
#'
#' @inheritParams svy_ratio
#' @inheritParams svy_interact
#' @inheritParams svy_quantile
#'
#' @family survey analysis functions
#'
#' @typedreturn
#'   A survey-summarized-median data frame
#'
#' @export
#'
svy_analysis <- function(
  design,
  analysis,
  vars,
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  ratio_key_sep = " -/- ",
  interact_key_sep = " -/- ",
  quantiles = c(0.25, 0.5, 0.75),
  ak = TRUE,
  ak_overall_sep = " @/@ ",
  ak_main_sep = " -/-",
  ak_var_to_value_sep = " %/% ",
  ...
) {
  #------ Checks

  # analysis is a single string in a list of accepted types
  analysis_type <- c(
    "mean",
    "median",
    "proportion",
    "quantile",
    "ratio",
    "interact"
  )
  checkmate::assert_subset(analysis, analysis_type, empty.ok = FALSE)

  # Validate common svy_ function parameters
  svy_param_check(
    design = design,
    vars = vars,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    separators = c(
      ratio_key_sep,
      interact_key_sep,
      ak_overall_sep,
      ak_main_sep,
      ak_var_to_value_sep
    )
  )

  #------ Analysis

  if (analysis == "mean") {
    an <- svy_mean(
      design = design,
      vars = vars,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ak = ak,
      ak_overall_sep = ak_overall_sep,
      ak_main_sep = ak_main_sep,
      ak_var_to_value_sep = ak_var_to_value_sep,
      ...
    )
  } else if (analysis == "median") {
    an <- svy_median(
      design = design,
      vars = vars,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ak = ak,
      ak_overall_sep = ak_overall_sep,
      ak_main_sep = ak_main_sep,
      ak_var_to_value_sep = ak_var_to_value_sep,
      ...
    )
  } else if (analysis == "proportion") {
    an <- svy_proportion(
      design = design,
      vars = vars,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ak = ak,
      ak_overall_sep = ak_overall_sep,
      ak_main_sep = ak_main_sep,
      ak_var_to_value_sep = ak_var_to_value_sep,
      ...
    )
  } else if (analysis == "ratio") {
    nums <- names(vars)
    denoms <- unname(vars)
    an <- svy_ratio(
      design = design,
      nums = nums,
      denoms = denoms,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ak = ak,
      ak_overall_sep = ak_overall_sep,
      ak_main_sep = ak_main_sep,
      ak_var_to_value_sep = ak_var_to_value_sep,
      ...
    )
  } else if (analysis == "interact") {
    an <- svy_interact(
      design = design,
      interact = vars,
      interact_key_sep = interact_key_sep,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ak = ak,
      ak_overall_sep = ak_overall_sep,
      ak_main_sep = ak_main_sep,
      ak_var_to_value_sep = ak_var_to_value_sep,
      ...
    )
  } else if (analysis == "quantile") {
    an <- svy_quantile(
      design = design,
      vars = vars,
      quantiles = quantiles,
      group = group,
      group_key_sep = group_key_sep,
      na_rm = na_rm,
      vartype = vartype,
      level = level,
      ...
    )

    if (ak) {
      rlang::warn("The analysis key has not been implemented for 'quantile'.")
    }
  } else {
    rlang::abort(paste0(
      "Analysis ",
      analysis,
      "is not implemented yet... or will not. Feel free to reach out!"
    ))
  }

  return(an)
}
