#' @title Survey quantile
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate quantiles from.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column. (default: " -/- ")
#' @typed na_rm: logical[1]
#'   Whether NAs in 'var' should be removed. (default: TRUE)
#' @param ... Other parameters to pass to `srvyr::survey_quantile()`.
#'
#' @inheritParams srvyr::survey_quantile
#'
#' @family survey analysis functions
#'
#' @typedreturn data.frame
#'   A survey-summarized-mean data frame
#'
#' @export
svy_quantile <- function(
  design,
  vars,
  quantiles = c(0.25, 0.5, 0.75),
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
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
    level = level
  )

  # quantiles is numeric between 0 and 1
  checkmate::assert_numeric(
    quantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1
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
    stat_type = "quantile",
    survey_fn = function(x, ...) {
      srvyr::survey_quantile(
        x,
        quantiles = quantiles,
        ...
      )
    },
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE,
    var_column_name = "var",
    add_var_value = FALSE,
    ...
  )

  return(analysis)
}
