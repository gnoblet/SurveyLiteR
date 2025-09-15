#' Get all numeric variables's medians
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @inheritParams svy_median
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_median_all <- function(
  design,
  survey,
  group = NULL,
  group_key_sep = " -/- ",
  label_survey = TRUE,
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95
) {
  suppressWarnings(
    num_vars <- c(
      get_survey_decimal(survey),
      get_survey_calculate(survey),
      get_survey_integer(survey)
    )
  )

  # if no variable to analyze
  if (length(num_vars) == 0) {
    rlang::warn(
      "There are no vars of types decimal, calculate or integer. An empty data.table is returned."
    )
    return(data.table::data.table())
  }

  # numeric variables that are not grouping columns
  vars_nin_group <- setdiff(num_vars, group)

  # Check if any numeric variables are used for grouping
  vars_in_group <- intersect(num_vars, group)
  if (length(vars_in_group) > 0) {
    if (length(vars_nin_group) == 0) {
      # All numeric variables are used for grouping - abort
      rlang::abort(c(
        "Grouping columns in `group` should be different than the numeric variables.",
        "i" = glue::glue(
          "The following numeric variables are used for grouping: ",
          glue::glue_collapse(vars_in_group, sep = ", ", last = " and ")
        )
      ))
    } else {
      # Some numeric variables are used for grouping - warn but continue
      rlang::warn(c(
        "Some numeric variables are used for grouping and won't be analyzed.",
        "i" = glue::glue(
          "The following numeric variables are used for grouping: ",
          glue::glue_collapse(vars_in_group, sep = ", ", last = " and ")
        )
      ))
    }
  }

  medians <- kobo_median(
    design = design,
    vars = vars_nin_group,
    survey = survey,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  return(medians)
}
