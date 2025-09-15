#' Get the mean for a numeric var
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate means from.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column.
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_mean
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_mean <- function(
  design,
  vars,
  survey,
  group = NULL,
  group_key_sep = " -/- ",
  label_survey = TRUE,
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95
) {
  #------ Checks

  # Validate common kobo_ function parameters
  kobo_param_check(
    design = design,
    vars = vars,
    survey = survey,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  #------ Gather arguments

  # set to NULL
  analysis <- NULL

  # Checks if 1. there are no numeric variables, and 2. that vars are in survey
  suppressWarnings(
    num_vars <- c(
      get_survey_decimal(survey),
      get_survey_calculate(survey),
      get_survey_integer(survey)
    )
  )

  # 1.
  if (length(num_vars) == 0) {
    rlang::warn(
      "There are no vars of types decimal, calculate or integer. An empty data.table is returned."
    )
    return(data.table::data.table())
  }

  # 2.
  if (any(!(vars %in% num_vars))) {
    vars_not_num_vars <- vars[!(vars %in% num_vars)]

    rlang::abort(
      c(
        "Variable is not a numeric variable in survey (calculate, decimal, integer).",
        "i" = glue::glue(
          "You may check that column ",
          glue::glue_collapse(vars_not_num_vars, sep = ", ", last = " and "),
          " is/are either a 'calculate', 'decimal' or 'integer' in 'survey'. Maybe verify that the survey sheet is the right and most updated version."
        )
      )
    )
  }

  # Calculate proportion
  mean <- svy_mean(
    design = design,
    vars = vars,
    group = group,
    group_key_sep = group_key_sep,
    vartype = vartype,
    level = level,
    na_rm = na_rm
  )

  # Type of analysis is numeric
  if (nrow(mean) > 0) {
    mean[, analysis := "mean"]
  }

  if (label_survey) {
    label <- get_survey_labels(survey = survey, vars = vars, output_df = TRUE)
    data.table::setnames(label, old = "label", new = "var_label")
    mean <- data.table::merge.data.table(
      x = mean,
      y = label,
      by.x = "var",
      by.y = "name",
      all.x = TRUE
    )
  }

  return(mean)
}
