#' Get all select_one proportions
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed choices: NULL | df[1+, (label = character, name = character, ...)]
#'   The choices sheet from Kobo. If not NULL, the function tries to retrieve labels for choices. If not NULL, the function tries to retrieve labels.
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @inheritParams svy_proportion
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_select_one_all <- function(
  design,
  survey,
  choices = NULL,
  group = NULL,
  group_key_sep = " -/- ",
  label_survey = TRUE,
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95
) {
  select_ones <- get_survey_select_one(survey)

  # select_ones that are not grouping columns
  select_ones <- select_ones[!(select_ones %in% group)]

  # if no variable to analyze
  if (length(select_ones) == 0) {
    rlang::warn(
      "There are no vars of type select_one which are not grouping columns. An empty data.table is returned."
    )
    return(data.table::data.table())
  }

  proportions <- kobo_select_one(
    design = design,
    vars = select_ones,
    survey = survey,
    choices = choices,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  return(proportions)
}
