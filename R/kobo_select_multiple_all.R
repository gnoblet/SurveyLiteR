#' Get all select_multiples proportions
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed choices: df[1+, (label = character, name = character, ...)]
#'   The choices sheet from Kobo to retrieve labels for choices.
#' @typed choices_sep: character[1]
#'   Separator used in the Kobo extracted variable names for select_multiple questions. Default to "/".
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. Default to TRUE.
#' @typed label_choices: logical[1]
#'   Whether to retrieve choices label from the choices sheet. Default to TRUE.
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
kobo_select_multiple_all <- function(
  design,
  survey,
  choices,
  group = NULL,
  group_key_sep = " -/- ",
  choices_sep = "/",
  label_survey = TRUE,
  label_choices = TRUE,
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95
) {
  select_multiples <- get_survey_select_multiple(survey)

  # If no variable to analyze
  if (length(select_multiples) == 0) {
    rlang::warn(
      "There are no vars of type select_multiple. An empty data.table is returned."
    )

    return(data.table::data.table())
  }

  proportions <- kobo_select_multiple(
    design = design,
    vars = select_multiples,
    survey = survey,
    choices = choices,
    choices_sep = choices_sep,
    group = group,
    group_key_sep = group_key_sep,
    label_survey = label_survey,
    label_choices = label_choices,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  return(proportions)
}
