#' Get the proportion for a select_one
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate proportions from.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed choices: NULL | df[1+, (label = character, name = character, ...)]
#'   The choices sheet from Kobo. If not NULL, the function tries to retrieve labels for choices.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. (default: TRUE)
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_proportion
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_select_one <- function(
  design,
  vars,
  survey,
  choices = NULL,
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
    choices = choices,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  #------ Run analysis

  # set to NULL
  analysis <- NULL

  # Get select_one variables from survey
  select_ones <- get_survey_select_one(survey)

  # select_ones that exists in design
  if (any(!(vars %in% select_ones))) {
    vars_not_select_one <- vars[!(vars %in% select_ones)]

    rlang::abort(
      c(
        "Variable is not a select_one in survey.",
        "i" = glue::glue(
          "You may check that column ",
          glue::glue_collapse(vars_not_select_one, sep = ", ", last = " and "),
          " is/are a `select_one` in 'survey'. Maybe verify that the survey sheet is the right and most updated version."
        )
      )
    )
  }

  # Calculate proportion
  proportion <- svy_proportion(
    design = design,
    vars = vars,
    group = group,
    group_key_sep = group_key_sep,
    vartype = vartype,
    level = level,
    na_rm = na_rm
  )

  # if not data.table, convert taking copy
  if (!data.table::is.data.table(proportion)) {
    proportion <- data.table::copy(data.table::as.data.table(proportion))
  }

  # Type of analysis is select_multiple
  proportion[, analysis := "select_one"]

  if (label_survey) {
    label <- get_survey_labels(survey = survey, vars = vars, output_df = TRUE)
    data.table::setnames(label, old = "label", new = "var_label")
    proportion <- data.table::merge.data.table(
      x = proportion,
      y = label,
      by.x = "var",
      by.y = "name",
      all.x = TRUE
    )
  }

  if (!is.null(choices)) {
    labels <- purrr::map(vars, \(x) {
      lab <- get_survey_choices(
        survey,
        choices,
        x,
        label = TRUE
      )
      # as character .SD for label and name
      lab[,
        c("label", "name") := lapply(.SD, as.character),
        .SDcols = c("label", "name")
      ]
    })

    labels <- data.table::rbindlist(labels, fill = TRUE)
    data.table::setnames(labels, old = "label", new = "var_value_label")

    proportion <- data.table::merge.data.table(
      x = proportion,
      y = labels,
      by.x = c("var", "var_value"),
      by.y = c("col", "name"),
      all.x = TRUE
    )
  }

  return(proportion)
}
