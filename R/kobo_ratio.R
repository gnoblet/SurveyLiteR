#' Get the ratio for numeric variables.
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. (default: TRUE)
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_ratio
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_ratio <- function(
  design,
  nums,
  denoms,
  ratio_key_sep = " -/- ",
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
    nums = nums,
    denoms = denoms,
    survey = survey,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    separators = ratio_key_sep
  )

  #------ Run analysis

  # set to NULL
  var_label <- var_label_num <- var_label_denom <- analysis <- var <- NULL

  # gather nums and denoms
  vars <- c(nums, denoms)

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
  ratio <- svy_ratio(
    design = design,
    nums = nums,
    denoms = denoms,
    ratio_key_sep = ratio_key_sep,
    group = group,
    group_key_sep = group_key_sep,
    vartype = vartype,
    level = level,
    na_rm = na_rm
  )

  # Type of analysis is select_multiple
  if (nrow(ratio) > 0) {
    ratio[, analysis := "ratio"]
  }

  if (label_survey && nrow(ratio) > 0) {
    label_nums <- get_survey_labels(
      survey = survey,
      vars = nums,
      output_df = TRUE
    )
    label_denoms <- get_survey_labels(
      survey = survey,
      vars = denoms,
      output_df = TRUE
    )

    ratio[,
      c("var_num", "var_denom") := data.table::tstrsplit(
        var,
        ratio_key_sep,
        fixed = TRUE
      )
    ]

    label_nums <- data.table::setnames(
      label_nums,
      old = "label",
      new = "var_label_num"
    )
    label_denoms <- data.table::setnames(
      label_denoms,
      old = "label",
      new = "var_label_denom"
    )

    ratio <- data.table::merge.data.table(
      x = ratio,
      y = label_nums,
      by.x = "var_num",
      by.y = "name",
      all.x = TRUE
    )
    ratio <- data.table::merge.data.table(
      x = ratio,
      y = label_denoms,
      by.x = "var_denom",
      by.y = "name",
      all.x = TRUE
    )

    ratio[,
      var_label := paste0(
        var_label_num,
        ratio_key_sep,
        var_label_denom
      )
    ]
    ratio[, c("var_num", "var_denom") := NULL]
  }

  return(ratio)
}
