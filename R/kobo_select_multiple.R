#' Get the proportion for a select_multiple
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate proportions from.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed choices: NULL | df[1+, (label = character, name = character, ...)]
#'   The choices sheet from Kobo. If not NULL, the function tries to retrieve labels for choices.
#' @typed choices_sep: character[1]
#'   Separator used in the Kobo extracted variable names for select_multiple questions. Default to "/".
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. Default to TRUE.
#' @typed label_choices: logical[1]
#'   Whether to retrieve choices label from the choices sheet. Default to TRUE.
#'
#' @inheritParams svy_mean
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @section Not removing missing values:
#'
#' The rationale when not removing missing values is the following:
#'
#' * Missing values for each dummy 1/0 column corresponding to response option are recoded to 0
#' * Then, the mean of 0s and 1s is computed, thus the % for all response options
#'
#' This allows to calculate the % for each choice over the whole dataset.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @export
kobo_select_multiple <- function(
  design,
  vars,
  survey,
  choices = NULL,
  choices_sep = "/",
  group = NULL,
  group_key_sep = " -/- ",
  label_survey = TRUE,
  label_choices = TRUE,
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95
) {
  #------ Check

  # common kobo_ function parameters
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

  # vars are select multiples (restrictive for a reason)
  select_multiples <- get_survey_select_multiple(survey)

  if (any(!(vars %in% select_multiples))) {
    vars_not_select_multiple <- vars[!(vars %in% select_multiples)]

    rlang::abort(
      c(
        "Variable is not a select_multiple in survey.",
        "i" = glue::glue(
          "You may check that column ",
          glue::glue_collapse(
            vars_not_select_multiple,
            sep = ", ",
            last = " and "
          ),
          " is/are a `select_multiple` in 'survey'. Maybe verify that the survey sheet is the right and most updated version."
        )
      )
    )
  }

  # select_multiples that are a grouping column
  # not needed for a select multiple, right, has it should be for child columns
  # Should we check for child columns? / most likely but also unlikely to be a use and break, ok for now
  # if (var %in% group) rlang::abort("Grouping columns in `group` should be different than `var`.")

  #------ Body

  # set to NULL
  child <- name <- var_value <- var_value_label <- analysis <- NULL

  # Should an option to calculate the select multiple over the whole dataset, i.e., replacing NA by 0 be done?
  # Yes

  make_kobo_select_multiple <- function(
    design,
    var,
    survey,
    choices,
    choices_sep,
    group,
    group_key_sep,
    label_survey,
    label_choices,
    na_rm,
    vartype,
    level
  ) {
    # Get child columns
    select_multiple_child <- get_survey_choices(
      survey = survey,
      choices = choices,
      col = var,
      sep = choices_sep
    )

    # Keep only childs that are in the datasets (on purpose)
    select_multiple_child_in_design <- subvec_in(
      select_multiple_child,
      colnames(design)
    )

    # STOP if there is not child column found in the dataset
    if (length(select_multiple_child_in_design) == 0) {
      rlang::abort(
        c(
          "No child column.",
          "*" = glue::glue(
            "There is no child column found for column '{var}' in `design`. The 1/0 child columns are needed for calculation."
          ),
          "i" = glue::glue(
            "You may check that column '{var}' is a `select_multiple` question. You may also check that the provided separator for choices is the right one (arg 'choices_sep' default to '/'). Finally, you can verify that the survey sheet is the right and most updated version."
          )
        )
      )
    }

    # if SOME child columns are in survey but not in the dataset, warn that they were discarded.
    select_multiple_child_not_in_design <- subvec_not_in(
      select_multiple_child,
      select_multiple_child_in_design
    )
    if (length(select_multiple_child_not_in_design) > 0) {
      rlang::warn(glue::glue(paste0(
        "The following child columns of '{var}' does not exist in `design`, there are removed of the calculation: ",
        paste(
          select_multiple_child_not_in_design,
          collapse = ", "
        )
      )))
    }

    # Get labels for later and then unite the name and column to grab the child columns and filter out the non-existent ones
    labs <- get_survey_choices(
      survey = survey,
      choices = choices,
      col = var,
      sep = choices_sep,
      label = TRUE
    )
    labs[, child := paste(col, name, sep = choices_sep)]

    labs <- labs[!(child %in% select_multiple_child_not_in_design), ]

    # If na_rm, recoded missing values to zero
    if (!na_rm) {
      design <- srvyr::mutate(
        design,
        srvyr::across(
          tidyselect::all_of(select_multiple_child_in_design),
          \(x) data.table::nafill(x, fill = 0)
        )
      )
    }

    # Calculate proportions as means of 0/1 dummy columns
    proportions <- purrr::map2(
      select_multiple_child_in_design,
      labs[["name"]],
      \(x, y) {
        # Get proportion per choice
        proportion <- svy_mean(
          design = design,
          vars = x,
          group = group,
          group_key_sep = group_key_sep,
          vartype = vartype,
          na_rm = na_rm,
          level = level
        )

        # Remove the overall name and replace by child choices only
        proportion <- deselect(df = proportion, vars = "var")

        proportion[, var_value := y]
        proportion <- set_order_before(
          df = proportion,
          vars = "var_value",
          target_var = "stat"
        )

        return(proportion)
      }
    )

    # Bind the list
    proportions <- data.table::rbindlist(proportions, fill = TRUE)

    # Statistic type is proportion
    # proportions[["stat_type"]] <- "proportion"
    # To be seen with RD and update the analysis key accordingly?

    # Type of analysis is select_multiple
    proportions[, analysis := "select_multiple"]

    proportions[, var := var]
    proportions <- set_order_before(
      df = proportions,
      vars = "var",
      target_var = "stat"
    )

    if (label_survey) {
      label <- get_survey_labels(
        survey = survey,
        vars = var,
        output_df = FALSE
      )

      proportions[,
        "var_label" := data.table::fifelse(
          length(label) == 0,
          NA_character_,
          label
        )
      ]
    }

    if (label_choices && !is.null(choices)) {
      data.table::setnames(
        labs,
        old = "label",
        new = "var_value_label"
      )
      labs[, name := as.character(name)]

      labs <- deselect(df = labs, vars = c("child", "col"))

      proportions[, var_value := as.character(var_value)]

      proportions <- data.table::merge.data.table(
        x = proportions,
        y = labs,
        by.x = "var_value",
        by.y = "name",
        all.x = TRUE
      )
    }

    return(proportions)
  }

  analysis <- purrr::map(
    vars,
    \(x) {
      an <- make_kobo_select_multiple(
        design = design,
        var = x,
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

      an[, var_value := as.character(var_value)]
    },
    .progress = TRUE
  )

  analysis <- data.table::rbindlist(analysis)

  # Ensure correct column order: group_key, group_key_value, var, var_value, stat
  analysis <- set_order_before(
    df = analysis,
    vars = "var_value",
    target_var = "stat"
  )

  return(analysis)
}
