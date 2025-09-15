#' Get the proportion of the interaction of variables
#'
#' A wrapper around [svy_interact()] to get the proportion of the interaction of variables, and using the Kobo tool to get labels.
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+]
#'   Variables to calculate interaction proportions from.
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed choices: NULL | df[1+, (label = character, name = character, ...)]
#'   The choices sheet from Kobo. If not NULL, the function tries to retrieve labels for choices. If not NULL, the function tries to retrieve labels.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed label_survey: logical[1]
#'   Whether to retrieve labels of vars from the survey sheet. (default: TRUE)
#'
#' @inheritParams svy_interact
#'
kobo_interact <- function(
  design,
  vars,
  interact_key_sep = " -/- ",
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
    level = level,
    separators = interact_key_sep
  )

  # label_survey is a logical
  checkmate::assert_logical(label_survey, len = 1, any.missing = FALSE)

  # na_rm is a logical
  checkmate::assert_logical(na_rm, len = 1, any.missing = FALSE)

  # vartype is a character string
  checkmate::assert_string(vartype)

  # level is a number between 0 and 1
  checkmate::assert_number(level, lower = 0, upper = 1, finite = TRUE)

  #------ Run analysis

  # set to NULL
  analysis <- . <- analysis_key <- interact_key <- interact_key_value <- var_label <- var_value_label <- var <- NULL

  # Calculate proportion of interactions
  interaction <- svy_interact(
    design = design,
    interact = vars,
    interact_key_sep = interact_key_sep,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level
  )

  # Type of analysis is interact
  if (nrow(interaction) > 0) {
    interaction[, analysis := "interact"]
  }

  # Get labels if requested
  if ((label_survey || !is.null(choices)) && nrow(interaction) > 0) {
    # Split interact_key and interact_key_value for processing
    # First, create expanded data for each variable in the interaction
    interaction_expanded <- interaction[,
      .(
        analysis_key,
        interact_var = unlist(strsplit(
          interact_key,
          interact_key_sep,
          fixed = TRUE
        )),
        interact_val = unlist(strsplit(
          interact_key_value,
          interact_key_sep,
          fixed = TRUE
        ))
      ),
      by = seq_len(nrow(interaction))
    ]

    # Add variable labels if requested
    if (label_survey) {
      var_labels <- get_survey_labels(
        survey = survey,
        vars = vars,
        output_df = TRUE
      )
      data.table::setnames(var_labels, old = "label", new = "var_label")

      # Merge variable labels
      interaction_expanded <- data.table::merge.data.table(
        x = interaction_expanded,
        y = var_labels,
        by.x = "interact_var",
        by.y = "name",
        all.x = TRUE
      )

      # Aggregate variable labels back to original rows
      var_label_agg <- interaction_expanded[,
        .(
          var_label = paste(var_label, collapse = interact_key_sep)
        ),
        by = analysis_key
      ]

      # Merge back to main interaction data
      interaction <- data.table::merge.data.table(
        x = interaction,
        y = var_label_agg,
        by = "analysis_key",
        all.x = TRUE
      )
    }

    # Add choice labels if provided
    if (!is.null(choices)) {
      # Get labels for all variables
      var_value_labels_list <- purrr::map(vars, function(x) {
        lab <- get_survey_choices(
          survey = survey,
          choices = choices,
          x,
          label = TRUE
        )
        lab[,
          c("label", "name") := lapply(.SD, as.character),
          .SDcols = c("label", "name")
        ]
        lab[, var := x]
        return(lab)
      })

      var_value_labels <- data.table::rbindlist(
        var_value_labels_list,
        fill = TRUE
      )
      data.table::setnames(
        var_value_labels,
        old = "label",
        new = "var_value_label"
      )

      # Merge choice labels with expanded interaction data
      interaction_expanded <- data.table::merge.data.table(
        x = interaction_expanded,
        y = var_value_labels,
        by.x = c("interact_var", "interact_val"),
        by.y = c("var", "name"),
        all.x = TRUE
      )

      # Aggregate choice labels back to original rows
      var_value_label_agg <- interaction_expanded[,
        .(
          var_value_label = paste(var_value_label, collapse = interact_key_sep)
        ),
        by = analysis_key
      ]

      # Merge back to main interaction data
      interaction <- data.table::merge.data.table(
        x = interaction,
        y = var_value_label_agg,
        by = "analysis_key",
        all.x = TRUE
      )
    }
  }

  return(interaction)
}
