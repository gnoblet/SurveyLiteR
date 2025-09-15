#' @title Survey interactions
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed interact: character[2+]
#'  Variables to calculate interactions from.
#' @typed interact_key_sep:
#'   A character string to separate interactions columns in a fancy 'interact_key" column. (default: " -/- ")
#' @typed group: character[1+] | NULL
#'  Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column.
#' @typed na_rm: logical[1]
#'   Whether to remove NAs from `interact`. (default: TRUE)
#' @typed ak: logical[1]
#'   Whether to add an analysis key column. (default: TRUE)
#' @typed ak_overall_sep: character[1]
#'   The overall separator between items, e.g. between the type of analysis and the variables information.
#' @typed ak_main_sep: character[1]
#'   The main separator between variables, e.g. between the two grouping columns.
#' @typed ak_var_to_value_sep: character[1]
#'   The separator between the variable and its value.
#' @param ... Other parameters to pass to `srvyr::survey_mean()`.
#'
#' @inheritParams srvyr::survey_mean
#'
#' @family survey analysis functions
#'
#' @typedreturn
#'   A survey-summarized-mean data frame
#'
#' @export
svy_interact <- function(
  design,
  interact,
  interact_key_sep = " -/- ",
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  ak = TRUE,
  ak_overall_sep = " @/@ ",
  ak_main_sep = " -/- ",
  ak_var_to_value_sep = " %/% ",
  ...
) {
  #------ Checks

  # Validate common svy_ function parameters
  svy_param_check(
    design = design,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    separators = c(
      interact_key_sep,
      ak_overall_sep,
      ak_main_sep,
      ak_var_to_value_sep
    )
  )

  # interact are in design (interaction-specific validation)
  checkmate::assert_character(interact, min.len = 2, any.missing = FALSE)
  check_missing_vars(design$variables, interact)

  # interact columns are not grouping columns
  if (!is.null(group) && any(interact %in% group)) {
    rlang::abort(
      "Grouping columns in `group` should be different than those in `interact`."
    )
  }

  # If interact is of length 1, throw a warning saying that it is not an interaction and it revert back to "svy_proportion()"
  if (length(interact) == 1) {
    rlang::warn(
      "The 'interact' argument is of length 1. It is not an interaction and the calculation is the same as 'svy_proportion()'."
    )
  }

  #------ Body

  # set to NULL
  n_unw <- NULL

  # Get number of rows
  n_tot <- nrow(design)

  # Remove NAs
  design_no_na <- srvyr::drop_na(design, {{ interact }})
  na_count_tot <- nrow(design) - nrow(design_no_na)
  if (na_rm) {
    design <- design_no_na
  }

  # Group design for calculation
  to_return <- srvyr::group_by(
    design,
    srvyr::across({{ group }}),
    srvyr::interact(interaction = srvyr::across({{ interact }}))
  )

  # Summarize design
  # - stat: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "stat" := srvyr::survey_mean(vartype = vartype, level = level, ...),
    "n_unw" := srvyr::unweighted(srvyr::n())
  )

  # as prop.table
  to_return <- data.table::copy(data.table::as.data.table(to_return))

  # Add stat type, unweughted proportions, total unweighted by group, and total n and total NAs
  to_return[,
    `:=`(
      stat_type = "proportion",
      stat_unw = prop.table(n_unw),
      n_tot_unw = sum(n_unw, na.rm = FALSE),
      n_tot = n_tot,
      na_count_tot = na_count_tot
    ),
    by = group
  ]

  # add total number of obs and total number od NAs
  to_return[, `:=`(n_tot = n_tot, na_count_tot = na_count_tot)]

  # set n_unw before n_tot_unw
  to_return <- set_order_before(
    df = to_return,
    vars = "n_unw",
    target_var = "n_tot_unw"
  )

  # rename interaction vars that are called "interactiom.varX"
  data.table::setnames(
    to_return,
    old = paste0("interaction.", interact),
    new = interact
  )

  # Get the interact keys and values
  to_return <- add_interact_key(
    df = to_return,
    interact = interact,
    interact_key_sep = interact_key_sep,
    before = "stat"
  )

  # Get the group keys and values
  if (!is.null(group) && length(group) > 0) {
    to_return <- add_group_key(
      df = to_return,
      group = group,
      group_key_sep = group_key_sep,
      before = "stat"
    )
  } else {
    to_return[, `:=`(
      group_key = NA_character_,
      group_key_value = NA_character_
    )]
    set_order_before(
      df = to_return,
      vars = c("group_key", "group_key_value"),
      target_var = "stat"
    )
  }

  # Add the analysis key
  if (ak) {
    to_return <- add_analysis_key(
      results = to_return,
      group_key_sep = group_key_sep,
      var_name = "interact_key",
      var_value_name = "interact_key_value",
      var_key_sep = interact_key_sep,
      overall_sep = ak_overall_sep,
      main_sep = ak_main_sep,
      var_to_value_sep = ak_var_to_value_sep
    )
  }

  return(to_return)
}
