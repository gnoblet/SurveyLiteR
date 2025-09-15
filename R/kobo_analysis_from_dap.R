#' Kobo survey analysis from a data analysis plan
#'
#' [kobo_analysis_from_dap] performs a survey analysis from a data analysis plan, while [kobo_analysis_from_dap_group] is a wrapper around the former to run multiple grouped analyses at once.
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed dap: df
#'   A well formatted data analysis plan.
#'
#' @section Specifics:
#'
#' A well formatted data analysis plan is a data frame with at least three columns: `analysis`, `vars`, `na_rm`, where,
#'
#' + `analysis` is one of the following types: "mean", "median", "select_multiple", "select_one", "ratio", "interact".
#' + `vars` are the variables to analyze. For "mean", "median", "select_multiple", and "select_one", it is a single variable. For "ratio", it is two variables separated by a comma, where the first one is the numerator and the second one the denominator. For "interact", it is two or more variables separated by commas, which will be interacted.
#' + `na_rm` is either "yes" or "no", indicating whether NAs should be removed for that analysis.
#'
#' * It takes the default for most variables, e.g. it tries to label, it calculates confidence intervals, and separators are the default.
#' * Choices cannot be NULL.
#' * The separator for choices can be changed depending on how your dataset's variable names are formatted.
#' * Level can be changed.
#'
#'
#' @inheritParams kobo_analysis
#'
#' @family functions for analyzing from Kobo tool
#'
#' @typedreturn
#'   A survey-summarized-median data frame
#'
#' @export
#'
kobo_analysis_from_dap <- function(
  design,
  dap,
  survey,
  choices,
  group = NULL,
  level = 0.95,
  choices_sep = "/"
) {
  #------ Checks

  # dap is a data frame
  checkmate::assert_data_frame(dap)

  # dap is a data.table
  if (!data.table::is.data.table(dap)) {
    dap <- data.table::as.data.table(dap)
  }

  # survey is a data frame with the right columns
  kobo_param_check(
    design = design,
    survey = survey,
    choices = choices,
    group = group
  )

  # all necesarry columns are here
  check_missing_vars(dap, c("analysis", "var", "na_rm"))

  # types are the right ones
  analysis_type <- c(
    "mean",
    "median",
    "select_multiple",
    "select_one",
    "ratio",
    "interact"
  )
  check_vars_in_set(
    df = dap,
    vars = "analysis",
    set = analysis_type,
    allow_missing = FALSE
  )

  # dap is not empty
  if (nrow(dap) == 0) {
    rlang::abort(
      "'dap' does not contain any line. Please provide a non-empty data analysis plan."
    )
  }

  #------ Run analysis

  # set to NULL
  id_analysis <- . <- var <- na_rm <- NULL

  # Create a id for analysis
  dap[, id_analysis := paste0("analysis_", .I)]

  # Split to map out the analyses
  split_dap <- named_group_split(dap, "id_analysis")

  # Map out the analysis
  analysis <- purrr::pmap(
    dap[, .(analysis, var, na_rm, id_analysis)],
    \(analysis, var, na_rm, id_analysis) {
      if (na_rm == "yes") {
        na_rm_lgl <- TRUE
      } else {
        na_rm_lgl <- FALSE
      }

      # For all analyses, but ratio, it's straightforward
      if (analysis == "ratio") {
        # For ratio, splitting "var" is needed
        ratio <- stringr::str_split_1(var, ",")
        # reremove any white space around the variable names
        ratio <- stringr::str_squish(ratio)
        # Prepare named vector
        var <- ratio[2]
        names(var) <- ratio[1]
      }

      if (analysis == "interact") {
        # For interaction, splitting "var" is needed
        # Split
        var <- stringr::str_split_1(var, ",")
        # squish
        var <- stringr::str_squish(var)
      }

      # Run the analysis
      an <- kobo_analysis(
        design,
        analysis = analysis,
        vars = var,
        survey = survey,
        choices = choices,
        group = group,
        na_rm = na_rm_lgl,
        level = level,
        choices_sep = choices_sep
      )

      an[, id_analysis := id_analysis]
    }
  )

  # Bind all analyses together
  analysis <- data.table::rbindlist(analysis, fill = TRUE)

  # Join back the added columns
  analysis <- data.table::merge.data.table(
    x = analysis,
    y = df_diff(df_a = dap, df_b = analysis, vars_to_keep = "id_analysis"),
    by = "id_analysis",
    all.x = TRUE,
    suffixes = c("", "_y")
  )

  return(analysis)
}


#' @rdname kobo_analysis_from_dap
#'
#' @typed l_group: list[character]
#'   A list of vectors of variables to group by.
#' @typed no_group: logical[1]
#'   If TRUE, the analysis without grouping is run. (default: TRUE)
#'
#' @export
kobo_analysis_from_dap_group <- function(
  design,
  dap,
  survey,
  choices,
  l_group,
  no_group = TRUE,
  level = 0.95,
  choices_sep = "/"
) {
  #------ Checks

  # Check if l_group is a list
  if (!is.list(l_group)) {
    rlang::abort("'l_group' should be a list.")
  }

  # Check if all elements are vectors of variables that exist in the design
  purrr::map(l_group, \(x) checkmate::assertSubset(x, colnames(design)))

  #------ Run analysis

  # Map over group list
  analysis <- purrr::map(
    l_group,
    \(x) {
      an <- kobo_analysis_from_dap(
        design = design,
        dap = dap,
        survey = survey,
        choices = choices,
        group = x,
        level = level,
        choices_sep = choices_sep
      )

      return(an)
    },
    .progress = TRUE
  )

  if (no_group) {
    analysis[["no_grouping"]] <- kobo_analysis_from_dap(
      design = design,
      dap = dap,
      survey = survey,
      choices = choices,
      level = level,
      choices_sep = choices_sep
    )
  }

  # Bind all analyses together
  analysis <- purrr::list_rbind(analysis)

  return(analysis)
}
