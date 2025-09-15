#' @title Auto survey analysis
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed group: character[1+] | NULL
#'  Variables to group by. (default: NULL for no group)
#' @typed group_key_sep: character[1]
#'   The string to separate grouping variables in a fancy 'group_key' column. (default: " -/- ")
#' @typed na_rm: logical[1]
#'   Whether NAs in 'var' should be removed. (default: TRUE)
#' @typed bind:
#'   Whether to output a data.table or a list. (default: TRUE for data.table)
#' @typed weight_var: character[1+] | NULL
#'   Names of weight variables to exclude from analysis. (default: NULL)
#' @typed stratum_var: character[1+] | NULL
#'   Names of stratum variables to exclude from analysis. (default: NULL)
#' @typed cluster_var: character[1+] | NULL
#'   Names of cluster variables to exclude from analysis. (default: NULL)
#' @typed id_var: character[1+] | NULL
#'   Names of ID variables to exclude from analysis. (default: NULL)
#' @typed fpc_var: character[1+] | NULL
#'   Names of finite population correction variables to exclude from analysis. (default: NULL)
#'
#' @section Automation:
#'
#' [auto_svy_analysis] performs a very basic automated analysis. All numeric columns get means and medians. All character columns get a proportion.
#'
#' @inheritParams svy_ratio
#' @inheritParams svy_interact
#' @inheritParams svy_quantile
#'
#' @family survey analysis functions
#'
#' @typedreturn
#'   A survey-summarized-median data frame
#'
#' @export
#'
auto_svy_analysis <- function(
  design,
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  bind = TRUE,
  weight_var = NULL,
  stratum_var = NULL,
  cluster_var = NULL,
  id_var = NULL,
  fpc_var = NULL
) {
  # Validate design object
  checkmate::assert_class(design, "tbl_svy")

  # Validate group parameter
  if (!is.null(group)) {
    checkmate::assert_character(group, min.len = 1, any.missing = FALSE)
    checkmate::assert_subset(group, colnames(design$variables))
  }

  # Get all variable names
  all_vars <- colnames(design$variables)

  # Validate and exclude user-specified survey design variables
  survey_design_vars <- c()

  # Combine all non-NULL parameters
  all_params <- list(weight_var, stratum_var, cluster_var, id_var, fpc_var)
  for (param in all_params) {
    if (!is.null(param) && length(param) > 0) {
      survey_design_vars <- c(survey_design_vars, param)
    }
  }

  # Check that specified survey design variables exist in the data
  if (length(survey_design_vars) > 0) {
    missing_design_vars <- setdiff(survey_design_vars, all_vars)
    if (length(missing_design_vars) > 0) {
      rlang::abort(paste(
        "Specified survey design variables not found in data:",
        paste(missing_design_vars, collapse = ", ")
      ))
    }
  }

  # Get analysis variables (excluding design variables and group variables)
  analysis_vars <- setdiff(all_vars, c(survey_design_vars, group))

  numeric_cols <- analysis_vars[purrr::map_lgl(
    design$variables[analysis_vars],
    \(x) is.numeric(x)
  )]

  # Initialize results list
  an <- list()

  # Run numeric analysis only if there are numeric variables
  if (length(numeric_cols) > 0) {
    numeric_analysis_type <- c("mean", "median")
    numeric_an <- purrr::map(
      numeric_analysis_type,
      \(x) {
        svy_analysis(
          design = design,
          analysis = x,
          vars = numeric_cols,
          group = group,
          group_key_sep = group_key_sep,
          na_rm = na_rm,
          vartype = vartype,
          level = level
        )
      },
      .progress = "numeric"
    )
    an <- c(an, purrr::set_names(numeric_an, numeric_analysis_type))
  }

  character_cols <- analysis_vars[purrr::map_lgl(
    design$variables[analysis_vars],
    \(x) is.character(x)
  )]

  # Run character analysis only if there are character variables
  if (length(character_cols) > 0) {
    char_an <- purrr::map(
      "proportion",
      \(x) {
        svy_analysis(
          design = design,
          analysis = x,
          vars = character_cols,
          group = group,
          group_key_sep = group_key_sep,
          na_rm = na_rm,
          vartype = vartype,
          level = level
        )
      },
      .progress = "character"
    )
    an[["prop"]] <- char_an[[1]]
  }

  if (!bind) {
    return(an)
  } else {
    an <- data.table::rbindlist(an, fill = TRUE)

    return(an)
  }
}
