#' Internal function to make survey analysis results
#'
#' This is a general helper function used by all svy_* functions to reduce
#' code duplication. It handles the common patterns of survey analysis
#' including NA handling, grouping, survey calculations, and result formatting.
#'
#' @param design A srvyr::design object
#' @param vars Character vector of variables to analyze
#' @param group Character vector of grouping variables (NULL for no grouping)
#' @param group_key_sep Separator for group key values
#' @param na_rm Whether to remove NAs
#' @param vartype Type of variance estimate
#' @param level Confidence level
#' @param stat_type Character string for the type of statistic
#' @param survey_fn Function to call for survey calculation (e.g., srvyr::survey_mean)
#' @param unweighted_fn Function to calculate unweighted statistic (e.g. mean)
#' @param group_by_var Whether to group by the variable itself (for proportions)
#' @param var_column_name What to call the variable column (default "var")
#' @param add_var_value Whether to add a var_value column
#' @param custom_grouping Custom grouping logic (function)
#' @param custom_var_handling Custom variable handling (function)
#' @param ... Additional arguments passed to survey_fn
#'
#' @return A data.table with survey results
#' @keywords internal
svy_make <- function(
  design,
  vars,
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  stat_type = "mean",
  survey_fn,
  unweighted_fn,
  group_by_var = FALSE,
  var_column_name = "var",
  add_var_value = TRUE,
  custom_grouping = NULL,
  custom_var_handling = NULL,
  ...
) {
  make_single <- function(
    design,
    var,
    group,
    group_key_sep,
    na_rm,
    vartype,
    level,
    stat_type,
    survey_fn,
    unweighted_fn,
    group_by_var,
    var_column_name,
    custom_grouping,
    custom_var_handling,
    ...
  ) {
    # set to NULL
    n_unw <- . <- NULL

    # Check if all values are NA
    if (all(is.na(design$variables[[var]]))) {
      rlang::warn(paste0(
        "Variable '",
        var,
        "' only contains missing values. Returning an empty data frame."
      ))
      empty_dt <- data.table::data.table(
        na_count_tot = integer(),
        n_tot = integer(),
        stat = double(),
        stat_unw = double(),
        n_unw = integer(),
        stat_type = character(),
        group_key = character(),
        var_value = character()
      )
      empty_dt[, (var_column_name) := character()]
      return(empty_dt)
    }

    # Get number of NAs and total observations
    na_count_tot <- sum(is.na(design$variables[[var]]))
    n_tot <- nrow(design)

    # Remove NAs if requested
    if (rlang::is_true(na_rm)) {
      design <- srvyr::filter(design, !is.na(!!rlang::sym(var)))
    }

    # Apply custom grouping if provided, otherwise use standard grouping
    if (!is.null(custom_grouping)) {
      to_return <- custom_grouping(design, var, group, group_by_var)
    } else {
      # Standard grouping logic
      if (group_by_var) {
        # Group by both group variables and the analysis variable
        to_return <- srvyr::group_by(
          design,
          srvyr::across(tidyselect::all_of(group)),
          srvyr::across(tidyselect::all_of(var))
        )
      } else {
        # Group only by group variables
        to_return <- srvyr::group_by(
          design,
          srvyr::across(tidyselect::all_of(group))
        )
      }
    }

    # Perform survey calculation
    to_return <- srvyr::summarize(
      to_return,
      stat = survey_fn(
        !!rlang::sym(var),
        vartype = vartype,
        level = level,
        ...
      ),
      stat_unw = srvyr::unweighted(unweighted_fn(!!rlang::sym(var))),
      n_unw = srvyr::unweighted(srvyr::n())
    )

    # Convert to data.table using copy to avoid shallow copy warnings
    to_return <- data.table::copy(data.table::as.data.table(to_return))

    # Add all columns at once to minimize := operations
    if (group_by_var) {
      # For proportions, we need different logic for unweighted proportions
      if (stat_type == "proportion") {
        if (length(group) > 0) {
          to_return[,
            `:=`(
              stat_type = stat_type,
              stat_unw = prop.table(n_unw),
              n_tot_unw = sum(n_unw, na.rm = FALSE),
              n_tot = n_tot,
              na_count_tot = na_count_tot
            ),
            by = c(group)
          ]
        } else {
          to_return[, `:=`(
            stat_type = stat_type,
            stat_unw = prop.table(n_unw),
            n_tot_unw = sum(n_unw, na.rm = FALSE),
            n_tot = n_tot,
            na_count_tot = na_count_tot
          )]
        }
      } else {
        # For other group_by_var cases
        if (length(group) > 0) {
          to_return[,
            `:=`(
              stat_type = stat_type,
              n_tot_unw = sum(n_unw, na.rm = FALSE),
              n_tot = n_tot,
              na_count_tot = na_count_tot
            ),
            by = c(group)
          ]
        } else {
          to_return[, `:=`(
            stat_type = stat_type,
            n_tot_unw = sum(n_unw, na.rm = FALSE),
            n_tot = n_tot,
            na_count_tot = na_count_tot
          )]
        }
      }
    } else {
      # For means, medians, etc., total unweighted is just the count
      to_return[, `:=`(
        stat_type = stat_type,
        n_tot_unw = n_unw,
        n_tot = n_tot,
        na_count_tot = na_count_tot
      )]
    }

    # Apply custom variable handling if provided
    if (!is.null(custom_var_handling)) {
      to_return <- custom_var_handling(to_return, var, var_column_name)
    } else {
      # Standard variable handling
      if (group_by_var) {
        # For proportions, rename the variable column to var_value
        data.table::setnames(to_return, var, "var_value")
        # Add var column before var_value
        to_return[, (var_column_name) := var]
        # Reorder columns to put var column before var_value
        to_return <- set_order_before(
          to_return,
          var_column_name,
          "var_value"
        )
      } else {
        # For means, medians, etc., add var column before stat
        to_return[, (var_column_name) := var]
        # Reorder columns to put var column before stat but not for quantiles
        # see how we'd like to support this in the future
        if ("stat" %in% colnames(to_return)) {
          to_return <- set_order_before(to_return, var_column_name, "stat")
        } else {
          # get first iteration of stat_q column using stringr
          stat_col <- colnames(to_return)[stringr::str_detect(
            colnames(to_return),
            "^stat_q[0-9]+$"
          )]
          if (length(stat_col) == 0) {
            to_return <- to_return
          } else {
            to_return <- set_order_before(
              to_return,
              var_column_name,
              stat_col[1]
            )
          }
        }
      }
    }

    # Add group keys
    if (!is.null(group) && group_key_sep != "") {
      to_return <- add_group_key(
        to_return,
        group,
        group_key_sep,
        before = var_column_name
      )
    } else {
      to_return[,
        c("group_key", "group_key_value") := .(NA_character_, NA_character_)
      ]
      to_return <- set_order_before(
        to_return,
        c("group_key", "group_key_value"),
        var_column_name
      )
    }

    return(to_return)
  }

  # Map over all variables
  analysis <- purrr::map(
    vars,
    function(x) {
      make_single(
        design = design,
        var = x,
        group = group,
        group_key_sep = group_key_sep,
        na_rm = na_rm,
        vartype = vartype,
        level = level,
        stat_type = stat_type,
        survey_fn = survey_fn,
        unweighted_fn = unweighted_fn,
        group_by_var = group_by_var,
        var_column_name = var_column_name,
        custom_grouping = custom_grouping,
        custom_var_handling = custom_var_handling,
        ...
      )
    }
  )

  # Combine results
  analysis <- data.table::rbindlist(analysis, fill = TRUE)

  # Return early if empty
  if (nrow(analysis) == 0) {
    return(analysis)
  }

  # Add var_value column if requested and not already present
  if (add_var_value && !"var_value" %in% colnames(analysis)) {
    analysis[, "var_value" := NA_character_]
    analysis <- set_order_before(analysis, "var_value", var_column_name)
  }

  return(analysis)
}


#' Helper function for ratio calculations
#'
#' Special helper for svy_ratio since it works with pairs of variables
#'
#' @param design A srvyr::design object
#' @param nums Character vector of numerator variables
#' @param denoms Character vector of denominator variables
#' @param ratio_key_sep Separator for ratio keys
#' @param group Character vector of grouping variables
#' @param group_key_sep Separator for group key values
#' @param na_rm Whether to remove NAs
#' @param vartype Type of variance estimate
#' @param level Confidence level
#' @param ... Additional arguments passed to srvyr::survey_ratio
#'
#' @return A data.table with survey ratio results
#' @keywords internal
svy_make_ratio <- function(
  design,
  nums,
  denoms,
  ratio_key_sep = " -/- ",
  group = NULL,
  group_key_sep = " -/- ",
  na_rm = TRUE,
  vartype = "ci",
  level = 0.95,
  ...
) {
  make_single_ratio <- function(
    design,
    num,
    denom,
    ratio_key_sep,
    group,
    group_key_sep,
    na_rm,
    vartype,
    level,
    ...
  ) {
    # set to NULL
    n_unw <- . <- var <- NULL

    # Check if all values are NA in either variable
    if (all(is.na(design$variables[[num]]))) {
      rlang::warn(paste0(
        "Variable '",
        num,
        "' only contains missing values. Returning an empty data frame."
      ))
      return(data.table::data.table(
        na_count_tot = integer(),
        n_tot = integer(),
        stat = double(),
        stat_unw = double(),
        n_unw = integer(),
        stat_type = character(),
        group_key = character(),
        var = character(),
        num = character(),
        denom = character()
      ))
    }

    if (all(is.na(design$variables[[denom]]))) {
      rlang::warn(paste0(
        "Variable '",
        denom,
        "' only contains missing values. Returning an empty data frame."
      ))
      return(data.table::data.table(
        na_count_tot = integer(),
        n_tot = integer(),
        stat = double(),
        stat_unw = double(),
        n_unw = integer(),
        stat_type = character(),
        group_key = character(),
        var = character(),
        num = character(),
        denom = character()
      ))
    }

    # Get number of NAs (either in num or denom)
    na_count_tot <- sum(
      is.na(design$variables[[denom]]) |
        is.na(design$variables[[num]])
    )
    n_tot <- nrow(design)

    # Remove NAs if requested
    if (na_rm) {
      design <- srvyr::filter(
        .data = design,
        !is.na(!!rlang::sym(num)) & !is.na(!!rlang::sym(denom))
      )
    }

    # Group design
    to_return <- srvyr::group_by(
      .data = design,
      srvyr::across(tidyselect::all_of(group))
    )

    # Survey ratio calculation
    to_return <- srvyr::summarize(
      .data = to_return,
      stat = srvyr::survey_ratio(
        !!rlang::sym(num),
        !!rlang::sym(denom),
        vartype = vartype,
        ...
      ),
      stat_unw = srvyr::unweighted(
        sum(!!rlang::sym(num)) / sum(!!rlang::sym(denom))
      ),
      n_unw = srvyr::unweighted(srvyr::n())
    )

    # Convert to data.table using copy to avoid shallow copy warnings
    to_return <- data.table::copy(data.table::as.data.table(to_return))

    # Add all columns at once
    if (length(group) > 0) {
      to_return[,
        `:=`(
          stat_type = "ratio",
          n_tot_unw = sum(n_unw, na.rm = FALSE),
          n_tot = n_tot,
          na_count_tot = na_count_tot
        ),
        by = c(group)
      ]
    } else {
      to_return[, `:=`(
        stat_type = "ratio",
        n_tot_unw = n_unw,
        n_tot = n_tot,
        na_count_tot = na_count_tot
      )]
    }

    # Add variable info
    to_return[, var := paste(num, denom, sep = ratio_key_sep)]
    to_return <- set_order_before(to_return, "var", "stat")

    # Add group keys
    if (!is.null(group) && group_key_sep != "") {
      to_return <- add_group_key(
        df = to_return,
        group = group,
        group_key_sep = group_key_sep,
        before = "var"
      )
    } else {
      to_return[,
        c("group_key", "group_key_value") := .(NA_character_, NA_character_)
      ]
      to_return <- set_order_before(
        df = to_return,
        vars = c("group_key", "group_key_value"),
        target_var = "var"
      )
    }

    return(to_return)
  }

  # Map over pairs of numerators and denominators
  analysis <- purrr::map2(
    nums,
    denoms,
    function(x, y) {
      make_single_ratio(
        design = design,
        num = x,
        denom = y,
        ratio_key_sep = ratio_key_sep,
        group = group,
        group_key_sep = group_key_sep,
        na_rm = na_rm,
        vartype = vartype,
        level = level,
        ...
      )
    }
  )

  # Combine results
  analysis <- data.table::rbindlist(analysis, fill = TRUE)

  return(analysis)
}
