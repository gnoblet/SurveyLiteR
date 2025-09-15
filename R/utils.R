# Utility functions from impactR.utils
# These functions have been copied from https://github.com/gnoblet/impactR.utils
# to avoid external dependency

#' Get a subset of a vector
#'
#' `subvec_in()` gets a subset of a vector if vector elements are in a given `set`. `subvec_not_in()` get a subset of a vector if vector elements are NOT in a fiven `set`.
#'
#' @typed vector: vec
#'   A vector to subset.
#' @typed set: vec
#'   A set-vector.
#'
#' @typedreturn
#'   A subset of a list or a vector.
#'
#' @keywords internal
subvec_in <- function(vector, set) {
  vector[vector %in% set]
}

#' @rdname subvec_in
#' @keywords internal
subvec_not_in <- function(vector, set) {
  vector[!(vector %in% set)]
}


#' Named group split
#'
#' @typed df: df[, 1+]
#'   A data frame.
#' @typed group: character[1]
#'   Variable name to group by.
#' @typed warn_if_set_dt: logical[1]
#'  Whether to warn if df is coerced to data.table. (default: FALSE)
#'
#' @typedreturn
#'   A split and named list of data.frames/data.tables.
#'
#' @keywords internal
named_group_split <- function(df, group, warn_if_set_dt = FALSE) {
  #-------- Checks

  # df must be a data.frame or data.table
  # if not, try to coerce
  checkmate::assertDataFrame(df)
  if (!checkmate::testDataTable(df)) {
    if (warn_if_set_dt) {
      rlang::warn("Coerces df to data.table")
    }

    data.table::setDT(df)
  }

  # group is a character scalar
  checkmate::assertString(group)

  # group must be a column in df
  check_missing_vars(df, group)

  #-------- Do the split

  l <- split(df, by = group, keep.by = FALSE, drop = TRUE)

  return(l)
}


#' Remove variables from a data frame that exist in another dataframe
#'
#' @typed df_a: df[, 1+]
#'   A data frame (or data.table) to remove variables from.
#'
#' @typed df_b: df[, 1+]
#'   A data frame (or data.table) that provides variable names to drop.
#'
#' @typed vars_to_keep: NULL | character[1+]
#'   Variables from `df_b` to keep in `df_a`. Supplying names here means "do **not** drop these columns".
#'
#' @typedreturn
#'   A data.table with the unwanted variables removed.
#'
#' @keywords internal
#'
#' @export
df_diff <- function(df_a, df_b, vars_to_keep = NULL) {
  #------ Checks

  # coerce both inputs to data.tables (if they aren't already)
  checkmate::assertDataFrame(df_a, min.cols = 1)
  checkmate::assertDataFrame(df_b, min.cols = 1)

  if (!checkmate::testDataTable(df_a)) {
    data.table::setDT(df_a)
    rlang::warn("Coerces df_a to data.table")
  }
  if (!checkmate::testDataTable(df_b)) {
    rlang::warn("Coerces df_b to data.table")
    data.table::setDT(df_b)
  }

  # vars_to_keep is NULL or character vector
  checkmate::assertCharacter(
    vars_to_keep,
    null.ok = TRUE,
    min.len = 1,
    any.missing = FALSE
  )

  #------ Remove df_b variables from df_a

  # drop those variables from df_a, if they are present there
  vars_to_drop <- setdiff(colnames(df_b), vars_to_keep)

  if (length(vars_to_drop) > 0) {
    df_a <- df_a[, setdiff(colnames(df_a), vars_to_drop), with = FALSE]
  }

  return(df_a)
}

#' Remove variable from a data frame
#'
#' @typed df: df[, 1+]
#'   A data frame.
#' @typed vars: character[1+]
#'  Variables to remove from `df`.
#' @typedreturn dt
#'   A data.table with columns removed
#'
#' @keywords internal
deselect <- function(df, vars) {
  # ------- Checks

  # df must be a data.frame or data.table
  checkmate::assertDataFrame(df)
  if (!data.table::is.data.table(df)) {
    rlang::warn("df set to data.table.")
    data.table::setDT(df)
  }
  # vars must be character vector and in df
  checkmate::assertCharacter(vars, min.len = 1, any.missing = FALSE)
  check_missing_vars(df, vars)

  #------- Deselect vars

  df[, (vars) := NULL]

  return(df)
}


#' Create column order to insert new column before target column
#'
#' @typed df: data.frame
#'   A data frame or data.table.
#' @typed vars: character[1]
#'   Name of the variables to move.
#' @typed target_var: character[1]
#'   Name of the target column that the variables should be moved before.
#' @typed warn_if_unchanged: logical[1]
#'  Whether to warn if the columns are already in the right place. (default: FALSE)
#'
#' @typedreturn character
#'   A character vector with the new column order.
#'
#' @keywords internal
set_order_before <- function(df, vars, target_var, warn_if_unchanged = FALSE) {
  #-------- Checks

  # df must be a data.frame or data.table
  checkmate::assertDataFrame(df)
  if (!data.table::is.data.table(df)) {
    rlang::warn("df set to data.table.")
    df <- data.table::as.data.table(df)
  }

  # vars must be character vector and in df
  checkmate::assertCharacter(vars, min.len = 1, any.missing = FALSE)
  check_missing_vars(df, vars)

  # target_var must be character scalar and in df
  checkmate::assertString(target_var)
  check_missing_vars(df, target_var)

  #------- If columns are already in right place, return df unchanged
  pos_vars <- which(names(df) %in% vars)
  pos_target <- which(names(df) == target_var)

  # Handle special case: trying to move column(s) before themselves
  if (target_var %in% vars) {
    rlang::warn("Vars are already in the right place. df unchanged.")
    return(df)
  }

  # Check if vars are already positioned immediately before target_var
  # First handle edge case: if target is at position <= length(vars),
  # vars cannot be positioned before it in sequence
  if (pos_target > length(vars) & warn_if_unchanged) {
    expected_positions <- (pos_target - length(vars)):(pos_target - 1)
    if (all(pos_vars == expected_positions)) {
      rlang::warn("Vars are already in the right place. df unchanged.")
      return(df)
    }
  }

  #------- Get target_var pos and set new order

  # Build the new ordering:
  # First, get all column names except the vars we're moving
  remaining_vars <- setdiff(names(df), vars)

  # Find the target position in the remaining columns
  pos_target_in_remaining <- which(remaining_vars == target_var)

  # Handle edge case when target is first column
  if (pos_target_in_remaining == 1) {
    new_order <- c(
      vars,
      remaining_vars[pos_target_in_remaining:length(remaining_vars)]
    )
  } else {
    new_order <- c(
      remaining_vars[1:(pos_target_in_remaining - 1)],
      vars,
      remaining_vars[pos_target_in_remaining:length(remaining_vars)]
    )
  }

  # set order
  data.table::setcolorder(x = df, neworder = new_order)

  return(df)
}


#' Add select multiple parent column
#'
#' Creates a parent column that concatenates the labels of selected options
#' from multiple binary columns (typically from select_multiple questions).
#' Can optionally create well-named child variables using parent_col and choices_sep.
#'
#' @typed dt: data.table
#'   A data.table to add the parent column to.
#' @typed parent_var: character[1]
#'   Name of the new parent column to create.
#' @typed child_vars: character[1+]
#'   Names of the child binary columns (0/1 values).
#' @typed choices: character[1+]
#'   Labels/choices corresponding to each child column.
#' @typed choices_sep: character[1]
#'   Separator used to create well-named child variables.
#'   child_vars will be renamed to paste0(parent_var, choices_sep, choices). (default: "/")
#' @typed sep: character[1]
#'   Separator to use when concatenating multiple selected options. (default: " ")
#' @typed na_value: logical[1] | character[1]
#'   Value to use when no options are selected. (default: NA)
#'
#' @typedreturn data.table
#'   The input data.table with the new parent column added.
#'
#' @keywords internal
#' @export
add_select_multiple_parent <- function(
  dt,
  parent_var,
  child_vars,
  choices,
  choices_sep = "/",
  sep = " ",
  na_value = NA
) {
  #-------- Checks

  # dt must be a data.table
  checkmate::assertDataFrame(dt)
  if (!data.table::is.data.table(dt)) {
    rlang::warn("dt set to data.table.")
    dt <- data.table::as.data.table(dt)
  }

  # parent_var must be character scalar
  checkmate::assertString(parent_var)

  # child_vars must be character vector and exist in dt
  checkmate::assertCharacter(child_vars, min.len = 1, any.missing = FALSE)
  check_missing_vars(dt, child_vars)

  # choices must be character vector with same length as child_vars
  checkmate::assertCharacter(choices, min.len = 1, any.missing = FALSE)
  if (length(choices) != length(child_vars)) {
    rlang::abort("choices must have the same length as child_vars")
  }

  # choices_sep must be character scalar or NULL
  checkmate::assertString(choices_sep)

  # sep must be character scalar
  checkmate::assertString(sep)

  #-------- Rename child columns if choices_sep is provided

  new_child_vars <- paste0(parent_var, choices_sep, choices)
  data.table::setnames(dt, old = child_vars, new = new_child_vars)
  child_vars <- new_child_vars

  #-------- Create parent column

  dt[,
    (parent_var) := purrr::pmap_chr(
      .SD,
      function(...) {
        values <- c(...)
        # If all values are NA, return NA
        if (all(is.na(values))) {
          return(na_value)
        }
        # Get labels for values that are 1 (excluding NAs)
        labels <- choices[values == 1 & !is.na(values)]
        labels <- paste(labels, collapse = sep)
        labels <- ifelse(labels == "", na_value, labels)
        return(labels)
      }
    ),
    .SDcols = child_vars
  ]

  #-------- Position parent column before first child column

  dt <- set_order_before(dt, parent_var, child_vars[1])

  return(dt)
}
