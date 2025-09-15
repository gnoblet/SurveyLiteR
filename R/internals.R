# Analysis key ------------------------------------------------------------

# Just a helper to not copy-paste in each svy_ function for grouping
add_group_key <- function(df, group, group_key_sep, before) {
  #-------- Parameter checks

  # df must be a data.frame or data.table
  checkmate::assertDataFrame(df)
  if (!data.table::is.data.table(df)) {
    df <- data.table::as.data.table(df)
  }

  # group must be character vector with existing column names
  checkmate::assertCharacter(group, min.len = 1, any.missing = FALSE)
  check_missing_vars(df, group)

  # group_key_sep must be character scalar
  checkmate::assertString(group_key_sep)

  # before must be character scalar and existing column
  checkmate::assertString(before)
  check_missing_vars(df, before)

  #-------- Add group key columns

  # set to NULL
  group_key_value <- NULL

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  # Add group key
  df[, group_key := group_key]

  # Add group key values
  # - .SDcols = group specifies which columns to include in .SD
  # - purrr::pmap_chr applies paste() row-wise across those selected columns
  df[,
    group_key_value := purrr::pmap_chr(
      .SD,
      ~ paste(..., sep = group_key_sep)
    ),
    .SDcols = group
  ]

  # Reorder columns to place group_key columns in front
  # Place group_key before the target column
  set_order_before(df, c("group_key", "group_key_value"), before)

  return(df)
}


# Just a helper to not copy-paste in each svy_ function for interacting
add_interact_key <- function(
  df,
  interact,
  interact_key_sep,
  before
) {
  #-------- Parameter checks

  # df must be a data.frame or data.table
  checkmate::assertDataFrame(df)
  if (!data.table::is.data.table(df)) {
    rlang::warn("df set to data.table.")
    data.table::setDT(df)
  }

  # interact must be character vector with existing column names
  checkmate::assertCharacter(interact, min.len = 1, any.missing = FALSE)
  check_missing_vars(df, interact)

  # interact_key_sep must be character scalar
  checkmate::assertString(interact_key_sep)

  # before must be character scalar and existing column
  checkmate::assertString(before)
  check_missing_vars(df, before)

  #-------- Add interact key columns

  # set to NULL
  interact_key_value <- NULL

  # Interact key
  interact_key <- paste(interact, collapse = interact_key_sep)

  # Add grou

  # Add interact key
  df[, interact_key := interact_key]

  # Add interact key values
  # - .SDcols = interact specifies which columns to include in .SD
  # - purrr::pmap_chr applies paste() row-wise across those selected columns
  df[,
    interact_key_value := purrr::pmap_chr(
      .SD,
      ~ paste(..., sep = interact_key_sep)
    ),
    .SDcols = interact
  ]

  # Reorder columns to place interact_key columns in front
  # Place interact_key before the target column
  set_order_before(
    df,
    c("interact_key", "interact_key_value"),
    before
  )

  return(df)
}


# Adds the analysis key to the results table from a svy_*() function
add_analysis_key <- function(
  results,
  group_key_name = "group_key",
  group_key_value_name = "group_key_value",
  group_key_sep = " -/- ",
  var_name = "var",
  var_value_name = "var_value",
  var_key_sep = " -/- ",
  overall_sep = " @/@ ",
  main_sep = " -/- ",
  var_to_value_sep = " %/% "
) {
  #-------- Parameter checks

  # results must be a data.frame or data.table
  checkmate::assertDataFrame(results)
  if (!data.table::is.data.table(results)) {
    results <- data.table::copy(data.table::as.data.table(results))
  } else {
    # Ensure we have a proper copy even if input is already data.table
    results <- data.table::copy(results)
  }

  # All column name parameters must be character scalars
  checkmate::assertString(group_key_name)
  checkmate::assertString(group_key_value_name)
  checkmate::assertString(var_name)
  checkmate::assertString(var_value_name)

  # Check that required columns exist
  required_cols <- c(
    group_key_name,
    group_key_value_name,
    var_name,
    var_value_name,
    "stat_type"
  )
  check_missing_vars(results, required_cols)

  # All separator parameters must be character scalars
  checkmate::assertString(group_key_sep)
  checkmate::assertString(var_key_sep)
  checkmate::assertString(overall_sep)
  checkmate::assertString(main_sep)
  checkmate::assertString(var_to_value_sep)

  #-------- Process analysis key

  # set to NULL
  analysis_key <- stat_type <- NULL

  # Extract group values
  x <- stringr::str_split(results[[group_key_name]], group_key_sep)
  y <- stringr::str_split(results[[group_key_value_name]], group_key_sep)

  to_add_group <- purrr::map2(
    x,
    y,
    \(x, y) {
      paste(x, y, sep = var_to_value_sep)
    }
  )

  to_add_group <- purrr::map_chr(
    to_add_group,
    stringr::str_c,
    collapse = main_sep
  )

  # Extract var values
  x <- stringr::str_split(results[[var_name]], var_key_sep)
  y <- stringr::str_split(results[[var_value_name]], var_key_sep)

  to_add_var <- purrr::map2(
    x,
    y,
    \(x, y) {
      paste(x, y, sep = var_to_value_sep)
    }
  )

  to_add_var <- purrr::map_chr(to_add_var, stringr::str_c, collapse = main_sep)

  # Add analysis_key column using data.table
  results[,
    analysis_key := paste0(
      stat_type,
      overall_sep,
      to_add_var,
      overall_sep,
      to_add_group
    )
  ]

  return(results)
}
