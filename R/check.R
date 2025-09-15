#' Check if values in a vector are all in a given set of allowed values
#'
#' @typed x: vector[1+]
#'   Values to check.
#' @typed allowed: vector[1+]
#'   Allowed values.
#' @typed property: character[1]
#'   Name of the property being checked (default: 'set').
#' @typed allow_missing: logical[1]
#'  Whether to allow missing values (default: FALSE).
#'
#' @typedreturn TRUE | error
#'   TRUE if all values are in the allowed set, otherwise throws an error.
#'
#' @keywords internal
check_values_in_set <- function(
  x,
  allowed,
  property = 'set',
  allow_missing = FALSE
) {
  #------ Checks

  checkmate::assert_vector(x, min.len = 1)
  checkmate::assert_vector(allowed, min.len = 1)
  checkmate::assert_character(property, len = 1)
  checkmate::assert_logical(allow_missing, len = 1)

  #------ Abort or not
  if (!all(x %in% allowed | (allow_missing & is.na(x)))) {
    invalid <- setdiff(x, allowed)

    # Make sure that allowed set and invalid are character
    allowed <- as.character(allowed)
    invalid <- as.character(invalid)

    rlang::abort(
      msg_invalid_values(invalid, allowed, property = property)
    )
  }

  return(TRUE)
}

#' Check for missing variables in a data frame
#'
#' @typed df: data.frame[,1+]
#'   A data frame
#' @typed vars: character[1+]
#'   Names of variables to check for in the data frame.
#' @typed property: character[1] | NULL
#'   Property being checked (default: 'vars').
#' @typed df_name: character[1]
#'   Name of the dataframe being checked, passed on to the abort message (default: 'df').
#'
#' @typedreturn TRUE | error
#'   TRUE if all variables are present, otherwise throws an error.
#'
#' @keywords internal
check_missing_vars <- function(df, vars, property = 'vars', df_name = 'df') {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df, min.cols = 1)

  # vars is a character vector
  checkmate::assert_character(vars, min.len = 1)

  # property is a character scalar or NULL
  checkmate::assert_character(property, len = 1, null.ok = TRUE)

  #------ Check for missing vars

  vars_nin <- setdiff(vars, colnames(df))
  if (length(vars_nin) > 0) {
    rlang::abort(
      msg_missing_vars(df = df_name, vars = vars_nin, property = property)
    )
  }

  return(TRUE)
}

#' Check if items of vectors in a list are a set of allowed values
#'
#' This function checks if the values in specified variables of a data frame are all within a given set of allowed values. If any values are not in the set, it generates an error message detailing the invalid values in which vars and the expected set.
#'
#' @typed l_x: list_named_vector[1+]
#'   A list of named vectors to check.
#' @typed set: vector[1+]
#'  Allowed values that the variables should contain.
#' @typed allow_missing: logical[1]
#'  Whether to allow missing values (default: FALSE).
#'
#' @typedreturn TRUE | error
#'   TRUE if all values in the specified variables are in the set, otherwise throws an error.
#'
#' @keywords internal
check_vecs_in_set <- function(
  l_x,
  set,
  allow_missing = FALSE
) {
  #------ Checks

  # l_x is a named list of vectors
  checkmate::assert_list(l_x, min.len = 1, names = "named")
  checkmate::assert_vector(set, min.len = 1)
  checkmate::assert_logical(allow_missing, len = 1)

  #------ Values not in set
  if (allow_missing) {
    set <- c(set, NA)
  }
  values_lgl <- purrr::map_lgl(
    l_x,
    \(x) {
      !all(unique(x) %in% set)
    }
  )
  if (any(values_lgl)) {
    l_x <- l_x[values_lgl]
    # Get values not in set for each item in l_x
    values_chr <- purrr::map(l_x, function(x) {
      x <- unique(x)
      x <- x[!(x %in% set)]
      ifelse(is.na(x), "NA", x)
    }) |>
      purrr::flatten_chr() |>
      unique()

    # Make sure that allowed set is character
    set <- as.character(set)

    # Error message
    rlang::abort(msg_invalid_values(
      values_chr,
      set,
      property = names(l_x)
    ))
  }

  return(TRUE)
}

#' Check if variables in a data frame are in a set of allowed values
#'
#' This function checks if the values in specified variables of a data frame are all within a given set of allowed values. If any values are not in the set, it generates an error message detailing the invalid values in which vars and the expected set.
#'
#' @typed df: data.frame[,1+]
#'   A data frame
#' @typed vars: character[1+]
#'  Names of variables to check in the data frame.
#' @typed set: vector[1+]
#'   Allowed values that the variables should contain.
#' @typed allow_missing: logical[1]
#'  Whether to allow missing values (default: FALSE).
#'
#' @typedreturn TRUE | error
#'   TRUE if all values in the specified variables are in the set, otherwise throws an error.
#'
#' @keywords internal
check_vars_in_set <- function(
  df,
  vars,
  set,
  allow_missing = FALSE
) {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # vars is a character vector
  checkmate::assert_character(vars, min.len = 1)
  # set is a vector of allowed values
  checkmate::assert_vector(set, min.len = 1)
  # allow_missing is a logical scalar
  checkmate::assert_logical(allow_missing, len = 1)

  # missing vars
  check_missing_vars(df, vars)

  #------ Values not in set

  # Check if all values in df[vars] are in the set
  # If allow_missing is TRUE, we also allow NA values
  if (allow_missing) {
    set <- c(set, NA)
  }

  # Handle column selection for both data.frames and data.tables
  if (data.table::is.data.table(df)) {
    df_subset <- df[, vars, with = FALSE]
  } else {
    df_subset <- df[vars]
  }

  values_lgl <- purrr::map_lgl(
    df_subset,
    \(x) {
      !all(unique(x) %in% set)
    }
  )

  if (any(values_lgl)) {
    vars <- vars[values_lgl]
    # Get values not in set for each column
    if (data.table::is.data.table(df)) {
      df_subset <- df[, vars, with = FALSE]
    } else {
      df_subset <- df[vars]
    }
    values_chr <- purrr::map(df_subset, function(x) {
      x <- unique(x)
      x <- x[!(x %in% set)]
      ifelse(is.na(x), "NA", x)
    }) |>
      purrr::flatten_chr() |>
      unique()

    # Make sure that allowed set is character
    set <- as.character(set)

    # Error message
    rlang::abort(msg_invalid_values(values_chr, set, property = vars))
  }

  return(TRUE)
}

#' Check duplicate values in a vector
#'
#' This function checks for duplicate values in a vector and throws an error if any duplicates are found.
#'
#' @typed vec: vector
#'  The vector to check for duplicates.
#' @typed property: character[1]
#'  The name of the property being checked (default: 'vec').
#'
#' @typedreturn TRUE | error
#'  TRUE if no duplicates are found, otherwise throws an error with a message listing the duplicate values.
#'
#' @keywords internal
check_dupes <- function(vec, property = 'vec') {
  #------ Checks

  # vec is a vector
  checkmate::assert_vector(vec)

  # property is a character scalar
  checkmate::assert_character(property, len = 1)

  #------ Check for duplicates
  dupes_vec <- unique(vec[duplicated(vec)])
  if (length(dupes_vec) > 0) {
    rlang::abort(c(
      glue::glue("Duplicate values found in {property}."),
      "*" = paste0(
        "Duplicate values: ",
        glue::glue_collapse(dupes_vec, sep = ", ", last = ", and ")
      )
    ))
  }

  return(TRUE)
}


#' Check class of items of a vector
#'
#' This function checks if all items in a vector are of a specified class. If any item is not of the specified class, it returns an error message.
#'
#' @typed vec: vector[1+]
#'  A vector of items to check.
#' @typed   class: character[1]
#'  The name of the class to check against.
#' @typed property: character[1]
#'  The name of the property being checked (default: 'vec').
#'
#' @typedreturn TRUE | error
#' TRUE if all items are of the specified class, otherwise throws an error.
#'
#' @keywords internal
check_vector_class <- function(
  vec,
  class,
  property = 'vec'
) {
  #------ Checks
  # vec is a vector of at least one element
  checkmate::assert_vector(vec, min.len = 1)
  # class_name is a character scalar
  class_name <- as.character(class)
  checkmate::assert_character(class, len = 1)

  #------ Check class of each item in the vec
  lgl_class <- purrr::map_lgl(vec, function(x) {
    checkmate::testClass(x, class_name)
  })
  if (!all(lgl_class)) {
    non_class_items <- vec[!lgl_class]
    rlang::abort(c(
      glue::glue("Not all items in '{property}' are of the specified class."),
      "*" = glue::glue(
        "Non-{class_name} items::",
        glue::glue_collapse(
          non_class_items,
          sep = ", ",
          last = ", and "
        )
      )
    ))
  }
  return(TRUE)
}


#' Check common parameters for svy_ functions
#'
#' This helper function consolidates the common parameter validation patterns
#' used across the svy_ family of functions to reduce code duplication.
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+] | NULL
#'   Variables to analyze. Can be NULL for functions that don't use vars.
#' @typed nums: character[1+] | NULL
#'   Numerator variables (for ratio functions). Can be NULL.
#' @typed denoms: character[1+] | NULL
#'   Denominator variables (for ratio functions). Can be NULL.
#' @typed group: character[1+] | NULL
#'   Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1] | NULL
#'   The string to separate grouping variables. Can be NULL to skip check.
#' @typed na_rm: logical[1] | NULL
#'   Should NAs be removed? Can be NULL to skip check.
#' @typed vartype: character[1] | NULL
#'   Type of variance estimation. Can be NULL to skip check.
#' @typed level: numeric[1] | NULL
#'   Confidence level between 0 and 1. Can be NULL to skip check.
#' @typed check_vars_not_in_group: logical[1]
#'   Whether to check that vars are not in group (default: TRUE).
#' @typed check_equal_length: logical[1]
#'   Whether to check that nums and denoms have equal length (default: TRUE).
#' @typed warn_low_ci: logical[1]
#'   Whether to warn when CI level is below 90% (default: TRUE).
#' @typed separators: character[1+] | NULL
#'   Additional separator parameters to validate. Can be NULL.
#'
#' @typedreturn TRUE | error
#'   TRUE if all validations pass, otherwise throws an error.
#'
#' @keywords internal
svy_param_check <- function(
  design,
  vars = NULL,
  nums = NULL,
  denoms = NULL,
  group = NULL,
  group_key_sep = NULL,
  na_rm = NULL,
  vartype = NULL,
  level = NULL,
  check_vars_not_in_group = TRUE,
  check_equal_length = TRUE,
  warn_low_ci = TRUE,
  separators = NULL
) {
  #------ Basic parameter checks

  # design is a design
  checkmate::assert_class(design, "tbl_svy")

  # vars validation (if provided)
  if (!is.null(vars)) {
    checkmate::assert_character(vars, min.len = 1, any.missing = FALSE)
    check_missing_vars(
      df = design$variables,
      vars = vars,
      property = 'vars',
      df_name = 'design'
    )
  }

  # nums validation (if provided)
  if (!is.null(nums)) {
    checkmate::assert_character(nums, min.len = 1, any.missing = FALSE)
    check_missing_vars(
      df = design$variables,
      vars = nums,
      property = 'nums',
      df_name = 'design'
    )
  }

  # denoms validation (if provided)
  if (!is.null(denoms)) {
    checkmate::assert_character(denoms, min.len = 1, any.missing = FALSE)
    check_missing_vars(
      df = design$variables,
      vars = denoms,
      property = 'denoms',
      df_name = 'design'
    )
  }

  # group validation (if provided)
  if (!is.null(group)) {
    checkmate::check_character(
      group,
      min.len = 1,
      null.ok = TRUE,
      any.missing = FALSE
    )

    if (!is.null(group)) {
      check_missing_vars(
        design$variables,
        group,
        property = 'group',
        df_name = 'design'
      )
    }
  }

  # Separator validations
  if (!is.null(group_key_sep)) {
    checkmate::assert_string(group_key_sep, min.chars = 1)
  }

  if (!is.null(separators)) {
    purrr::walk(separators, checkmate::assert_string)
  }

  # na_rm validation (if provided)
  if (!is.null(na_rm)) {
    checkmate::assert_logical(na_rm, len = 1, any.missing = FALSE)
  }

  # vartype validation (if provided)
  if (!is.null(vartype)) {
    checkmate::assert_string(vartype)
  }

  # level validation (if provided)
  if (!is.null(level)) {
    checkmate::assert_number(level, lower = 0, upper = 1, finite = TRUE)
  }

  #------ Cross-parameter checks

  # Check nums and denoms have equal length
  if (check_equal_length && !is.null(nums) && !is.null(denoms)) {
    if (length(nums) != length(denoms)) {
      rlang::abort(
        "Lengths of `nums` and `denoms` are different. Please provide the same number of numerators and denominators."
      )
    }
  }

  # Check vars are not in group
  if (check_vars_not_in_group && !is.null(vars) && !is.null(group)) {
    vars_in_group <- vars %in% group
    if (any(vars_in_group)) {
      rlang::abort(c(
        "Grouping columns in `group` should be different than the ones in `vars`.",
        "i" = glue::glue(
          "The following variables are in both `vars` and `group`: ",
          glue::glue_collapse(vars[vars_in_group], sep = ", ", last = " and ")
        )
      ))
    }
  }

  # Check nums/denoms are not in group (for ratio functions)
  if (check_vars_not_in_group && !is.null(nums) && !is.null(group)) {
    nums_in_group <- nums %in% group
    if (any(nums_in_group)) {
      rlang::abort(c(
        "Grouping columns in `group` should be different than the ones in `nums`.",
        "i" = glue::glue(
          "The following variables are in both `nums` and `group`: ",
          glue::glue_collapse(nums[nums_in_group], sep = ", ", last = " and ")
        )
      ))
    }
  }

  if (check_vars_not_in_group && !is.null(denoms) && !is.null(group)) {
    denoms_in_group <- denoms %in% group
    if (any(denoms_in_group)) {
      rlang::abort(c(
        "Grouping columns in `group` should be different than the ones in `denoms`.",
        "i" = glue::glue(
          "The following variables are in both `denoms` and `group`: ",
          glue::glue_collapse(
            denoms[denoms_in_group],
            sep = ", ",
            last = " and "
          )
        )
      ))
    }
  }

  #------ Warnings

  # Warn on low CI level
  if (warn_low_ci && !is.null(level) && !is.null(vartype)) {
    if (level < 0.9 && vartype == "ci") {
      rlang::warn("The confidence level used is below 90%.")
    }
  }

  return(TRUE)
}


#' Check common parameters for kobo_ functions
#'
#' This helper function consolidates the common parameter validation patterns
#' used across the kobo_ family of functions to reduce code duplication.
#'
#' @typed design: srvyr::design
#'   A srvyr::design object.
#' @typed vars: character[1+] | NULL
#'   Variables to analyze. Can be NULL for functions that don't use vars.
#' @typed nums: character[1+] | NULL
#'   Numerator variables (for ratio functions). Can be NULL.
#' @typed denoms: character[1+] | NULL
#'   Denominator variables (for ratio functions). Can be NULL.
#' @typed survey: data.frame | NULL
#'   The survey sheet from Kobo. Can be NULL to skip survey validation.
#' @typed choices: data.frame | NULL
#'   The choices sheet from Kobo. Can be NULL.
#' @typed group: character[1+] | NULL
#'   Variables to group by. Default to NULL for no group.
#' @typed group_key_sep: character[1] | NULL
#'   The string to separate grouping variables. Can be NULL to skip check.
#' @typed na_rm: logical[1] | NULL
#'   Should NAs be removed? Can be NULL to skip check.
#' @typed vartype: character[1] | NULL
#'   Type of variance estimation. Can be NULL to skip check.
#' @typed level: numeric[1] | NULL
#'   Confidence level between 0 and 1. Can be NULL to skip check.
#' @typed check_vars_not_in_group: logical[1]
#'   Whether to check that vars are not in group (default: TRUE).
#' @typed check_equal_length: logical[1]
#'   Whether to check that nums and denoms have equal length (default: TRUE).
#' @typed warn_low_ci: logical[1]
#'   Whether to warn when CI level is below 90% (default: TRUE).
#' @typed separators: character[1+] | NULL
#'   Additional separator parameters to validate. Can be NULL.
#' @typed survey_required_cols: character[1+]
#'   Required columns in survey sheet (default: c("type", "name")).
#' @typed choices_required_cols: character[1+]
#'   Required columns in choices sheet (default: c("label", "name")).
#'
#' @typedreturn TRUE | error
#'   TRUE if all validations pass, otherwise throws an error.
#'
#' @keywords internal
kobo_param_check <- function(
  design,
  vars = NULL,
  nums = NULL,
  denoms = NULL,
  survey = NULL,
  choices = NULL,
  group = NULL,
  group_key_sep = NULL,
  na_rm = NULL,
  vartype = NULL,
  level = NULL,
  check_vars_not_in_group = TRUE,
  check_equal_length = TRUE,
  warn_low_ci = TRUE,
  separators = NULL,
  survey_required_cols = c("type", "name"),
  choices_required_cols = c("label", "name")
) {
  #------ Use existing svy_param_check for common validations

  svy_param_check(
    design = design,
    vars = vars,
    nums = nums,
    denoms = denoms,
    group = group,
    group_key_sep = group_key_sep,
    na_rm = na_rm,
    vartype = vartype,
    level = level,
    check_vars_not_in_group = check_vars_not_in_group,
    check_equal_length = check_equal_length,
    warn_low_ci = warn_low_ci,
    separators = separators
  )

  #------ Kobo-specific validations

  # Survey sheet validation
  if (!is.null(survey)) {
    checkmate::assert_data_frame(survey)
    check_missing_vars(
      survey,
      survey_required_cols,
      property = 'columns',
      df_name = 'survey'
    )
  }

  # Choices sheet validation
  if (!is.null(choices)) {
    checkmate::assert_data_frame(choices)
    check_missing_vars(
      choices,
      choices_required_cols,
      property = 'columns',
      df_name = 'choices'
    )
  }

  return(TRUE)
}
