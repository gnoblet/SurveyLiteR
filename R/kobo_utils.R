#' Get survey labels from survey names
#'
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`.
#' @typed vars: character[1+]
#'  Variable names.
#' @typed output_df: logical[1]
#'   Whether the output be a data frame (or a vector) Default to TRUE.
#'
#' @typedreturn
#'   The labels as a vector.
#'
#' @keywords internal
get_survey_labels <- function(survey, vars, output_df = TRUE) {
  #------ Checks

  # survey is a data.frame with at least columns type, name and label
  checkmate::assert_data_frame(survey)
  check_missing_vars(
    survey,
    c("type", "name", "label"),
    property = 'columns',
    df_name = 'survey'
  )

  # if survey is not a data.table, convert it
  if (!data.table::is.data.table(survey)) {
    data.table::setDT(survey)
    rlang::warn("survey was converted to a data.table.")
  }

  # vars is a character vector of length >= 1
  checkmate::assert_character(vars, min.len = 1, any.missing = FALSE)

  # output_df is a single logical
  checkmate::assert_logical(output_df, len = 1, any.missing = FALSE)

  #------ Get labels

  name <- label <- . <- NULL # To avoid R CMD check note

  to_return <- survey[name %in% vars, .(name, label)]

  if (nrow(to_return) == 0) {
    rlang::warn(glue::glue(
      "None of the column names were found in survey$name.",
      "An empty vector or an empty data.table is returned.",
      .sep = "\n"
    ))

    if (output_df) {
      return(data.table::data.table(
        name = character(),
        label = character()
      ))
    } else if (!output_df) {
      return(character())
    }
  }

  to_return_distinct <- unique(to_return, by = c("label", "name"))

  if (nrow(to_return_distinct) < nrow(to_return)) {
    rlang::warn(glue::glue(
      sep = "\n",
      "There are some duplicated lines.",
      "The duplicates were removed, but please check."
    ))
    to_return <- to_return_distinct
  }

  if (output_df) {
    return(to_return)
  } else {
    to_return <- to_return[["label"]]
    return(to_return)
  }
}

#' Get choices from survey name (and concatenate if you want!)
#'
#' @typed survey: df[1+, (type = character, list_name = character, name = character, ...)]
#'   A survey sheet from Kobo (already split with columns list_name and name present).
#' @typed choices: df[1+, (list_name = character, label = character, name = character, ...)]
#'   A choices sheet from Kobo (with column list_name, label and name).
#' @typed col: character[1]
#'   A column.
#' @typed conc: logical[1]
#'   Boolean. Should choices be concatenated to column name? Default to TRUE. Can only be used together with label = FALSE.
#' @typed label: logical[1]
#'   Whether labels be returned. It returns a data.frame with two columns: "name" and "label".
#' @typed sep: character[1]
#'   Separator for choices' concatenation.
#'
#' @typedreturn character | df
#'   A character vector of choices or pasted to `col` choices with a separator.
#'
#' @keywords internal
get_survey_choices <- function(
  survey,
  choices,
  col,
  conc = TRUE,
  label = FALSE,
  sep = "_"
) {
  #------ Checks

  # survey is a data.frame with at least columns type, list_name and name
  checkmate::assert_data_frame(survey)
  check_survey(survey)

  # choices is a data.frame with at least columns list_name, label and name
  checkmate::assert_data_frame(choices)
  check_missing_vars(
    choices,
    c("list_name", "label", "name"),
    property = 'columns',
    df_name = 'choices'
  )

  # col is a single string
  checkmate::assert_string(col)

  # conc is a single logical
  checkmate::assert_logical(conc, len = 1, any.missing = FALSE)

  # label is a single logical
  checkmate::assert_logical(label, len = 1, any.missing = FALSE)

  # sep is a single string
  checkmate::assert_string(sep)

  #------ Get choices

  name <- list_name <- . <- NULL # To avoid R CMD check note

  # survey and choices to data.table if not already
  if (!data.table::is.data.table(survey)) {
    survey <- data.table::as.data.table(survey)
  }
  if (!data.table::is.data.table(choices)) {
    choices <- data.table::as.data.table(choices)
  }

  to_return <- survey[name == col, list_name]

  if (length(to_return) == 0) {
    rlang::warn(glue::glue(
      "Col: '{col}' is not in survey$name.",
      "An empty vector or an empty data.table is returned.",
      .sep = "\n"
    ))

    if (label) {
      return(data.table::data.table(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  # If there are more than one row, throw a warning but continue keeping the 1st row
  if (length(to_return) > 1) {
    rlang::warn(glue::glue(
      sep = "\n",
      "There are more than one line in the survey sheet for col '{col}'.",
      "The head was sliced to go on, but please check."
    ))

    to_return <- to_return[1]
  }

  if (is.na(to_return)) {
    rlang::warn(glue::glue(
      "There is no list_name listed in survey for col: '{col}'.",
      "An empty vector or an empty data.table is returned, please check.",
      .sep = "\n"
    ))

    if (label) {
      return(data.table::data.table(
        name = character(),
        label = character(),
        col = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  if (length(subvec_in(to_return, choices[["list_name"]])) == 0) {
    rlang::warn(glue::glue(
      "There is no corresponding list_name in choices for col: '{col}'.",
      "An empty vector or an empty data.table is returned.",
      .sep = "\n"
    ))

    if (label) {
      return(data.table::data.table(
        name = character(),
        label = character(),
        col = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  to_return <- choices[list_name == to_return]

  if (!label) {
    to_return <- to_return[["name"]]

    if (rlang::is_true(conc)) {
      to_return <- stringr::str_c(col, to_return, sep = sep)
    }
  } else {
    to_return <- to_return[, .(name, label)]
    to_return[, col := col]
  }

  return(to_return)
}

#' Get questions by type from the survey sheet
#'
#' `get_survey_type()` is a generic; the other functions get the specified obvious type from the survey sheet.
#'
#' @typed survey: df[1+, (type = character, name = character, ...)]
#'   The survey sheet from Kobo with column "type" split already, see `?split_survey()`. If `label_survey` is TRUE, survey must also contain a "label" column. See `split_survey()`
#' @typed question_type: character[1]
#'   Usually "integer", "select_one" or "select_multiple".
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @typedreturn
#'   A character vector of select_one questions.
#'
#' @keywords internal
get_survey_type <- function(survey, question_type) {
  UseMethod("get_survey_type")
}

#' @keywords internal
get_survey_type.data.frame <- function(survey, question_type) {
  #------ Checks

  # survey is a data.frame with at least columns type and name
  checkmate::assert_data_frame(survey)
  check_missing_vars(
    survey,
    c("type", "name")
  )

  # if not a data.table, convert it
  if (!data.table::is.data.table(survey)) {
    survey <- data.table::copy(data.table::as.data.table(survey))
  }

  # Check if type exists https://support.kobotoolbox.org/question_types.html
  if (
    !(question_type %in%
      c(
        "integer",
        "decimal",
        "range",
        "text",
        "select_one",
        "select_multiple",
        "select_one_from_file",
        "select_mutiple_from_file",
        "rank",
        "note",
        "geopoint",
        "geotrace",
        "geoshape",
        "date",
        "time",
        "dateTime",
        "image",
        "audio",
        "background-audio",
        "video",
        "file",
        "barcode",
        "calculate",
        "acknowledge",
        "hidden",
        "xml-external"
      ))
  ) {
    rlang::abort(
      "Did you mean the right type? Please check: https://support.kobotoolbox.org/question_types.html."
    )
  }

  type <- name <- . <- NULL # To avoid R CMD check note

  got <- survey[type == question_type, name]

  # Warning if yielding zero row
  if (length(got) == 0) {
    rlang::warn(paste0(
      "There is no question in the survey sheet that is of type ",
      question_type,
      "."
    ))
  }

  return(got)
}

#' @rdname get_survey_type
#' @keywords internal
get_survey_select_one <- function(survey) {
  get_survey_type(survey, "select_one")
}

#' @rdname get_survey_type
#' @keywords internal
get_survey_select_multiple <- function(survey) {
  get_survey_type(survey, "select_multiple")
}

#' @rdname get_survey_type
#' @keywords internal
get_survey_calculate <- function(survey) {
  get_survey_type(survey, "calculate")
}

#' @rdname get_survey_type
#' @keywords internal
get_survey_integer <- function(survey) {
  get_survey_type(survey, "integer")
}

#' @rdname get_survey_type
#' @keywords internal
get_survey_decimal <- function(survey) {
  get_survey_type(survey, "decimal")
}

#' Split survey type and list name
#'
#' @typed survey: df[1+, (...)]
#'   A survey sheet from Kobo
#' @typed col_to_split: character[1]
#'   Usually `type`
#' @typed into: character[1+]
#'   Vector of columns names to split to. Default to c("type", "list_name" )
#' @typed sep: character[1]
#'   The separator. Default to " ".
#' @typed fill: character[1]
#'   How to fill. Default to NA on the right.
#' @param ... Params to pass to `tidyr::separate()`.
#'
#' @typedreturn
#'   A survey data.table, split
#'
#' @keywords internal
#'
#' @export
split_survey <- function(
  survey,
  col_to_split,
  into = c("type", "list_name"),
  sep = " ",
  fill = "right",
  ...
) {
  # Check if the column to split is in survey
  col_to_split_name <- col_to_split
  check_missing_vars(survey, col_to_split_name)

  # Convert to data.table if not already
  if (!data.table::is.data.table(survey)) {
    survey <- data.table::as.data.table(survey)
  }

  # Split using data.table syntax
  split_cols <- data.table::tstrsplit(
    x = survey[[col_to_split_name]],
    sep,
    fill = NA
  )

  # Set column names
  for (i in seq_along(into)) {
    if (i <= length(split_cols)) {
      data.table::set(survey, j = into[i], value = split_cols[[i]])
    } else {
      data.table::set(survey, j = into[i], value = NA_character_)
    }
  }

  return(survey)
}

#' Check if survey is minimally well-defined
#'
#' @typed survey: df[1+, (...)]
#'   A data frame from a survey sheet
#'
#' @keywords internal
check_survey <- function(survey) {
  cols <- c("type", "list_name", "name", "label")
  check_missing_vars(survey, cols)
}
