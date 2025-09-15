# Tests for kobo_utils.R functions

# Setup test data
setup_test_data <- function() {
  # Mock survey data
  survey <- data.table::data.table(
    type = c(
      "select_one list1",
      "select_multiple list2",
      "integer",
      "text",
      "calculate",
      "decimal"
    ),
    name = c("q1", "q2", "q3", "q4", "q5", "q6"),
    label = c(
      "Question 1",
      "Question 2",
      "Question 3",
      "Question 4",
      "Question 5",
      "Question 6"
    )
  )

  # Split survey (as expected by most functions)
  survey_split <- data.table::data.table(
    type = c(
      "select_one",
      "select_multiple",
      "integer",
      "text",
      "calculate",
      "decimal"
    ),
    list_name = c("list1", "list2", NA, NA, NA, NA),
    name = c("q1", "q2", "q3", "q4", "q5", "q6"),
    label = c(
      "Question 1",
      "Question 2",
      "Question 3",
      "Question 4",
      "Question 5",
      "Question 6"
    )
  )

  # Mock choices data
  choices <- data.table::data.table(
    list_name = c("list1", "list1", "list1", "list2", "list2"),
    name = c("opt1", "opt2", "opt3", "opt1", "opt2"),
    label = c("Option 1", "Option 2", "Option 3", "Choice 1", "Choice 2")
  )

  list(
    survey = survey,
    survey_split = survey_split,
    choices = choices
  )
}

# Test get_survey_labels function
test_that("get_survey_labels returns correct labels as data frame", {
  test_data <- setup_test_data()

  result <- get_survey_labels(
    test_data$survey_split,
    c("q1", "q3"),
    output_df = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("q1", "q3"))
  expect_equal(result$label, c("Question 1", "Question 3"))
})

test_that("get_survey_labels returns correct labels as vector", {
  test_data <- setup_test_data()

  result <- get_survey_labels(
    test_data$survey_split,
    c("q1", "q3"),
    output_df = FALSE
  )

  expect_type(result, "character")
  expect_equal(result, c("Question 1", "Question 3"))
})

test_that("get_survey_labels warns and returns empty when no columns found", {
  test_data <- setup_test_data()

  expect_warning(
    result <- get_survey_labels(
      test_data$survey_split,
      "missing_col",
      output_df = TRUE
    ),
    "None of the column names were found"
  )
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("get_survey_labels warns about duplicates", {
  test_data <- setup_test_data()
  # Create survey with duplicates
  survey_dup <- rbind(test_data$survey_split, test_data$survey_split[1, ])

  expect_warning(
    result <- get_survey_labels(survey_dup, "q1"),
    "There are some duplicated lines"
  )
})

# Test get_survey_choices function
test_that("get_survey_choices returns choices with concatenation", {
  test_data <- setup_test_data()

  result <- get_survey_choices(
    test_data$survey_split,
    test_data$choices,
    "q1",
    conc = TRUE,
    label = FALSE
  )

  expect_type(result, "character")
  expect_equal(result, c("q1_opt1", "q1_opt2", "q1_opt3"))
})

test_that("get_survey_choices returns choices without concatenation", {
  test_data <- setup_test_data()

  result <- get_survey_choices(
    test_data$survey_split,
    test_data$choices,
    "q1",
    conc = FALSE,
    label = FALSE
  )

  expect_type(result, "character")
  expect_equal(result, c("opt1", "opt2", "opt3"))
})

test_that("get_survey_choices returns labels when requested", {
  test_data <- setup_test_data()

  result <- get_survey_choices(
    test_data$survey_split,
    test_data$choices,
    "q1",
    label = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_equal(names(result), c("name", "label", "col"))
  expect_equal(result$name, c("opt1", "opt2", "opt3"))
  expect_equal(result$col, rep("q1", 3))
})

test_that("get_survey_choices warns when column not found in survey", {
  test_data <- setup_test_data()

  expect_warning(
    result <- get_survey_choices(
      test_data$survey_split,
      test_data$choices,
      "missing_col"
    ),
    "is not in survey\\$name"
  )
  expect_equal(result, character(0))
})

test_that("get_survey_choices warns when list_name is NA", {
  test_data <- setup_test_data()

  expect_warning(
    result <- get_survey_choices(
      test_data$survey_split,
      test_data$choices,
      "q3" # integer question with NA list_name
    ),
    "There is no list_name listed in survey"
  )
})

test_that("get_survey_choices warns when list_name not in choices", {
  test_data <- setup_test_data()
  # Add a question with list_name not in choices
  survey_bad <- test_data$survey_split
  survey_bad[1, "list_name"] <- "missing_list"

  expect_warning(
    result <- get_survey_choices(survey_bad, test_data$choices, "q1"),
    "There is no corresponding list_name in choices"
  )
})

# Test get_survey_type and related functions
test_that("get_survey_type returns correct questions for valid types", {
  test_data <- setup_test_data()

  result <- get_survey_type(test_data$survey_split, "select_one")
  expect_equal(result, "q1")

  result <- get_survey_type(test_data$survey_split, "integer")
  expect_equal(result, "q3")
})

test_that("get_survey_type errors for invalid question type", {
  test_data <- setup_test_data()

  expect_error(
    get_survey_type(test_data$survey_split, "invalid_type"),
    "Did you mean the right type"
  )
})

test_that("get_survey_type warns when no questions found", {
  test_data <- setup_test_data()

  expect_warning(
    result <- get_survey_type(test_data$survey_split, "date"),
    "There is no question.*that is of type date"
  )
  expect_equal(result, character(0))
})

test_that("get_survey_type errors when type column missing", {
  test_data <- setup_test_data()
  survey_no_type <- test_data$survey_split[,
    !names(test_data$survey_split) %in% "type"
  ]

  expect_error(get_survey_type(survey_no_type, "integer"))
})

# Test specific type getter functions
test_that("get_survey_select_one works correctly", {
  test_data <- setup_test_data()

  result <- get_survey_select_one(test_data$survey_split)
  expect_equal(result, "q1")
})

test_that("get_survey_select_multiple works correctly", {
  test_data <- setup_test_data()

  result <- get_survey_select_multiple(test_data$survey_split)
  expect_equal(result, "q2")
})

test_that("get_survey_calculate works correctly", {
  test_data <- setup_test_data()

  result <- get_survey_calculate(test_data$survey_split)
  expect_equal(result, "q5")
})

test_that("get_survey_integer works correctly", {
  test_data <- setup_test_data()

  result <- get_survey_integer(test_data$survey_split)
  expect_equal(result, "q3")
})

test_that("get_survey_decimal works correctly", {
  test_data <- setup_test_data()

  result <- get_survey_decimal(test_data$survey_split)
  expect_equal(result, "q6")
})

# Test split_survey function
test_that("split_survey splits type column correctly", {
  test_data <- setup_test_data()

  result <- split_survey(test_data$survey, "type")

  expect_true("list_name" %in% names(result))
  expect_equal(
    result$type,
    c(
      "select_one",
      "select_multiple",
      "integer",
      "text",
      "calculate",
      "decimal"
    )
  )
  expect_equal(result$list_name, c("list1", "list2", NA, NA, NA, NA))
})

test_that("split_survey works with custom parameters", {
  test_data <- setup_test_data()

  # Create test data with different separator
  survey_custom <- data.table::data.table(
    type = c("select_one|list1", "integer|", "text|"),
    name = c("q1", "q2", "q3")
  )

  result <- split_survey(survey_custom, "type", sep = "\\|")

  expect_equal(result$type, c("select_one", "integer", "text"))
  expect_equal(result$list_name, c("list1", NA, NA))
})

test_that("split_survey errors when column to split is missing", {
  test_data <- setup_test_data()

  expect_error(split_survey(test_data$survey, "missing_col"))
})

# Test check_survey function
test_that("check_survey passes for well-formed survey", {
  test_data <- setup_test_data()

  expect_true(check_survey(test_data$survey_split))
})

test_that("check_survey errors when required columns are missing", {
  test_data <- setup_test_data()
  survey_incomplete <- test_data$survey_split[,
    !names(test_data$survey_split) %in% "label"
  ]

  expect_error(check_survey(survey_incomplete))
})

# Edge case tests
test_that("functions handle empty data frames", {
  empty_survey <- data.table::data.table(
    type = character(0),
    list_name = character(0),
    name = character(0),
    label = character(0)
  )

  empty_choices <- data.table::data.table(
    list_name = character(0),
    name = character(0),
    label = character(0)
  )

  expect_warning(
    result <- get_survey_labels(empty_survey, "q1"),
    "None of the column names were found"
  )

  expect_warning(
    result <- get_survey_type(empty_survey, "integer"),
    "There is no question.*that is of type integer"
  )
})

test_that("functions handle single row data frames", {
  single_survey <- data.table::data.table(
    type = "integer",
    list_name = NA,
    name = "q1",
    label = "Question 1"
  )

  result <- get_survey_labels(single_survey, "q1")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "q1")

  result <- get_survey_integer(single_survey)
  expect_equal(result, "q1")
})
