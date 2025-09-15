# Tests for utils.R functions

# Test subvec_in function
test_that("subvec_in returns elements that are in the set", {
  vector <- c("a", "b", "c", "d", "e")
  set <- c("b", "d", "f")
  result <- subvec_in(vector, set)
  expect_equal(result, c("b", "d"))
})

test_that("subvec_in returns empty vector when no elements are in set", {
  vector <- c("a", "b", "c")
  set <- c("x", "y", "z")
  result <- subvec_in(vector, set)
  expect_equal(result, character(0))
})

test_that("subvec_in works with numeric vectors", {
  vector <- c(1, 2, 3, 4, 5)
  set <- c(2, 4, 6)
  result <- subvec_in(vector, set)
  expect_equal(result, c(2, 4))
})

test_that("subvec_in returns all elements when all are in set", {
  vector <- c("a", "b", "c")
  set <- c("a", "b", "c", "d")
  result <- subvec_in(vector, set)
  expect_equal(result, c("a", "b", "c"))
})

# Test subvec_not_in function
test_that("subvec_not_in returns elements that are NOT in the set", {
  vector <- c("a", "b", "c", "d", "e")
  set <- c("b", "d", "f")
  result <- subvec_not_in(vector, set)
  expect_equal(result, c("a", "c", "e"))
})

test_that("subvec_not_in returns all elements when none are in set", {
  vector <- c("a", "b", "c")
  set <- c("x", "y", "z")
  result <- subvec_not_in(vector, set)
  expect_equal(result, c("a", "b", "c"))
})

test_that("subvec_not_in works with numeric vectors", {
  vector <- c(1, 2, 3, 4, 5)
  set <- c(2, 4, 6)
  result <- subvec_not_in(vector, set)
  expect_equal(result, c(1, 3, 5))
})

test_that("subvec_not_in returns empty vector when all elements are in set", {
  vector <- c("a", "b", "c")
  set <- c("a", "b", "c", "d")
  result <- subvec_not_in(vector, set)
  expect_equal(result, character(0))
})

# Test named_group_split function
test_that("named_group_split splits data frame by group and names lists", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    value = 1:5
  )
  result <- named_group_split(df, "group")

  expect_type(result, "list")
  expect_equal(names(result), c("A", "B", "C"))
  expect_equal(nrow(result$A), 2)
  expect_equal(nrow(result$B), 2)
  expect_equal(nrow(result$C), 1)
  expect_equal(result$A$value, c(1, 2))
  expect_equal(result$B$value, c(3, 4))
  expect_equal(result$C$value, 5)
})

test_that("named_group_split works with numeric groups", {
  df <- data.frame(
    group = c(1, 1, 2, 2),
    value = c("a", "b", "c", "d")
  )
  result <- named_group_split(df, "group")

  expect_equal(names(result), c("1", "2"))
  expect_equal(nrow(result$`1`), 2)
  expect_equal(nrow(result$`2`), 2)
})

test_that("named_group_split throws error when group variable is missing", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(named_group_split(df, "missing_col"))
})

test_that("named_group_split throws error with multiple grouping columns", {
  df <- data.table::data.table(a = 1:3, b = 4:6)
  expect_error(
    named_group_split(df, c("a", "b")),
    "on 'group' failed: Must have length 1."
  )
})

# Test df_diff function
test_that("df_diff removes columns from df_a that exist in df_b", {
  df_a <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  df_b <- data.frame(b = 1:2, c = 3:4, e = 5:6)

  result <- df_diff(df_a, df_b)

  expect_equal(names(result), c("a", "d"))
  expect_equal(nrow(result), 3)
})

test_that("df_diff throws an error if any missing values in vars_to_keep", {
  df_a <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  df_b <- data.frame(b = 1:2, c = 3:4, e = 5:6)

  expect_error(
    df_diff(df_a, df_b, c('e', NA)),
    "on 'vars_to_keep' failed: Contains missing values"
  )
})

test_that("df_diff keeps specified columns from df_b", {
  df_a <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  df_b <- data.frame(b = 1:2, c = 3:4, e = 5:6)

  result <- df_diff(df_a, df_b, 'e')

  # Should remove b and c (from df_b) but keep e (specified to keep)
  expect_equal(names(result), c("a", "d"))
})

test_that("df_diff works when df_a has no overlapping columns with df_b", {
  df_a <- data.frame(a = 1:3, b = 4:6)
  df_b <- data.frame(c = 1:2, d = 3:4)

  result <- df_diff(df_a, df_b)

  expect_equal(names(result), c("a", "b"))
  expect_equal(result, df_a)
})

test_that("df_diff throws error when specified keep columns don't exist in df_b", {
  df_a <- data.frame(a = 1:3, b = 4:6)
  df_b <- data.frame(c = 1:2, d = 3:4)

  expect_error(df_diff(df_a, df_b, missing_col))
})

# Test deselect function
test_that("deselect removes specified columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- deselect(df, c("b", "d"))

  expect_equal(names(result), c("a", "c"))
  expect_equal(nrow(result), 3)
})

test_that("deselect works with single column", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  result <- deselect(df, "b")

  expect_equal(names(result), c("a", "c"))
})

test_that("deselect works when removing all but one column", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  result <- deselect(df, c("a", "b"))

  expect_equal(names(result), "c")
  expect_equal(result$c, c(7, 8, 9))
})

test_that("deselect throws error when specified columns don't exist", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(deselect(df, "missing_col"))
})


# Test set_order_before function
test_that("set_order_before moves single column before target", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- set_order_before(df, "d", "b")

  expect_equal(names(result), c("a", "d", "b", "c"))
  expect_equal(result$d, c(10, 11, 12))
  expect_equal(result$b, c(4, 5, 6))
})

test_that("set_order_before moves multiple columns before target", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15)

  result <- set_order_before(df, c("d", "e"), "b")

  expect_equal(names(result), c("a", "d", "e", "b", "c"))
  expect_equal(result$d, c(10, 11, 12))
  expect_equal(result$e, c(13, 14, 15))
})

test_that("set_order_before moves column to beginning when target is first", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- set_order_before(df, "d", "a")

  expect_equal(names(result), c("d", "a", "b", "c"))
  expect_equal(result$d, c(10, 11, 12))
})

test_that("set_order_before moves column to second-to-last when target is last", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- set_order_before(df, "a", "d")

  expect_equal(names(result), c("b", "c", "a", "d"))
  expect_equal(result$a, c(1, 2, 3))
})

test_that("set_order_before works with data.table", {
  df <- data.table::data.table(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- set_order_before(df, "c", "b")

  expect_true(data.table::is.data.table(result))
  expect_equal(names(result), c("a", "c", "b", "d"))
})

test_that("set_order_before converts data.frame to data.table with warning", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_warning(
    result <- set_order_before(df, "c", "b"),
    "df set to data.table"
  )
  expect_true(data.table::is.data.table(result))
})

test_that("set_order_before returns unchanged when columns already in right place", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  expect_warning(
    result <- set_order_before(df, "b", "c", warn_if_unchanged = TRUE),
    "Vars are already in the right place. df unchanged."
  )
  expect_equal(names(result), c("a", "b", "c", "d"))
})

test_that("set_order_before returns unchanged when multiple columns already in right place", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15)

  expect_warning(
    result <- set_order_before(df, c("b", "c"), "d", warn_if_unchanged = TRUE),
    "Vars are already in the right place. df unchanged."
  )
  expect_equal(names(result), c("a", "b", "c", "d", "e"))
})

test_that("set_order_before throws error when vars not in df", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    set_order_before(df, "missing_col", "b"),
    "The following variables from vars are missing: missing_col"
  )
})

test_that("set_order_before throws error when target_var not in df", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    set_order_before(df, "b", "missing_target"),
    "The following variables from vars are missing: missing_target"
  )
})

test_that("set_order_before throws error when vars contains NA", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    set_order_before(df, c("b", NA), "c"),
    "on 'vars' failed: Contains missing values"
  )
})

test_that("set_order_before throws error when vars is empty", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    set_order_before(df, character(0), "c"),
    "on 'vars' failed: Must have length >= 1"
  )
})

test_that("set_order_before works with single column dataframe", {
  df <- data.frame(a = 1:3)

  expect_warning(
    result <- set_order_before(df, "a", "a"),
    "Vars are already in the right place. df unchanged."
  )
  expect_equal(names(result), "a")
})

test_that("set_order_before preserves column data types", {
  df <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = as.factor(c("A", "B", "C")),
    d = c(TRUE, FALSE, TRUE)
  )

  result <- set_order_before(df, c("c", "d"), "b")

  expect_equal(names(result), c("a", "c", "d", "b"))
  expect_type(result$a, "integer")
  expect_type(result$b, "character")
  expect_s3_class(result$c, "factor")
  expect_type(result$d, "logical")
})

test_that("set_order_before works when moving columns from end to middle", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15, f = 16:18)

  result <- set_order_before(df, c("e", "f"), "c")

  expect_equal(names(result), c("a", "b", "e", "f", "c", "d"))
})

# Test add_select_multiple_parent function
test_that("add_select_multiple_parent creates parent column with selected options", {
  dt <- data.table::data.table(
    skills_communication = c(1, 0, 1, 0),
    skills_leadership = c(0, 1, 1, 0),
    skills_technical = c(1, 1, 0, 1)
  )

  choices <- c("Communication", "Leadership", "Technical")
  child_cols <- c(
    "skills_communication",
    "skills_leadership",
    "skills_technical"
  )

  result <- add_select_multiple_parent(dt, "skills", child_cols, choices)

  expect_equal(
    result$skills,
    c(
      "Communication Technical",
      "Leadership Technical",
      "Communication Leadership",
      "Technical"
    )
  )
  expect_true("skills" %in% names(result))
})

test_that("add_select_multiple_parent handles NA values correctly", {
  dt <- data.table::data.table(
    opt1 = c(1, 0, NA, 0),
    opt2 = c(0, NA, 1, 0),
    opt3 = c(0, 0, 0, 0)
  )

  choices <- c("Option1", "Option2", "Option3")
  child_cols <- c("opt1", "opt2", "opt3")

  result <- add_select_multiple_parent(dt, "combined", child_cols, choices)

  expect_equal(result$combined, c("Option1", NA, "Option2", NA))
})

test_that("add_select_multiple_parent works with custom separator", {
  dt <- data.table::data.table(
    a = c(1, 1),
    b = c(1, 0),
    c = c(0, 1)
  )

  choices <- c("A", "B", "C")
  child_cols <- c("a", "b", "c")

  result <- add_select_multiple_parent(
    dt,
    "combined",
    child_cols,
    choices,
    sep = " | "
  )

  expect_equal(result$combined, c("A | B", "A | C"))
  # not that generally speaking, when all are 0s a questionnaire should provide a 'none' value to be consistent
})

test_that("add_select_multiple_parent works with custom na_value", {
  dt <- data.table::data.table(
    opt1 = c(0, 1),
    opt2 = c(0, 0)
  )

  choices <- c("Option1", "Option2")
  child_cols <- c("opt1", "opt2")

  result <- add_select_multiple_parent(
    dt,
    "combined",
    child_cols,
    choices,
    na_value = "None"
  )

  expect_equal(result$combined, c("None", "Option1"))
})

test_that("add_select_multiple_parent throws error when choices and child_cols length mismatch", {
  dt <- data.table::data.table(
    a = c(1, 0),
    b = c(0, 1)
  )

  choices <- c("A", "B", "C") # 3 choices
  child_cols <- c("a", "b") # 2 columns

  expect_error(
    add_select_multiple_parent(dt, "combined", child_cols, choices),
    "choices must have the same length as child_vars"
  )
})

test_that("add_select_multiple_parent throws error when child columns missing", {
  dt <- data.table::data.table(
    a = c(1, 0),
    b = c(0, 1)
  )

  choices <- c("A", "B", "C")
  child_cols <- c("a", "b", "missing_col")

  expect_error(
    add_select_multiple_parent(dt, "combined", child_cols, choices),
    "Missing variables in df"
  )
})

test_that("add_select_multiple_parent converts data.frame to data.table with warning", {
  df <- data.frame(
    a = c(1, 0),
    b = c(0, 1)
  )

  choices <- c("A", "B")
  child_cols <- c("a", "b")

  expect_warning(
    result <- add_select_multiple_parent(df, "combined", child_cols, choices),
    "dt set to data.table"
  )
  expect_true(data.table::is.data.table(result))
})

test_that("add_select_multiple_parent renames child columns with choices_sep", {
  dt <- data.table::data.table(
    q1 = c(1, 0, 1),
    q2 = c(0, 1, 1),
    q3 = c(1, 0, 0)
  )

  choices <- c("communication", "leadership", "technical")
  child_cols <- c("q1", "q2", "q3")

  result <- add_select_multiple_parent(
    dt,
    "skills",
    child_cols,
    choices,
    choices_sep = "_"
  )

  # Check that child columns were renamed
  expected_names <- c(
    "skills_communication",
    "skills_leadership",
    "skills_technical",
    "skills"
  )
  expect_true(all(expected_names %in% names(result)))
  expect_false(any(c("q1", "q2", "q3") %in% names(result)))

  # Check that parent column has correct values
  expect_equal(
    result$skills,
    c("communication technical", "leadership", "communication leadership")
  )
})


test_that("add_select_multiple_parent positions parent column before first child column", {
  dt <- data.table::data.table(
    a = 1:3,
    skills_communication = c(1, 0, 1),
    skills_leadership = c(0, 1, 1),
    skills_technical = c(1, 0, 0),
    z = 4:6
  )

  choices <- c("Communication", "Leadership", "Technical")
  child_cols <- c(
    "skills_communication",
    "skills_leadership",
    "skills_technical"
  )

  result <- add_select_multiple_parent(
    dt,
    "skills",
    child_cols,
    choices
  )

  # Check that parent column is positioned before first child column
  expected_names <- c(
    "a",
    "skills",
    "skills/Communication",
    "skills/Leadership",
    "skills/Technical",
    "z"
  )
  expect_equal(names(result), expected_names)
})

test_that("add_select_multiple_parent positions parent column correctly with choices_sep", {
  dt <- data.table::data.table(
    a = 1:3,
    q1 = c(1, 0, 1),
    q2 = c(0, 1, 1),
    q3 = c(1, 0, 0),
    z = 4:6
  )

  choices <- c("communication", "leadership", "technical")
  child_cols <- c("q1", "q2", "q3")

  result <- add_select_multiple_parent(
    dt,
    "skills",
    child_cols,
    choices,
    choices_sep = "_"
  )

  # Check that parent column is positioned before first child column (after renaming)
  expected_names <- c(
    "a",
    "skills",
    "skills_communication",
    "skills_leadership",
    "skills_technical",
    "z"
  )
  expect_equal(names(result), expected_names)
})
