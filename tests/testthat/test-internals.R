# Tests for internals.R functions

# Test add_group_key function
test_that("add_group_key works with basic inputs", {
  df <- data.frame(
    id = 1:4,
    region = c("North", "South", "North", "East"),
    district = c("A", "B", "A", "C"),
    value = c(10, 20, 30, 40)
  )

  result <- add_group_key(
    df = df,
    group = c("region", "district"),
    group_key_sep = "_",
    before = "value"
  )

  # Check that result is a data.table
  expect_true(data.table::is.data.table(result))

  # Check that group_key column was added with correct value
  expect_equal(unique(result$group_key), "region_district")

  # Check that group_key_value was created correctly
  expected_values <- c("North_A", "South_B", "North_A", "East_C")
  expect_equal(result$group_key_value, expected_values)

  # Check column ordering - group_key and group_key_value should be before 'value'
  col_names <- names(result)
  value_pos <- which(col_names == "value")
  group_key_pos <- which(col_names == "group_key")
  group_key_value_pos <- which(col_names == "group_key_value")

  expect_true(group_key_pos < value_pos)
  expect_true(group_key_value_pos < value_pos)
  expect_true(group_key_pos < group_key_value_pos)
})

test_that("add_group_key works with single grouping variable", {
  df <- data.frame(
    id = 1:3,
    category = c("A", "B", "A"),
    score = c(85, 90, 88)
  )

  result <- add_group_key(
    df = df,
    group = "category",
    group_key_sep = "-",
    before = "score"
  )

  expect_equal(result$group_key_value, c("A", "B", "A"))
  expect_equal(unique(result$group_key), "category")
})

test_that("add_group_key works with data.table input", {
  df <- data.table::data.table(
    x = 1:2,
    y = c("foo", "bar"),
    z = c(100, 200)
  )

  result <- add_group_key(
    df = df,
    group = "y",
    group_key_sep = "|",
    before = "z"
  )

  expect_true(data.table::is.data.table(result))
  expect_equal(result$group_key_value, c("foo", "bar"))
})

test_that("add_group_key handles custom separators correctly", {
  df <- data.frame(
    a = c("X", "Y"),
    b = c("1", "2"),
    c = c("alpha", "beta")
  )

  result <- add_group_key(
    df = df,
    group = c("a", "b"),
    group_key_sep = "***",
    before = "c"
  )

  expect_equal(result$group_key_value, c("X***1", "Y***2"))
})

# Parameter validation tests for add_group_key
test_that("add_group_key throws error for invalid df parameter", {
  expect_error(
    add_group_key(
      df = "not_a_dataframe",
      group = "x",
      group_key_sep = "_",
      before = "y"
    ),
    "Assertion on 'df' failed"
  )
})

test_that("add_group_key throws error for missing group columns", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_group_key(
      df = df,
      group = c("a", "missing_col"),
      group_key_sep = "_",
      before = "b"
    ),
    "Missing variables"
  )
})

test_that("add_group_key throws error for non-character group parameter", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_group_key(
      df = df,
      group = 123,
      group_key_sep = "_",
      before = "b"
    ),
    "Assertion on 'group' failed"
  )
})

test_that("add_group_key throws error for non-character group_key_sep", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_group_key(
      df = df,
      group = "a",
      group_key_sep = 123,
      before = "b"
    ),
    "Assertion on 'group_key_sep' failed"
  )
})

test_that("add_group_key throws error for missing before column", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_group_key(
      df = df,
      group = "a",
      group_key_sep = "_",
      before = "missing_col"
    ),
    "Missing variables"
  )
})

# Test add_interact_key function
test_that("add_interact_key works with basic inputs", {
  df <- data.frame(
    id = 1:4,
    var1 = c("A", "B", "A", "C"),
    var2 = c("X", "Y", "X", "Z"),
    result = c(10, 20, 30, 40)
  )

  result <- add_interact_key(
    df = df,
    interact = c("var1", "var2"),
    interact_key_sep = "x",
    before = "result"
  )

  # Check that result is a data.table
  expect_true(data.table::is.data.table(result))

  # Check that interact_key column was added with correct value
  expect_equal(unique(result$interact_key), "var1xvar2")

  # Check that interact_key_value was created correctly
  expected_values <- c("AxX", "BxY", "AxX", "CxZ")
  expect_equal(result$interact_key_value, expected_values)

  # Check column ordering
  col_names <- names(result)
  result_pos <- which(col_names == "result")
  interact_key_pos <- which(col_names == "interact_key")
  interact_key_value_pos <- which(col_names == "interact_key_value")

  expect_true(interact_key_pos < result_pos)
  expect_true(interact_key_value_pos < result_pos)
  expect_true(interact_key_pos < interact_key_value_pos)
})

test_that("add_interact_key works with single interaction variable", {
  df <- data.frame(
    treatment = c("Control", "Treatment", "Control"),
    outcome = c(5, 8, 6)
  )

  result <- add_interact_key(
    df = df,
    interact = "treatment",
    interact_key_sep = "_",
    before = "outcome"
  )

  expect_equal(result$interact_key_value, c("Control", "Treatment", "Control"))
  expect_equal(unique(result$interact_key), "treatment")
})

test_that("add_interact_key works with data.table input", {
  df <- data.table::data.table(
    factor1 = c("High", "Low"),
    factor2 = c("Urban", "Rural"),
    measurement = c(15, 25)
  )

  result <- add_interact_key(
    df = df,
    interact = c("factor1", "factor2"),
    interact_key_sep = ":",
    before = "measurement"
  )

  expect_true(data.table::is.data.table(result))
  expect_equal(result$interact_key_value, c("High:Urban", "Low:Rural"))
})

# Parameter validation tests for add_interact_key
test_that("add_interact_key throws error for invalid df parameter", {
  expect_error(
    add_interact_key(
      df = list(a = 1),
      interact = "x",
      interact_key_sep = "_",
      before = "y"
    ),
    "Assertion on 'df' failed"
  )
})

test_that("add_interact_key throws error for missing interact columns", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_interact_key(
      df = df,
      interact = c("a", "missing_col"),
      interact_key_sep = "_",
      before = "b"
    ),
    "Missing variables"
  )
})

test_that("add_interact_key throws error for non-character interact parameter", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_interact_key(
      df = df,
      interact = 123,
      interact_key_sep = "_",
      before = "b"
    ),
    "Assertion on 'interact' failed"
  )
})

test_that("add_interact_key throws error for non-character interact_key_sep", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_interact_key(
      df = df,
      interact = "a",
      interact_key_sep = 123,
      before = "b"
    ),
    "Assertion on 'interact_key_sep' failed"
  )
})

test_that("add_interact_key throws error for missing before column", {
  df <- data.frame(a = 1, b = 2)

  expect_error(
    add_interact_key(
      df = df,
      interact = "a",
      interact_key_sep = "_",
      before = "missing_col"
    ),
    "Missing variables"
  )
})

# Test edge cases
test_that("add_group_key handles empty separator", {
  df <- data.frame(
    x = c("A", "B"),
    y = c("1", "2"),
    z = c(10, 20)
  )

  result <- add_group_key(
    df = df,
    group = c("x", "y"),
    group_key_sep = "",
    before = "z"
  )

  expect_equal(result$group_key_value, c("A1", "B2"))
})

test_that("add_interact_key handles special characters in data", {
  df <- data.frame(
    category = c("A-1", "B/2"),
    type = c("X@Y", "Z#W"),
    value = c(1, 2)
  )

  result <- add_interact_key(
    df = df,
    interact = c("category", "type"),
    interact_key_sep = "|",
    before = "value"
  )

  expect_equal(result$interact_key_value, c("A-1|X@Y", "B/2|Z#W"))
})

test_that("functions preserve original data integrity", {
  original_df <- data.frame(
    id = 1:3,
    group = c("A", "B", "A"),
    value = c(10, 20, 30)
  )

  # Test add_group_key
  result1 <- add_group_key(
    df = original_df,
    group = "group",
    group_key_sep = "_",
    before = "value"
  )

  # Original columns should still exist with same values
  expect_equal(result1$id, c(1, 2, 3))
  expect_equal(result1$group, c("A", "B", "A"))
  expect_equal(result1$value, c(10, 20, 30))

  # Test add_interact_key
  result2 <- add_interact_key(
    df = original_df,
    interact = "group",
    interact_key_sep = "-",
    before = "value"
  )

  # Original columns should still exist with same values
  expect_equal(result2$id, c(1, 2, 3))
  expect_equal(result2$group, c("A", "B", "A"))
  expect_equal(result2$value, c(10, 20, 30))
})

# Test add_analysis_key function
test_that("add_analysis_key works with basic inputs", {
  results <- data.frame(
    group_key = c("region -/- district", "region -/- district"),
    group_key_value = c("North -/- A", "South -/- B"),
    var = c("age -/- gender", "age -/- gender"),
    var_value = c("young -/- male", "old -/- female"),
    stat_type = c("mean", "proportion"),
    estimate = c(25.5, 0.6)
  )

  result <- add_analysis_key(results)

  # Check that result is a data.table
  expect_true(data.table::is.data.table(result))

  # Check that analysis_key column was added
  expect_true("analysis_key" %in% names(result))

  # Check the structure of analysis_key
  expected_key1 <- "mean @/@ age %/% young -/- gender %/% male @/@ region %/% North -/- district %/% A"
  expected_key2 <- "proportion @/@ age %/% old -/- gender %/% female @/@ region %/% South -/- district %/% B"

  expect_equal(result$analysis_key[1], expected_key1)
  expect_equal(result$analysis_key[2], expected_key2)
})

test_that("add_analysis_key works with custom parameters", {
  results <- data.frame(
    my_group_key = c("var1 var2"),
    my_group_value = c("val1 val2"),
    my_var = c("question1 question2"),
    my_var_value = c("ans1 ans2"),
    stat_type = c("median"),
    result = c(15.2)
  )

  result <- add_analysis_key(
    results,
    group_key_name = "my_group_key",
    group_key_value_name = "my_group_value",
    var_name = "my_var",
    var_value_name = "my_var_value",
    group_key_sep = " ",
    var_key_sep = " ",
    overall_sep = " || ",
    main_sep = " & ",
    var_to_value_sep = " = "
  )

  expected_key <- "median || question1 = ans1 & question2 = ans2 || var1 = val1 & var2 = val2"
  expect_equal(result$analysis_key[1], expected_key)
})

test_that("add_analysis_key works with single variables", {
  results <- data.frame(
    group_key = c("location"),
    group_key_value = c("urban"),
    var = c("income"),
    var_value = c("high"),
    stat_type = c("count"),
    n = c(150)
  )

  result <- add_analysis_key(results)

  expected_key <- "count @/@ income %/% high @/@ location %/% urban"
  expect_equal(result$analysis_key[1], expected_key)
})

test_that("add_analysis_key works with data.table input", {
  results <- data.table::data.table(
    group_key = c("category"),
    group_key_value = c("type1"),
    var = c("measure"),
    var_value = c("positive"),
    stat_type = c("total"),
    value = c(42)
  )

  result <- add_analysis_key(results)

  expect_true(data.table::is.data.table(result))
  expect_true("analysis_key" %in% names(result))
})

test_that("add_analysis_key preserves original data", {
  original_results <- data.frame(
    group_key = c("x y"),
    group_key_value = c("1 2"),
    var = c("a b"),
    var_value = c("m n"),
    stat_type = c("test"),
    original_col = c("preserve_me")
  )

  result <- add_analysis_key(original_results)

  # Check that original columns are preserved
  expect_equal(result$group_key, "x y")
  expect_equal(result$group_key_value, "1 2")
  expect_equal(result$var, "a b")
  expect_equal(result$var_value, "m n")
  expect_equal(result$stat_type, "test")
  expect_equal(result$original_col, "preserve_me")
})

# Parameter validation tests for add_analysis_key
test_that("add_analysis_key throws error for invalid results parameter", {
  expect_error(
    add_analysis_key("not_a_dataframe"),
    "Assertion on 'results' failed"
  )
})

test_that("add_analysis_key throws error for missing required columns", {
  results <- data.frame(
    group_key = "test",
    # missing group_key_value
    var = "test",
    var_value = "test",
    stat_type = "test"
  )

  expect_error(
    add_analysis_key(results),
    "Missing variables"
  )
})

test_that("add_analysis_key throws error for non-character column names", {
  results <- data.frame(
    group_key = "test",
    group_key_value = "test",
    var = "test",
    var_value = "test",
    stat_type = "test"
  )

  expect_error(
    add_analysis_key(results, group_key_name = 123),
    "Assertion on 'group_key_name' failed"
  )
})

test_that("add_analysis_key throws error for non-character separators", {
  results <- data.frame(
    group_key = "test",
    group_key_value = "test",
    var = "test",
    var_value = "test",
    stat_type = "test"
  )

  expect_error(
    add_analysis_key(results, overall_sep = 123),
    "Assertion on 'overall_sep' failed"
  )
})

test_that("add_analysis_key handles empty strings correctly", {
  results <- data.frame(
    group_key = c(""),
    group_key_value = c(""),
    var = c("test"),
    var_value = c("value"),
    stat_type = c("mean")
  )

  result <- add_analysis_key(results)

  # Should still create analysis_key even with empty group components
  expect_true("analysis_key" %in% names(result))
  expect_true(nchar(result$analysis_key[1]) > 0)
})

test_that("add_analysis_key handles multiple rows correctly", {
  results <- data.frame(
    group_key = c("region", "region", "country"),
    group_key_value = c("north", "south", "france"),
    var = c("age", "age", "income"),
    var_value = c("young", "old", "high"),
    stat_type = c("mean", "mean", "median"),
    estimate = c(25, 65, 50000)
  )

  result <- add_analysis_key(results)

  expect_equal(length(result$analysis_key), 3)
  expect_true(all(nchar(result$analysis_key) > 0))

  # Each row should have different analysis_key values
  expect_equal(length(unique(result$analysis_key)), 3)
})
