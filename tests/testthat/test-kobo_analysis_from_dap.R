# Tests for kobo_analysis_from_dap and kobo_analysis_from_dap_group functions

# Helper function to create test data for kobo_analysis_from_dap
create_dap_test_data <- function(choices_sep = "/") {
  set.seed(123)
  n <- 100

  # Create sample survey data with various variable types
  data <- data.frame(
    # Numeric variables
    age = round(rnorm(n, mean = 35, sd = 10)),
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    score = rnorm(n, mean = 75, sd = 12),
    expenses = round(rnorm(n, mean = 30000, sd = 10000)),
    household_size = round(runif(n, min = 1, max = 8)),

    # Select_one variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    education = sample(
      c("Primary", "Secondary", "University"),
      n,
      replace = TRUE
    ),
    employment = sample(
      c("Employed", "Unemployed", "Student"),
      n,
      replace = TRUE
    ),

    # Select_multiple variables (binary flags)
    skills_communication = sample(c(0, 1), n, replace = TRUE),
    skills_leadership = sample(c(0, 1), n, replace = TRUE),
    skills_technical = sample(c(0, 1), n, replace = TRUE),

    # Interaction variables
    var1 = sample(c("A", "B", "C"), n, replace = TRUE),
    var2 = sample(c("X", "Y"), n, replace = TRUE),
    var3 = sample(c("High", "Low"), n, replace = TRUE),

    # Grouping variables
    survey_round = sample(c("Round1", "Round2"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE),

    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  data <- add_select_multiple_parent(
    data,
    parent_var = "skills",
    child_vars = c(
      "skills_communication",
      "skills_leadership",
      "skills_technical"
    ),
    choices = c("communication", "leadership", "technical"),
    choices_sep = choices_sep,
    sep = " "
  )

  # Ensure no zero denominators for ratio analysis
  data$household_size[data$household_size == 0] <- 1

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet
  survey <- data.table::data.table(
    type = c(
      "integer",
      "decimal",
      "decimal",
      "decimal",
      "integer",
      "select_one",
      "select_one",
      "select_one",
      "select_multiple",
      "select_one",
      "select_one",
      "select_one"
    ),
    list_name = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "regions",
      "education_levels",
      "employment_status",
      "skills",
      "options1",
      "options2",
      "options3"
    ),
    name = c(
      "age",
      "income",
      "score",
      "expenses",
      "household_size",
      "region",
      "education",
      "employment",
      "skills",
      "var1",
      "var2",
      "var3"
    ),
    label = c(
      "Age in years",
      "Annual income",
      "Test score",
      "Monthly expenses",
      "Household size",
      "Geographic region",
      "Education level",
      "Employment status",
      "Skills assessment",
      "Variable 1",
      "Variable 2",
      "Variable 3"
    )
  )

  # Create choices sheet
  choices <- data.table::data.table(
    list_name = c(
      "regions",
      "regions",
      "regions",
      "regions",
      "education_levels",
      "education_levels",
      "education_levels",
      "employment_status",
      "employment_status",
      "employment_status",
      "skills",
      "skills",
      "skills",
      "options1",
      "options1",
      "options1",
      "options2",
      "options2",
      "options3",
      "options3"
    ),
    name = c(
      "North",
      "South",
      "East",
      "West",
      "Primary",
      "Secondary",
      "University",
      "Employed",
      "Unemployed",
      "Student",
      "communication",
      "leadership",
      "technical",
      "A",
      "B",
      "C",
      "X",
      "Y",
      "High",
      "Low"
    ),
    label = c(
      "Northern Region",
      "Southern Region",
      "Eastern Region",
      "Western Region",
      "Primary Education",
      "Secondary Education",
      "University Education",
      "Employed Full-time",
      "Unemployed",
      "Student",
      "Communication Skills",
      "Leadership Skills",
      "Technical Skills",
      "Option A",
      "Option B",
      "Option C",
      "Choice X",
      "Choice Y",
      "High Level",
      "Low Level"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

# Helper function to create simple DAP
create_simple_dap <- function() {
  data.table::data.table(
    analysis = c(
      "mean",
      "median",
      "select_one",
      "select_multiple",
      "ratio",
      "interact"
    ),
    var = c(
      "age",
      "score",
      "region",
      "skills",
      "income,household_size",
      "var1,var2"
    ),
    na_rm = c("yes", "yes", "no", "yes", "yes", "no")
  )
}

# Helper function to create complex DAP
create_complex_dap <- function() {
  data.table::data.table(
    analysis = c(
      "mean",
      "mean",
      "median",
      "select_one",
      "select_one",
      "select_multiple",
      "ratio",
      "ratio",
      "interact",
      "interact"
    ),
    var = c(
      "age",
      "income",
      "score",
      "region",
      "education",
      "skills",
      "income,household_size",
      "expenses,household_size",
      "var1,var2",
      "var1,var2,var3"
    ),
    na_rm = c(
      "yes",
      "no",
      "yes",
      "yes",
      "no",
      "yes",
      "yes",
      "no",
      "yes",
      "no"
    ),
    priority = c(1, 2, 1, 1, 2, 1, 1, 2, 1, 2),
    category = c(
      "demographics",
      "economics",
      "assessment",
      "geography",
      "education",
      "skills",
      "ratios",
      "ratios",
      "interactions",
      "interactions"
    )
  )
}

# Test basic functionality
test_that("kobo_analysis_from_dap works with simple DAP", {
  test_data <- create_dap_test_data()
  dap <- create_simple_dap()

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_true("id_analysis" %in% colnames(result))
  expect_equal(length(unique(result$id_analysis)), nrow(dap))

  # Check that all analysis types are present
  expect_setequal(unique(result$analysis), dap$analysis)

  # Check that original DAP columns are preserved
  expect_true("na_rm" %in% colnames(result))
})

test_that("kobo_analysis_from_dap handles mean analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "mean")
  expect_equal(unique(result$stat_type), "mean")
  expect_equal(nrow(result), 1)
})

test_that("kobo_analysis_from_dap handles median analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "median",
    var = "score",
    na_rm = "no"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "median")
  expect_equal(unique(result$stat_type), "median")
  expect_equal(nrow(result), 1)
})

test_that("kobo_analysis_from_dap handles select_one analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "select_one",
    var = "region",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_one")
  expect_equal(unique(result$stat_type), "proportion")
  expect_true(nrow(result) > 1) # Multiple categories
})

test_that("kobo_analysis_from_dap handles select_multiple analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "select_multiple",
    var = "skills",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_multiple")
  expect_equal(unique(result$stat_type), "mean")
  expect_true(nrow(result) >= 3) # At least 3 skills
})

test_that("kobo_analysis_from_dap handles ratio analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "ratio",
    var = "income,household_size",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "ratio")
  expect_equal(unique(result$stat_type), "ratio")
  expect_equal(nrow(result), 1)
  expect_true(grepl("income.*household_size", result$var))
})

test_that("kobo_analysis_from_dap handles interact analysis correctly", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "interact",
    var = "var1,var2",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")
  expect_equal(unique(result$stat_type), "proportion")
  expect_true("interact_key" %in% colnames(result))
  expect_true(nrow(result) > 1) # Multiple interactions
})

test_that("kobo_analysis_from_dap handles three-way interactions", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "interact",
    var = "var1,var2,var3",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")
  expect_true(all(grepl("var1.*var2.*var3", result$interact_key)))
})

# Test with grouping
test_that("kobo_analysis_from_dap works with grouping", {
  test_data <- create_dap_test_data()
  dap <- create_simple_dap()

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    group = "gender"
  )

  expect_s3_class(result, "data.table")
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_setequal(unique(result$group_key_value), c("Male", "Female"))
})

test_that("kobo_analysis_from_dap works with multiple grouping variables", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("gender", "survey_round")
  )

  expect_s3_class(result, "data.table")
  expect_true(all(grepl("gender.*survey_round", result$group_key)))
})

# Test complex DAP with additional columns
test_that("kobo_analysis_from_dap preserves additional DAP columns", {
  test_data <- create_dap_test_data()
  dap <- create_complex_dap()

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  # Check that additional columns are preserved
  expect_true("priority" %in% colnames(result))
  expect_true("category" %in% colnames(result))
  expect_setequal(unique(result$priority), c(1, 2))
  expect_true(length(unique(result$category)) > 1)
})

# Test parameter validation
test_that("kobo_analysis_from_dap validates DAP structure", {
  test_data <- create_dap_test_data()

  # Missing required columns
  expect_error(
    kobo_analysis_from_dap(
      design = test_data$design,
      dap = data.table::data.table(analysis = "mean"),
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "Missing variables"
  )

  # Invalid analysis type
  dap_invalid <- data.table::data.table(
    analysis = "invalid_analysis",
    var = "age",
    na_rm = "yes"
  )

  expect_error(
    kobo_analysis_from_dap(
      design = test_data$design,
      dap = dap_invalid,
      survey = test_data$survey,
      choices = test_data$choices
    )
  )
})

test_that("kobo_analysis_from_dap erros at empty DAP (no columns)", {
  test_data <- create_dap_test_data()

  expect_error(
    kobo_analysis_from_dap(
      design = test_data$design,
      dap = data.table::data.table(),
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "Assertion on 'df' failed: Must have at least 1 cols, but has 0 cols."
  )
})

# Test na_rm handling
test_that("kobo_analysis_from_dap handles na_rm correctly", {
  test_data <- create_dap_test_data()

  # Add some NAs
  test_data$data$age[1:5] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  dap_rm <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  dap_keep <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "no"
  )

  result_rm <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap_rm,
    survey = test_data$survey,
    choices = test_data$choices
  )

  result_keep <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap_keep,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")
  # Results should be different due to NA handling
  expect_false(identical(result_rm$stat, result_keep$stat))
})

# Test different confidence levels
test_that("kobo_analysis_from_dap handles different confidence levels", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  result_95 <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    level = 0.95
  )

  result_99 <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    level = 0.99
  )

  # 99% CI should be wider than 95% CI
  ci_width_95 <- result_95$stat_upp - result_95$stat_low
  ci_width_99 <- result_99$stat_upp - result_99$stat_low

  expect_true(ci_width_99 >= ci_width_95)
})

# Test choices_sep parameter
test_that("kobo_analysis_from_dap handles custom choices_sep", {
  test_data <- create_dap_test_data(choices_sep = "|")
  dap <- data.table::data.table(
    analysis = "select_multiple",
    var = "skills",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    choices_sep = "|"
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_multiple")
  expect_equal(unique(result$stat_type), "mean")
  expect_length(result$var_value, 3) # Three skills
})

# Test variable parsing for ratios and interactions
test_that("kobo_analysis_from_dap parses ratio variables correctly", {
  test_data <- create_dap_test_data()

  # Test with spaces around comma
  dap <- data.table::data.table(
    analysis = "ratio",
    var = " income , household_size ",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "ratio")
  expect_true(grepl("income.*household_size", result$var))
})

test_that("kobo_analysis_from_dap parses interaction variables correctly", {
  test_data <- create_dap_test_data()

  # Test with spaces around commas
  dap <- data.table::data.table(
    analysis = "interact",
    var = " var1 ,var2 , var3",
    na_rm = "yes"
  )

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")
  expect_true(all(grepl("var1.*var2.*var3", result$interact_key)))
})

# Tests for kobo_analysis_from_dap_group
test_that("kobo_analysis_from_dap_group works with multiple groups", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = c("mean", "select_one"),
    var = c("age", "region"),
    na_rm = c("yes", "yes")
  )

  l_group <- list(
    "gender_only" = "gender",
    "round_only" = "survey_round",
    "both" = c("gender", "survey_round")
  )

  result <- kobo_analysis_from_dap_group(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    l_group = l_group,
    no_group = TRUE
  )

  expect_s3_class(result, "data.table")

  # Should have results for each group + no grouping
  expected_groups <- c("gender", "survey_round", "gender -/- survey_round", NA)
  result_groups <- unique(result$group_key)
  expect_true(all(expected_groups %in% result_groups))
})

test_that("kobo_analysis_from_dap_group works without no_group", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  l_group <- list(
    "gender_only" = "gender",
    "round_only" = "survey_round"
  )

  result <- kobo_analysis_from_dap_group(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices,
    l_group = l_group,
    no_group = FALSE
  )

  expect_s3_class(result, "data.table")

  # Should not have ungrouped results
  expect_false(any(is.na(result$group_key)))
})

test_that("kobo_analysis_from_dap_group validates l_group parameter", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  # Test non-list input
  expect_error(
    kobo_analysis_from_dap_group(
      design = test_data$design,
      dap = dap,
      survey = test_data$survey,
      choices = test_data$choices,
      l_group = "gender" # Should be a list
    ),
    "'l_group' should be a list"
  )

  # Test invalid variables in group
  expect_error(
    kobo_analysis_from_dap_group(
      design = test_data$design,
      dap = dap,
      survey = test_data$survey,
      choices = test_data$choices,
      l_group = list("invalid" = "nonexistent_var")
    )
  )
})

# Test edge cases and error conditions
test_that("kobo_analysis_from_dap handles missing variables gracefully", {
  test_data <- create_dap_test_data()
  dap <- data.table::data.table(
    analysis = "mean",
    var = "nonexistent_variable",
    na_rm = "yes"
  )

  expect_error(
    kobo_analysis_from_dap(
      design = test_data$design,
      dap = dap,
      survey = test_data$survey,
      choices = test_data$choices
    )
  )
})

test_that("kobo_analysis_from_dap preserves id_analysis uniqueness", {
  test_data <- create_dap_test_data()
  dap <- create_complex_dap()

  result <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Each original DAP row should have a unique id_analysis
  expect_equal(length(unique(result$id_analysis)), nrow(dap))

  # Check that id_analysis follows expected pattern
  expect_true(all(grepl("^analysis_", result$id_analysis)))
})

# Test consistency with direct kobo_analysis calls
test_that("kobo_analysis_from_dap produces consistent results with direct calls", {
  test_data <- create_dap_test_data()

  # Test mean analysis consistency
  dap_mean <- data.table::data.table(
    analysis = "mean",
    var = "age",
    na_rm = "yes"
  )

  result_dap <- kobo_analysis_from_dap(
    design = test_data$design,
    dap = dap_mean,
    survey = test_data$survey,
    choices = test_data$choices
  )

  result_direct <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "age",
    survey = test_data$survey,
    na_rm = TRUE
  )

  expect_equal(result_dap$stat, result_direct$stat, tolerance = 1e-10)
  expect_equal(result_dap$analysis, result_direct$analysis)
})
