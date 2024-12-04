library(testthat)

test_that("build_prompts_from_files works correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Define file paths for the test prompts
  claude_user_file <- test_path("test-prompts/claude_user.md")
  gemini_user_file <- test_path("test-prompts/gemini_user.md")
  groq_system_file <- test_path("test-prompts/groq_system.md")
  groq_user_file <- test_path("test-prompts/groq_user.md")
  openai_system_file <- test_path("test-prompts/groq_system.md")
  openai_user_file <- test_path("test-prompts/groq_user.md")
  llamafile_system_file <- test_path("test-prompts/groq_system.md")
  llamafile_user_file <- test_path("test-prompts/groq_user.md")

  # Ensure the test files exist
  expect_true(file.exists(claude_user_file))
  expect_true(file.exists(gemini_user_file))
  expect_true(file.exists(groq_system_file))
  expect_true(file.exists(groq_user_file))

  # Define the roles and API
  groq_roles <- openai_roles <- llamafile_roles <- c("system", "user")
  api <- "llamafile"

  # Create a test data frame
  test_data <- data.frame(country = c("USA", "Canada", "Mexico"))

  # Test the function with the test files and data
  prompts <- build_prompts_from_files(
    files = c(groq_system_file, groq_user_file),
    roles = groq_roles,
    api = api,
    data = test_data
  )

  # Check the structure of the returned prompts
  expect_type(prompts, "list")
  expect_equal(length(prompts), 3)
  expect_equal(class(prompts), c(api, "combined_prompts", "list"))

  # Check that each prompt is correctly formatted
  expect_true(all(sapply(prompts, function(x) is.list(x) && length(x) == 2)))
  expect_equal(names(prompts), c("1", "2", "3"))

  # Test the function without data interpolation
  prompts_no_data <- build_prompts_from_files(
    files = c(groq_system_file, groq_user_file),
    roles = groq_roles,
    api = api
  )

  # Check the structure of the returned prompts
  expect_type(prompts_no_data, "list")
  expect_equal(length(prompts_no_data), 1)
  expect_equal(class(prompts_no_data), c(api, "combined_prompts", "list"))

  # Check that the prompt is correctly formatted
  expect_true(is.list(prompts_no_data[[1]]))
  expect_equal(length(prompts_no_data[[1]]), 2)
  expect_equal(names(prompts_no_data), "1")
})

test_that("build_prompts_from_files handles errors correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Define file paths for the test prompts
  claude_user_file <- test_path("test-prompts/claude_user.md")
  invalid_file <- test_path("invalid_file.md")

  # Ensure the test files exist
  expect_true(file.exists(claude_user_file))
  expect_false(file.exists(invalid_file))

  # Define the roles and API
  roles <- c("user")
  api <- "claude"

  # Expect an error when using a non-existent file
  expect_error(
    build_prompts_from_files(
      files = c(claude_user_file, invalid_file),
      roles = roles,
      api = api
    ),
    "File does not exist: 'invalid_file.md'"
  )

  # Expect an error when using invalid roles
  expect_error(
    build_prompts_from_files(
      files = c(claude_user_file, claude_user_file),
      roles = c("user", "invalid_role"),
      api = api
    ),
    "Must be a subset of \\{'user','system','assistant'\\}"
  )
})
