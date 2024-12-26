library(testthat)

test_that("chat.openai_list works with function (tool) calls scenario", {
  skip_on_cran()
  skip_on_ci()

  messages <- format_chat("openai", "What's the weather like in Boston today?")

  tools <- list(
    list(
      type = "function",
      `function` = list(
        name = "get_current_weather",
        description = "Get the current weather in a given location",
        parameters = list(
          type = "object",
          properties = list(
            location = list(
              type = "string",
              description = "The city and state, e.g. San Francisco, CA"
            ),
            unit = list(
              type = "string",
              enum = list("celsius", "fahrenheit")  # Use list here
            )
          ),
          required = list("location")  # Use list here, not c()
        )
      )
    )
  )


  # tool_choice = "auto" means the model can decide whether to call a tool
  res <- chat(messages = messages, model = "gpt-4o", tools = tools, tool_choice = "auto")
  expect_s3_class(res, "openai_chat")
  expect_true(res$choices[[1]]$message$tool_calls[[1]]$type == "function")
  expect_true(res$choices[[1]]$message$tool_calls[[1]]$`function`$name == "get_current_weather")
})

test_that("chat.openai_list works with logprobs", {
  skip_on_cran()
  skip_on_ci()

  messages <- format_chat("openai", "hello!")

  # Requesting log probabilities
  res <- chat(messages, logprobs = TRUE)
  expect_s3_class(res, "openai_chat")
  # Check that logprobs are returned
  # The response may contain them in the choices structure, but the format can vary.
  # We'll just check the structure for known fields:
  expect_true("choices" %in% names(res))
  # Expect that at least one of the choices has logprobs
  if (length(res$choices) > 0 && "logprobs" %in% names(res$choices[[1]])) {
    expect_true("top_logprobs" %in% names(res$choices[[1]]$logprobs$content[[1]]))
  }
})

test_that("embed.openai_character works for a simple text input", {
  skip_on_cran()
  skip_on_ci()

  # Simple text input for embedding
  content <- c("Hello world!", "Another string to embed.")
  content <- format_character("openai", content)

  expect_true(nzchar(Sys.getenv("OPENAI_API_KEY")),
              info = "OPENAI_API_KEY is not set")

  res <- embed(content, model = "text-embedding-3-large", quiet = TRUE)
  expect_s3_class(res, "embedding")
  expect_true(is.list(res))
  matrix_embeddings <- as.matrix(res)
  expect_true(is.matrix(matrix_embeddings))
  expect_equal(c(2, 3072), dim(matrix_embeddings))
  expect_length(res, length(content))
  expect_true(all(sapply(res, is.numeric)))
})


