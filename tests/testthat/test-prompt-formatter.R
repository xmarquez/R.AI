library(tidyverse)

test_that("Single prompts work", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  apis_to_test <- c("openai", "mistral", "gemini", "claude", "groq", "cerebras")
  test_prompts <- apis_to_test |> purrr::map(~ format_chat(.x, "What is the capital of France?"))

  test_responses <- purrr::map(test_prompts, chat)
  test_responses_content <- purrr::map(test_responses, get_content)
  all_paris <- stringr::str_detect(test_responses_content, "Paris") |>
    unlist() |>
    all()
  expect_true(all_paris)
  response_usage <- test_responses |> purrr::map_df(get_usage)
  expect_identical(names(response_usage), c("input_tokens", "output_tokens", "total_tokens",
                                            "input_tokens_details", "output_tokens_details", "model"))
  expect_true(is.integer(response_usage$input_tokens))


})

test_that("Prompts with data work", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  data <- dplyr::tibble(country = c("France", "United States", "Paraguay"))
  apis_to_test <- c("openai", "mistral", "gemini", "claude", "groq", "cerebras")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, "What is the capital of {country}?", data = data))

  test_responses <- purrr::map(test_prompts, \(x) call_api(x, quiet = TRUE))
  all_correct_capitals <- test_responses |>
    purrr::list_rbind() |>
    tidyr::unnest(response) |>
    dplyr::pull(response) |>
    stringr::str_detect("Paris|Washington|Asunción") |>
    all()

  expect_equal(length(test_responses |>
                        purrr::list_rbind() |>
                        tidyr::unnest(response) |>
                        dplyr::pull(response)), nrow(data)*length(apis_to_test))
  expect_true(all_correct_capitals)
})

test_that("Single prompts with system messages work", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  apis_to_test <- c("openai", "mistral", "gemini", "claude", "groq", "cerebras")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, "What is the capital of France?",
                             system = "You always respond with witty and delightful riddles whose solution is the answer to the question. Never mention the capital city of France."))

  expect_true(!is.null(attr(test_prompts[[3]], "system")))
  expect_true(!is.null(attr(test_prompts[[4]], "system")))
  test_responses <- purrr::map(test_prompts, chat)
  test_responses_content <- purrr::map(test_responses, get_content)
  all_paris <- stringr::str_detect(test_responses_content, "Paris") |>
    unlist() |>
    all()
  expect_true(!all_paris)
})

test_that("Prompts with data and system messages work", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  data <- dplyr::tibble(country = c("France", "United States", "Paraguay"))
  apis_to_test <- c("openai", "mistral", "gemini", "claude", "groq", "cerebras")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, "What is the capital of {country}?", data = data,
                             system = "You always respond with witty and delightful riddles whose solution is the answer to the question. Never mention any capital cities, especially not Paris, Asunción, or Washington DC."))

  expect_true(!is.null(attr(test_prompts[[3]][[1]], "system")))
  expect_true(!is.null(attr(test_prompts[[4]][[1]], "system")))
  test_responses <- purrr::map(test_prompts, \(x) call_api(x, quiet = TRUE))
  all_correct_capitals <- test_responses |>
    purrr::list_rbind() |>
    tidyr::unnest(response) |>
    dplyr::pull(response) |>
    stringr::str_detect("Paris|Washington|Asunción") |>
    all()

  expect_true(!all_correct_capitals)
})

test_that("JSON mode works", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  data <- dplyr::tibble(country = c("France", "United States", "Paraguay"))
  apis_to_test <- c("openai", "mistral", "gemini", "groq", "cerebras")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, "What is the capital of {country}?", data = data,
                             system = "You always reply with JSON, with the schema {country: capital}. Use JSON only, and don't add anything else."))

  expect_true(!is.null(attr(test_prompts[[3]][[1]], "system")))
  test_responses <- purrr::map(test_prompts, \(x) call_api(x, quiet = TRUE, json_mode = TRUE))
  all_valid_json <- test_responses |>
    purrr::list_rbind() |>
    tidyr::unnest(response) |>
    dplyr::pull(response) |>
    purrr::map_lgl(jsonlite::validate) |>
    all()

  expect_true(all_valid_json)
})

test_that("Images are uploaded and described", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  danish_building <- test_path("test-images/building picture.jpeg")
  apis_to_test <- c("openai", "mistral",
                    "gemini",
                    "claude", "groq")
  models_to_test <- c("gpt-4o-mini", "pixtral-12b",
                      "gemini-1.5-flash-latest",
                      "claude-3-5-sonnet-20241022",
                      "llama-3.2-90b-vision-preview")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, c("Describe this image. Can you guess where this building is located, and what it is?",
                                   danish_building)))
  test_responses <- purrr::map2(test_prompts, models_to_test, \(x, y) chat(x, model = y, quiet = TRUE))
  response_contents <- test_responses |> purrr::map_chr(get_content)
  key_words <- response_contents |>
    stringr::str_detect("clock|Danish|Denmark|European|Scandinavia") |>
    all()
  expect_true(key_words)
})

test_that("Image URLs are described in OpenAI and Mistral", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  apis_to_test <- c("openai", "mistral")
  models_to_test <- c("gpt-4o-mini", "pixtral-12b")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, c("Describe this image. What is this building?",
                                   "https://upload.wikimedia.org/wikipedia/commons/9/91/View_of_Empire_State_Building_from_Rockefeller_Center_New_York_City_dllu_Cropped.jpg")))
  test_responses <- purrr::map2(test_prompts, models_to_test, \(x, y) chat(x, model = y, quiet = TRUE))
  response_contents <- test_responses |> purrr::map_chr(get_content)
  expect_type(response_contents, "character")
  key_words <- response_contents |>
    stringr::str_detect("Empire State Building") |>
    all()
  expect_true(key_words)
})

test_that("PDFs are uploaded and described", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  my_pdf <- test_path("test-images/Márquez - The mechanisms of personalization.pdf")
  apis_to_test <- c("gemini",
                    "claude")
  models_to_test <- c("gemini-1.5-flash-latest",
                      "claude-3-5-sonnet-20241022")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, c("Who is the author of this document, and what are its key claims?",
                                   my_pdf)))
  test_responses <- purrr::map2(test_prompts, models_to_test, \(x, y) chat(x, model = y, quiet = TRUE))
  response_contents <- test_responses |> purrr::map_chr(get_content)
  expect_type(response_contents, "character")
  key_words <- response_contents |>
    stringr::str_detect("Xavier Márquez") |>
    all()
  expect_true(key_words)
})

test_that("PDF text is extracted using Rtika", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  my_pdf <- test_path("test-images/Márquez - The mechanisms of personalization.pdf")
  apis_to_test <- c("openai", "gemini", "claude")
  models_to_test <- c("gpt-4o-mini", "gemini-1.5-flash-latest",
                      "claude-3-5-sonnet-20241022")
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, c("Who is the author of this document, and what are its key claims?",
                                   my_pdf), rtika = TRUE))
  test_responses <- purrr::map2(test_prompts, models_to_test, \(x, y) chat(x, model = y, quiet = TRUE))
  response_contents <- test_responses |> purrr::map_chr(get_content)
  key_words <- response_contents |>
    stringr::str_detect("Xavier Márquez") |>
    all()
  expect_true(key_words)
})

test_that("Caching works properly for OpenAI and Claude", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  my_pdf <- test_path("test-images/Márquez - The mechanisms of personalization.pdf")
  apis_to_test <- c("openai", "claude")
  models_to_test <- c("gpt-4o-mini",
                      "claude-3-5-sonnet-20241022")
  questions <- tibble::tibble(question = c("Who is the author of this document?",
                                           "What are its key claims?",
                                           "What are its main contributions?"))
  test_prompts <- apis_to_test |>
    purrr::map(~ format_chat(.x, c("Here's a document <document>",
                                   my_pdf, "<document>", "{question}"), cache = 3, data = questions,
                             rtika = TRUE))
  test_responses <- purrr::map2(test_prompts, models_to_test, \(x, y) call_api(x, model = y, quiet = TRUE))
  openai_cache_usage <- test_responses[[1]]$input_tokens_details |>
    purrr::list_rbind()
  expect_true(sum(openai_cache_usage$cached_tokens > 0) > 1)

  claude_cache_usage <- test_responses[[2]]$input_tokens_details |>
    purrr::list_rbind()

  expect_true(sum(claude_cache_usage$cache_creation_input_tokens  > 0) == 1)
  expect_true(sum(claude_cache_usage$cache_read_input_tokens > 0) > 1)

})

test_that("Ollama works", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  test_prompt <- format_chat("ollama", "What is the capital of France?")
  test_response <- chat(test_prompt, "llama3.2:latest")
  response_content <- get_content(test_response)
  key_words <- response_content |>
    stringr::str_detect("Paris") |>
    all()
  expect_true(key_words)
})
