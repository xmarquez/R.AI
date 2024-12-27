## code to prepare `DATASET` dataset goes here
library(tidyverse)

json_file <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"
models <- jsonlite::fromJSON(json_file) |>
  purrr::map(\(x) tibble::as_tibble_row(x) |>
               dplyr::mutate(dplyr::across(dplyr::everything(), as.character))) |>
  purrr::list_rbind(names_to = "model") |>
  dplyr::filter(model != "sample_spec") |>
  dplyr::mutate(dplyr::across(dplyr::matches("max_|cost_|output_vector_size"), as.numeric),
                dplyr::across(dplyr::matches("supports"), as.logical))

models_df <- models |>
  filter(litellm_provider %in% c("groq", "openai", "gemini", "cerebras",
                                 "anthropic", "mistral", "cohere",
                                 "voyage", "deepseek")) |>
  mutate(api = dplyr::case_when(litellm_provider == "anthropic" ~ "claude",
                         TRUE ~ litellm_provider),
         model = stringr::str_remove(model, "anthropic/|gemini/|groq/|mistral/|cohere/|voyage/|deepseek/"),
         input_cost = input_cost_per_token,
         output_cost = output_cost_per_token) |>
  distinct()

preferred_models <- tibble(
  api = c("groq", "claude", "openai", "gemini",
           "mistral", "cerebras", "deepseek", "cohere"),
  cheapest = c("llama-3.1-8b-instant", "claude-3-haiku-20240307",
               "gpt-4o-mini", "gemini-1.5-flash-latest",
               "ministral-3b-latest", "llama3.1-8b", "deepseek-chat",
               "command-r"),
  largest = c("llama-3.1-70b-versatile", "claude-3-5-sonnet-20241022",
           "o1-preview", "gemini-2.0-flash-thinking-exp",
           "mistral-large-latest","llama3.3-70b",
           "deepseek-chat", "command-r-plus"),
  best = c("llama-3.1-70b-versatile", "claude-3-5-sonnet-20241022",
           "o1-preview", "gemini-2.0-flash-thinking-exp",
           "mistral-large-latest","llama3.3-70b",
           "deepseek-chat", "command-r-plus")

)

usethis::use_data(models_df, preferred_models, overwrite = TRUE, internal = TRUE)
