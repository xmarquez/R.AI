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
                                 "voyage")) |>
  mutate(api = dplyr::case_when(litellm_provider == "anthropic" ~ "claude",
                         TRUE ~ litellm_provider),
         model = stringr::str_remove(model, "anthropic/|gemini/|groq/|mistral/|cohere/|voyage/"),
         input_cost = input_cost_per_token,
         output_cost = output_cost_per_token) |>
  distinct()

preferred_models <- models_df |>
  dplyr::filter(mode == "chat") |>
  dplyr::mutate(cheapest = dplyr::case_when(api == "groq" ~ "llama-3.1-8b-instant",
                                            api == "claude" ~ "claude-3-haiku-20240307",
                                            api == "openai" ~ "gpt-4o-mini",
                                            api == "gemini" ~ "gemini-1.5-flash-latest",
                                            api == "mistral" ~ "ministral-3b-latest",
                                            api == "cerebras" ~ "llama3.1-8b"),
                largest = dplyr::case_when(api == "groq" ~ "llama-3.2-90b-text-preview",
                                           api == "claude" ~ "claude-3-opus-20240229",
                                           api == "openai" ~ "gpt-4o",
                                           api == "gemini" ~ "gemini-1.5-pro-latest",
                                           api == "mistral" ~ "mistral-large-latest",
                                           api == "cerebras" ~ "llama3.3-70b"),
                best = dplyr::case_when(api == "groq" ~ "llama-3.1-70b-versatile",
                                        api == "claude" ~ "claude-3-5-sonnet-20241022",
                                        api == "openai" ~ "o1-preview",
                                        api == "gemini" ~ "gemini-2.0-flash-thinking-exp",
                                        api == "mistral" ~ "mistral-large-latest",
                                        api == "cerebras" ~ "llama3.3-70b")) |>
  dplyr::select(dplyr::all_of(c("api", "cheapest", "largest", "best"))) |>
  dplyr::distinct()

usethis::use_data(models_df, preferred_models, overwrite = TRUE, internal = TRUE)
