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
  filter(litellm_provider %in% c("groq", "openai", "gemini", "anthropic")) |>
  mutate(api = case_when(litellm_provider == "anthropic" ~ "claude",
                         TRUE ~ litellm_provider),
         model = str_remove(model, "anthropic/|gemini/|groq/"),
         input_cost = input_cost_per_token,
         output_cost = output_cost_per_token) |>
  distinct()

usethis::use_data(models_df, overwrite = TRUE, internal = TRUE)
