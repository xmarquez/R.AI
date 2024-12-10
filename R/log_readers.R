#' Extract Raw Log Data
#'
#' This function extracts raw log data created by the package
#' [crew](https://cran.r-project.org/package=crew) when running in a
#' [targets](https://cran.r-project.org/package=targets) pipeline from the
#' specified directory. It looks for log files matching the given pattern and
#' reads lines containing log entries marked with "___LOG___".
#'
#' @param path The path to the directory containing log files. Defaults to the
#'   current directory (".").
#' @param glob A pattern to match the log files. Defaults to "crew-*.log".
#'
#' @return A tibble containing log data with columns: `time`, `api`,
#'   `prompt_name`, `prompt_set`, `model`, `current_prompt`, and
#'   `total_prompts`. If no logs are found, returns an empty tibble.
#'
#' @export
crew_log_raw <- function(path = ".", glob = "crew-*.log") {
  # List all log files in the specified directory matching the pattern
  log_files <- fs::dir_ls(path = path, glob = glob)

  # Extract log lines containing "___LOG___" marker from each file
  log_lines <- purrr::map(log_files, \(file) {
    grep(pattern = "___LOG___", x = readLines(file), value = TRUE)
  }) |>
    purrr::discard(\(x) length(x) == 0)  # Remove any files that have no matching lines

  # If log lines were found, process them further
  if (length(log_lines) > 0) {
    log_lines <- log_lines |>
      # Remove the "___LOG___" marker from each log line
      purrr::map(\(x) stringr::str_remove_all(x, "___LOG___")) |>
      # Parse the log lines into a tibble using readr::read_delim
      purrr::map(\(x) readr::read_delim(
        I(x),
        delim = "|",
        col_names = c("X", "time", "api", "prompt_name", "prompt_set", "model", "current_prompt", "total_prompts"),
        col_types = "cTcccciic",
        show_col_types = FALSE
      ) |>
        # Remove unnecessary columns starting with "X"
        dplyr::select(-dplyr::starts_with("X"))) |>
      # Combine all parsed log tibbles into one
      purrr::list_rbind(names_to = "file")
  } else {
    # If no log lines were found, return an empty tibble
    log_lines <- tibble()
  }

  # Return the final tibble with log data
  log_lines
}

#' Extract Log Data for API Retries
#'
#' This function extracts log data related to failed API requests created by the
#' package [crew](https://cran.r-project.org/package=crew) when running in a
#' [targets](https://cran.r-project.org/package=targets) pipeline from the
#' specified directory. It looks for lines indicating failed requests and
#' retrieves the error codes and retry intervals.
#'
#' @param path The path to the directory containing log files. Defaults to the
#'   current directory (".").
#' @param glob A pattern to match the log files. Defaults to "crew-*.log".
#'
#' @return A tibble containing log data with columns: `file`, `error_codes`, and
#'   `retry_interval`. If no logs are found, returns an empty tibble.
#'
#' @examples
#' # Extract retry log data from the current directory
#' crew_log_retries()
#'
#' @export
crew_log_retries <- function(path = ".", glob = "crew-*.log") {
  # List all log files in the specified directory matching the pattern
  log_files <- fs::dir_ls(path = path, glob = glob)

  # Extract lines that indicate a failed request from each file
  log_lines <- purrr::map(log_files, \(file) {
    grep(pattern = "^Request failed", x = readLines(file), value = TRUE)
  }) |>
    purrr::discard(\(x) length(x) == 0)  # Remove any files that have no matching lines

  # If failed request lines were found, extract error codes and retry intervals
  if (length(log_lines) > 0) {
    error_codes <- log_lines |>
      # Extract error codes from the log lines
      purrr::map(\(x) stringr::str_extract(x, "\\[\\d+\\]"))

    retry_interval <- log_lines |>
      # Extract retry intervals from the log lines
      purrr::map(\(x) stringr::str_extract(x, "(?<=Retrying in )\\d+\\.?(\\d+)?"))

    # Create a tibble with the extracted data
    log_lines <- dplyr::tibble(file = names(log_lines), error_codes, retry_interval)
  } else {
    # If no failed request lines were found, return an empty tibble
    log_lines <- dplyr::tibble()
  }

  # Return the final tibble with retry data
  log_lines
}

#' Summarize Log Data
#'
#' This function provides a summary of log data created by the package
#' [crew](https://cran.r-project.org/package=crew) when running in a
#' [targets](https://cran.r-project.org/package=targets) pipeline extracted from
#' the specified directory. The summary includes the number of prompts
#' processed, the time of the first and last activity, the total processing
#' time, and an estimate of prompts processed per hour (PPH).
#'
#' @param path A character string specifying the directory containing log files.
#'   Defaults to the current directory (".").
#' @param glob A pattern to match the log files. Defaults to "crew-*.log".
#'
#' @return A tibble summarizing the log data with the following columns:
#'   - `model`: The model used for processing.
#'   - `api`: The API used for the requests.
#'   - `prompt_name`: The name of the prompt being processed.
#'   - `prompt_set`: The unique identifier for the set of prompts.
#'   - `file`: The log file where the data was recorded.
#'   - `prompts_processed`: The total number of prompts processed.
#'   - `first_activity`: The timestamp of the first recorded activity.
#'   - `last_activity`: The timestamp of the last recorded activity.
#'   - `total_time`: The total time taken for processing, calculated as a
#'   lubridate period.
#'   - `pph`: The number of prompts processed per hour.
#'   - `eta`: The estimated time remaining to process all prompts.
#'   - `last_error_code`: The last error code recorded (if any).
#'   - `last_retry_interval`: The duration of the last retry interval (if any).
#'
#' @details The function first extracts raw log data using [crew_log_raw()] and
#' then aggregates it by `model`, `api`, `prompt_name`, `prompt_set`, and `file`
#' to compute summary statistics. It also integrates retry information from
#' [crew_log_retries()] if applicable.
#'
#' If no logs are found matching the specified `path` and `glob`, the function
#' returns an empty tibble.
#'
#' @seealso [crew_log_raw()] for extracting raw log data and
#' [crew_log_retries()] for retry-specific information.
#'
#' @export
crew_log_summary <- function(path = ".", glob = "crew-*.log") {
  model <- api <- prompt_name <- prompt_set <- current_prompt <- time <- NULL
  last_activity <- prompts_processed <- total_time <- total_prompts <- pph <- NULL
  first_activity <- last_retry_interval <- NULL

  # Extract raw log data from the specified directory
  log_summary <- crew_log_raw(path = path, glob = glob) |>
    # Group data by model, api, prompt_name, prompt_set, and file
    dplyr::group_by(model, api, prompt_name, prompt_set, file) |>
    # Summarize data to get prompts processed, first and last activity times, total time, etc.
    dplyr::summarise(
      prompts_processed = unique(max(current_prompt)),
      first_activity = min(time),
      last_activity = max(time),
      # Calculate total processing time as a period
      total_time = (last_activity - first_activity) |>
        lubridate::as.period() |>
        round(),
      # Calculate prompts processed per hour
      pph = prompts_processed / as.numeric(total_time, units = "hours"),
      # Estimate time remaining to process all prompts
      eta = lubridate::dhours((unique(total_prompts) - prompts_processed) / pph) |>
        lubridate::as.period() |>
        round()
    )

  # Extract retry data from the specified directory
  rate_limits <- crew_log_retries(path = path, glob = glob) |>
    # Unnest columns containing error codes and retry intervals
    tidyr::unnest(dplyr::all_of(c("error_codes", "retry_interval"))) |>
    # Group by file and take the last entry for each file
    dplyr::group_by(file) |>
    dplyr::slice_tail(n = 1) |>
    # Rename columns for clarity
    purrr::set_names(c("file", "last_error_code", "last_retry_interval")) |>
    # Convert retry interval to a duration object
    dplyr::mutate(last_retry_interval = lubridate::dseconds(last_retry_interval))

  # Join the log summary with the retry data and return the final tibble
  log_summary |>
    dplyr::left_join(rate_limits) |>
    dplyr::ungroup()
}
