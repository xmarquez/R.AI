#' Check if Llamafile is Running on localhost:8080
#'
#' This function checks if a Llamafile instance is currently running at
#' localhost:8080.
#'
#' @return TRUE if any Llamafile instance is running, otherwise FALSE.
#'
#' @family llamafile
#' @export
is_llamafile_running <- function() {
  res <- try(health.llama_cpp(), silent = TRUE)
  if (inherits(res, "try-error") || res$status != "ok") {
    return(FALSE)
  }
  return(TRUE)
}

#' Start Llamafile Instance
#'
#' This function attempts to start the Llamafile executable if it is not already
#' running.
#'
#' @param llamafile_path The path to the Llamafile executable.
#' @param threads The number of threads to allocate for Llamafile. Default is
#'   10. Doesn't work on Windows
#'
#' @family llamafile
#' @export
start_llamafile <- function(llamafile_path, threads = 10) {
  checkmate::assert_file_exists(llamafile_path)
  checkmate::assert_int(threads, lower = 1, null.ok = FALSE)

  if (is_llamafile_running()) {
    running_model <- which_llamafile_running()
    stop(glue::glue("{running_model} is already running on http://localhost:8080. Kill the current llamafile before starting a new one."))
  }

  # llamafile_path <- fs::path_real(llamafile_path)

  if (.Platform$OS.type == "windows") {
    # This works
    shell.exec(shQuote(llamafile_path))
    # But this doesn't:
    # command <- sprintf('start "" /B ./%s -t %d', llamafile_path, threads)
    # shell(command, wait = FALSE, intern = FALSE)
  } else {
    # Use nohup on Linux
    nohup_command <- sprintf("bash -c 'nohup %s -t %d > /dev/null 2>&1 &'", shQuote(llamafile_path), threads)
    system(nohup_command, wait = FALSE, intern = FALSE)
  }
}




#' Kill the Running Llamafile Process
#'
#' This function checks if a Llamafile instance is running and attempts to terminate it.
#'
#' @return TRUE if the Llamafile process was successfully terminated, FALSE otherwise.
#'
#' @family llamafile
#' @export
kill_llamafile <- function() {
  if(!is_llamafile_running()) {
    warning("No llamafile instance appears to be running.")
    return(FALSE)
  }

  running_model <- which_llamafile_running()

  if (.Platform$OS.type == "windows") {
    # Terminate process on Windows
    system(glue::glue("taskkill /F /IM {running_model}.llamafile.exe"), show.output.on.console = FALSE)
  } else {
    # Terminate process on Unix-like systems
    system(glue::glue("pkill -f {running_model}.llamafile"), wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # Re-check if Llamafile is still running
  if (!is_llamafile_running()) {
    message("Llamafile instance successfully terminated.")
    return(TRUE)
  } else {
    warning("Failed to terminate the Llamafile instance.")
    return(FALSE)
  }
}

#' Determines which llamfile model is running
#'
#' Determines which llamafile model is running, if any.
#'
#' @return The name of the runnning llamafile model, or `NA` if no llamafile is
#'   running.
#'
#' @family llamafile
#' @export
which_llamafile_running <- function() {
  if(is_llamafile_running()) {
    res <- props.llama_cpp()
    running_model <- res$default_generation_settings$model |>
      stringr::str_remove(".gguf")
    return(running_model)
  } else {
    warning("No llamafile instance running.")
    return(NA_character_)
  }
}
