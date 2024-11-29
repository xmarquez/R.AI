single_file_codebase <- function() {
  R_files <- fs::dir_ls(here::here("R"))
  test_files <- fs::dir_ls(here::here("tests"), recurse = TRUE, type = "file")
  DESCRIPTION <- fs::dir_ls(here::here(), glob = "DESCRIPTION")
  all_files <- c(R_files, test_files, DESCRIPTION)
  codebase <- purrr::map(all_files, readr::read_lines)
  write_collapsed_lines(codebase) |>
    writeLines("codebase.txt")
}

write_collapsed_lines <- function(lst) {
  lst |>
    purrr::imap(~ paste0(.y, "\n", paste(.x, collapse = "\n"))) |>
    paste(collapse = "\n\n")
}
