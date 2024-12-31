if (file.exists("renv")) {
  source("renv/activate.R")
  .libPaths(c("/usr/local/lib/R/site-library", .libPaths()))
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

# box.lsp languageserver external hook
if (nzchar(system.file(package = "box.lsp"))) {
  options(
    languageserver.parser_hooks = list(
      "box::use" = box.lsp::box_use_parser
    )
  )
}

# auto match brackets and quotes
options(radian.auto_match = TRUE)

# auto indentation for new line and curly braces
options(radian.auto_indentation = TRUE)
options(radian.tab_size = 4)

# timeout in seconds to cancel completion if it takes too long
# set it to 0 to disable it
options(radian.completion_timeout = 0.05)

# insert new line between prompts
options(radian.insert_new_line = FALSE)

options(radian.escape_key_map = list(
          list(key = "-", value = " <- "),
          list(key = "ctrl+shift+m", value = " %>% ")
        ))
