# Internal environment to store package state
.sknote_env <- new.env(parent = emptyenv())

#' Helper function to capture system information
get_system_info <- function() {
  sys_info <- Sys.info()
  user_name <- sys_info[["user"]]
  if (is.na(user_name) || user_name == "") {
    user_name <- Sys.getenv("USERNAME", Sys.getenv("USER", "Unknown"))
  }

  list(
    User = user_name,
    OS = paste(sys_info[["sysname"]], sys_info[["release"]]),
    Machine = sys_info[["machine"]],
    R_Version = R.version.string,
    R_Home = R.home(),
    Working_Dir = getwd(),
    Platform = R.version$platform
  )
}

#' Format system info as HTML
format_sys_info_html <- function(sys_info) {
  safe_info <- lapply(sys_info, htmltools::htmlEscape)
  paste0(
    '<div class="sys-info">\n',
    '<h3>🖥️ System Information</h3>\n',
    '<ul>\n',
    '<li><strong>User:</strong> ', safe_info$User, '</li>\n',
    '<li><strong>OS:</strong> ', safe_info$OS, '</li>\n',
    '<li><strong>Machine:</strong> ', safe_info$Machine, '</li>\n',
    '<li><strong>R Version:</strong> ', safe_info$R_Version, '</li>\n',
    '<li><strong>R Home:</strong> ', safe_info$R_Home, '</li>\n',
    '<li><strong>Working Dir:</strong> ', safe_info$Working_Dir, '</li>\n',
    '<li><strong>Platform:</strong> ', safe_info$Platform, '</li>\n',
    '</ul>\n',
    '</div>\n'
  )
}

#' Format system info as text
format_sys_info_text <- function(sys_info) {
  paste0(
    "--- SYSTEM INFORMATION ---\n",
    "User           : ", sys_info$User, "\n",
    "OS             : ", sys_info$OS, "\n",
    "Machine        : ", sys_info$Machine, "\n",
    "R Version      : ", sys_info$R_Version, "\n",
    "R Home         : ", sys_info$R_Home, "\n",
    "Working Dir    : ", sys_info$Working_Dir, "\n",
    "Platform       : ", sys_info$Platform, "\n",
    "--------------------------------\n\n"
  )
}

#' Start SKthink Console Note (sknote)
#'
#' @param format Output format. Options: "clipboard", "txt", "md", "html"
#' @param file_path File name and location for saving
#' @param save_plots Logical. Whether to save figures (Default: TRUE)
#' @param plot_dir Directory to save figures (Default: "sknote_plots")
#' @export
sknote <- function(format = c("clipboard", "txt", "md", "html"),
                   file_path = NULL,
                   save_plots = TRUE,
                   plot_dir = "sknote_plots") {

  format <- match.arg(format)

  # Capture system info at start
  sys_info <- get_system_info()

  # Set default file name
  if (format %in% c("txt", "md", "html") && is.null(file_path)) {
    file_path <- paste0("sknote_log.", ifelse(format == "html", "html", format))
  }

  if (!is.null(.sknote_env$callback_id)) {
    message("⚠️ sknote is already running. Use sknote_stop() first.")
    return(invisible())
  }

  # Initialize file (if file-based format)
  if (!is.null(file_path)) {
    # Create directory if it doesn't exist
    dir_name <- dirname(file_path)
    if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

    file.create(file_path, showWarnings = FALSE)
    cat("", file = file_path, append = FALSE)

    # Write HTML header structure
    if (format == "html") {
      current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      html_header <- paste0('<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>SKthink Log</title>
<style>
  body { font-family: sans-serif; max-width: 900px; margin: 20px auto; padding: 20px; background: #f9f9f9; }
  .code-block { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: monospace; }
  .output { background: #fff; padding: 10px; border-left: 4px solid #007acc; margin: 10px 0; font-family: monospace; white-space: pre-wrap; }
  .figure { text-align: center; margin: 20px 0; }
  .figure img { max-width: 100%; border: 1px solid #ddd; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
  hr { border: 0; border-top: 1px solid #ddd; margin: 30px 0; }
  h3 { color: #333; }
  .entry { margin-bottom: 30px; }
  .sys-info { background: #e8f4f8; border: 1px solid #bce8f1; padding: 15px; border-radius: 5px; margin-bottom: 30px; }
  .sys-info ul { list-style-type: none; padding: 0; margin: 10px 0 0 0; }
  .sys-info li { margin-bottom: 5px; font-size: 0.95em; }
  .sys-info strong { color: #31708f; }
</style>
</head>
<body>
<h1>📝 SKthink Session Log</h1>
<p><em>Started at: ', current_time, '</em></p>
', format_sys_info_html(sys_info), '
<hr>
')
      cat(html_header, file = file_path, append = TRUE)
    } else {
      # Add system info for txt/md formats
      cat(format_sys_info_text(sys_info), file = file_path, append = TRUE)
    }
  }

  # Setup plot saving
  if (save_plots) {
    if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
    .sknote_env$plot_counter <- 0
    .sknote_env$plot_dir <- plot_dir
    .sknote_env$sknote_active <- TRUE
  }

  # Helper function to encode image to base64 for HTML
  get_base64_image <- function(path) {
    if (!file.exists(path)) return("")
    raw_data <- readBin(path, "raw", file.info(path)$size)
    base64_str <- base64enc::base64encode(raw_data)
    return(paste0("data:image/png;base64,", base64_str))
  }

  # Main callback function - executes after each console command
  callback_func <- function(expr, value, ok, visible) {
    code_text <- paste(deparse(expr), collapse = "\n")
    out_text <- ""

    # Capture output by re-evaluating with capture.output
    # This captures ALL output including print(), cat(), etc.
    out_lines <- c()

    # Try to capture output
    if (visible || inherits(expr, "call") || is.language(expr)) {
      temp_out <- tryCatch({
        # Capture all output from evaluating the expression
        capture.output({
          # Evaluate in parent frame
          eval.parent(expr)
        })
      }, error = function(e) {
        character(0)
      })

      if (length(temp_out) > 0) {
        out_text <- paste(temp_out, collapse = "\n")
      }
    }

    # --- FIGURE CAPTURING LOGIC ---
    plot_filename <- NULL
    plot_html_embed <- NULL

    if (save_plots) {
      # Check if there's an active graphics device with content
      current_dev <- grDevices::dev.cur()

      if (current_dev > 1) {
        .sknote_env$plot_counter <- .sknote_env$plot_counter + 1
        temp_file <- file.path(.sknote_env$plot_dir, paste0("plot_", .sknote_env$plot_counter, ".png"))

        # Save the current plot
        saved <- tryCatch({
          # Copy current device to PNG
          grDevices::png(temp_file, width = 800, height = 600, res = 100)
          grDevices::dev.copy(which = current_dev)
          grDevices::dev.off()

          # Verify file was created
          if (file.exists(temp_file) && file.info(temp_file)$size > 1000) {
            TRUE
          } else {
            FALSE
          }
        }, error = function(e) {
          if (grDevices::dev.cur() > 1) grDevices::dev.off()
          if (file.exists(temp_file)) file.remove(temp_file)
          FALSE
        })

        if (saved) {
          plot_filename <- temp_file
          if (format == "html" && requireNamespace("base64enc", quietly = TRUE)) {
            plot_html_embed <- get_base64_image(temp_file)
          }
        } else {
          .sknote_env$plot_counter <- .sknote_env$plot_counter - 1
        }
      }
    }
    # --------------------------------

    # Generate output based on format
    if (format == "clipboard") {
      full_text <- paste0("> ", code_text, "\n")
      if (nzchar(out_text)) {
        full_text <- paste0(full_text, out_text, "\n")
      }
      if (!is.null(plot_filename)) {
        full_text <- paste0(full_text, "[Figure saved at: ", plot_filename, "]\n")
      }
      full_text <- paste0(full_text, "\n")

      current_clip <- tryCatch(clipr::read_clip(), error = function(e) "")
      clipr::write_clip(paste0(current_clip, full_text))

    } else if (format == "txt") {
      full_text <- paste0("---\n> ", code_text, "\n")
      if (nzchar(out_text)) {
        full_text <- paste0(full_text, out_text, "\n")
      }
      if (!is.null(plot_filename)) {
        full_text <- paste0(full_text, "[Figure saved at: ", plot_filename, "]\n")
      }
      full_text <- paste0(full_text, "\n")
      cat(full_text, file = file_path, append = TRUE)

    } else if (format == "md") {
      full_text <- paste0("```r\n", code_text, "\n```\n")
      if (nzchar(out_text)) {
        full_text <- paste0(full_text, "\n**Output:**\n```\n", out_text, "\n```\n")
      }
      if (!is.null(plot_filename)) {
        rel_path <- gsub("\\\\", "/", plot_filename)
        full_text <- paste0(full_text, "\n![Plot ", .sknote_env$plot_counter, "](", rel_path, ")\n")
      }
      full_text <- paste0(full_text, "\n---\n\n")
      cat(full_text, file = file_path, append = TRUE)

    } else if (format == "html") {
      safe_code <- htmltools::htmlEscape(code_text)
      safe_out <- htmltools::htmlEscape(out_text)

      html_block <- paste0(
        '<div class="entry">\n',
        '<h3>Code:</h3>\n<div class="code-block"><code>',
        safe_code,
        '</code></div>\n'
      )

      if (nzchar(out_text)) {
        html_block <- paste0(html_block, '<h3>Output:</h3>\n<div class="output">', safe_out, '</div>\n')
      }

      if (!is.null(plot_html_embed)) {
        html_block <- paste0(html_block, '<div class="figure"><img src="', plot_html_embed, '" alt="Plot"></div>\n')
      } else if (!is.null(plot_filename)) {
        html_block <- paste0(html_block, '<div class="figure"><p>Plot saved: ', plot_filename, '</p></div>\n')
      }

      html_block <- paste0(html_block, '<hr></div>\n')
      cat(html_block, file = file_path, append = TRUE)
    }

    return(TRUE)
  }

  cb_id <- addTaskCallback(callback_func)
  .sknote_env$callback_id <- cb_id
  .sknote_env$file_path <- file_path

  msg <- "🟢 SKthink: sknote started! "
  if (!is.null(file_path)) {
    msg <- paste0(msg, "Saving to: ", normalizePath(file_path, mustWork = FALSE))
  } else {
    msg <- paste0(msg, "Copying to clipboard.")
  }
  if (save_plots) {
    msg <- paste0(msg, " (Figures in '", plot_dir, "')")
  }

  message(msg)
  invisible()
}

#' Stop SKthink Console Note (sknote)
#' @export
sknote_stop <- function() {
  if (is.null(.sknote_env$callback_id)) {
    message("⚠️ sknote is not currently running.")
    return(invisible())
  }

  removeTaskCallback(.sknote_env$callback_id)
  .sknote_env$sknote_active <- FALSE

  file_p <- .sknote_env$file_path
  format <- if(!is.null(file_p)) tools::file_ext(file_p) else "clipboard"

  # Close HTML tags if HTML format
  if (format == "html" && !is.null(file_p)) {
    cat('\n</body>\n</html>', file = file_p, append = TRUE)
  }

  .sknote_env$callback_id <- NULL
  .sknote_env$file_path <- NULL

  if (!is.null(file_p)) {
    message("🔴 SKthink: sknote stopped. File saved at: ", normalizePath(file_p, mustWork = FALSE))
  } else {
    message("🔴 SKthink: sknote stopped.")
  }
  invisible()
}
