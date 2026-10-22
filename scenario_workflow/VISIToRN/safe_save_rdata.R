safe_save_rdata <- function(object_names,
                            file,
                            envir = parent.frame(),
                            max_attempts = 5L,
                            retry_wait_seconds = 1,
                            compress = TRUE) {

  object_names <- as.character(object_names)
  missing_objects <- object_names[
    !vapply(
      object_names,
      exists,
      logical(1),
      envir = envir,
      inherits = FALSE
    )
  ]

  if (length(missing_objects) > 0) {
    stop(
      "Cannot save missing object(s): ",
      paste(missing_objects, collapse = ", ")
    )
  }

  dir.create(
    dirname(file),
    showWarnings = FALSE,
    recursive = TRUE
  )

  temp_file <- tempfile(
    pattern = paste0(".", basename(file), "."),
    tmpdir = dirname(file),
    fileext = ".tmp"
  )

  on.exit({
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }
  }, add = TRUE)

  save(
    list = object_names,
    file = temp_file,
    envir = envir,
    compress = compress
  )

  source_md5 <- unname(tools::md5sum(temp_file))

  target_matches_source <- function() {
    if (!file.exists(file)) {
      return(FALSE)
    }

    target_md5 <- tryCatch(
      unname(tools::md5sum(file)),
      error = function(e) NA_character_
    )

    identical(source_md5, target_md5)
  }

  # If the existing file already has the same contents, no replacement is
  # needed. This is common after restarting a pipeline on a OneDrive folder.
  replace_ok <- target_matches_source()

  if (!replace_ok) {
    for (attempt in seq_len(as.integer(max_attempts))) {
      suppressWarnings(
        file.copy(
          from = temp_file,
          to = file,
          overwrite = TRUE,
          copy.mode = TRUE,
          copy.date = FALSE
        )
      )

      replace_ok <- target_matches_source()

      if (replace_ok) {
        break
      }

      if (attempt < max_attempts) {
        Sys.sleep(retry_wait_seconds)
      }
    }
  }

  # Some OneDrive reparse-point files reject in-place overwrite with
  # "Invalid argument". Move the old file aside and promote the complete
  # temporary file instead. The old file is retained as a backup.
  backup_file <- NA_character_

  if (!replace_ok && file.exists(file)) {
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    backup_file <- paste0(file, ".old-", timestamp)

    old_moved <- isTRUE(
      suppressWarnings(file.rename(file, backup_file))
    )

    if (old_moved) {
      promoted <- isTRUE(
        suppressWarnings(file.rename(temp_file, file))
      )

      if (!promoted) {
        promoted <- isTRUE(
          suppressWarnings(
            file.copy(temp_file, file, overwrite = FALSE)
          )
        )
      }

      replace_ok <- promoted && target_matches_source()

      if (replace_ok) {
        message(
          "Existing R data file was retained as backup: ",
          backup_file
        )
      } else {
        # Keep any failed promoted file for diagnosis and restore the
        # original target whenever possible.
        if (file.exists(file)) {
          failed_file <- paste0(file, ".failed-", timestamp)
          suppressWarnings(file.rename(file, failed_file))
        }

        if (!file.exists(file) && file.exists(backup_file)) {
          suppressWarnings(file.rename(backup_file, file))
        }
      }
    }
  }

  if (!replace_ok && !file.exists(file)) {
    promoted <- isTRUE(
      suppressWarnings(file.rename(temp_file, file))
    )

    if (!promoted) {
      promoted <- isTRUE(
        suppressWarnings(
          file.copy(temp_file, file, overwrite = FALSE)
        )
      )
    }

    replace_ok <- promoted && target_matches_source()
  }

  if (!replace_ok) {
    fallback_file <- paste0(
      file,
      ".new-",
      format(Sys.time(), "%Y%m%d-%H%M%S")
    )

    retained <- isTRUE(
      suppressWarnings(file.rename(temp_file, fallback_file))
    )

    if (!retained) {
      retained <- isTRUE(
        suppressWarnings(
          file.copy(temp_file, fallback_file, overwrite = FALSE)
        )
      )
    }

    stop(
      "Could not safely update the R data file: ",
      normalizePath(file, winslash = "/", mustWork = FALSE),
      if (!is.na(backup_file) && file.exists(backup_file)) {
        paste0(
          "\nThe previous file is retained at: ",
          normalizePath(backup_file, winslash = "/", mustWork = FALSE)
        )
      } else {
        ""
      },
      if (retained) {
        paste0(
          "\nA complete replacement file was retained at: ",
          normalizePath(
            fallback_file,
            winslash = "/",
            mustWork = FALSE
          )
        )
      } else {
        ""
      },
      "\nClose applications using the file or pause OneDrive sync, then rerun."
    )
  }

  invisible(
    normalizePath(file, winslash = "/", mustWork = TRUE)
  )
}
