# Normalize and serialize the supported Splits Metadata contract.

normalize_splits_metadata_rows <- function(df) {
  if (!"RecordType" %in% names(df) || "Type" %in% names(df)) return(df)
  record_type <- trimws(as.character(df$RecordType))
  expected <- c("Channel", "Break", "Rename", "SAP")
  if (any(!record_type %in% expected))
    stop("Splits Metadata has an unsupported RecordType.")
  required <- list(
    Channel = c("SplitOrder"),
    Break = c("BreakDimension", "BreakSeparator", "BreakPartCount"),
    Rename = c("RenameSource", "RenameAlias"),
    SAP = c("VariableSplit", "MergeName", "MergeOrder", "ModelMetric")
  )
  for (kind in unique(record_type)) {
    missing <- setdiff(required[[kind]], names(df))
    if (length(missing))
      stop(paste("Splits Metadata is missing", paste(missing, collapse = ", "),
                 "for", kind, "rows."))
  }
  if (!"Channel" %in% names(df) ||
      anyNA(df$Channel) || any(!nzchar(trimws(as.character(df$Channel)))))
    stop("Every Splits Metadata row needs a Channel.")
  channel_names <- as.character(df$Channel[record_type == "Channel"])
  if (anyDuplicated(channel_names) ||
      any(!df$Channel %in% channel_names))
    stop("Each Channel needs exactly one Channel row in Splits Metadata.")
  df$Type <- ifelse(record_type == "Channel", "Config",
                    ifelse(record_type == "SAP", "Merge", record_type))
  df$Name <- df$Splits <- ""
  if (!"SplitOrder" %in% names(df)) df$SplitOrder <- ""

  part_cols <- grep("^BreakPart[0-9]+$", names(df), value = TRUE)
  part_cols <- part_cols[order(as.integer(sub("^BreakPart", "", part_cols)))]
  for (i in which(record_type == "Break")) {
    parts <- if (length(part_cols)) as.character(unlist(df[i, part_cols, drop = FALSE],
                                                     use.names = FALSE)) else character(0)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    df$SplitOrder[i] <- df$BreakDimension[i] %||% ""
    df$Name[i] <- paste(parts, collapse = "|")
    df$Splits[i] <- paste(df$BreakSeparator[i] %||% " - ",
                          df$BreakPartCount[i] %||% length(parts), sep = "|")
  }
  rename_idx <- which(record_type == "Rename")
  if (length(rename_idx)) {
    df$SplitOrder[rename_idx] <- df$RenameSource[rename_idx]
    df$Name[rename_idx] <- df$RenameAlias[rename_idx]
  }
  sap_idx <- which(record_type == "SAP")
  if (length(sap_idx)) {
    key <- paste(df$Channel[sap_idx], df$MergeName[sap_idx],
                 df$ModelMetric[sap_idx], sep = "\r")
    groups <- split(sap_idx, factor(key, levels = unique(key)))
    for (indices in groups) {
      order_value <- suppressWarnings(as.integer(df$MergeOrder[indices]))
      indices <- indices[order(is.na(order_value), order_value, seq_along(indices))]
      first <- indices[[1]]
      df$Name[first] <- df$MergeName[first]
      df$Splits[first] <- paste(df$VariableSplit[indices], collapse = " ||| ")
      if (length(indices) > 1L) df$Type[indices[-1L]] <- "SAP component"
    }
    df <- df[df$Type != "SAP component", , drop = FALSE]
  }
  df
}
export_channels_csv <- function(channels, global_config = NULL) {
  if (!length(channels)) return(data.frame())
  breaks <- unlist(lapply(channels, function(cfg) cfg$dimension_breaks %||% list()),
                   recursive = FALSE)
  max_parts <- if (length(breaks)) max(vapply(breaks, function(b)
    length(b$names %||% character(0)), integer(1))) else 0L
  part_cols <- if (max_parts) paste0("BreakPart", seq_len(max_parts)) else character(0)
  columns <- c(
    "ConfigVersion", "Channel", "RecordType", "ModelVariable", "SplitOrder",
    "ActivityKeyword", "SpendKeyword", "ModelMetric", "VarNameInclude",
    "MinPeriod", "MaxPeriod", "UpdateLabel", "TimeBreakLabel", "GeoLabel",
    "BreakDefaultSeparator", "BreakDimension", "BreakSeparator", "BreakPartCount",
    part_cols, "BreakMissingPartValue", "RenameSource", "RenameAlias",
    "VariableSplit", "MergeName", "MergeOrder"
  )
  rows <- list()
  add_row <- function(values) {
    row <- stats::setNames(as.list(rep("", length(columns))), columns)
    row[names(values)] <- values
    rows[[length(rows) + 1L]] <<- as.data.frame(row, check.names = FALSE)
  }
  date_value <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1]])) "" else as.character(x[[1]])
  }
  for (nm in names(channels)) {
    cfg <- channels[[nm]]
    add_row(list(
      ConfigVersion = "3", Channel = nm, RecordType = "Channel",
      ModelVariable = cfg$model_variable %||% "",
      SplitOrder = paste(cfg$split_columns %||% character(0), collapse = "|"),
      ActivityKeyword = cfg$activity_keyword %||% "",
      SpendKeyword = cfg$spend_keyword %||% "",
      ModelMetric = normalize_model_metric(cfg$model_metric %||% "activity"),
      VarNameInclude = paste(cfg$varname_include %||% character(0), collapse = " ||| "),
      MinPeriod = date_value(cfg$min_period), MaxPeriod = date_value(cfg$max_period),
      UpdateLabel = global_config$update_label %||% "",
      TimeBreakLabel = cfg$time_break_label %||% "",
      GeoLabel = normalize_geo_label(cfg$geo_label %||% ""),
      BreakDefaultSeparator = cfg$break_default_separator %||% " - ",
      BreakMissingPartValue = canonical_break_missing_part_value()
    ))
    for (b in cfg$dimension_breaks %||% list()) {
      parts <- as.character(b$names %||% character(0))
      values <- list(
        ConfigVersion = "3", Channel = nm, RecordType = "Break",
        BreakDimension = b$column %||% "", BreakSeparator = b$separator %||% " - ",
        BreakPartCount = as.character(b$n_parts %||% length(parts)),
        BreakMissingPartValue = canonical_break_missing_part_value()
      )
      for (i in seq_along(parts)) values[[paste0("BreakPart", i)]] <- parts[[i]]
      add_row(values)
    }
    for (als in cfg$dimension_aliases %||% list()) {
      add_row(list(ConfigVersion = "3", Channel = nm, RecordType = "Rename",
                   RenameSource = als$source %||% "", RenameAlias = als$alias %||% ""))
    }
    for (m in cfg$saved_merges %||% list()) {
      if (!isTRUE(m$active)) next
      components <- unlist(m$merged, use.names = FALSE)
      for (i in seq_along(components)) {
        add_row(list(ConfigVersion = "3", Channel = nm, RecordType = "SAP",
                     ModelMetric = m$metric %||% "", VariableSplit = components[[i]],
                     MergeName = m$new_name %||% "", MergeOrder = as.character(i)))
      }
    }
  }
  dplyr::bind_rows(rows)
}

export_splits_metadata_csv <- function(channels, global_config = NULL) {
  export_channels_csv(channels, global_config)
}

# =============================================================================
# KEYWORD DETECTORS
# =============================================================================

# detect_activity_keyword

# Read and normalize current and legacy Splits Metadata files.
    clean_config_col_names <- function(x) {
      x <- trimws(as.character(x))
      x <- sub("^\ufeff", "", x)
      x <- sub("^<U\\+FEFF>", "", x)
      x <- sub("^Ã¯\\.\\.", "", x)
      x
    }

    normalize_channel_config_df <- function(df) {
      if (is.null(df)) return(df)
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      names(df) <- clean_config_col_names(names(df))
      char_cols <- names(df)[vapply(df, is.character, logical(1))]
      for (col in char_cols) {
        df[[col]][is.na(df[[col]])] <- ""
        if (!col %in% c("BreakSeparator", "BreakDefaultSeparator"))
          df[[col]] <- trimws(df[[col]])
      }
      normalize_splits_metadata_rows(df)
    }

    read_channel_config_content <- function(config_text) {
      read_attempt <- function(kind) {
        con <- textConnection(config_text)
        on.exit(close(con), add = TRUE)
        switch(
          kind,
          tab = read.delim(con, stringsAsFactors = FALSE,
                           check.names = FALSE, na.strings = c("", "NA")),
          semi = read.csv2(con, stringsAsFactors = FALSE,
                           check.names = FALSE, na.strings = c("", "NA")),
          csv = read.csv(con, stringsAsFactors = FALSE,
                         check.names = FALSE, na.strings = c("", "NA"))
        )
      }

      first_line <- strsplit(config_text %||% "", "\r?\n")[[1]][1] %||% ""
      preferred <- c(
        if (grepl("\t", first_line, fixed = TRUE)) "tab",
        if (grepl(",", first_line, fixed = TRUE)) "csv",
        if (grepl(";", first_line, fixed = TRUE)) "semi",
        "csv", "tab", "semi"
      )

      fallback <- NULL
      for (kind in unique(preferred)) {
        df <- tryCatch(read_attempt(kind), error = function(e) NULL)
        if (is.null(df)) next
        df <- normalize_channel_config_df(df)
        fallback <- fallback %||% df
        if (all(c("Channel", "Type") %in% names(df))) return(df)
      }
      fallback
    }

    read_channel_config_file <- function(path) {
      if (is.null(path) || !file.exists(path))
        stop("Splits Metadata file was not uploaded correctly.")
      df <- tryCatch(
        data.table::fread(
          file = path,
          sep = "auto",
          data.table = FALSE,
          check.names = FALSE,
          na.strings = "NA",
          fill = TRUE,
          quote = "\"",
          encoding = "UTF-8"
        ),
        error = function(e) NULL
      )
      if (is.null(df) || !"Channel" %in% clean_config_col_names(names(df)) ||
          !any(c("Type", "RecordType") %in% clean_config_col_names(names(df)))) {
        txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        df <- read_channel_config_content(txt)
      } else {
        df <- normalize_channel_config_df(df)
      }
      if (is.null(df) || !nrow(df))
        stop("Splits Metadata file is empty or could not be parsed.")
      df
    }

    config_df_from_pending <- function(parsed_config) {
      if (is.data.frame(parsed_config)) return(normalize_channel_config_df(parsed_config))
      parsed_config$df %||% NULL
    }

    parse_merge_splits_config <- function(merged_raw) {
      merged_raw <- trimws(as.character(merged_raw %||% ""))
      if (!nzchar(merged_raw)) return(character(0))

      if (grepl("\\s\\|\\|\\|\\s", merged_raw, perl = TRUE)) {
        parts <- strsplit(merged_raw, "\\s\\|\\|\\|\\s", perl = TRUE)[[1]]
        return(Filter(nzchar, trimws(parts)))
      }

      tokens <- trimws(strsplit(merged_raw, "\\|", fixed = FALSE)[[1]])
      tokens <- tokens[nzchar(tokens)]
      if (!length(tokens)) return(character(0))

      time_break_token <- function(x) {
        grepl("^[[:alpha:]]+TimeBreak$", x, ignore.case = TRUE)
      }
      geo_label_token <- function(x) {
        grepl("^GeoLabel\\d+$", x, ignore.case = TRUE)
      }

      out <- character(0)
      for (tok in tokens) {
        if ((time_break_token(tok) || geo_label_token(tok)) && length(out) > 0) {
          out[length(out)] <- paste0(out[length(out)], "|", tok)
        } else {
          out <- c(out, tok)
        }
      }
      out
    }

    parse_config_varnames <- function(raw) {
      raw <- trimws(as.character(raw %||% ""))
      if (is.na(raw) || !nzchar(raw)) return(character(0))
      sep <- if (grepl("\\s\\|\\|\\|\\s", raw, perl = TRUE)) "\\s\\|\\|\\|\\s" else "\\|"
      parts <- trimws(strsplit(raw, sep, perl = TRUE)[[1]])
      unique(parts[nzchar(parts)])
    }

        time_break_labels_in_text <- function(...) {
          txt <- paste(..., collapse = " ")
          if (!nzchar(trimws(txt))) return(character(0))
          hits <- stringr::str_extract_all(
            txt,
            stringr::regex("(?:First|Second|Third|Fourth|Fifth)TimeBreak",
                           ignore_case = TRUE)
          )[[1]]
          unique(hits[nzchar(hits)])
        }

        stale_time_break_merge <- function(cfg, merge_name, merged_raw) {
          labels <- time_break_labels_in_text(merge_name, merged_raw)
          canonical <- trimws(as.character(cfg$time_break_label %||% ""))
          stale <- if (!length(labels)) {
            character(0)
          } else if (!nzchar(canonical)) {
            labels
          } else {
            labels[!tolower(labels) %in% tolower(canonical)]
          }
          if (isTRUE(cfg$legacy_break_missing_part_migrated) &&
              length(cfg$dimension_breaks %||% list()) > 0L) {
            merge_text <- paste(merge_name, merged_raw)
            has_legacy_total <- grepl(
              "(^|[^[:alnum:]])Total([^[:alnum:]]|$)",
              merge_text,
              ignore.case = TRUE,
              perl = TRUE
            )
            if (has_legacy_total) {
              stale <- c(stale, "Total (legacy missing break part)")
            }
          }
          unique(stale)
        }

        apply_config_keywords <- function(cfg, row) {
      if ("ActivityKeyword" %in% names(row)) {
        act_kw <- trimws(as.character(row$ActivityKeyword[[1]] %||% ""))
        if (!is.na(act_kw) && nzchar(act_kw)) cfg$activity_keyword <- act_kw
      }
      if ("SpendKeyword" %in% names(row)) {
        spend_kw <- trimws(as.character(row$SpendKeyword[[1]] %||% ""))
        if (!is.na(spend_kw) && nzchar(spend_kw)) cfg$spend_keyword <- spend_kw
      }
      if ("ModelMetric" %in% names(row)) {
        mm <- trimws(as.character(row$ModelMetric[[1]] %||% ""))
        if (!is.na(mm) && nzchar(mm)) cfg$model_metric <- normalize_model_metric(mm)
      } else if (is.null(cfg$model_metric)) {
        cfg$model_metric <- "activity"
      }
      if ("VarNameInclude" %in% names(row)) {
        vi <- parse_config_varnames(row$VarNameInclude[[1]] %||% "")
        if (length(vi)) cfg$varname_include <- vi
      }
      if ("TimeBreakLabel" %in% names(row)) {
        tbr <- trimws(as.character(row$TimeBreakLabel[[1]] %||% ""))
        cfg$legacy_time_break_label <- if (!is.na(tbr)) tbr else ""
        # VOF and ModelDetails define the active label. Legacy metadata remains
        # available for audit but cannot override that source.
        if (!identical(cfg$source %||% "", "vof"))
          cfg$time_break_label <- if (!is.na(tbr)) tbr else ""
      }
      if ("GeoLabel" %in% names(row)) {
        gl <- normalize_geo_label(row$GeoLabel[[1]] %||% "")
        cfg$geo_label <- gl
      }
      missing_value <- if ("BreakMissingPartValue" %in% names(row)) {
        trimws(as.character(row$BreakMissingPartValue[[1]] %||% ""))
      } else {
        ""
      }
      has_legacy_missing_value <- !is.na(missing_value) &&
        nzchar(missing_value) &&
        !identical(tolower(missing_value), "unknown")
      cfg$legacy_break_missing_part_migrated <-
        isTRUE(cfg$legacy_break_missing_part_migrated) || has_legacy_missing_value
      cfg$break_missing_part_value <- canonical_break_missing_part_value()
      if ("BreakDefaultSeparator" %in% names(row)) {
        default_sep <- as.character(row$BreakDefaultSeparator[[1]] %||% "")
        if (!is.na(default_sep) && nzchar(default_sep))
          cfg$break_default_separator <- default_sep
      }
      if (nzchar(cfg$time_break_label %||% "")) cfg$geo_label <- ""
      cfg
    }

infer_time_break_from_config_merges <- function(nm, merge_rows) {
      if (!nrow(merge_rows)) return("")
      rows <- merge_rows[trimws(merge_rows$Channel) == trimws(nm), , drop = FALSE]
      if (!nrow(rows)) return("")
      txt <- paste(c(rows$Name %||% "", rows$Splits %||% "", rows$BreakInfo %||% ""),
                   collapse = " ")
      matches <- gregexpr("\\|[[:alpha:]]+TimeBreak", txt, ignore.case = TRUE, perl = TRUE)[[1]]
      if (identical(matches[1], -1L)) return("")
      vals <- regmatches(txt, list(matches))[[1]]
      vals <- unique(sub("^\\|", "", vals))
      vals <- vals[nzchar(vals)]
  if (length(vals) == 1L) vals[[1]] else ""
}

infer_merge_view <- function(merge_name, merged_raw) {
  txt <- paste(c(merge_name, merged_raw), collapse = " ")
  has_nonfocus <- grepl("_Before\\s+", txt, ignore.case = TRUE) ||
    grepl("\\|[[:alpha:]]+TimeBreak", txt, ignore.case = TRUE)
  if (has_nonfocus) "nonfocus" else "focus"
}
