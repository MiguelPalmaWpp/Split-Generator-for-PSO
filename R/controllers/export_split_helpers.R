register_export_split_helpers <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    normalize_export_split <- function(x) {
      trimws(as.character(x))
    }

    strip_export_time <- function(x) {
      stringr::str_remove(
        as.character(x),
        "(_Before .*|_Before_.*|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$"
      )
    }

    split_key_variants <- function(x) {
      x <- normalize_export_split(x)
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return(character(0))
      squish <- function(v) tolower(stringr::str_squish(as.character(v)))
      unique(c(x, strip_export_time(x), squish(x), squish(strip_export_time(x))))
    }

    split_export_signature <- function(x, cfg = list()) {
      base <- strip_export_time(x)
      suffix <- tolower(trimws(stringr::str_remove(as.character(x), stringr::fixed(base))))
      vars <- unique(normalize_export_split(cfg$varname_include %||% character(0)))
      vars <- vars[!is.na(vars) & nzchar(vars)]
      vars <- vars[order(nchar(vars), decreasing = TRUE)]

      matched_var <- ""
      remainder <- base
      for (vn in vars) {
        vn_l <- tolower(vn)
        base_l <- tolower(base)
        if (identical(base_l, vn_l)) {
          matched_var <- vn
          remainder <- ""
          break
        }
        prefix <- paste0(vn, "_")
        if (startsWith(base_l, tolower(prefix))) {
          matched_var <- vn
          remainder <- substring(base, nchar(prefix) + 1L)
          break
        }
      }

      if (!nzchar(matched_var)) {
        pieces <- strsplit(base, "_", fixed = TRUE)[[1]]
        matched_var <- pieces[1] %||% ""
        remainder <- if (length(pieces) > 1) paste(pieces[-1], collapse = "_") else ""
      }

      parts <- strsplit(remainder, "_", fixed = TRUE)[[1]]
      parts <- normalize_export_split(parts)
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(
        tolower(normalize_export_split(matched_var)),
        suffix,
        paste(sort(tolower(parts)), collapse = "|"),
        sep = "||"
      )
    }

    summarize_split_diagnostics <- function(df, nm, cfg = list()) {
      if (is.null(df) || !nrow(df) || !"VariableSplit" %in% names(df)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric(),
          total_spend = numeric()
        ))
      }

      out <- as.data.frame(df)
      out$VariableSplit <- normalize_export_split(out$VariableSplit)
      out <- out[!is.na(out$VariableSplit) & nzchar(out$VariableSplit), , drop = FALSE]
      if (!nrow(out)) {
        return(tibble::tibble(
          VariableSplit = character(),
          MainModelVariableName = character(),
          total_activity = numeric(),
          total_spend = numeric()
        ))
      }

      if (!"MainModelVariableName" %in% names(out)) {
        if ("model_var" %in% names(out)) {
          out$MainModelVariableName <- out$model_var
        } else {
          out$MainModelVariableName <- cfg$model_variable %||% cfg$channel_name %||% nm
        }
      }
      out$MainModelVariableName <- normalize_export_split(out$MainModelVariableName)
      out$MainModelVariableName[!nzchar(out$MainModelVariableName)] <-
        cfg$model_variable %||% cfg$channel_name %||% nm

      if (!"total_activity" %in% names(out)) out$total_activity <- NA_real_
      if (!"total_spend" %in% names(out)) out$total_spend <- NA_real_
      out$total_activity <- suppressWarnings(as.numeric(out$total_activity))
      out$total_spend <- suppressWarnings(as.numeric(out$total_spend))

      out %>%
        dplyr::group_by(.data$VariableSplit, .data$MainModelVariableName) %>%
        dplyr::summarise(
          total_activity = if (all(is.na(.data$total_activity))) NA_real_
          else sum(.data$total_activity, na.rm = TRUE),
          total_spend = if (all(is.na(.data$total_spend))) NA_real_
          else sum(.data$total_spend, na.rm = TRUE),
          .groups = "drop"
        )
    }

    activity_splits_from_rag <- function(res, cfg = list(), nm = "") {
      empty <- tibble::tibble(
        VariableSplit = character(),
        MainModelVariableName = character(),
        total_activity = numeric()
      )
      if (is.null(res) || is.null(res$rag)) return(empty)

      rag_df <- as.data.frame(res$rag)
      if (!nrow(rag_df)) return(empty)

      cross_cols <- res$cross_cols %||% cfg$cross_cols %||% character(0)
      id_cols <- intersect(c(cross_cols, "Period"), names(rag_df))
      num_cols <- setdiff(names(rag_df)[vapply(rag_df, is.numeric, logical(1))], id_cols)
      if (!length(num_cols)) return(empty)

      spend_kw <- cfg$spend_keyword %||% "Spend"
      if (nzchar(spend_kw)) {
        num_cols <- num_cols[!grepl(spend_kw, num_cols, ignore.case = TRUE)]
      }
      num_cols <- num_cols[!grepl("^ModelTotal$|^TotalCheck$|^Total$", num_cols, ignore.case = TRUE)]
      if (!length(num_cols)) return(empty)

      act_kw <- cfg$activity_keyword %||% ""
      act_cols <- if (nzchar(act_kw)) {
        grep(act_kw, num_cols, ignore.case = TRUE, value = TRUE)
      } else {
        character(0)
      }

      diag_cols <- character(0)
      if (!is.null(res$act_diagnoses) &&
          "VariableSplit" %in% names(res$act_diagnoses)) {
        diag_cols <- normalize_export_split(unique(res$act_diagnoses$VariableSplit))
        diag_cols <- intersect(num_cols, diag_cols)
      }
      act_cols <- unique(c(act_cols, diag_cols))
      if (!length(act_cols)) return(empty)

      active_cols <- act_cols[vapply(act_cols, function(col) {
        vals <- suppressWarnings(as.numeric(rag_df[[col]]))
        any(!is.na(vals) & vals != 0)
      }, logical(1))]
      if (!length(active_cols)) return(empty)

      mmv <- cfg$model_variable %||% cfg$channel_name %||% nm
      tibble::tibble(
        VariableSplit = active_cols,
        MainModelVariableName = mmv,
        total_activity = vapply(active_cols, function(col) {
          sum(suppressWarnings(as.numeric(rag_df[[col]])), na.rm = TRUE)
        }, numeric(1))
      ) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }

    spend_splits_from_rag <- function(res, cfg = list(), nm = "") {
      empty <- tibble::tibble(
        VariableSplit = character(),
        MainModelVariableName = character(),
        total_spend = numeric()
      )
      if (is.null(res) || is.null(res$rag)) return(empty)

      rag_df <- as.data.frame(res$rag)
      if (!nrow(rag_df)) return(empty)

      cross_cols <- res$cross_cols %||% cfg$cross_cols %||% character(0)
      id_cols <- intersect(c(cross_cols, "Period"), names(rag_df))
      num_cols <- setdiff(names(rag_df)[vapply(rag_df, is.numeric, logical(1))], id_cols)
      num_cols <- num_cols[!grepl("^ModelTotal$|^TotalCheck$|^Total$", num_cols, ignore.case = TRUE)]
      if (!length(num_cols)) return(empty)

      spend_kw <- cfg$spend_keyword %||% "Spend"
      spend_cols <- if (nzchar(spend_kw)) {
        grep(spend_kw, num_cols, ignore.case = TRUE, value = TRUE)
      } else {
        character(0)
      }

      diag_cols <- character(0)
      if (!is.null(res$cost_diagnoses) &&
          "VariableSplit" %in% names(res$cost_diagnoses)) {
        diag_cols <- normalize_export_split(unique(res$cost_diagnoses$VariableSplit))
        diag_cols <- intersect(num_cols, diag_cols)
      }
      spend_cols <- unique(c(spend_cols, diag_cols))
      if (!length(spend_cols)) return(empty)

      active_cols <- spend_cols[vapply(spend_cols, function(col) {
        vals <- suppressWarnings(as.numeric(rag_df[[col]]))
        any(!is.na(vals) & vals != 0)
      }, logical(1))]
      if (!length(active_cols)) return(empty)

      mmv <- cfg$model_variable %||% cfg$channel_name %||% nm
      tibble::tibble(
        VariableSplit = active_cols,
        MainModelVariableName = mmv,
        total_spend = vapply(active_cols, function(col) {
          sum(suppressWarnings(as.numeric(rag_df[[col]])), na.rm = TRUE)
        }, numeric(1))
      ) %>%
        dplyr::distinct(VariableSplit, MainModelVariableName, .keep_all = TRUE)
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
