# Lightweight list contracts shared across the app layers.

validate_data_bundle <- function(data) {
  required <- c("all_rags", "analytical", "dates_df")
  missing <- setdiff(required, names(data %||% list()))
  if (length(missing)) {
    return(paste("Data bundle is missing:", paste(missing, collapse = ", ")))
  }
  NULL
}

validate_channel_config <- function(cfg) {
  if (is.null(cfg) || !is.list(cfg)) return("Channel config must be a list.")
  if (!nzchar(as.character(cfg$model_variable %||% ""))) {
    return("Channel config is missing model_variable.")
  }
  NULL
}

validate_processed_result <- function(result) {
  if (is.null(result) || !is.list(result)) {
    return("Processed result must be a list.")
  }
  if (is.null(result$rag) || !is.data.frame(result$rag)) {
    return("Processed result is missing its rag data frame.")
  }
  NULL
}

validate_media_index_result <- function(index) {
  required <- c("channels", "connection_map", "role_map", "summary", "vof_contract")
  missing <- setdiff(required, names(index %||% list()))
  if (length(missing)) {
    return(paste("Media Variable Index is missing:", paste(missing, collapse = ", ")))
  }
  if (!is.list(index$channels) || !is.data.frame(index$connection_map)) {
    return("Media Variable Index has invalid channel or connection data.")
  }
  NULL
}

new_process_payload <- function(channel, all_rags, analytical, dates_df, cfg,
                                cross_cols, start_report_date,
                                end_report_date, update_label,
                                schema_metadata = NULL,
                                operation_id = "sync",
                                data_signature_value = "",
                                config_signature_value = "",
                                result_version = 0L) {
  structure(list(
    operation_id = as.character(operation_id),
    channel = as.character(channel),
    data_signature = as.character(data_signature_value),
    config_signature = as.character(config_signature_value),
    result_version = as.integer(result_version),
    queued_at = Sys.time(),
    all_rags = all_rags,
    analytical = analytical,
    dates_df = dates_df,
    cfg = cfg,
    cross_cols = cross_cols,
    start_report_date = start_report_date,
    end_report_date = end_report_date,
    update_label = update_label,
    schema_metadata = schema_metadata
  ), class = c("pso_process_payload", "list"))
}
