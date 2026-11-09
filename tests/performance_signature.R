# Verify cache signatures change when data changes, including a middle row.
# Identical inputs must keep the same signature for cache reuse.
source("R/utils/performance.R")

first <- data.frame(id = as.numeric(1:10), value = as.numeric(1:10))
changed <- first
changed$value[[5L]] <- 999
stopifnot(!identical(data_signature(first), data_signature(changed)))
stopifnot(identical(data_signature(first), data_signature(first)))
stopifnot(identical(data_signature(NULL), "null"))

cat("PERFORMANCE_SIGNATURE_OK\n")
