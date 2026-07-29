read_system_resource <- function(path) {
  if (!file.exists(path)) return(character())
  tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character())
}

cpu_set_size <- function(x) {
  x <- trimws(as.character(x)[1L])
  if (is.na(x) || !nzchar(x)) return(NA_integer_)
  ranges <- strsplit(x, ",", fixed = TRUE)[[1L]]
  counts <- vapply(ranges, function(range) {
    bounds <- suppressWarnings(as.integer(strsplit(trimws(range), "-", fixed = TRUE)[[1L]]))
    if (anyNA(bounds) || !length(bounds) || length(bounds) > 2L) return(NA_integer_)
    if (length(bounds) == 1L) return(1L)
    if (bounds[[2L]] < bounds[[1L]]) return(NA_integer_)
    bounds[[2L]] - bounds[[1L]] + 1L
  }, integer(1L))
  if (anyNA(counts)) NA_integer_ else sum(counts)
}

cpu_quota_size <- function(x) {
  fields <- strsplit(trimws(as.character(x)[1L]), "[[:space:]]+")[[1L]]
  if (length(fields) < 2L || identical(fields[[1L]], "max")) return(NA_integer_)
  values <- suppressWarnings(as.numeric(fields[1:2]))
  if (anyNA(values) || any(values <= 0)) return(NA_integer_)
  max(1L, as.integer(ceiling(values[[1L]] / values[[2L]])))
}

detect_available_threads <- function() {
  detected <- tryCatch(
    parallel::detectCores(logical = TRUE),
    error = function(e) NA_integer_
  )
  candidates <- as.numeric(detected)

  status <- tryCatch(
    readLines("/proc/self/status", warn = FALSE),
    error = function(e) character()
  )
  allowed <- grep("^Cpus_allowed_list:", status, value = TRUE)
  if (length(allowed)) {
    candidates <- c(
      candidates,
      cpu_set_size(sub("^Cpus_allowed_list:[[:space:]]*", "", allowed[[1L]]))
    )
  }

  cpuset_paths <- c(
    "/sys/fs/cgroup/cpuset.cpus.effective",
    "/sys/fs/cgroup/cpuset/cpuset.cpus"
  )
  for (path in cpuset_paths) {
    value <- read_system_resource(path)
    if (length(value)) candidates <- c(candidates, cpu_set_size(value))
  }

  cpu_max <- read_system_resource("/sys/fs/cgroup/cpu.max")
  if (length(cpu_max)) {
    candidates <- c(candidates, cpu_quota_size(cpu_max))
  } else {
    quota <- suppressWarnings(as.numeric(read_system_resource(
      "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
    )))
    period <- suppressWarnings(as.numeric(read_system_resource(
      "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
    )))
    if (length(quota) && length(period) && is.finite(quota) && quota > 0 &&
        is.finite(period) && period > 0) {
      candidates <- c(candidates, max(1L, as.integer(ceiling(quota / period))))
    }
  }

  candidates <- candidates[is.finite(candidates) & candidates >= 1]
  if (!length(candidates)) return(1L)
  max(1L, as.integer(floor(min(candidates))))
}

memory_value_bytes <- function(x) {
  x <- trimws(as.character(x)[1L])
  if (is.na(x) || !nzchar(x) || identical(x, "max")) return(NA_real_)
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value) || value <= 0) NA_real_ else value
}

detect_available_memory_mb <- function() {
  candidates <- numeric()
  meminfo <- tryCatch(
    readLines("/proc/meminfo", warn = FALSE),
    error = function(e) character()
  )
  total <- grep("^MemTotal:", meminfo, value = TRUE)
  if (length(total)) {
    kb <- suppressWarnings(as.numeric(sub(
      "^MemTotal:[[:space:]]*([0-9]+).*", "\\1", total[[1L]]
    )))
    if (is.finite(kb) && kb > 0) candidates <- c(candidates, kb * 1024)
  }

  cgroup_paths <- c(
    "/sys/fs/cgroup/memory.max",
    "/sys/fs/cgroup/memory/memory.limit_in_bytes"
  )
  for (path in cgroup_paths) {
    value <- memory_value_bytes(read_system_resource(path))
    if (is.finite(value)) candidates <- c(candidates, value)
  }

  if (!length(candidates) && identical(unname(Sys.info()[["sysname"]]), "Darwin")) {
    sysctl <- tryCatch(
      suppressWarnings(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE)),
      error = function(e) character()
    )
    value <- memory_value_bytes(sysctl)
    if (is.finite(value)) candidates <- c(candidates, value)
  }

  if (!length(candidates)) return(Inf)
  max(1, floor(min(candidates) / 1024^2))
}

detect_system_resources <- function() {
  list(
    threads = detect_available_threads(),
    memory_mb = detect_available_memory_mb()
  )
}
