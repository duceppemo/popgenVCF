\name{benchmark_datasets}
\alias{new_benchmark_dataset_entry}
\alias{validate_benchmark_dataset_entry}
\alias{new_benchmark_dataset_catalogue}
\alias{register_benchmark_dataset}
\alias{list_benchmark_datasets}
\alias{benchmark_dataset_cache_dir}
\alias{verify_benchmark_dataset}
\alias{resolve_benchmark_dataset}
\alias{benchmark_dataset_from_entry}
\alias{default_benchmark_dataset_catalogue}
\title{Benchmark dataset catalogue and cache}
\usage{
new_benchmark_dataset_entry(id, version = "1", scale = "tiny",
  source_type = c("embedded", "local", "remote"), filename,
  checksum = NA_character_, source, organism = "unspecified",
  analyses = character(), required_software = character(),
  estimated_runtime_seconds = NA_real_, estimated_memory_mb = NA_real_,
  published = TRUE, metadata = list())

validate_benchmark_dataset_entry(x)
new_benchmark_dataset_catalogue(entries = list())
register_benchmark_dataset(catalogue, entry)
list_benchmark_datasets(catalogue, scale = NULL, organism = NULL,
  analysis = NULL, source_type = NULL)
benchmark_dataset_cache_dir(cache_dir = NULL)
verify_benchmark_dataset(path, checksum = NA_character_)
resolve_benchmark_dataset(entry, cache_dir = NULL, offline = FALSE,
  force = FALSE, quiet = TRUE)
benchmark_dataset_from_entry(entry, cache_dir = NULL, offline = FALSE,
  reader = function(path) readRDS(path))
default_benchmark_dataset_catalogue()
}
\arguments{
\item{id}{Stable dataset identifier.}
\item{version}{Dataset release version.}
\item{scale}{Dataset scale.}
\item{source_type}{Embedded, local, or remote source.}
\item{filename}{File name used within the cache.}
\item{checksum}{Expected SHA256 checksum.}
\item{source}{Materializer function, local path, or remote URL.}
\item{organism}{Organism label.}
\item{analyses}{Supported analysis identifiers.}
\item{required_software}{External software annotations.}
\item{estimated_runtime_seconds, estimated_memory_mb}{Resource estimates.}
\item{published}{Whether a remote entry is available.}
\item{metadata}{Additional named metadata.}
\item{x}{Dataset entry to validate.}
\item{entries}{Initial catalogue entries.}
\item{catalogue}{Dataset catalogue.}
\item{entry}{Dataset entry.}
\item{analysis}{Optional analysis filter.}
\item{cache_dir}{Optional cache root.}
\item{path}{Dataset file path.}
\item{offline}{Disable remote access.}
\item{force}{Refresh a valid cached file.}
\item{quiet}{Suppress download progress.}
\item{reader}{Function reading the cached file.}
}
\value{Canonical dataset entries or catalogues, filtered data tables, verified cache paths, or benchmark datasets.}
\description{Defines versioned benchmark dataset records and provides deterministic cache resolution, SHA256 verification, atomic fetch-on-demand, offline reuse, catalogue filtering, and conversion to benchmark dataset loaders.}
