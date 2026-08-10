\name{genome_scan_module_spec}
\alias{genome_scan_module_spec}
\title{Construct the sliding-window genome scan analysis module descriptor}
\usage{
genome_scan_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
The descriptor owns the complete genome-scan registry contract: windowed Weir-Cockerham global FST, windowed diversity, and windowed Tajima's D, tracked along physical position rather than only as a single genome-wide summary, while preserving the existing runner, result schema, and population-metadata requirements.
}
