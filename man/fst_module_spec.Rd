\name{fst_module_spec}
\alias{fst_module_spec}
\title{Construct the FST analysis module descriptor}
\usage{
fst_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs the self-contained registry contract for global and pairwise Weir-Cockerham FST analysis. The descriptor owns execution, validation, output names, scientific references, resource classification, and contract version while preserving the existing estimates, confidence intervals, population-metadata requirements, and output schemas.
}
