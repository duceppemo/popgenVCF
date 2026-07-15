\name{ibd_module_spec}
\alias{ibd_module_spec}
\title{Construct the isolation-by-distance analysis module descriptor}
\usage{
ibd_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs the self-contained registry contract for Mantel tests and isolation-by-distance analysis. The descriptor owns execution, the IBS dependency, configuration enablement, validation, output naming, scientific references, resource classification, and contract version while preserving existing spatial capability requirements.
}
