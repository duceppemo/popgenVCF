\name{dapc_module_spec}
\alias{dapc_module_spec}
\title{Construct the DAPC analysis module descriptor}
\usage{
dapc_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs the self-contained registry contract for discriminant analysis of principal components. The descriptor owns execution, validation, output names, the diversity dependency, configuration enablement, scientific references, resource classification, and contract version while preserving existing population-metadata requirements and results.
}
