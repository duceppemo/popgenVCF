\name{amova_module_spec}
\alias{amova_module_spec}
\title{Construct the AMOVA analysis module descriptor}
\usage{
amova_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs the self-contained registry contract for analysis of molecular variance. The descriptor owns execution, the diversity dependency, configuration enablement, validation, output naming, scientific reference, resource classification, and contract version while preserving the existing AMOVA implementation.
}
