\name{ibs_module_spec}
\alias{ibs_module_spec}
\title{Construct the IBS/MDS analysis module descriptor}
\usage{
ibs_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs the self-contained registry contract for identity-by-state matrices and multidimensional scaling. The descriptor owns execution, validation, output names, scientific references, resource classification, and contract version while preserving the existing VCF-only workflow and downstream dependencies.
}
