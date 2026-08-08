\name{kinship_module_spec}
\alias{kinship_module_spec}
\title{Construct the kinship/relatedness analysis module descriptor}
\usage{
kinship_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
The descriptor owns the complete pairwise-kinship registry contract: KING-robust kinship estimation (Manichaikul et al. 2010), the kinship and IBS0 matrices, the full pairs table, and a close-relative summary table while preserving the existing runner, result schema, and VCF-only behavior.
}
