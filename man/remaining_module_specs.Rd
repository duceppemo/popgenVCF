\name{remaining_module_specs}
\alias{diversity_module_spec}
\alias{tree_module_spec}
\alias{admixture_module_spec}
\alias{faststructure_module_spec}
\alias{snmf_module_spec}
\alias{chromosome_module_spec}
\title{Construct built-in analysis module descriptors}
\usage{
diversity_module_spec()

tree_module_spec()

admixture_module_spec()

faststructure_module_spec()

snmf_module_spec()

chromosome_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
Constructs self-contained registry contracts for diversity, neighbour-joining
trees, ADMIXTURE, fastStructure, sNMF, and chromosome-specific analyses. Each
descriptor owns execution, dependencies, enablement, validation, declared
outputs, scientific references, resource classification, and contract version.
}
