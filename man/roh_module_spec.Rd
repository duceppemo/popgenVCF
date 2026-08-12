\name{roh_module_spec}
\alias{roh_module_spec}
\title{Construct the runs-of-homozygosity (ROH) analysis module descriptor}
\usage{
roh_module_spec()
}
\value{
A \code{PopgenVCFModuleSpec} object.
}
\description{
The descriptor owns the complete ROH registry contract: HMM-based autozygosity detection (\code{bcftools roh}, Narasimhan et al. 2016), the full per-run table, a per-sample FROH summary table, and a run-length-class breakdown of FROH (short/intermediate/long, Ceballos et al. 2018) while preserving the existing runner, result schema, and VCF-only behavior.
}
