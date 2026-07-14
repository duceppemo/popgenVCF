\name{run_pipeline}
\alias{run_pipeline}
\title{Run the Population Genomics Pipeline}
\usage{
run_pipeline(
  config,
  registry = default_analysis_registry(),
  selected = NULL
)
}
\arguments{
  \item{config}{A validated configuration list or path to a YAML configuration file.}
  \item{registry}{A \code{PopgenVCFRegistry}; defaults to the built-in registry.}
  \item{selected}{Optional module names. Dependencies are included automatically.}
}
\value{Invisibly returns a \code{PopgenVCFAnalysis} object. Results are also written to the configured output directory.}
\description{Runs deterministic input preparation and quality control, resolves the registered analysis dependency graph, and executes the selected modules in topological order.}
\examples{
\dontrun{
config <- default_config()
config$input$vcf <- "cohort.vcf.gz"
config$input$metadata <- "populations.tsv"
config$output$directory <- "results"
run_pipeline(config, selected = c("pca", "fst"))
}
}
