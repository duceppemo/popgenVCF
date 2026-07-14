\name{analysis_registry}
\alias{analysis_registry}
\alias{new_analysis_registry}
\alias{register_analysis}
\alias{list_analyses}
\alias{resolve_analysis_order}
\alias{execute_analysis_registry}
\alias{default_analysis_registry}
\title{Dependency-aware analysis registry}
\description{Register analysis modules, resolve their dependency graph, and execute them against a validated popgenVCF analysis state.}
\usage{
new_analysis_registry()

register_analysis(
  registry,
  name,
  run,
  requires = character(),
  enabled = TRUE,
  description = "",
  validate = validate_module_result,
  outputs = name,
  references = character(),
  resource_class = "standard",
  contract_version = "1.0"
)

list_analyses(registry)

resolve_analysis_order(registry, config, selected = NULL)

execute_analysis_registry(analysis, context, registry, selected = NULL)

default_analysis_registry()
}
\arguments{
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{name}{Unique module name.}
  \item{run}{Module runner accepting \code{analysis} and \code{context}.}
  \item{requires}{Character vector of prerequisite module names.}
  \item{enabled}{Logical value or function accepting the validated config.}
  \item{description}{Short module description.}
  \item{validate}{Result validator accepting result, analysis, and context.}
  \item{outputs}{Declared analysis-result names.}
  \item{references}{Scientific references supporting the module.}
  \item{resource_class}{Expected resource class.}
  \item{contract_version}{Module-contract version.}
  \item{config}{Validated popgenVCF configuration.}
  \item{selected}{Optional module names.}
  \item{analysis}{A \code{PopgenVCFAnalysis} object.}
  \item{context}{Runtime context shared by module runners.}
}
\value{A registry, module table, execution order, or updated analysis/context bundle depending on the function.}
