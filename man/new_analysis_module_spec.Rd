\name{new_analysis_module_spec}
\alias{new_analysis_module_spec}
\alias{register_analysis_module}
\alias{print.PopgenVCFModuleSpec}
\title{Create and register self-contained analysis module descriptors}
\usage{
new_analysis_module_spec(
  name,
  run,
  requires = character(),
  enabled = TRUE,
  description = "",
  validate = validate_module_result,
  outputs = name,
  references = character(),
  resource_class = "standard",
  contract_version = "1.0",
  artifacts = character(),
  artifacts_must_exist = FALSE
)

register_analysis_module(registry, module)

\method{print}{PopgenVCFModuleSpec}(x, ...)
}
\arguments{
  \item{name}{Unique module name.}
  \item{run}{Function accepting \code{(analysis, context)}.}
  \item{requires}{Prerequisite module names.}
  \item{enabled}{Logical value or configuration predicate.}
  \item{description}{Short module description.}
  \item{validate}{Statistical-result validator.}
  \item{outputs}{Declared analysis result names.}
  \item{references}{Scientific references.}
  \item{resource_class}{One of \code{light}, \code{standard}, \code{heavy}, or \code{external}.}
  \item{contract_version}{Module contract version.}
  \item{artifacts}{Required artifact identifiers without the module prefix.}
  \item{artifacts_must_exist}{Whether artifact files must exist after execution.}
  \item{registry}{A \code{PopgenVCFRegistry} object.}
  \item{module}{A \code{PopgenVCFModuleSpec} object.}
  \item{x}{A module descriptor.}
  \item{...}{Unused.}
}
\value{
\code{new_analysis_module_spec()} returns a \code{PopgenVCFModuleSpec}.
\code{register_analysis_module()} returns the updated registry.
The print method returns \code{x} invisibly.
}
\description{
A module descriptor owns the complete registry contract for one analysis,
including execution, dependencies, validation, declared results, scientific
references, resource class, and publication artifacts.
}
