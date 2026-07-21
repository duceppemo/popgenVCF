\name{execute_public_analysis}
\alias{execute_public_analysis}
\title{Execute a Canonical Public Analysis Request}
\usage{
execute_public_analysis(
  request,
  analysis,
  context,
  registry,
  engine = new_execution_engine(),
  selected = NULL
)
}
\arguments{
\item{request}{A validated \code{PopgenVCFPublicAPIRequest} whose operation is \code{analysis.execute}.}
\item{analysis}{A \code{PopgenVCFAnalysis} object.}
\item{context}{Runtime context supplied to registered modules.}
\item{registry}{A \code{PopgenVCFRegistry} object.}
\item{engine}{A \code{PopgenVCFExecutionEngine} object.}
\item{selected}{Optional module names passed to the authoritative planner.}
}
\value{A validated \code{PopgenVCFPublicAPIResponse}.}
\description{
Delegates planning and execution to the authoritative Phase 8 execution engine and translates its terminal ledger into the stable Phase 10 public response contract. The adapter does not implement scheduling, retries, checkpointing, or module execution.
}
