\name{manuscript-pandoc-rendering}
\alias{pandoc_status}
\alias{pandoc_render_arguments}
\alias{render_manuscript}
\alias{validate_manuscript_render}
\title{Render canonical manuscripts with Pandoc}
\usage{
pandoc_status(executable = Sys.which("pandoc"))

pandoc_render_arguments(manuscript_directory, format = c("html", "docx"), output = NULL)

render_manuscript(manuscript_directory, format = c("html", "docx"),
  pandoc = Sys.which("pandoc"), dry_run = FALSE, overwrite = FALSE)

validate_manuscript_render(x)
}
\arguments{
  \item{executable, pandoc}{Path to the Pandoc executable.}
  \item{manuscript_directory}{A validated manuscript source directory.}
  \item{format}{Output format, either \code{html} or \code{docx}.}
  \item{output}{Optional explicit output path.}
  \item{dry_run}{Return an execution record without invoking Pandoc.}
  \item{overwrite}{Permit replacement of an existing rendered output.}
  \item{x}{A \code{PopgenVCFManuscriptRender} record.}
}
\value{
\code{pandoc_status()} returns tool identity metadata;
\code{pandoc_render_arguments()} returns a character argument vector;
\code{render_manuscript()} returns a \code{PopgenVCFManuscriptRender};
\code{validate_manuscript_render()} returns \code{TRUE} invisibly.
}
\description{
Builds and optionally executes deterministic Pandoc commands for canonical
manuscript source directories while preserving tool identity, logs, exit
status, and output checksums.
}
