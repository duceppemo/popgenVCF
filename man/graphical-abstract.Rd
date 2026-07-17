\name{graphical-abstract}
\alias{new_graphical_abstract}
\alias{validate_graphical_abstract}
\alias{write_graphical_abstract}
\title{Deterministic graphical abstract specifications}
\usage{
new_graphical_abstract(manuscript, panels = list(), title = NULL,
  message = NULL, alt_text = NULL, width_px = 1800L,
  height_px = 1000L, resolution_dpi = 300L,
  orientation = c("landscape", "portrait", "square"),
  background = "white")

validate_graphical_abstract(x, strict = TRUE)

write_graphical_abstract(graphical_abstract, directory,
  overwrite = FALSE, strict = FALSE)
}
\arguments{
\item{manuscript}{A canonical \code{PopgenVCFManuscript} object.}
\item{panels}{Ordered panel records containing immutable artifact identities and existing file paths.}
\item{title}{Author-supplied graphical abstract title.}
\item{message}{Author-supplied central message.}
\item{alt_text}{Author-supplied accessibility description.}
\item{width_px, height_px}{Target pixel dimensions.}
\item{resolution_dpi}{Target resolution in dots per inch.}
\item{orientation}{Target landscape, portrait, or square orientation.}
\item{background}{Background description.}
\item{x}{A graphical abstract specification to validate.}
\item{strict}{Whether missing author inputs are errors.}
\item{graphical_abstract}{A \code{PopgenVCFGraphicalAbstract} object.}
\item{directory}{Parent output directory.}
\item{overwrite}{Whether an existing output directory may be replaced.}
}
\description{
Creates, validates, and writes deterministic graphical-abstract assembly
specifications without altering canonical scientific assets.
}
\value{
\code{new_graphical_abstract()} returns a \code{PopgenVCFGraphicalAbstract}
object. Validation returns \code{TRUE} invisibly. Writing returns the normalized
output directory invisibly.
}
