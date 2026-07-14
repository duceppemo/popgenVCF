\name{configuration}
\alias{default_config}
\alias{read_config}
\alias{validate_config}
\title{Create, Read, and Validate Configuration}
\usage{
default_config()
read_config(path)
validate_config(cfg)
}
\arguments{
  \item{path}{Path to a YAML configuration file.}
  \item{cfg}{A configuration list.}
}
\value{A configuration list.}
\description{Functions for creating defaults, merging YAML values, and validating pipeline settings.}
