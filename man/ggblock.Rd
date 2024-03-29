% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{ggblock}
\alias{ggblock}
\title{Plot Blockmodel matrix}
\usage{
ggblock(
  g,
  blocks = NULL,
  cols = NULL,
  show_blocks = FALSE,
  show_labels = FALSE
)
}
\arguments{
\item{g}{igraph object with a sign edge attribute.}

\item{blocks}{vector of block membership as obtained, e.g. from \link{signed_blockmodel}}

\item{cols}{colors used for negative and positive ties}

\item{show_blocks}{logical. Should block borders be displayed? (Default: FALSE)}

\item{show_labels}{logical. Should node labels be displayed? (Default: FALSE)}
}
\value{
ggplot2 object
}
\description{
Plot Blockmodel matrix
}
\examples{
\dontrun{
library(igraph)
data("tribes")
clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
ggblock(tribes, clu$membership, show_blocks = TRUE, show_labels = TRUE)
}
}
\author{
David Schoch
}
