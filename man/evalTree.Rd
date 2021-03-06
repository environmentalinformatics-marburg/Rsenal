% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evalTree.R
\name{evalTree}
\alias{evalTree}
\title{Evaluate performance of a conditional inference tree}
\usage{
evalTree(
  independ = NULL,
  depend,
  data,
  seed = 10L,
  size = 1000L,
  minbucket = 100L,
  n.cores = 1L,
  ...
)
}
\arguments{
\item{independ}{numeric. Column index(es) of independent variable(s).}

\item{depend}{numeric. Column index of dependent variable.}

\item{data}{\code{data.frame} with independent and dependent variables.}

\item{seed}{integer. Seed required for random number generation, see 
\code{\link{set.seed}}.}

\item{size}{integer. Size of the training sample.}

\item{minbucket}{integer. Minimum sum of weights in a terminal node.}

\item{n.cores}{integer. Number of cores for parallel computing.}

\item{...}{Further arguments passed on to \code{\link{ctree_control}}.}
}
\value{
Model evaluation statistics.
}
\description{
This function evaluates the performance of a so-called conditional
inference tree (see ?ctree for details) by calculating various scores, 
e.g. accuracy, probability of detection, and false alarm ratio. See
\url{http://cawcr.gov.au/projects/verification/} for further information.
}
\seealso{
\code{\link{ctree}}, \code{\link{ctree_control}}.
}
\author{
Florian Detsch
}
