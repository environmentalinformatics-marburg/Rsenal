% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated_ffs.R
\name{ffs-deprecated}
\alias{ffs-deprecated}
\title{Forward feature selection}
\usage{
ffs(predictors, response, method = "rf"
, metric = ifelse(is.factor(response), "Accuracy", "RMSE")
, maximize = ifelse(metric == "RMSE", FALSE, TRUE), withinSD = FALSE
, trControl = trainControl(), tuneLength = 3, tuneGrid = NULL
, seed = sample(1:1000, 1), runParallel = FALSE, ...)
}
\arguments{
\item{predictors}{see \code{\link{train}}}

\item{response}{see \code{\link{train}}}

\item{method}{see \code{\link{train}}}

\item{metric}{see \code{\link{train}}}

\item{maximize}{see \code{\link{train}}}

\item{withinSD}{Logical Models are only selected if they are better than the 
currently best models Standard error}

\item{trControl}{see \code{\link{train}}}

\item{tuneLength}{see \code{\link{train}}}

\item{tuneGrid}{see \code{\link{train}}}

\item{seed}{A random number}

\item{runParallel}{Logical}

\item{...}{arguments passed to the classification or regression routine 
(such as randomForest). Errors will occur if values for tuning parameters are 
passed here.}
}
\value{
A list of class train. Beside of the usual train contentm
the object contains the vector "selectedvars" and "selectedvars_perf"
that give the order of the variables selected as well as their corresponding
performance (starting from the first two variables)
}
\description{
A simple forward feature selection algorithm
}
\details{
Models with two predictors are first trained using all possible 
pairs of predictor variables. The best model of these initial models is kept.
On the basis of this best model the predictor variables are iteratively
increased and each of the remaining variables is tested for its improvement
of the currently best model. The process stops if none of the remaining 
variables increases the model performance when added to the current best model.

The internal cross validation can be run in parallel. See information
on parallel processing of carets train functions for details.

Using withinSE will favour models with less variables and
probably shorten the calculation time
}
\note{
This validation is particulary suitable for 
leave-one-station-out cross validations where variable selection
MUST be based on the performance of the model on the hold out station.
A computational time expesnsive alternative is the best subset
selection (\code{bss}).
}
\examples{
 \dontrun{
data(iris)
ffsmodel <- ffs(iris[,1:4],iris$Species)
ffsmodel$selectedvars
ffsmodel$selectedvars_perf 
}
}
\seealso{
\code{\link{train}},  \code{bss}, 
\code{\link{trainControl}},\code{\link{rfe}}

\code{\link{Rsenal-deprecated}}
}
\author{
Hanna Meyer
}
\keyword{internal}
