% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validationCountsIris-data.R
\docType{data}
\name{validationCountsIris}
\alias{validationCountsIris}
\title{validationCountsIris data set}
\format{A data frame as described in the documentation of \code{\link{validate}}.}
\usage{
validationCountsIris
}
\description{
Example validation data file to be used for example report generation.
}
\details{
This is the output of

\code{
x <- validate(iris, target = "Sepal.Length",
causes = resultsIris$Causes,
results_df = resultsIris)
}

\code{validationCountsIris <- x[[2]]}
}
\keyword{datasets}
