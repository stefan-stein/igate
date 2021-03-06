% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate}
\alias{validate}
\title{Validates results after using \code{\link{igate}} or \code{\link{categorical.igate}}.}
\usage{
validate(validation_df, target, causes, results_df, type = NULL)
}
\arguments{
\item{validation_df}{Data frame to be used for validation. It is recommended to use
a different data frame from the one used in \code{\link{igate}}/ \code{\link{categorical.igate}}.
The same data frame can be used if just a sanity check of the results is performed. This
data frame must contain the \code{target} variable as well as all the causes determined
by \code{\link{igate}}/ \code{\link{categorical.igate}}.}

\item{target}{Target variable that was used in \code{\link{igate}} or \code{\link{categorical.igate}}.}

\item{causes}{Causes determined by \code{\link{igate}} or \code{\link{categorical.igate}}.
If you saved the results of \code{\link{igate}}/ \code{\link{categorical.igate}} in an object
\code{results}, simply use \code{results$Causes} here.}

\item{results_df}{The data frame containing the results of \code{\link{igate}} or \code{\link{categorical.igate}}.}

\item{type}{The type of igate that was performed: either \code{"continuous"} or \code{"categorical"}. If not provided
function will try to guess the correct type based on the type of \code{validation_df[[target]]}.}
}
\value{
A list of three data frames is returned. The first data frame contains those observations
in \code{validation_df} that fall into *all* the good resp. bad control bands specified in \code{results_df}.
The columns are \code{target}, then one column for each of the \code{causes} and a new column
\code{expected_quality} which is \code{"good"} if the observation falls into all the good
control bands and \code{"bad"} if it falls into all the bad control bands.

The second data frame has three columns
\tabular{ll}{
\code{Cause} \tab Each of the \code{causes}.\cr
\code{Good_Count} \tab If we selected all those observations that fall into the good band
of this cause, how many observations would we select?\cr
\code{Bad_Count} \tab If we selected all those observations that fall into the bad band
of this cause, how many observations would we select?
}

The third data frame summarizes the first data frame: If \code{type = "continuous"} it has
three columns:
\tabular{ll}{
\code{expected_quality} \tab Either \code{"good"} or \code{"bad"}.\cr
\code{max_target} \tab The maximum value for \code{target} for the observations with "good"
expected quality resp. "bad" expected quality. \cr
\code{min_target} \tab Minimum value of \code{target} for good resp. bad expected quality.
}
If \code{type = "categorical"} it has the following three columns:
\tabular{ll}{
\code{expected_quality} \tab Either \code{"good"} or \code{"bad"}.\cr
\code{Category} \tab A list of categories of the observations with expected quality good resp. bad. \cr
\code{Frequency} \tab A count how often the respective \code{Category} appears amongs the observations with
good/ bad expected quality.
}
}
\description{
Takes a new data frame to be used for validation and the causes and control bands
obtained from \code{\link{igate}} or \code{\link{categorical.igate}} and returns
all those observations that fall within these control bands.
}
\details{
If a value of \code{Good_Count} or \code{Bad_count} is very low in the second
data frame, it means that this cause is excluding a lot of observations from the
first data frame. Consider re-running \code{validate} with this cause removed from
\code{causes}.
}
\examples{
validate(iris, target = "Sepal.Length", causes = resultsIris$Causes, results_df = resultsIris)

}
