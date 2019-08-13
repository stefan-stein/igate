#'validatedObsIris data set
#'
#'Example validation data file to be used for example report generation.
#'
#'@details
#'This is the output of
#'
#'\code{
#'x <- validate(iris, target = "Sepal.Length",
#'causes = resultsIris$Causes,
#'results_df = resultsIris)
#'}
#'
#'\code{validatedObsIris <- x[[1]]}
#'
#' @format A data frame as described in the documentation of \code{\link{validate}}.
#'
"validatedObsIris"
