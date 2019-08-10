#' Generates report about a conducted igate.
#'
#' Takes results from a previous igate and automatically generates a html report
#' for it.
#'
#' @param df The data frame that was analysed with \code{\link{igate}} or \code{\link{categorical.igate}}.
#' @param versus What value of \code{versus} was used?
#' @param target What \code{target} was used?
#' @param type Was \code{\link{igate}} (use \code{type = "continuous"}) or \code{\link{categorical.igate}} (use \code{type = "categorical"}) conducted?
#' @param test Which hypothesis test was used alongside the counting method?
#' @param ssv Which \code{ssv} have been used in the analysis? If \code{NULL}, it
#' will be assumed that \code{ssv = NULL} was passed to \code{\link{igate}} or \code{\link{categorical.igate}}
#' and all numeric variables in \code{df} will be used.
#' @param outlier_removal_target Was outlier removal conducted for \code{target}? If \code{type == "categorical"}
#' this is set to \code{FALSE} automatically.
#' @param outlier_removal_ssv Was outlier removal conducted for each \code{ssv}?
#' @param good_outcome Are \code{"low"} or \code{"high"} values of \code{target} good? Or, in
#' case of a categorical \code{target} the name of the best category as a string.
#' @param results_path Filepath to a .csv file containing results of \code{\link{igate}} or \code{\link{categorical.igate}}.
#' @param validation Logical. Has validation of the results been performed?
#' @param validation_path R object containing the validated observations, i.e. first data frame returned by \code{\link{validate}}.
#' @param validation_counts R object containing the counts from validation, i.e. the second data frame returned by \code{\link{validate}}.
#' @param validation_summary R object containing the summary of \code{validation_path}, i.e. the third data frame returned by \code{\link{validate}}.
#'
#' @examples
#' ## For continuous target
#' ## If you want to create a new igate from scratch, this is the last step
#' ## and relies on executing the other functions in this package first.
#' ## Run igate
#' results <- igate(iris, target = "Sepal.Length")
#' ## Produce regression plots
#' igate.regressions(iris, target = "Sepal.Length")
#' ## Validate findings and store results
#' x <- validate(iris, target = "Sepal.Length", causes = results$Causes, results_df = results)
#' ## Split above results
#' validatedObs <- x[[1]]
#' validationCounts <- x[[2]]
#' validationSummary <- x[[3]]
#' ## Create report
#' report(df = iris, target = "Sepal.Length", type = "continuous", good_outcome = "low",
#' results_path = "results", validation = TRUE, validation_path = "validatedObs",
#' validation_counts = "validationCounts", validation_summary = "validationSummary")
#'
#'
#' @export



report <- function(df,
                   versus = 8,
                   target = "Sepal.Length",
                   type = "continuous",
                   test = "w",
                   ssv = NULL,
                   outlier_removal_target = TRUE,
                   outlier_removal_ssv = TRUE,
                   good_outcome = "low",
                   results_path = "resultsIris",
                   validation = FALSE,
                   validation_path = "validatedObsIris",
                   validation_counts = "validationCountsIris",
                   validation_summary = "validationSummaryIris"){
  # If no ssv are provided, all the numeric columns have been used as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]%>%
      select(-contains("time"),
             -contains("visit"))
    ssv <- names(df_num)
    # This only works for continuous target. For categorical target would give empty vector.
    if(type=="continuous"){
      ssv <- ssv[-which( ssv == target)]
    }
  }
  if(type == "categorical"){outlier_removal_target <- FALSE}

  path_to_markdown <- system.file("rmd", "iGATE_Report.Rmd", package = "igate")
  image_directory <- getwd()
  print(image_directory)
  rmarkdown::render(path_to_markdown,
                    output_dir = getwd(),
                    params = list(
    df_name = deparse(substitute(df)),
    versus = versus,
    target = target,
    type = type,
    test = test,
    ssv = ssv,
    outlier_removal_target = outlier_removal_target,
    outlier_removal_ssv = outlier_removal_ssv,
    good_outcome = good_outcome,
    results_path = results_path,
    validation = validation,
    validation_path = validation_path,
    validation_counts = validation_counts,
    validation_summary = validation_summary,
    image_directory = image_directory
  ))

}


