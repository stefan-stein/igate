#' Generates report about a conducted gbpca.
#'
#' Takes results from a previous gbpca and automatically generates a html report
#' for it.
#'
#' @param df The data frame that was analysed with \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#' @param versus What value of \code{versus} was used?
#' @param target What \code{target} was used?
#' @param test Which hypothesis test was used alongside the counting method?
#' @param ssv Which \code{ssv} have been used in the analysis? If \code{NULL}, it
#' will be assumed that \code{ssv = NULL} was passed to \code{\link{gbpca}} or \code{\link{categorical.gbpca}}
#' and all numeric variables in \code{df} will be used.
#' @param outlier_removal_target Was outlier removal conducted for \code{target}?
#' @param outlier_removal_ssv Was outlier removal conducted for each \code{ssv}?
#' @param good_end Are \code{"low"} or \code{"high"} values of \code{target} good?
#' @param results_path Filepath to a .csv file containing results of \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#' @param validation Logical. Has validation of the results been performed?
#' @param validation_path Filepath to a .csv file containing the results validation.
#' @param validation_counts Filepath to a .csv file containing the counts from validation.
#'
#' @examples
#' ## If you want to create a new gbpca from scratch, this is the last step
#' ## and relies on executing the other functions in this package first.
#' ## Run gbpca
#' results <- gbpca(iris, target = "Sepal.Length")
#' ## Produce regression plots
#' gbpca.regressions(iris, target = "Sepal.Length")
#' ## Validate findings and store results
#' x <- validate(iris, target = "Sepal.Length", causes = results$Causes, results_df = results)
#' ## Split above results
#' validatedObs <- as.data.frame(x[1])
#' validationCounts <- as.data.frame(x[2])
#' ## Create report
#' report(df = iris, target = "Sepal.Length", results_path = "results",
#' validation = TRUE, validation_path = "validatedObs", validation_counts = "validationCounts")
#'
#' @export



report <- function(df,
                   versus = 8,
                   target = "Sepal.Length",
                   test = "w",
                   ssv = NULL,
                   outlier_removal_target = TRUE,
                   outlier_removal_ssv = TRUE,
                   good_end = "low",
                   results_path = "resultsIris",
                   validation = FALSE,
                   validation_path = "validatedObsIris",
                   validation_counts = "validationCountsIris"){
  # If no ssv are provided, all the numeric columns have been used as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]%>%
      select(-contains("time"),
             -contains("visit"))
    ssv <- names(df_num)
    ssv <- ssv[-which( ssv == target)]
  }
  path_to_markdown <- system.file("rmd", "Good_Bad_Report.Rmd", package = "gbpca")
  image_directory <- getwd()
  print(image_directory)
  rmarkdown::render(path_to_markdown,
                    output_dir = getwd(),
                    params = list(
    df_name = deparse(substitute(df)),
    versus = versus,
    target = target,
    test = test,
    ssv = ssv,
    outlier_removal_target = outlier_removal_target,
    outlier_removal_ssv = outlier_removal_ssv,
    good_end = good_end,
    results_path = results_path,
    validation = validation,
    validation_path = validation_path,
    validation_counts = validation_counts,
    image_directory = image_directory
  ))

}


