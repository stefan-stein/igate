#' Generates report about a conducted igate.
#'
#' Takes results from a previous igate and automatically generates a html report
#' for it. Be aware that running this function will create an html document in your current working directory.
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
#' @param results_path Name of R object (as string) containing the results of \code{\link{igate}} or \code{\link{categorical.igate}}.
#' @param validation Logical. Has validation of the results been performed?
#' @param validation_path Name R object (as string) containing the validated observations, i.e. first data frame returned by \code{\link{validate}}.
#' @param validation_counts Name of R object (as string) containing the counts from validation, i.e. the second data frame returned by \code{\link{validate}}.
#' @param validation_summary Name of R object (as string) containing the summary of \code{validation_path}, i.e. the third data frame returned by \code{\link{validate}}.
#' @param image_directory Directory which contains the plots from \code{igate}, \code{igate.regressions} etc.
#' @param output_name Desired name of the output file. File extension .html will be added automatically if not supplied.
#' If \code{NULL} will be *iGATE_Report.html*.
#' @param output_directory Directory into which the report should be saved. To save to the current working directory,
#' use \code{output_directory = getwd()}.
#'
#' @return An html file named "iGATE_Report.html" will be output to the current working directory, containing details
#' about the conducted analysis. This includes a list of the analysed SSV, as well as tables with the results from
#' \code{\link{igate}}/ \code{\link{categorical.igate}} and plots from \code{\link{igate.regressions}}/
#' \code{\link{categorical.freqplot}}.
#'
#' @examples
#'
#' ## Example for categorical target variable
#' # If you want to conduct an igate analysis from scratch, running report
#' # is the last step and relies on executing the other functions in this package first.
#' # Run categorical.igate
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' results <- categorical.igate(df, target = "cyl", best.cat = "8", worst.cat = "4")
#' # Produce density plots
#' # Suppose you only want to analyse further the first three identified ssv
#' results <- results[1:3,]
#' categorical.freqplot(mtcars, target = "cyl", ssv = results$Causes , savePlots = TRUE)
#'
#' report(df = df, target = "cyl", type = "categorical", good_outcome = "8",
#' results_path = "results",
#' output_name = "testing_igate", output_directory = tempdir())
#'
#'
#' @importFrom rmarkdown render pandoc_available
#'
#' @export



report <- function(df,
                   versus = 8,
                   target,
                   type = "continuous",
                   test = "w",
                   ssv = NULL,
                   outlier_removal_target = TRUE,
                   outlier_removal_ssv = TRUE,
                   good_outcome = "low",
                   results_path,
                   validation = FALSE,
                   validation_path = NULL,
                   validation_counts = NULL,
                   validation_summary = NULL,
                   image_directory = tempdir(),
                   output_name = NULL,
                   output_directory){

  # Check if pandoc is available
  required_pandoc <- "1.12.3"
  if(!rmarkdown::pandoc_available(required_pandoc)){
    message(paste0("Pandoc version >=", required_pandoc, " is required but was not found
                   (see the help page ?rmarkdown::pandoc_available). Please install pandoc."))
  }else{

    if(missing(output_directory)){
      stop("Please specify output directory.")
    }
    if(!validation && (!is.null(validation_path) || !is.null(validation_counts) || !is.null(validation_summary))){
      stop("You used validation = FALSE, but provided at least one of validation_path, validation_counts or validation_summary.
         Please provide all three of them and use validation = TRUE.")
    }
    if(validation && (is.null(validation_path) || is.null(validation_counts) || is.null(validation_summary))){
      stop("You used validation = TRUE, but did not provide all of validation_path, validation_counts and validation_summary.")
    }

    # If no ssv are provided, all the numeric columns have been used as ssv
    if(is.null(ssv)){
      nums <- sapply(df, is.numeric)
      df_num <- df[,nums]%>%
        select(-contains("time"),
               -contains("visit"))
      ssv <- names(df_num)
      # This only works for continuous target. For categorical target would give empty vector.
      if(type=="continuous"){
        ssv <- ssv[-which(ssv == target)]
      }
    }
    if(type == "categorical"){outlier_removal_target <- FALSE}

    path_to_markdown <- system.file("rmd", "iGATE_Report.Rmd", package = "igate")

    rmarkdown::render(path_to_markdown,
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
                      ),
                      output_file = output_name,
                      output_dir = output_directory,
                      intermediates_dir = tempdir(),
                      knit_root_dir = tempdir())

  }

}


