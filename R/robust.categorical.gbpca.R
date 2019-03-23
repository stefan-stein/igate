# GBPCA for categorical variables -----------------------------------------

# Outlier removal for categorical target makes no sense

#' Robust gbpca for categorical target variables
#'
#' This function performs a robust good/bad - pairwise comparison analysis for a categorical target variable
#' by repeatedly running \code{categorical.gbpca} and only returning those parameters that are selected more often than a
#' certain threshold.
#'
#' @param df Data frame to be analysed.
#' @param versus How many Best of the Best and Worst of the Worst do we collect? By default, we will collect 8 of each.
#' @param target Target variable to be analysed. Must be categorical.
#' Use \code{\link{gbpca}} for continuous \code{target}.
#' @param best.cat The best category. The \code{versus} BOB will be selected randomly from this
#' category.
#' @param worst.cat The worst category. The \code{versus} WOW will be selected randomly from this
#' category.
#' @param test Statistical hypothesis test to be used to determine influential
#' process parameters. Choose between Wilcoxon Rank test (\code{"w"}, default)
#' and Student's t-test (\code{"t"}).
#' @param ssv A vector of suspected sources of variation. These are the variables
#' in \code{df} which we believe might have an influence on the \code{target} variable and
#' will be tested. If no list of \code{ssv} is provided, the test will be performed
#' on all numeric variables.
#' @param outlier_removal_ssv Logical. Should outlier removal be performed for each \code{ssv} (default: \code{TRUE})?
#' @param iterations Integer. How often should categorical.gbpca be performed?
#' @param threshold Between 0 and 1. Only parameters that are selected at least \code{floor(iterations*threshold)}
#' times are returned.
#'
#'
#' @return A data frame with the summary statistics for those parameters that were selected
#' at least \code{floor(iterations*threshold)} times:
#'\tabular{ll}{
#' \code{Causes} \tab Those \code{ssv} that have been found to be influential to the \code{target} variable.\cr
#' \code{median_count} \tab The median value returned by the counting method for this parameter. \cr
#' \code{median_p_value} \tab The median p-value of the hypothesis test performed, i.e. either of the
#' Wilcoxon rank test (in case \code{test = "w"}) or the t-test (if \code{test = "t"}).\cr
#' \code{median_good_lower_bound} \tab The median lower bound for this \code{Cause} for good quality.\cr
#' \code{median_good_upper_bound} \tab The median upper bound for this \code{Cause} for good quality.\cr
#' \code{median_bad_lower_bound} \tab The median lower bound for this \code{Cause} for bad quality.\cr
#' \code{median_bad_upper_bound} \tab The median upper bound for this \code{Cause} for bad quality.
#' }
#'
#'
#'
#' @details We collect the Best of the Best and the Worst of the Worst
#' dynamically dependent on the current \code{ssv}. That means, for each \code{ssv} we first
#' remove all the observations with missing values for that \code{ssv} from \code{df}.
#' Then, based on the remaining observations, we randomly select \code{versus}
#' observations from the the best category (“Best of the Best”, short BOB)  and
#' \code{versus} observations from the worst category
#' (“Worst of the Worst”, short WOW). By default, we select 8 of each. Since this selection
#' happens randomly, it is recommended to use \code{robust.categorical.gbpca} over \code{\link{categorical.gbpca}}.
#' After the selection we compare BOB and WOW using the the counting method and the specified
#' hypothesis test. If the distributions of the \code{ssv} in BOB and WOW are
#' significantly different, the current \code{ssv} has been identified as influential
#' to the \code{target} variable. An \code{ssv} is considered influential, if the test returns
#' a count larger/ equal to 6 and/ or a p-value of less than 0.05.
#' For the next \code{ssv} we again start with the entire dataset \code{df}, remove all
#' the observations with missing values for that new \code{ssv} and then select our
#' new BOB and WOW. In particular, for each \code{ssv} we might select different observations.
#' This dynamic selection is necessary, because in case of an incomplete data set,
#' if we select the same BOB and WOW for all the \code{ssv}, we might end up with many
#' missing values for particular \code{ssv}. In that case the hypothesis test loses
#' statistical power, because it is used on a smaller sample or worse, might
#' fail altogether if the sample size gets too small.
#'
#' For those \code{ssv} determined to be significant, control bands are extracted. The rationale is:
#' If the value for an \code{ssv} is in the interval [\code{good_lower_bound},\code{good_upper_bound}]
#' the \code{target} is likely to be good. If it is in the interval
#' [\code{bad_lower_bound},\code{bad_upper_bound}], the \code{target} is likely to be bad.
#'
#' @examples robust.categorical.gbpca(mtcars, target = "cyl",
#' best.cat = "8", worst.cat = "4", iterations = 50, threshold = 0.5)
#'
#' @export
#'
#' @importFrom stats median
#' @importFrom utils capture.output
#' @importFrom dplyr select filter contains arrange desc %>% group_by data_frame n summarise
#'



robust.categorical.gbpca <- function(df,
                                     versus = 8,
                                     target = "cyl",
                                     best.cat = "8",
                                     worst.cat = "4",
                                     test = "w",
                                     ssv = NULL,
                                     outlier_removal_ssv = TRUE,
                                     iterations = 50,
                                     threshold = 0.5){

  results <- data_frame()
  for (i in 1:iterations) {
    if(i %% floor(0.1*iterations) == 0){print(paste0("Iteration ", i))}
    # To suppress console output by categorical.gbpca
    dummy <- capture.output(res <- categorical.gbpca(df, versus, target, best.cat, worst.cat, test, ssv, outlier_removal_ssv))

    results <- rbind(results, res)
    rm(dummy, res)
  }
  y <- results%>%
    group_by(Causes)%>%
    summarise(Frequency = n())%>%
    filter(Frequency>=floor(iterations*threshold))
  results_aggregated <- results%>%
    filter(Causes %in% (y$Causes))%>%
    group_by(Causes)%>%
    summarise(median_count = median(Count),
              median_p_value = median(p.values),
              median_good_lower_bound = median(good_lower_bound),
              median_good_upper_bound = median(good_upper_bound),
              median_bad_lower_bound = median(bad_lower_bound),
              median_bad_upper_bound = median(bad_upper_bound))%>%
    arrange(desc(median_count))

  return(results_aggregated)
}
