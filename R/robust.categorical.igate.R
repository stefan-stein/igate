# iGATE for categorical variables -----------------------------------------

# Outlier removal for categorical target makes no sense

#' Robust igate for categorical target variables
#'
#' This function performs a robust an initial Guided Analysis for parameter testing and
#' controlband extraction (iGATE) for a categorical target variable by repeatedly running
#' \code{\link{categorical.igate}} and only returning those parameters that are selected more often than a
#' certain threshold.
#'
#' @param df Data frame to be analysed.
#' @param versus How many Best of the Best and Worst of the Worst do we collect? By default, we will collect 8 of each.
#' @param target Target variable to be analysed. Must be categorical.
#' Use \code{\link{igate}} for continuous \code{target}.
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
#' @param iterations Integer. How often should categorical.igate be performed? A message about how many iterations
#' have been perfermed so far will be printed to the console every \code{0.1*iterations} iterations.
#' @param threshold Between 0 and 1. Only parameters that are selected at least \code{floor(iterations*threshold)}
#' times are returned.
#'
#'
#' @return A list with two elements. The first element is named \code{aggregated_results}:
#' A data frame with the summary statistics for those parameters that were selected
#' at least \code{floor(iterations*threshold)} times:
#'\tabular{ll}{
#' \code{Causes} \tab Those \code{ssv} that have been found to be influential to the \code{target} variable.\cr
#' \code{rel_frequency} \tab Relative frequency of how often this \code{Cause} was selected, i.e.
#' \code{(number of times it was selected) / iterations} \cr
#' \code{median_count} \tab The median value returned by the counting method for this parameter. \cr
#' \code{median_p_value} \tab The median p-value of the hypothesis test performed, i.e. either of the
#' Wilcoxon rank test (in case \code{test = "w"}) or the t-test (if \code{test = "t"}).\cr
#' \code{median_good_lower_bound} \tab The median lower bound for this \code{Cause} for good quality.\cr
#' \code{median_good_upper_bound} \tab The median upper bound for this \code{Cause} for good quality.\cr
#' \code{median_bad_lower_bound} \tab The median lower bound for this \code{Cause} for bad quality.\cr
#' \code{median_bad_upper_bound} \tab The median upper bound for this \code{Cause} for bad quality.
#' }
#'
#' The second element is a list of \code{iterations} data frames named \code{individual_runs}, containing
#' the raw results from each individual run of \code{\link{categorical.igate}}. This can be useful if one is
#' interested in more than only the summary statistics returned in \code{aggregated_results}.
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
#' happens randomly, it is recommended to use \code{robust.categorical.igate} over \code{\link{categorical.igate}}.
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
#' This process is repeated \code{iterations} times and only those \code{ssv} that are selected in
#' at least \code{floor(iterations * threshold)} times are returned in the final output.
#'
#' @examples
#'
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' results <- robust.categorical.igate(mtcars, target = "cyl",
#' best.cat = "8", worst.cat = "4", iterations = 50, threshold = 0.5)
#'
#' # To get the aggregated results
#' results$aggregated_results
#'
#'
#' @export
#'
#' @importFrom stats median
#' @importFrom utils capture.output
#' @importFrom dplyr select filter contains arrange desc %>% group_by data_frame n summarise bind_rows left_join everything
#'



robust.categorical.igate <- function(df,
                                     versus = 8,
                                     target,
                                     best.cat,
                                     worst.cat,
                                     test = "w",
                                     ssv = NULL,
                                     outlier_removal_ssv = TRUE,
                                     iterations = 50,
                                     threshold = 0.5){

  if(sum(names(df) == target) != 1){
    stop(paste0(target,
                " is not a valid column name for ",
                deparse(substitute(df)),
                ".\nGot sum(names(df) == target) = ", sum(names(df) == target),
                ", but need 1."))
  }
  if(!(test == "w" || test == "t")){
    stop(paste0(test,
                " is not a valid hypothesis test. See documentation (?robust.categorical.igate)"))
  }

  # These two lines are only here to appease R CMD check
  Causes <- rel_frequency <- Count <- p.values <- median_count <- NULL
  good_lower_bound <- good_upper_bound <- bad_lower_bound <- bad_upper_bound <- NULL

  results <- vector("list", length = iterations)
  for (i in 1:iterations) {
    if(i %% floor(0.1*iterations) == 0){message(paste0("Iteration ", i))}
    # To suppress console output by categorical.igate
    suppressMessages(res <- categorical.igate(df, versus, target, best.cat, worst.cat, test, ssv, outlier_removal_ssv, count_critical_value = 0))

    results[[i]] <- res
    rm(res)
  }


  freq <- results%>%bind_rows()%>%
    filter(Count >= 6)%>%
    group_by(Causes)%>%
    summarise(rel_frequency = n() / iterations)

  results_aggregated <- results%>%
    bind_rows()%>%
    group_by(Causes)%>%
    summarise(median_count = median(Count),
              median_p_value = median(p.values),
              median_good_lower_bound = median(good_lower_bound),
              median_good_upper_bound = median(good_upper_bound),
              median_bad_lower_bound = median(bad_lower_bound),
              median_bad_upper_bound = median(bad_upper_bound))%>%
    dplyr::left_join(freq, by = "Causes")%>%
    select(Causes, rel_frequency, everything())%>%
    filter(rel_frequency >= threshold)%>%
    arrange(desc(rel_frequency), desc(median_count))

  # results_aggregated <- results%>%
  #   bind_rows()%>%
  #   group_by(Causes)%>%
  #   summarise(rel_frequency = n() / iterations,
  #             median_count = median(Count),
  #             median_p_value = median(p.values),
  #             median_good_lower_bound = median(good_lower_bound),
  #             median_good_upper_bound = median(good_upper_bound),
  #             median_bad_lower_bound = median(bad_lower_bound),
  #             median_bad_upper_bound = median(bad_upper_bound))%>%
  #   filter(rel_frequency >= threshold)%>%
  #   arrange(desc(rel_frequency), desc(median_count))

  # return(results_aggregated)
  return(list(aggregated_results = results_aggregated,
              individual_runs = results))
}
