
# iGATE for categorical variables -----------------------------------------

# Outlier removal for categorical target makes no sense

#' igate function for categorical target variables
#'
#' This function performs an initial Guided Analysis for parameter testing and controlband extraction (iGATE) for
#' a categorical target variable on a dataset and returns those parameters found to be influential.
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
#'
#'
#' @return A data frame with the following columns
#'\tabular{ll}{
#' \code{Causes} \tab Those \code{ssv} that have been found to be influential to the \code{target} variable.\cr
#' \code{Count} \tab The value returned by the counting method. \cr
#' \code{p.value} \tab The p-value of the hypothesis test performed, i.e. either of the
#' Wilcoxon rank test (in case \code{test = "w"}) or the t-test (if \code{test = "t"}).\cr
#' \code{good_lower_bound} \tab The lower bound for this \code{Cause} for good quality.\cr
#' \code{good_upper_bound} \tab The upper bound for this \code{Cause} for good quality.\cr
#' \code{bad_lower_bound} \tab The lower bound for this \code{Cause} for bad quality.\cr
#' \code{bad_upper_bound} \tab The upper bound for this \code{Cause} for bad quality.\cr
#' \code{na_removed} \tab How many missing values were in the data set for this \code{Cause}?\cr
#' \code{ties_best_cat} \tab How many observations fall into the best category? \cr
#' \code{ties_worst_cat} \tab How many observations fall into the worst category?
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
#' (“Worst of the Worst”, short WOW). By default, we select 8 of each.
#' Next, we compare BOB and WOW using the the counting method and the specified
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
#' Furthermore some summary statistics are provided: \code{na_removed} tells us
#' how many observations have been removed for a particular \code{ssv}. When
#' selecting the \code{versus} BOB/ WOW, the selection is done randomly from within
#' the best/ worst category, i.e. the \code{versus} BOB/ WOW are not uniquely
#' determined. The randomness in the selection is quantified by \code{ties_best_cat,
#' ties_worst_cat}, which gives the size of the best/ worst category respectively.
#'
#' @examples categorical.igate(mtcars, target = "cyl", best.cat = "8", worst.cat = "4")
#'
#' @export
#'
#' @importFrom dplyr select filter contains arrange desc min_rank %>%
#' @importFrom grDevices boxplot.stats dev.off png
#' @importFrom graphics abline boxplot plot
#' @importFrom stats lm p.adjust t.test var wilcox.test
#'


categorical.igate <- function(df,
                             versus = 8,
                             target,
                             best.cat,
                             worst.cat,
                             test = "w",
                             ssv = NULL,
                             outlier_removal_ssv = TRUE){


  # Preparations ------------------------------------------------------------
  if(sum(names(df) == target) != 1){
    stop(paste0(target,
                " is not a valid column name for ",
                deparse(substitute(df)),
                ".\nGot sum(names(df) == target) = ", sum(names(df) == target),
                ", but need 1."))
  }
  if(!(test == "w" || test == "t")){
    stop(paste0(test,
                " is not a valid hypothesis test. See documentation (?categorical.igate)"))
  }
  # Remove columns with only missing values
  df <- df[,colSums(is.na(df)) < nrow(df)]


  #Only keep obs that are in best.cat and worst.cat
  df <- df%>%filter(df[[target]] %in% c(best.cat, worst.cat))


  # If no ssv are provided, we take all the numeric columns as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]
    ssv <- names(df_num)
  }
  #only keep data corresponding to ssv
  df_clean <- df[,which(names(df) %in% ssv)]
  # this is for windows version in case there is only one ssv
  if(is.vector(df_clean)){
    df_clean <- as.data.frame(df_clean)
    names(df_clean) <- ssv
  }

  # Summary vectors ---------------------------------------------------------

  l_ssv <- length(ssv)

  # These are going to be counts from the counting method and p-values from the follow up test
  test_results <- rep(-1, l_ssv)
  p_values <- rep(-1, l_ssv)
  # Good band and bad band bounds
  good_band_lower_bound <- rep(-1, l_ssv)
  good_band_upper_bound <- rep(-1, l_ssv)
  bad_band_lower_bound <- rep(-1, l_ssv)
  bad_band_upper_bound <- rep(-1, l_ssv)

  # Keep track of how many NAs we remove
  na_removed <- rep(-1, l_ssv)

  #Keep track out of how many ties we sampled the BOB/WOW
  tied_obs_best <- rep(-1, l_ssv)
  tied_obs_worst <- rep(-1, l_ssv)

  # Which follow up test are we using?
  h.test <- function(x,y){
    if(test == "t"){t.test(x,y)}
    else{wilcox.test(x,y)}
  }



  # Analysis ----------------------------------------------------------------


  #Console output of which test we are using
  message(paste0("Using pairwise comparison with ", versus, " BOB vs. ", versus, " WOW."))
  message(paste0("Using counting method with ",
               if(test == "t"){"t-test"}
               else if(test == "w"){"Wilcoxon rank test"}
               else{"ERROR: unrecognized test. Please use t or w as test argument."},
               " as follow up test."))


  # Dynamically select BOB and WOW
  for(i in 1:l_ssv){
    #we need to use df[[target]], because target is not in df_clean
    BOB.WOW_i <- data.frame(Big_Y = df[[target]], df_clean[,i])
    # Remove all missing records
    na_removed[i] <- sum(is.na(BOB.WOW_i[,2]))
    BOB.WOW_i <- BOB.WOW_i[!is.na(BOB.WOW_i[,2]),]
    if(nrow(BOB.WOW_i) < 2*versus){
      message(paste("Not enough non-missing values for", ssv[i]))
      next
    }
    #Outlier removal for ssv
    if(outlier_removal_ssv){
      box_stats <- boxplot.stats(unlist(BOB.WOW_i[,2]))
      BOB.WOW_i <- BOB.WOW_i[BOB.WOW_i[,2] >= box_stats$stats[1]
                             & BOB.WOW_i[,2] <= box_stats$stats[5],]
    }

    #Select versus obs at lower end and upper end
    obs.best.cat <- BOB.WOW_i[BOB.WOW_i$Big_Y == best.cat,]
    tied_obs_best[i] <- nrow(obs.best.cat)
    if(tied_obs_best[i] < 8){
      message(paste("Not enough best cat obsevations for", names(BOB.WOW_i)[2]))
      test_results[i] <-  -2
      next
    }else{
      sample.best.cat <- obs.best.cat[sample(1:tied_obs_best[i], 8),]
    }

    #worst cat
    obs.worst.cat <- BOB.WOW_i[BOB.WOW_i$Big_Y == worst.cat,]
    tied_obs_worst[i] <- nrow(obs.worst.cat)
    if(tied_obs_worst[i] < 8){
      message(paste("Not enough worst cat obsevations for", names(BOB.WOW_i)[2]))
      test_results[i] <-  -2
      next
    }else{
      sample.worst.cat <- obs.worst.cat[sample(1:tied_obs_worst[i], 8),]
    }



      BOB_i <- sample.best.cat
      WOW_i <- sample.worst.cat

    # Testing
    ith_counting_test <- counting.test(BOB_i[,2], WOW_i[,2])
    test_results[i] <- ith_counting_test$count
    good_band_lower_bound[i] <- ith_counting_test$good_band_lower_bound
    good_band_upper_bound[i] <- ith_counting_test$good_band_upper_bound
    bad_band_lower_bound[i] <- ith_counting_test$bad_band_lower_bound
    bad_band_upper_bound[i] <- ith_counting_test$bad_band_upper_bound

    #follow up test if count is larger than 6
    if(test_results[i] >= 6){
      tryCatch(p_values[i] <- h.test(BOB_i[,2], WOW_i[,2])$p.value,
               error = function(e) {message(paste("Skip constant column", names(BOB.WOW_i)[2]));
                 p_values[i] <-  -2})
    }
  }
  #To please R CMD check
  Count <- NULL
  results <- data.frame(Causes = ssv,
                        Count = test_results,
                        p.values = p_values,
                        good_lower_bound = good_band_lower_bound,
                        good_upper_bound = good_band_upper_bound,
                        bad_lower_bound = bad_band_lower_bound,
                        bad_upper_bound = bad_band_upper_bound,
                        na_removed = na_removed,
                        ties_best_cat = tied_obs_best,
                        ties_worst_cat = tied_obs_worst)%>%
    filter(Count >= 6)%>%
    arrange(desc(Count))
  final.results <- data.frame(results,
                              adjusted.p.values = p.adjust(results$p.values))
  final.results

}


