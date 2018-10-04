library(readr)
library(readxl)
library(tidyverse)
library(stringr) #for replacing special characters in variable names
# GBPCA: Outlier removal and dynamic obs selection ------------------

#' gbpca function for continuous target variables
#'
#' This function performs an good/bad - pairwise comparison analysis on a dataset and returns those parameters found to be influential.
#' @param df Data frame to be analysed.
#' @param versus How many Best of the Best and Worst of the Worst do we collect? By default, we will collect 8 of each.
#' @param target Target varaible to be analysed. Must be continuous. Use \code{categorical.gbpca()} for categorical target.
#' @param test Statistical hypothesis test to be used to determine influential
#' process parameters. Choose between Wilcoxon Rank test (\code{"w"}, default) and Student's t-test (\code{"t"}).
#' @param ssv A vector of suspected sources of variation. These are the variables
#' in \code{df} which we believe might have an influence on the target variable and
#' will be tested. If no list of ssv is provided, the test will be performed
#' on all numeric variables.
#' @param outlier_removal_target Logical. Should outliers (with respect to the target variable)
#' be removed from df (default: \code{TRUE})? Important: This only makes sense if no
#' prior outlier removal has been performed on df, i.e. \code{df} still contains all
#' the data. Otherwise calculation for outlier threshold will be falsified.
#' @param outlier_removal_ssv Logical. Should outlier removal be performed for each ssv (default: \code{TRUE})?
#' @param good_end Are low (default) or high values of target variable good? This is needed
#' to determine the control bands.
#'
#'
#' @return A data frame with the following columns
#'\tabular{ll}{
#' \code{Causes} \tab Those ssv that have been found to be influential to the target variable.\cr
#' \code{Count} \tab The value returned by the counting method. \cr
#' \code{p.value} \tab The p-value of the hypothesis test performed, i.e. either of the
#' Wilcoxon rank test (in case \code{test = "w"}) or the t-test (if \code{test = "t"}).\cr
#' \code{good_lower_bound} \tab The lower bound for this \code{Cause} for good quality.\cr
#' \code{good_upper_bound} \tab The upper bound for this \code{Cause} for good quality.\cr
#' \code{bad_lower_bound} \tab The lower bound for this \code{Cause} for bad quality.\cr
#' \code{bad_upper_bound} \tab The upper bound for this \code{Cause} for bad quality.\cr
#' \code{na_removed} \tab How many missing values were in the data set for this \code{Cause}?\cr
#' \code{ties_lower_end} \tab Number of tied observations at lower end of \code{target} when selecting the
#' \code{versus} BOB/ WOW.\cr
#' \code{competition_lower_end} \tab For how many positions are the \code{tied_obs_lower} competing?\cr
#' \code{ties_upper_end} \tab Number of tied observations at upper end of \code{target} when selecting the
#' \code{versus} BOB/ WOW.\cr
#' \code{competition_upper_end} \tab For how many positions are the \code{tied_obs_upper} competing?\cr
#' \code{adjusted.p.values} \tab The \code{p.values} adjusted via Bonferroni correction.
#' }
#'
#'
#'
#' @details We collect the Best of the Best and the Worst of the Worst
#' dynamically dependent on the current ssv. That means, for each ssv we first
#' remove all the observations with missing values for that ssv from df.
#' Then, based on the remaining observations, we select versus observations with
#' the best values for the target variable (“Best of the Best”, short BOB)  and
#' versus observations with the worst values for the target variable
#' (“Worst of the Worst”, short WOW). By default, we select 8 of each.
#' Next, we compare BOB and WOW using the the counting method and the specified
#' hypothesis test. If the distributions of the ssv in BOB and WOW are
#' significantly different, the current ssv has been identified as influential
#' to the target variable. An ssv is considered influential, if the test returns
#' a count larger/ equal to 6 and/ or a p-value of less than 0.05.
#' For the next ssv we again start with the entire dataset df, remove all
#' the observations with missing values for that new ssv and then select our
#' new BOB and WOW. In particular, for each ssv we might select different observations.
#' This dynamic selection is necessary, because in case of an incomplete data set,
#' if we select the same BOB and WOW for all the ssv, we might end up with many
#' missing values for particular ssv. In that case the hypothesis test loses
#' statistical power, because it is used on a smaller sample or worse, might
#' fail altogether if the sample size gets too small.
#'
#' For those ssv determined to be significant, control bands are extracted. The rationale is:
#' If the value for an ssv is in the interval [\code{good_lower_bound},\code{good_upper_bound}]
#' the target is likely to be good. If it is in the interval
#' [\code{bad_lower_bound},\code{bad_upper_bound}], the target is likely to be bad.
#'
#' Furthermore some summary statistics are provided: When selecting the \code{versus} BOB/ WOW, tied values for target
#' can mean that the \code{versus} BOB/ WOW are not uniquely determined. In that case we randomly select
#' from the tied observations to give us exactly \code{versus} observations per group.
#' \code{ties_lower_end, cometition_lower_end, ties_upper_end, competition_upper_end}
#' quantify this randomness. How to interpret these values: \emph{lower end} refers to
#' the group whose \code{target} values are \emph{low} and \emph{upper end} to the one whose
#' \code{target} values are high. For example if a low value for \code{target} is good,
#' \emph{lower end} refers to the BOB and \emph{upper end} to the WOW. We determine the \code{versus}
#' BOB/ WOW via
#'
#' \code{lower_end <- df[min_rank(df$target)<=versus,]}
#'
#'If there are tied observations, \code{nrow(lower_end)} can be larger than \code{versus}. In \code{ties_lower_end} we
#'record how many observations in \code{lower_end$target} have the \emph{highest} value and in \code{competition_lower_end}
#'we record for how many places they are competing, i.e.
#'\code{competing_for_lower <- versus - (nrow(lower_end) - ties_lower_end)}.
#'The values for \code{ties_upper_end} and \code{competition_upper_end} are determined analogously.
#'
#'
#' @examples gbpca(iris, target = "Sepal.Length")
#'
#' @export
#'




gbpca <- function(df,
                             versus = 8,
                             target = "max17_343_36",
                             test = "w",
                             ssv = NULL,
                             outlier_removal_target = TRUE,
                             outlier_removal_ssv = TRUE,
                             good_end = "low"
){


# Preparations ------------------------------------------------------------

  # Remove columns with only missing values
  df <- df[,colSums(is.na(df)) < nrow(df)]

  # Remove outliers
  if(outlier_removal_target){
    box_stats <- boxplot.stats(df[[target]]) #we need this as extra variable tp keep track of removed records (we overwrite df)
    png(filename = paste0("Boxplot_of_", target,".png"),
         width = 573,
         height = 371)
    boxplot(df[[target]], xlab = target,
            main = paste0("Boxplot of ", target, ", containing ", length(box_stats$out), " outliers." ) )
    dev.off()
    df <- df%>%dplyr::filter(df[[target]] <= box_stats$stats[5])
    print(paste0(length(box_stats$out), " outliers have been removed."))
    print(paste0("Retaining ", length(df[[target]]), " observations."))

  }

  # If no ssv are provided, we take all the numeric columns as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]%>%
      select(-contains("time"),
             -contains("visit"))
    ssv <- names(df_num)
    ssv <- ssv[-which( ssv == target)]
  }
  #only keep data corresponding to ssv
  df_clean <- df[,which(names(df) %in% ssv)]


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
  tied_obs_lower <- rep(-1, l_ssv)
  competing_for_lower <- rep(-1, l_ssv)
  tied_obs_upper <- rep(-1, l_ssv)
  competing_for_upper <- rep(-1, l_ssv)

  # Which follow up test are we using?
  h.test <- function(x,y){
    if(test == "t"){t.test(x,y)}
    else if(test == "w"){wilcox.test(x,y)}
    else{print("Error: Unrecognized test")}
  }



# Analysis ----------------------------------------------------------------


  #Console output of which test we are using
  print(paste0("Using pairwise comparison with ", versus, " BOB vs. ", versus, " WOW."))
  print(paste0("Using counting method with ",
               if(test == "t"){"t test"}
               else if(test == "w"){"Wilcoxon rank test"}
               else{"ERROR: unrecognized test. Please use t or w as test argument."},
               " as follow up test."))


  # Dynamically select BOB and WOW
  for(i in 1:l_ssv){
    print(names(df_clean[i]))
    BOB.WOW_i <- data.frame(Big_Y = df[[target]], df_clean[,i])
    # Remove all missing records
    na_removed[i] <- sum(is.na(BOB.WOW_i[,2]))
    BOB.WOW_i <- BOB.WOW_i[!is.na(BOB.WOW_i[,2]),]
    #Outlier removal for ssv
    if(outlier_removal_ssv){
      box_stats <- boxplot.stats(unlist(BOB.WOW_i[,2]))
      BOB.WOW_i <- BOB.WOW_i[BOB.WOW_i[,2] >= box_stats$stats[1]
                             & BOB.WOW_i[,2] <= box_stats$stats[5],]
      #print(paste0("For ", ssv[i], " retain ", nrow(BOB.WOW_i), " obs after ssv based outlier removal."))
    }

    #Select versus obs at lower end and upper end
    lower_end <- BOB.WOW_i[min_rank(BOB.WOW_i$Big_Y)<=versus,]
    #reduce size of lower_end to versus
    if(nrow(lower_end) > versus){
        tied_value <- max(lower_end$Big_Y)
        tied_obs <- lower_end[lower_end$Big_Y == tied_value,]
        tied_obs_lower[i] <- nrow(tied_obs)
        #how many samples from tied_obs do we need?
        competing_for_lower[i] <- versus - (nrow(lower_end) - nrow(tied_obs)) #by def this is >0
        random_samples <- tied_obs[sample(1:nrow(tied_obs), competing_for_lower[i]),]
        lower_end <- rbind(lower_end[lower_end$Big_Y < tied_value,],
                       random_samples)
    }else{
      tied_obs_lower[i] <- 0
      competing_for_lower[i] <- 0
    }
    #Reduce the size of upper_end to versus
    upper_end <- BOB.WOW_i[min_rank(desc(BOB.WOW_i$Big_Y))<=versus,]
    if(nrow(upper_end) > versus){
      tied_value <- min(upper_end$Big_Y)
      tied_obs <- upper_end[upper_end$Big_Y == tied_value,]
      tied_obs_upper[i] <- nrow(tied_obs)
      #how many samples from tied_obs do we need?
      competing_for_upper[i] <- versus - (nrow(upper_end) - nrow(tied_obs)) #by def this is >0
      random_samples <- tied_obs[sample(1:nrow(tied_obs), competing_for_upper[i]),]
      upper_end <- rbind(upper_end[upper_end$Big_Y > tied_value,],
                     random_samples)
    }else{
      tied_obs_upper[i] <- 0
      competing_for_upper[i] <- 0
    }

    #assign BOB/ WOW
    if(good_end == "low"){
      BOB_i <- lower_end
      WOW_i <- upper_end
    }else if(good_end == "high"){
      BOB_i <- upper_end
      WOW_i <- lower_end
    }else{print("Ordering for target variable not correctly specified.")}


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
               error = function(e) {print(paste("Skip constant column", names(BOB.WOW_i)[2]));
               p_values[i] <-  -2})
    }
    }

    results <- data.frame(Causes = ssv,
                Count = test_results,
                p.values = p_values,
                good_lower_bound = good_band_lower_bound,
                good_upper_bound = good_band_upper_bound,
                bad_lower_bound = bad_band_lower_bound,
                bad_upper_bound = bad_band_upper_bound,
                na_removed = na_removed,
                ties_lower_end = tied_obs_lower,
                competition_lower_end = competing_for_lower,
                ties_upper_end = tied_obs_upper,
                competition_upper_end = competing_for_upper)%>%
        dplyr::filter(Count >= 6)%>%
        arrange(desc(Count))
    final.results <- data.frame(results,
                                adjusted.p.values = p.adjust(results$p.values))
    final.results

}

