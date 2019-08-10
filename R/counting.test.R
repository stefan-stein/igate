##IMPORTANT NOTICE:
#This currently only works for complete datasets, if we have missing values the
#N/A columns will be sortet to the bottom of the dataset, falsifying the final count.




#' Performs the counting test
#'
#' This test is based on Tukey's "A Quick, Compact, Two-Sample Test to Duckworth's
#' Specifications", Technometrics, Vol. 1, No. 1 (1959), p.31-48. The test is chosen here
#' because of its easy interpretability.
#'
#' @param B,W Numeric vectors with best observations (\code{B}) and worst observations
#' (\code{W}).
#'
#' @return A data frame with the following columns
#' \tabular{ll}{
#' \code{count} \tab The count test statistic described in Tukey's paper, adjusted for tied observations.
#' The original test statistic as described originally in the paper need not exist in case
#' of tied observations, this implemantation remedies this.\cr
#' \code{good_band_lower_bound} \tab Lower bound for good observations (\code{B}).\cr
#' \code{good_band_upper_bound} \tab Upper bound for good observations (\code{B}).\cr
#' \code{bad_band_lower_bound} \tab Lower bound for bad observations (\code{W}).\cr
#' \code{bad_band_upper_bound} \tab Upper bound for bad observations (\code{W}).
#' }
#'
#' @details We form \code{rbind(B,W)} and order it. If \code{B} and \code{W}
#' differ significantly, ordering \code{rbind(B,W)} will find observations of one
#' group at the top and observations of the other at the bottom. We then count how
#' many observations of one group are at the top and how many of the other are at the
#' bottom. The sum of the two values gives us the \code{count} test statistic.
#' A critical value of \code{count >= 6} correponds to a p-value of roughly 0.05
#' and is independent of sample size and distributional assumptions.
#' These clustered observations at the top and bottom of the ordered list also
#' determine the control bands \code{good_band_lower_bound},
#' \code{good_band_upper_bound},\code{bad_band_lower_bound},
#' \code{bad_band_upper_bound}: We look if observations from group \code{B}
#' are at the top or bottom. The highest/ lowest values for observations of group \code{B}
#' within that cluser are \code{good_band_lower_bound} and
#' \code{good_band_upper_bound}. We proceed with group \code{W} respectively. If
#' no such clusters form at the end of the ordered list, the control bands are
#' set to -1.
#'
#' @examples
#' B <- 1:8
#' W <- 5:12
#' counting.test(B,W)
#'
#'
#'
#' @export
#'
counting.test <- function(B,W){
  #How many observations do we have?
  n_observations <- length(unlist(B)) + length(unlist(W))

  B_df <- data.frame(target = B, label = "B")
  W_df <- data.frame(target = W, label = "W")
  BOB_WOW <- rbind(B_df, W_df)
  #Order in ASCENDING order of target
  BOB_WOW_ordered <- BOB_WOW[order(BOB_WOW$target),]
  #Check, if first and last entry in ordered set are from the same group
  if(BOB_WOW_ordered[1,2] == BOB_WOW_ordered[n_observations,2]){
    data.frame(count = 0,
               good_band_lower_bound = -1,
               good_band_upper_bound = -1,
               bad_band_lower_bound = -1,
               bad_band_upper_bound = -1)
  }
  #If they are not, perform the counting method
  else{
    #Make sure all records with label == label are at top
    label <- BOB_WOW_ordered[1,2]
    BOB_WOW_ordered <- within(BOB_WOW_ordered,
                              label <- factor(label,
                                              levels = c(as.character(BOB_WOW_ordered[1,2]),
                                                         as.character(BOB_WOW_ordered[n_observations,2]))))
    BOB_WOW_ordered <- BOB_WOW_ordered[order(BOB_WOW_ordered$target, BOB_WOW_ordered$label),]

    #The group at the top of the list (lower values)
    top_count <- 0
    i <- 1
    while(BOB_WOW_ordered[i,2]==label){
      top_count <- top_count + 1
      i <- i + 1
    }
    #Handling ties
    if(BOB_WOW_ordered[i,1] == BOB_WOW_ordered[i-1,1]){

      tied_obs <- BOB_WOW_ordered[BOB_WOW_ordered$target == BOB_WOW_ordered[i,1],]
      #if only two obs are tied, take average of counts,
      #if more than two are tied, treat as from other group.
      # - 0.5 because of the way we ordered: labels coresspunding to top lable are above other lable
      if(nrow(tied_obs) == 2){top_count <- top_count - 0.5}
      #count again until we reach the threshold
      else{
        threshold <- BOB_WOW_ordered[i,1]
        top_count <- 0
        i <- 1
        while(BOB_WOW_ordered[i,1] < threshold){
          top_count <- top_count + 1
          i <- i + 1
        }
      }
    }

    # Set bands for top group
    #If top_count < 1, we can switch ob at top such that at top and bottom of
    #the list we have obs from the same group. In that case the total count is 0
    #and we are done.
    if(top_count < 1){
      data.frame(count = 0,
                 good_band_lower_bound = -1,
                 good_band_upper_bound = -1,
                 bad_band_lower_bound = -1,
                 bad_band_upper_bound = -1)
    }else{
      top_group_lower_bound <- BOB_WOW_ordered[1,1]
      top_group_upper_bound <- BOB_WOW_ordered[ceiling(top_count),1]

    #The group at the bottom of the list

    label <- BOB_WOW_ordered[n_observations,2]
    bottom_count <- 0
    i <- n_observations
    while(BOB_WOW_ordered[i,2]==label){
      bottom_count <- bottom_count + 1
      i <- i - 1
    }
    #Handling ties
    if(BOB_WOW_ordered[i,1] == BOB_WOW_ordered[i+1,1]){

      tied_obs <- BOB_WOW_ordered[BOB_WOW_ordered$target == BOB_WOW_ordered[i,1],]
      #if only two obs are tied, take average of counts, if more than two are tied, treat as from other group
      if(nrow(tied_obs) == 2){bottom_count <- bottom_count - 0.5}
      else{
        threshold <- BOB_WOW_ordered[i+1,1]
        bottom_count <- 0
        i <- n_observations
        while(BOB_WOW_ordered[i,1] > threshold){
          bottom_count <- bottom_count + 1
          i <- i - 1
        }
      }
    }

    #If bottom_count < 1, we can switch ob at bottom such that at top and bottom of
    #the list we have obs from the same group. In that case the total count is 0
    #and we are done.
    if(bottom_count < 1){
      data.frame(count = 0,
                 good_band_lower_bound = -1,
                 good_band_upper_bound = -1,
                 bad_band_lower_bound = -1,
                 bad_band_upper_bound = -1)
    }else{
      bottom_group_lower_bound <- BOB_WOW_ordered[n_observations - ceiling(bottom_count) + 1,1]
      bottom_group_upper_bound <- BOB_WOW_ordered[n_observations,1]


    #assign bounds to good/ baad
    if(BOB_WOW_ordered[1,2] == "B"){
      good_band_lower_bound <- top_group_lower_bound
      good_band_upper_bound <- top_group_upper_bound
      bad_band_lower_bound <- bottom_group_lower_bound
      bad_band_upper_bound <- bottom_group_upper_bound
    }else{
      good_band_lower_bound <- bottom_group_lower_bound
      good_band_upper_bound <- bottom_group_upper_bound
      bad_band_lower_bound <- top_group_lower_bound
      bad_band_upper_bound <- top_group_upper_bound
    }

    #Return overall count
    count <- top_count + bottom_count
    data.frame(count = count,
               good_band_lower_bound = good_band_lower_bound,
               good_band_upper_bound = good_band_upper_bound,
               bad_band_lower_bound = bad_band_lower_bound,
               bad_band_upper_bound = bad_band_upper_bound)
  } #else in line 108
  } #else in line 69
  } #else in line 23
}
