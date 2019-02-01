# Validation function after performing gbpca



#' Validates results after using \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#'
#' Takes a new data frame to be used for validation and the causes and control bands
#' obtained from \code{\link{gbpca}} or \code{\link{categorical.gbpca}} and returns
#' all those observations that fall within these control bands.
#'
#' @param validation_df Data frame to be used for validation. It is recommended to use
#' a different data frame from the one used in \code{\link{gbpca}}/ \code{\link{categorical.gbpca}}.
#' The same data frame can be used if just a sanity check of the results is performed. This
#' data frame must contain the \code{target} variable as well as all the causes determined
#' by \code{\link{gbpca}}/ \code{\link{categorical.gbpca}}.
#' @param target Target variable that was used in \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#' @param causes Causes determined by \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#' If you saved the results of \code{\link{gbpca}}/ \code{\link{categorical.gbpca}} in an object
#' \code{results}, simply use \code{results$Causes} here.
#' @param results_df The data frame containing the results of \code{\link{gbpca}} or \code{\link{categorical.gbpca}}.
#'
#' @return A list of two data frames is returned. The first data frame contains those observations
#' in \code{validation_df} that fall into *all* the good resp. bad control bands specified in \code{results_df}.
#' The columns are \code{target}, then one column for each of the \code{causes} and a new column
#' \code{expected_quality} which is \code{"good"} if the observations falls into all the good
#' control bands and \code{"bad"} if it falls into all the bad control bands.
#'
#' The second data frame has three columns
#' \tabular{ll}{
#' \code{Cause} \tab Each of the \code{causes}.\cr
#' \code{Good_Count} \tab If we selected all those observations that fall into the good band
#' of this cause, how many observations would we select?\cr
#' \code{Bad_Count} \tab If we selected all those observations that fall into the bad band
#' of this cause, how many observations would we select?
#' }
#'
#' @details If a value of \code{Good_Count} or \code{Bad_count} is very low in the second
#' data frame, it means that this cause is excluding a lot of observations from the
#' first data frame. Consider re-running \code{validate} with this cause removed from
#' \code{causes}.
#'
#' @examples validate(iris, target = "Sepal.Length", causes = resultsIris$Causes, results_df = resultsIris)
#'
#' @export
#'
#' @importFrom dplyr mutate select


validate <- function(validation_df,
                     target,
                     causes,
                     results_df){
  columns <- c(target, as.character(causes))
  relevant_columns <- validation_df%>%select(c(target, as.character(causes)))
  number_columns <- ncol(relevant_columns)
  number_rows <- nrow(relevant_columns)
  #Count how many good/ bad obs per cause. First entry is meaningless
  #(this is the columns with target), but we keep it like this to make indexing easier in for-loop below
  count_good_obs <- rep(-1,(number_columns))
  count_bad_obs <- rep(-1,(number_columns))
  #we start with all obs and keep intersecting with obs falling into good/bad bands of
  #each cause
  good_obs <- 1:number_rows
  bad_obs <- 1:number_rows
  #start at 2, because first column is target
  for (i in 2:number_columns) {
    #what row in results_df corresponds to the current cause?
    what_row <- which(names(relevant_columns)[i] == results_df[,1])
    #extract good/ bad bands
    good_bands <- unlist(results_df[what_row, 4:5])
    bad_bands <- unlist(results_df[what_row, 6:7])
    good_obs_i <- which(relevant_columns[,i] >= good_bands[1]
                        & relevant_columns[,i] <= good_bands[2])
    count_good_obs[i] <- length(good_obs_i)
    good_obs <- intersect(good_obs,good_obs_i)
    bad_obs_i <- which(relevant_columns[,i] >= bad_bands[1]
                        & relevant_columns[,i] <= bad_bands[2])
    count_bad_obs[i] <- length(bad_obs_i)
    bad_obs <- intersect(bad_obs,bad_obs_i)
  }
  good_validated_obs <- relevant_columns[good_obs,]%>%
    mutate(expected_quality = "good")
  bad_validated_obs <- relevant_columns[bad_obs,]%>%
    mutate(expected_quality = "bad")
  validated_obs <- rbind(good_validated_obs, bad_validated_obs)
  validation_counts <- data.frame(Cause = causes,
                                  Good_Count = count_good_obs[2:number_columns],
                                  Bad_count = count_bad_obs[2:number_columns])

  return(list(validated_obs, validation_counts))

}
