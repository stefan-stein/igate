# Validation function after performing igate



#' Validates results after using \code{\link{igate}} or \code{\link{categorical.igate}}.
#'
#' Takes a new data frame to be used for validation and the causes and control bands
#' obtained from \code{\link{igate}} or \code{\link{categorical.igate}} and returns
#' all those observations that fall within these control bands.
#'
#' @param validation_df Data frame to be used for validation. It is recommended to use
#' a different data frame from the one used in \code{\link{igate}}/ \code{\link{categorical.igate}}.
#' The same data frame can be used if just a sanity check of the results is performed. This
#' data frame must contain the \code{target} variable as well as all the causes determined
#' by \code{\link{igate}}/ \code{\link{categorical.igate}}.
#' @param target Target variable that was used in \code{\link{igate}} or \code{\link{categorical.igate}}.
#' @param causes Causes determined by \code{\link{igate}} or \code{\link{categorical.igate}}.
#' If you saved the results of \code{\link{igate}}/ \code{\link{categorical.igate}} in an object
#' \code{results}, simply use \code{results$Causes} here.
#' @param results_df The data frame containing the results of \code{\link{igate}} or \code{\link{categorical.igate}}.
#' @param type The type of igate that was performed: either \code{"continuous"} or \code{"categorical"}. If not provided
#' function will try to guess the correct type based on the type of \code{validation_df[[target]]}.
#'
#' @return A list of three data frames is returned. The first data frame contains those observations
#' in \code{validation_df} that fall into *all* the good resp. bad control bands specified in \code{results_df}.
#' The columns are \code{target}, then one column for each of the \code{causes} and a new column
#' \code{expected_quality} which is \code{"good"} if the observation falls into all the good
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
#' The third data frame summarizes the first data frame: If \code{type = "continuous"} it has
#' three columns:
#' \tabular{ll}{
#' \code{expected_quality} \tab Either \code{"good"} or \code{"bad"}.\cr
#' \code{max_target} \tab The maximum value for \code{target} for the observations with "good"
#' expected quality resp. "bad" expected quality. \cr
#' \code{min_target} \tab Minimum value of \code{target} for good resp. bad expected quality.
#' }
#' If \code{type = "categorical"} it has the following three columns:
#' \tabular{ll}{
#' \code{expected_quality} \tab Either \code{"good"} or \code{"bad"}.\cr
#' \code{Category} \tab A list of categories of the observations with expected quality good resp. bad. \cr
#' \code{Frequency} \tab A count how often the respective \code{Category} appears amongs the observations with
#' good/ bad expected quality.
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
#' @importFrom dplyr mutate select filter


validate <- function(validation_df,
                     target,
                     causes,
                     results_df,
                     type = NULL){
  if(is.null(type)){
    if(is.numeric(validation_df[[target]]) && !is.factor(validation_df[[target]])){
      print("Guessing that perfromed igate was continuous. Using type = 'continuous'.")
      type <- "continuous"
    }
    else if(is.character(validation_df[[target]]) || is.factor(validation_df[[target]])){
      print("Guessing that perfromed igate was categorical. Using type = 'categorical'.")
      type <- "categorical"
    }
    else{
      print("Sorry, was not able to guess type of igate. Please specify type as either 'continuous' or 'categorical'.")
    }
  }
  # Check that type of target and specified type fit
  if((type == "continuous") && (is.character(validation_df[[target]]) || is.factor(validation_df[[target]]))){
    stop("Type was specified as continuous, but validation_df[[target]] is character or factor.")
  }
  if((type == "categorical") && (is.numeric(validation_df[[target]]) && !is.factor(validation_df[[target]]))){
    stop("Type was specified as categorical, but validation_df[[target]] is numeric and not a factor.")
  }
  if((type != "categorical") && (type != "continuous")){
    stop("Invalid type. Please use either 'categorical' or 'continuous'.")
  }
  # This is only here to appease R CMD check
  expected_quality <- NULL

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

  if(type == "continuous"){
    good <- validated_obs%>%filter(expected_quality=="good")
    bad <- validated_obs%>%filter(expected_quality=="bad")
    validation_summary <- data_frame(expected_quality = c("good", "bad"),
                                     max_target = c(max(good[[target]]), max(bad[[target]])),
                                     min_target = c(min(good[[target]]), min(bad[[target]])))
  }
  if(type == "categorical"){
    good_table <- validated_obs%>%filter(expected_quality == "good")
    bad_table <- validated_obs%>%filter(expected_quality == "bad")
    good_table <- table(good_table[[target]])
    bad_table <- table(bad_table[[target]])

    validation_summary <- data_frame(expected_quality = c(rep("good", length(good_table)),
                                                          rep("bad", length(bad_table))),
                                     Category = c(as.data.frame(good_table, stringsAsFactors = F)$Var1, as.data.frame(bad_table, stringsAsFactors = F)$Var1),
                                     Frequency = c(as.data.frame(good_table, stringsAsFactors = F)$Freq, as.data.frame(bad_table, stringsAsFactors = F)$Freq))
  }

  return(list(validated_obs, validation_counts, validation_summary))

}
