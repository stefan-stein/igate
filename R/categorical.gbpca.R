
# GBPCA for categorical variables -----------------------------------------

# Outlier removal for categorical target makes no sense


library(readr)
library(readxl)
library(tidyverse)
library(stringr) #for replacing special characters in variable names

categorical.gbpca <- function(df,
                             versus = 8,
                             target = "gradecat_345_36",
                             best.cat = "HiB++",
                             worst.cat = "CGO",
                             test = "w",
                             ssv = NULL,
                             outlier_removal_ssv = TRUE,
                             good_end = "low"
){


  # Preparations ------------------------------------------------------------

  # Remove columns with only missing values
  df <- df[,colSums(is.na(df)) < nrow(df)]


  #Only keep obs that are in best.cat and worst.cat
  df <- df%>%filter(df[[target]] %in% c(best.cat, worst.cat))


  # If no ssv are provided, we take all the numeric columns as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]%>%
      select(-contains("time"),
             -contains("visit"))
    ssv <- names(df_num)
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
  tied_obs_best <- rep(-1, l_ssv)
  tied_obs_worst <- rep(-1, l_ssv)

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
    #we need to use df[[target]], because target is not in df_clean
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
    obs.best.cat <- BOB.WOW_i[BOB.WOW_i$Big_Y == best.cat,]
    tied_obs_best[i] <- nrow(obs.best.cat)
    if(tied_obs_best[i] < 8){
      print(paste("Not enough best cat obsevations for", names(BOB.WOW_i)[2]))
      test_results[i] <-  -2
      next
    }else{
      sample.best.cat <- obs.best.cat[sample(1:tied_obs_best[i], 8),]
    }

    #worst cat
    obs.worst.cat <- BOB.WOW_i[BOB.WOW_i$Big_Y == worst.cat,]
    tied_obs_worst[i] <- nrow(obs.worst.cat)
    if(tied_obs_worst[i] < 8){
      print(paste("Not enough best cat obsevations for", names(BOB.WOW_i)[2]))
      test_results[i] <-  -2
      next
    }else{
      sample.worst.cat <- obs.worst.cat[sample(1:tied_obs_worst[i], 8),]
    }



    #assign BOB/ WOW
    if(good_end == "low"){
      BOB_i <- sample.best.cat
      WOW_i <- sample.worst.cat
    }else if(good_end == "high"){
      BOB_i <- sample.worst.cat
      WOW_i <- sample.best.cat
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
                        ties_best_cat = tied_obs_best,
                        ties_worst_cat = tied_obs_worst)%>%
    filter(Count >= 6)%>%
    arrange(desc(Count))
  final.results <- data.frame(results,
                              adjusted.p.values = p.adjust(results$p.values))
  final.results

}


