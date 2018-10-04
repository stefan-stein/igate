report <- function(df,
                   versus = 8,
                   target = "max17_343_36",
                   test = "w",
                   ssv = NULL,
                   outlier_removal_target = TRUE,
                   outlier_removal_ssv = TRUE,
                   good_end = "low",
                   results_path = "Data/results.csv",
                   good_bad_bands_path = "Data/good_bad_bands.csv",
                   validation = FALSE,
                   validation_path = "Data/good_bad_validated_records.csv",
                   validation_counts = "Data/good_bad_counts.csv"){
  # If no ssv are provided, all the numeric columns have been used as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]%>%
      select(-contains("time"),
             -contains("visit"))
    ssv <- names(df_num)
    ssv <- ssv[-which( ssv == target)]
  }

  rmarkdown::render("Good_Bad_Report.Rmd", params = list(
    df_name = deparse(substitute(df)),
    versus = versus,
    target = target,
    test = test,
    ssv = ssv,
    outlier_removal_target = outlier_removal_target,
    outlier_removal_ssv = outlier_removal_ssv,
    good_end = good_end,
    results_path = results_path,
    good_bad_bands_path = good_bad_bands_path,
    validation = validation,
    validation_path = validation_path,
    validation_counts = validation_counts
  ))

}


