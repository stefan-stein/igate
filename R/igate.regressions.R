# Regression analysis -----------------------------------------------------

#'Produces the regression plots for sanity check in iGATE
#'
#'This function takes a data frame, a target variable and a list of ssv and
#'produces a regression plot of each ssv against the target. The output can written as
#'.png file into the current working directory. Also, summary statistics are provided.
#'
#' @param df Data frame to be analysed.
#' @param target Target varaible to be analysed.
#' @param ssv A vector of suspected sources of variation. These are the variables
#' in \code{df} which we believe might have an influence on the target variable and
#' will be tested. If no list of ssv is provided, the test will be performed
#' on all numeric variables.
#' @param outlier_removal_target Logical. Should outliers (with respect to the target variable)
#' be removed from df (default: \code{TRUE})? Important: This only makes sense if no
#' prior outlier removal has been performed on df, i.e. \code{df} still contains all
#' the data. Otherwise calculation for outlier threshold will be falsified.
#' @param outlier_removal_ssv Logical. Should outlier removal be performed for each ssv (default: \code{TRUE})?
#' @param savePlots Logical. If \code{FALSE} (the default) regression plots will be output to the standard plotting
#' device. If \code{TRUE}, regression plots will additionally be saved to \code{image_directory} as png files.
#' @param image_directory Directory to which plots should be saved. This is only used if \code{savePlots = TRUE} and
#' defaults to the temporary directory of the current R session, i.e. \code{tempdir()}. To save plots to the current
#' working directory set \code{savePlots = TRUE} and \code{image_directory = getwd()}.
#'
#' @return The regression plots of \code{target} against each \code{ssv} are written as
#' .png file into the current working directory. Also, a data frame with the following
#' columns is output
#' \tabular{ll}{
#' \code{Causes} \tab The \code{ssv} that were analysed.\cr
#' \code{outliers_removed} \tab How many outliers (with respect to this \code{ssv})
#' have been removed before fitting the linear model?\cr
#' \code{observations_retained} \tab After outlier removal was performed, how many observations
#' were left and used to fit the model?\cr
#' \code{regression_plot} \tab Logical. Was fitting the model successful? It can fail,
#' for example, if a ssv is constant.\cr
#' \code{r_squared} \tab r^2 value of model.\cr
#' \code{gradient, intercept} \tab Gradient and intercept of fitted model.
#' }
#'
#'
#'@details Regression plots for each \code{ssv} against \code{target} are produced and
#'svaed to current working directory. Also a data frame with summary statistics is produced,
#'see \bold{Value} for details.
#'
#' @examples igate.regressions(iris, target = "Sepal.Length")
#'
#' @export
#'
#' @importFrom stringr str_replace_all




igate.regressions <- function(df,
                              target,
                              ssv =NULL,
                              outlier_removal_target = TRUE,
                              outlier_removal_ssv = TRUE,
                              savePlots = FALSE,
                              image_directory = tempdir()){

  if(sum(names(df) == target) != 1){
    stop(paste0(target,
                " is not a valid column name for ",
                deparse(substitute(df)),
                ".\nGot sum(names(df) == target) = ", sum(names(df) == target),
                ", but need 1."))
  }
  if((image_directory != tempdir()) && !savePlots){
    stop(paste0("You specified a directory to save the images in (",
                image_directory, ") but used savePlots = ", savePlots, ". If you want to save the images, please use savePlots = TRUE."))
  }
  # Remove outliers
  if(outlier_removal_target){
    box_stats <- boxplot.stats(df[[target]]) #we need this as extra variable tp keep track of removed records (we overwrite df)
    df <- df%>%filter(df[[target]] <= box_stats$stats[5])
    message(paste0(length(box_stats$out), " outliers have been removed."))
    message(paste0("Retaining ", length(df[[target]]), " observations."))
  }


  # If no ssv are provided, we take all the numeric columns as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]
    ssv <- names(df_num)
    ssv <- ssv[-which( ssv == target)]
  }
  #only keep data corresponding to ssv
  df_ssv <- df[,which(names(df) %in% ssv)]
  # this is for windows version in case there is only one ssv
  if(is.vector(df_ssv)){
    df_ssv <- as.data.frame(df_ssv)
    names(df_ssv) <- ssv
  }

  #collect summary statistics about outliers
  outlier.df <- data.frame(Causes = names(df_ssv),
                           outliers_removed = rep(-1, ncol(df_ssv)),
                           observations_retained = rep(-1, ncol(df_ssv)),
                           regression_plot = rep(FALSE, ncol(df_ssv)),
                           r_squared = rep(-1, ncol(df_ssv)),
                           gradient = rep(-1, ncol(df_ssv)),
                           intercept = rep(-1, ncol(df_ssv)))

  for(i in 1:length(ssv)){
    #Outlier removal per ssv?
    if(outlier_removal_ssv){
      box_stats_i <- boxplot.stats(unlist(df_ssv[,i]))
      df_clean <- data.frame(df_ssv, df[[target]])
      df_clean <- filter(df_clean, df_clean[,i] >= box_stats_i$stats[1],df_clean[,i] <= box_stats_i$stats[5])
      colnames(df_clean) <- c(names(df_ssv),target)
      outlier.df[i,2] <- length(box_stats_i$out)
      outlier.df[i,3] <- nrow(df_clean)
    }else{
      df_clean <-  data.frame(df[[target]], df_ssv)
      colnames(df_clean) <- c(target, names(df_ssv))
    }

    #check for constant columns which make the abline() command fail
    if(var(df_clean[,i]) == 0){
      outlier.df[i,4] <- FALSE
    }else{
      fit <- lm(df_clean[[target]]~unlist(df_clean[,i]))

      if(savePlots){


        plot(unlist(df_clean[,i]),df_clean[[target]],
             main = paste0("Linear regression plot of\n ", names(df_clean)[i], " against ", target,
                           ", r^2 = ", round(summary(fit)$r.squared, 3)),
             sub = paste0("Formula: ", target, " = ",
                          round(fit$coefficients[2], 3), "*", names(df_clean)[i], " + ", round(fit$coefficients[1],3)),
             xlab = names(df_clean)[i],
             ylab = target)
        abline(fit)
        dev.print(png, file = paste0(image_directory, "/",
                                     str_replace_all(names(df_clean)[i], "[^[:alnum:]]", ""),
                                     "_against_", target,".png"),
                  width = 573, height = 371)

      }else{

        plot(unlist(df_clean[,i]),df_clean[[target]],
             main = paste0("Linear regression plot of\n ", names(df_clean)[i], " against ", target,
                           ", r^2 = ", round(summary(fit)$r.squared, 3)),
             sub = paste0("Formula: ", target, " = ",
                          round(fit$coefficients[2], 3), "*", names(df_clean)[i], " + ", round(fit$coefficients[1],3)),
             xlab = names(df_clean)[i],
             ylab = target)
        abline(fit)

      }

      outlier.df[i,4] <- TRUE
      outlier.df[i,5] <- summary(fit)$r.squared
      outlier.df[i,6] <- fit$coefficients[2]
      outlier.df[i,7] <- fit$coefficients[1]
    }
  }
  outlier.df

}
