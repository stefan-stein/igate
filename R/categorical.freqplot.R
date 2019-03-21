# Frequency plot for categorical variables


#'Produces frequency plots (normed to density plots to account for
#'different category sizes) for sanity check in categorical GBPCA.
#'
#'This function takes a data frame, a categorical target variable and a list of ssv and
#'produces a density plot of each ssv and each category of the target variable. The output is written as
#'.png file into the current working directory. Also, summary statistics are provided. NOTE:
#'The files will be saved into the current working directory, so consider changing the
#'working directory to a new empty folder before running this function.
#'
#' @param df Data frame to be analysed.
#' @param target Categorical target varaible to be analysed.
#' @param ssv A vector of suspected sources of variation. These are the variables
#' in \code{df} which we believe might have an influence on the target variable and
#' will be tested. If no list of ssv is provided, the test will be performed
#' on all numeric variables.
#' @param outlier_removal_ssv Logical. Should outlier removal be performed for each ssv (default: \code{TRUE})?
#'
#' @return The density plots of each category of \code{target} against each \code{ssv} are written as
#' .png file into the current working directory. Also, a data frame with the following
#' columns is output
#' \tabular{ll}{
#' \code{Causes} \tab The \code{ssv} that were analysed.\cr
#' \code{outliers_removed} \tab How many outliers (with respect to this \code{ssv})
#' have been removed before drawing the plot?\cr
#' \code{observations_retained} \tab After outlier removal was performed, how many observations
#' were left and used to fit the model?\cr
#' \code{frequency_plot} \tab Logical. Was plotting successful? No plot will be
#' produced if a ssv is constant.
#' }
#'
#'
#'@details Frequency plots for each \code{ssv} against  each category of the \code{target} are produced and
#'svaed to current working directory. Also a data frame with summary statistics is produced,
#'see \bold{Value} for details.
#'
#'
#'@examples categorical.freqplot(mtcars, target = "cyl")
#'
#' @export
#'
#' @importFrom stringr str_replace_all
#' @importFrom grDevices boxplot.stats
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot ggsave geom_freqpoly aes_string





categorical.freqplot <- function(df,
                                 target = "cyl",
                                 ssv =NULL,
                                 outlier_removal_ssv = TRUE){

  # If numeric, convert it first!
  if(is.numeric(df[[target]])){
    df[[target]] <- as.character(df[[target]])

  }


  # If no ssv are provided, we take all the numeric columns as ssv
  if(is.null(ssv)){
    nums <- sapply(df, is.numeric)
    df_num <- df[,nums]
    ssv <- names(df_num)
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
                           frequency_plot = rep(FALSE, ncol(df_ssv)))


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



    if(var(df_clean[,i]) == 0){
      outlier.df[i,4] <- FALSE
    }else{
      #Compute binwidth for freqploy using struges formula

      binwidth <- (max(df_clean[,i]) - min(df_clean[,i]))/(ceiling(log2(nrow(df_clean)))+1)

      ggplot(data = df_clean, mapping = aes_string(x = names(df_clean)[i], "..density.." ,color = target))+
        geom_freqpoly(binwidth = binwidth)

      ggsave(filename = paste0(str_replace_all(names(df_clean)[i], "[^[:alnum:]]", ""), "_against_", target,".png"))

      outlier.df[i,4] <- TRUE
    }
  }


  outlier.df

}
