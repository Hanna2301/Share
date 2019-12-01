library(RODBC)
#library(xlsx)
library(dplyr)
library(ggplot2)
library(data.table)
library(mgcv)

# to calculate GINI

#################################################### FUNCTIONS
###############################################       normalizedGini

normalizedGini = function(actual, pred, expo) {								
  Gini <- function(a, p) {								
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")								
    temp.df <- data.frame(actual = a, pred = p, range=expo)								
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]								
    population.delta <- 1 / length(a)								
    total.losses <- sum(a)								
    null.losses <- rep(population.delta, length(a)) 							
    accum.losses <- temp.df$actual / total.losses 								
    gini.sum <- cumsum(accum.losses - null.losses) 					
    sum(gini.sum) / length(a)								
  }								
  Gini(actual, pred) / Gini(actual, actual)								
}								


###############################################       normalizedGiniF

normalizedGiniF = function(actual, pred, expo) {								
  Gini <- function(a, p) {								
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")								
    temp.df <- data.frame(actual = a, pred = p, range=expo)								
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range, -temp.df$actual),]								
    population.delta <- 1 / length(a)								
    total.losses <- sum(a)								
    null.losses <- rep(population.delta, length(a)) 							
    accum.losses <- temp.df$actual / total.losses 								
    gini.sum <- cumsum(accum.losses - null.losses)							
    sum(gini.sum) / length(a)								
  }								
  Gini(actual, pred) / Gini(actual, actual)								
}	


###############################################       normalizedGiniS

normalizedGiniS = function(actual, pred) {								
  Gini <- function(a, p) {								
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")								
    temp.df <- data.frame(actual = a, pred = p)								
    temp.df <- temp.df[order(-temp.df$pred,  -temp.df$actual),]								
    population.delta <- 1 / length(a)								
    total.losses <- sum(a)								
    null.losses <- rep(population.delta, length(a)) 								
    accum.losses <- temp.df$actual / total.losses 							
    gini.sum <- cumsum(accum.losses - null.losses) 							
    sum(gini.sum) / length(a)								
  }								
  Gini(actual, pred) / Gini(actual, actual)								
}	


