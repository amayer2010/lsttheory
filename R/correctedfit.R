
##

#' Fit correction for large SEM
#'
#' This function calculates a corrected \eqn{\chi^2}-value based on Yuan et al. (2015). 
#' A corresponding p-value and RMSEA value are computed based on the corrected \eqn{\chi^2} value. 
#' This correction is intended for SEM with many manifest variables. The \eqn{\chi^2}-value is multiplied 
#' with an empirical correction factor \eqn{e = (N - (2.381 + 0.361 \cdot p + 0.006 \cdot q) ) / (N - 1)}, 
#' where p is the total number of manifest variables and q is the number of free parameters.
#'
#' @param model a fitted lavaan or lsttheory model
#' @author Julia Norget
#' @references Yuan, K.-H., Tian, Y., & Yanagihara, H. (2015). Empirical correction to the likelihood ratio statistic for structural equation modeling with many variables. Psychometrika, 80(2), 379–405.
#' @export
#' @import lavaan
#' 
correctedfit <- function(model){
  if(class(model)[1L] == "lavaan"){
    lavmodel <- model
  }else if (class(model)[1L] == "lstmodel"){
    lavmodel <- model@lavaanres
  }else{
    stop("Please provide a lavaan model or lstmodel.")
  }
  if(lavInspect(lavmodel, "converged")){
    chisq <- fitmeasures(lavmodel, "chisq")
    df <- fitmeasures(lavmodel, "df")
    
    N <- lavInspect(lavmodel, "ntotal")
    p <- length(lavInspect(lavmodel, "obs")$mean)
    q <- as.integer(fitmeasures(lavmodel, "npar"))
    # correction factor e
    e = (N - (2.381 + 0.361*p + 0.006*q) ) / (N - 1)
    
    # corrected chisq statistic
    chisq.corrected <- e*chisq
    
    # corresponding p-value
    pvalue <- pchisq(chisq.corrected, df = df, lower.tail=FALSE)
    
    rmsea.adjusted <- sqrt( pmax(chisq.corrected - df, 0) ) / sqrt(df*(N))
    fit <- (c( chisq.corrected, df, pvalue, rmsea.adjusted))
  }
  else{
    fit <- c(NA, NA, NA, NA)
  }
  names(fit) <- c("chisq.corrected", "df", "p-value", "rmsea.corrected")
  
  print(round(fit,3))
  invisible(fit)
}
