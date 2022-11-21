
## These "global" variables are used in subset() or dplyr commands and are
## mistakenly classified as gloabal variables by R CMD Check. Therefore they
## are added here as "global" variables to avoid the NOTE.
utils::globalVariables(c("est", "gamma_t_equiv", "label", "lhs", 
                         "manifest_thetacovariates", "op")) 


#' Dataset taucong.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y1.
#'   \item y2.
#'   \item y3.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 300 rows and 3 variables
#' @name taucong
NULL


#' Dataset multitraitmultistate02.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y11.
#'   \item y21.
#'   \item y12.
#'   \item y22.
#'   \item y13.
#'   \item y23.
#'   \item y14.
#'   \item y24.
#'   \item x.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 1000 rows and 9 variables
#' @name multitraitmultistate02
NULL



#' Dataset multitraitmultistate.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y11.
#'   \item y21.
#'   \item y12.
#'   \item y22.
#'   \item y13.
#'   \item y23.
#'   \item y14.
#'   \item y24.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 1000 rows and 8 variables
#' @name multitraitmultistate
NULL



#' Dataset multistate02.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y11.
#'   \item y21.
#'   \item y31.
#'   \item y12.
#'   \item y22.
#'   \item y32.
#'   \item y13.
#'   \item y23.
#'   \item y33.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 1000 rows and 9 variables
#' @name multistate02
NULL



#' Dataset multistate.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y11.
#'   \item y21.
#'   \item y12.
#'   \item y22.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 400 rows and 4 variables
#' @name multistate
NULL


