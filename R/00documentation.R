
##
## These "global" variables are used in subset() or dplyr commands and are
## mistakenly classified as global variables by R CMD Check. Therefore they
## are added here as "global" variables to avoid the NOTE.
utils::globalVariables(c("est", "gamma_t_equiv", "label", "lhs",
                         "manifest_thetacovariates", "op"))

#' lsttheory
#' 
#' Compute several models of latent state-trait theory..
#' _PACKAGE


#' Dataset d_taucong.
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
#' @name d_taucong
NULL


#' Dataset d_multitraitmultistate02.
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
#' @name d_multitraitmultistate02
NULL



#' Dataset d_multitraitmultistate.
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
#' @name d_multitraitmultistate
NULL



#' Dataset d_multistate02.
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
#' @name d_multistate02
NULL



#' Dataset d_multistate.
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
#' @name d_multistate
NULL


#' Dataset mmLSTrf_exampledata.
#' 
#' A simulated dataset illustrating data for a study design with 2 fixed 
#' situations, 3 measurement occasions, 2 methods and 3 indicators. 
#' 
#' The variables are named after the following format: \eqn{Y_{imts}}
#' (i = indicator, m = method, t = occasion, s = fixed situation)
#' \itemize{
#'   \item \eqn{Y_{1111}}
#'   \item \eqn{Y_{2111}}
#'   \item \eqn{Y_{3111}}
#'   \item \eqn{Y_{1211}}
#'   \item \eqn{Y_{2211}}
#'   \item ...
#'   \item \eqn{Y_{2132}}
#'   \item \eqn{Y_{3132}}
#'   \item \eqn{Y_{1132}}
#'   \item \eqn{Y_{2232}}
#'   \item \eqn{Y_{3232}}
#' }
#' 
#' This format reflects the order of indicator variables in a path diagram, where
#' indicators are first grouped by fixed situations, within those they are then
#' grouped by occasions and within those they are lastly grouped by methods. 
#' The resulting nested structure has indicators nested within methods, nested
#' within occasions, nested within fixed situations.
#' 
#' The specified population values underlying the simulated data are: 
#' \itemize{
#'   \item \eqn{E(T_{111})} = 2.90
#'   \item \eqn{Comm(T_{112})} = 0.74
#'   \item \eqn{E(T_{112})} = 3.50
#'   \item \eqn{\epsilon_{imts}} = 0.15
#'   \item \eqn{Var(T_{111})} = 0.40
#'   \item \eqn{\alpha_{ims}} = 0.00
#'   \item \eqn{Var(T_{112})} = 0.98
#'   \item \eqn{\lambda_{ims}} = 1.00
#'   \item \eqn{Var(O_{11t1})} = 0.20
#'   \item \eqn{\delta_{ims}} = 1.00
#'   \item \eqn{Var(O_{11t2})} = 0.35
#'   \item \eqn{\gamma_{ims}} = 1.00
#'   \item \eqn{Var(TM_{ims})} = 0.10
#'   \item \eqn{\beta_{1112}} = 0.35
#'   \item \eqn{Var(OM_{mts})} = 0.10
#'   \item \eqn{\beta_{0112}} = -0.21
#'   \item \eqn{Var(\omega_{11s})} = 0.20
#'   }
#' Trait factors are essentially parallel, other latent variables are essentially 
#' equivalent. Scalar MI holds across fixed situations and methods. Latent 
#' variables are orthogonal apart from trait factors.
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 500 rows and 36 variables
#' @name mmLSTrf_exampledata
NULL



#' Dataset happy
#' 
#' Restructured wide-format data from Weiss et al. (2021), containing the items 
#' happiness ("How happy do you feel at the moment") and 
#' life satisfaction ("How satisfied are you with your life at the moment?") on five-point Likert scales.
#' Data was assessed 5 times a day on 5 days during an experience-sampling study.
#' The original dataset is available online at: https://osf.io/kwp6n/ 
#' The variables are:
#' 
#' \itemize{
#'   \item happy_1. Self-reported happiness on occasion 1. 
#'   \item satisfaction_1. Self-reported life satisfaction on occasion 1.
#'   \item happy_2. Self-reported happiness on occasion 2.
#'   \item satisfaction_2. Self-reported life satisfaction on occasion 2.
#'   \item ...
#'   \item happy_25. Self-reported happiness on occasion 25.
#'   \item satisfaction_25. Self-reported life satisfaction on occasion 25.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 425 rows and 50 variables
#' @name happy
NULL



#' Dataset happybig5
#' 
#' Restructured wide-format data from Weiss et al. (2021), containing the items 
#' happiness ("How happy do you feel at the moment") and 
#' life satisfaction ("How satisfied are you with your life at the moment?") on five-point Likert scales.
#' Data was assessed 5 times a day on 5 days during an experience-sampling study.
#' Additionally, this dataset contains measures of the Big Five personality dimensions from an intake session before the experience sampling phase.
#' The Big Five personality dimensions reflect mean values of 2 items each, one of which reverse coded (Gosling et al., 2003; Muck et al., 2007)
#' This dataset can be used to demonstrate how covariates (Big Five) contribute to trait components of Latent State-Trait models. 
#' The original dataset (including codebooks) is available online at: https://osf.io/kwp6n/ 
#' The variables are:
#' 
#' \itemize{
#'   \item happy_1. Self-reported happiness on occasion 1. 
#'   \item satisfaction_1. Self-reported life satisfaction on occasion 1.
#'   \item happy_2. Self-reported happiness on occasion 2.
#'   \item satisfaction_2. Self-reported life satisfaction on occasion 2.
#'   \item ...
#'   \item happy_25. Self-reported happiness on occasion 25.
#'   \item satisfaction_25. Self-reported life satisfaction on occasion 25.
#'   \item Big5_OE_M. Openness.
#'   \item Big5_CO_M. Conscientiousness.
#'   \item Big5_EX_M. Extraversion.
#'   \item Big5_AG_M. Agreeableness.
#'   \item Big5_ES_M. Emotional Stability.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 425 rows and 55 variables
#' @name happybig5
NULL


############## namespace ###########

#' @importFrom utils tail
NULL

#' @importFrom magrittr %>%
NULL
