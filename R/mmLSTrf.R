#' Multi-Method Latent-State-Trait Random-Fixed Models
#' 
#' A function to estimate Multi-Method Latent-State-Trait Random-Fixed SEM 
#' models based on the MM-LST-RF model developed by Hintz, Geiser & Shiffman, 
#' 2019.
#' 
#' 
#' \strong{Data Structure:}\cr
#' The order of the indicators Y_imts (i = indicator, m = method, t = time-point
#' /occasion, s = fixed situation) need to match the following format:
#' Y_1111, Y_2111, Y_1211, Y_2211, Y_1121, Y_2121, Y_1221, Y_2221, Y_1112, etc.
#' This format reflects the order of indicator variables in a path diagram where
#' indicators are first grouped by fixed situations, within those they are then
#' grouped by occasions and within those they are lastly grouped by methods. 
#' The resulting nested structure has indicators nested within methods, nested
#' within occasions, nested within fixed situations.
#' 
#' The number of fixed situations, occasions (time-points), methods and number 
#' of indicators can take any value >= 2.
#' However, the model is restricted to an equal number of indicators for each 
#' method, occasion and fixed situation. For example, if method A has 3 
#' indicators, method B must also have 3 indicators. Accordingly, if 
#' there are 2 methods with 3 indicators each, every  occasion will have 6 
#' indicators (3 from each method).  
#' In the same vein, the number of methods needs to be equal across occasions 
#' and the number of occasions needs to be equal across fixed situations. 
#' Lastly, only one Trait Factor is estimated per fixed situation.
#'
#' \strong{Syntax notation:}\cr
#' Example for Y_2132 with i = 2, m = 1, t = 3, s = 2 (covariances with s = 3)
#' \tabular{lll}{
#' \strong{Variable / Parameter}           \tab \strong{Notation}  \tab \strong{Example}   \cr
#' Trait factors (TF)                      \tab Ts                 \tab T2                 \cr
#' Occasion factors (OF)                   \tab Ots                \tab O32                \cr
#' Trait-method factors (TMF)              \tab TMims              \tab TM212              \cr
#' Occasion-method factors (OMF)           \tab OMmts              \tab OM132              \cr
#' TF mean                                 \tab M_Ts               \tab M_T2               \cr
#' TF variance                             \tab V_Ts               \tab V_T2               \cr
#' OF variance                             \tab V_Ots              \tab V_O32              \cr
#' OMF variance                            \tab V_OMmts            \tab V_OM132            \cr
#' TMF variance                            \tab V_TMims            \tab V_TM212            \cr
#' Observed variable variance              \tab V_Y_imts           \tab V_(variable name)  \cr
#' TF covariance                           \tab Cv_TaxTb           \tab Cv_T2xT3           \cr
#' OF covariance                           \tab Cv_OaxOb           \tab Cv_O32xO33         \cr
#' TMF covariance                          \tab Cv_TMaxTMb         \tab Cv_TM212xTM213     \cr
#' OMF covariance                          \tab Cv_OMaxOMb         \tab Cv_OM132xOM133     \cr
#' Residual variance epsilon               \tab eps_imts           \tab eps_2132           \cr
#' Intercept alpha                         \tab alph_ims           \tab alph_212           \cr
#' Loading lambda                          \tab lam_ims            \tab lam_212            \cr
#' Loading delta                           \tab del_ims            \tab del_212            \cr
#' Loading gamma                           \tab gam_ims            \tab gam_212            \cr
#' Difference TF                           \tab Dif_Ts             \tab Dif_T2             \cr
#' Regression intercept (TF)               \tab b0_Ts              \tab b0_T2              \cr
#' Regression slope beta (TF)              \tab b1_Ts              \tab b1_T2              \cr
#' Residual variance omega (TF)            \tab omg_Ts             \tab omg_T2             \cr
#' Difference TMF                          \tab Dif_TMims          \tab Dif_TM212          \cr
#' Regression slope beta (TMF)             \tab b1_TMims           \tab b1_T212            \cr
#' Residual variance omega (TMF)           \tab omg_TMims          \tab omg_T212           \cr
#' Commonality TF                          \tab Com_Ts             \tab Com_T2             \cr
#' Fixed situation specificity TF          \tab SitSp_Ts           \tab SitSp_T2           \cr
#' Commonality TMF                         \tab Com_TMims          \tab Com_TM212          \cr
#' Fixed situation specificity TMF         \tab SitSp_TMims        \tab SitSp_TM212        \cr
#' Observed variable reliability           \tab RelY_imts          \tab Rel(variable name) \cr
#' Observed variable consistency           \tab ConY_imts          \tab Con(variable name) \cr
#' Observed variable specificity           \tab SpeY_imts          \tab Spe(variable name) \cr
#' }
#' Note that for difference and commonality variables the fixed reference
#' situation is always s = 1. 
#' 
#'
#' @param data Data frame.\cr 
#' This data frame contains only the indicator variables which will be used to 
#' fit the MM-LST-RF model. For more information about the required data 
#' structure refer to the function details.
#' The first indicator & the first method will be used as reference indicator & 
#' reference method across occasions and fixed situations.
#' @param nSit Integer. Default = 2.\cr 
#' Number of fixed situations. Must be >= 2.
#' Used to calculate the number of Trait Factors (TF).
#' @param nTime Integer. Default = 2.\cr 
#' Number of time-points. Must be >= 2. 
#' Used to calculate the number of Occasion Factors (OF).
#' @param nMth Integer. Default = 2.\cr 
#' Number of methods. Must be >= 2.
#' Used to calculate the number of Trait-Method Factors (TMF) & Occasion-Method
#' Factors (OMF).
#' @param structural Character string. Default = \code{"TF"}.\cr                  
#' Adds structural part to the model. Can be one of the following:
#' \itemize{
#' \item \code{"none"} estimates only the measurement model.
#' \item \code{"TF"} models Person x Situation interactions. The TF from fixed 
#' situation s = 1 serves as reference to be contrasted against. \code{TFcov} will be 
#' set to \code{FALSE}.
#' \item \code{"TMF"} models Method x Situation interactions. The TMF from fixed 
#' situation s = 1 serve as references to be contrasted against. Only TMF of the 
#' same indicator i and method m will be contrasted across situations.\code{TFcov} 
#' will be set to \code{FALSE}.
#' \item \code{"both"} models both Person x Situation & Method x Situation interactions
#' modeled. \code{TFcov} and \code{TMFcov} will be set to \code{FALSE}.
#' }
#' @param includeOMF Logical. Default = \code{TRUE}.\cr 
#' If \code{TRUE}, OMF will be estimated. 
#' If \code{FALSE}, no OMF will be estimated. 
#' Can be used when only small or no occasion-method effects are expected.
#' @param lat.cov List. Default = \code{FALSE}.\cr 
#' Specifies covariances between latent variables.
#' \itemize{
#' \item \code{TFcov}. If \code{TRUE}, covariances between all TF will be estimated. 
#' Will automatically be set to \code{FALSE} if \code{structural = "TF" || "both"}.
#' If \code{FALSE}, no covariances between TF will be estimated.
#' \item \code{OFcov}. If \code{TRUE}, covariances of OF with their respective 
#' counterparts in other fixed situations will be estimated (e.g., Cv(O21,
#' O_22), Cv(O_21,O_23) and Cv(O_22, O_23)). 
#' If FALSE, no covariances between OF will be estimated.
#' \item \code{TMFcov}. If \code{TRUE}, covariances between all TMF will be estimated. 
#' Will automatically be set to FALSE if \code{structural = "TMF" || "both"}.
#' If \code{FALSE}, no covariances between TMF will be estimated.
#' \item \code{OMFcov}. If \code{TRUE}, covariances of OMF with their respective 
#' counterparts in other fixed situations will be estimated.
#' If \code{FALSE}, no covariances between OMF will be estimated.
#' }
#' It is recommended to estimate covariances sparingly (especially for TMF) as 
#' this quickly inflates the amount of free model parameters. 
#' For the estimation of only a select number of covariances, specifications 
#' can be passed to the function via the \code{"addsyntax"} argument. 
#' @param meanstructure Logical. Default = \code{FALSE}.\cr 
#' If \code{TRUE}, model will be estimated with a meanstructure. 
#' If \code{FALSE}, model will be estimated without a meanstructure. 
#' @param meas.invar Character string. Default = \code{"time.invar"}.\cr
#' Measurment invariance assumptions across groups. Can be one of the following:
#' \itemize{
#' \item \code{"time.invar"} = assumes time invariance of indicator loadings + intercepts
#' across occasions.
#' \item \code{"metric.m"} assumes time invariance & equal loadings across methods. 
#' \item \code{"metric.s"} assumes time invariance & equal loadings across fixed situations.
#' \item \code{"metric.b"} assumes time invariance & equal loadings across methods & fixed 
#' situations. 
#' \item \code{"scalar.m"} assumes time invariance & equal loading + intercepts across 
#' methods.  
#' \item \code{"scalar.s"} assumes time invariance & equal loading + intercepts across 
#' fixed situations. 
#' \item \code{"scalar.b"} assumes time invariance & equal loading + intercepts across 
#' methods & fixed situations.
#' \item \code{"residual.m"} assumes time invariance & equal loading + intercepts + error 
#' variances across methods.  
#' \item \code{"residual.s"} assumes time invariance & equal loading + intercepts + error 
#' variances across fixed situations.
#' \item \code{"residual.b"} assumes time invariance & equal loading + intercepts + error 
#' variances across methods & fixed situations.
#' }
#' @param equiv.ass List. Default: TF = \code{"par"}, OF & OMF= \code{"ess.equiv"}.\cr
#' Equivalence assumptions for latent variables.
#' For TF, OF & OMF the following equivalence assumptions can be made separately 
#' for each latent variable:
#' \itemize{
#' \item \code{"cong"} for congeneric measures makes no restrictions apart from setting the
#' loading of the first indicator of the latent variable to 1 and the intercept 
#' to 0 for identification purposes.
#' \item \code{"ess.equiv"} for essentially equivalent measures assumes equal loadings of
#' all indicators of the latent variable.
#' }
#' For TF the following additional equivalence assumptions can be made:
#' \enumerate{
#' \item \code{"equiv"} for equivalent measures assumes equal loadings + intercepts of all 
#' indicators of TF.
#' \item \code{"ess.par"} for essentially parallel measures assumes equal loadings + error 
#' variances of all indicators of TF.
#' \item \code{"par"} for parallel measures assumes equal loadings, intercepts + error 
#' variances of all indicators of TF.
#' } 
#' No equivalence assumptions need to be specified for TMF since they are defined 
#' as residual latent variables and all loadings are automatically fixed to one 
#' due to the combination of identification requirements and time invariance.
#' @param addsyntax Character string.\cr
#' Optional argument where additional model specifications, such as covariances
#' between specific TMFs and TFs, can be passed to the function and will 
#' automatically be added to the generated lavaan syntax. 
#' For more information about the used variable notation refer to the function 
#' details.
#' @param ... Further arguments passed to \code{lavaan::sem()}.
#' @return Object of class mmLSTrf.
#' @author Tinhof Dora
#' @references Hintz F., Geiser C., & Shiffman S. (2019). A latent state-trait 
#' model for analyzing states, traits, situations, method effects, and their 
#' interactions. Journal of Personality (1-21). https://doi.org/10.1111/jopy.12400
#' @examples 
#' modelfit <- mmLSTrf(mmLSTrf_SimulatedDataExample, nSit=2, nTime=3, nMth=2, structural="TF", 
#'                     includeOMF=TRUE, lat.cov=list(TFcov=FALSE, OFcov=FALSE, 
#'                     TMFcov=FALSE, OMFcov=FALSE), meanstructure=TRUE, 
#'                     meas.invar="scalar.b", equiv.ass=list(TF="ess.par", 
#'                     OF="ess.equiv", OMF="ess.equiv"), addsyntax="")
#' print(modelfit)
#' @export
#' @import lavaan

mmLSTrf <- function(data, nSit=2, nTime=2, nMth=2, structural="TF", includeOMF=TRUE, 
                    lat.cov=list(TFcov=FALSE, OFcov=FALSE, TMFcov=FALSE, 
                    OMFcov=FALSE), meanstructure=FALSE, meas.invar="time.invar", 
                    equiv.ass=list(TF="par", OF="ess.equiv", OMF="ess.equiv"), 
                    addsyntax="", ...){  
  
  
  
  checkinput_mmLSTrf(data, nSit, nTime, nMth, structural, includeOMF, lat.cov,
                     meanstructure, meas.invar, equiv.ass, addsyntax)

  
  
  mod <- createmmLSTrf(data, nSit, nTime, nMth, structural, includeOMF, lat.cov,
                       meanstructure, meas.invar, equiv.ass) 
  
  
  completesyntax <- createCompleteSyntax_mmLSTrf(mod) 
  completesyntax <- paste0(addsyntax, completesyntax, sep="\n") 
    #"addsyntax" first so manual input gets priority when there are potential duplicates
  
  
  lavaanres <- sem(completesyntax, data=data, orthogonal=TRUE, ...)

  
 
  ## save results in mod
  mod@lavaansyntax <- completesyntax
  mod@lavaanres <- lavaanres                                        
  
  
  mod@coeff.fac <- list(CommTF=coef(lavaanres, type="user")[mod@labels$CommTF], 
                        SitSpeTF=coef(lavaanres, type="user")[mod@labels$SitSpeTF], 
                          #Only calculated when TFcov = TRUE and structural != "TF" || "both" 
                        CommTMF=coef(lavaanres, type="user")[mod@labels$CommTMF], 
                        SitSpeTMF=coef(lavaanres, type="user")[mod@labels$SitSpeTMF]) 
                          #Only calculated when TMFcov = TRUE and structural != "TMF" || "both"
                      
  mod@coeff.ind <- list(RelY=coef(lavaanres, type="user")[mod@labels$RelY],
                        ConY=coef(lavaanres, type="user")[mod@labels$ConY],
                        SpeY=coef(lavaanres, type="user")[mod@labels$SpeY])

  
  return(mod)  
}

######################## check input, errors & warnings ############################

checkinput_mmLSTrf <- function(data, nSit, nTime, nMth, structural, includeOMF, 
                               lat.cov, meanstructure, meas.invar, equiv.ass, 
                               addsyntax){
  
  if(length(data) < 2*nSit*nTime*nMth){
    stop("Model requires at least 2 indicators per method!")
  }
  
  
  if((length(data)/(nSit*nTime*nMth)) %% 1 != 0){
    stop("Number of variables in the selected data set don't match the number of
          imputed nSit, nTime and nMth values. Doublecheck the imputed values and 
          the variables in the dataframe! There should be an even amount of 
          indicators for each method / occasion / situation.")
  }
  
  
  if(!is.numeric(nSit)){stop("nSit must be numeric!")}
  if(!is.numeric(nTime)){stop("nTime must be numeric!")}
  if(!is.numeric(nMth)){stop("nMth must be numeric!")}
  
  
  if(!is.character(addsyntax)){stop("addsyntax must be character string!")}
  
  
  if(!any(structural == c("none", "TF", "TMF", "both"))){
    stop("structural must be one of the following character strings: 
         'none', 'TF', 'TMF', 'both'!")
  }
  
  
  if(!is.logical(includeOMF)){stop("includeOMF must be logical!")}
  
  if(!is.list(lat.cov)){
    stop("lat.cov must be a list of the format: c(TFcov=xxx, OFcov=xxx, 
         TMFcov=xxx, OMF=xxx) whereby xxx can either be TRUE or FALSE") 
  }
  if(!is.logical(lat.cov$TFcov)){stop("TFcov must be logical!")}
  if(!is.logical(lat.cov$OFcov)){stop("OFcov must be logical!")}
  if(!is.logical(lat.cov$TMFcov)){stop("TMFcov must be logical!")}
  if(!is.logical(lat.cov$OMFcov)){stop("OMFcov must be logical!")}
  
  if(!is.logical(meanstructure)){stop("meanstructure must be logical!")}
  
  
  meas_vec <- c("time.invar", "metric.m", "metric.s", "metric.b", "scalar.m", 
                "scalar.s", "scalar.b", "residual.m", "residual.s", "residual.b")
  
  if(!any(meas.invar == meas_vec)){
    stop("meas.invar must be one of the following character strings: 
         'time.invar', 'metric.m', 'metric.s', 'metric.b', 'scalar.m', 
         'scalar.s', 'scalar.b', 'residual.m', 'residual.s', 'residual.b'!")
  }
  
  
  equiv_vec1 <- c("cong", "ess.equiv", "equiv", "ess.par", "par")
  equiv_vec2 <- c("cong", "ess.equiv")
  if(!is.list(equiv.ass)){
    stop("equiv.ass must be a list of the format: c(TF='xxx', OF='xxx', 
         OMF='xxx') whereby 'xxx' can be one either 'cong' or 'ess.equiv'. 
         For TF 'xxx' can also be 'equiv', 'ess.par' or 'par'.") 
  }
  
  if(!any(equiv.ass$TF == equiv_vec1)){
    stop("List element 'TF' of equiv.ass must be one of the following 
         character strings: 'cong', 'ess.equiv', 'equiv', 'ess.par', 'par'")
  }
  
  if(!any(equiv.ass$OF == equiv_vec2)){
    stop("List element 'OF' of equiv.ass must be one of the following 
         character strings: 'cong', 'ess.equiv'")
  }
  
  if(!any(equiv.ass$OMF == equiv_vec2)){
    stop("List element 'OMF' of equiv.ass must be one of the following 
         character strings: 'cong', 'ess.equiv'")
  }
  
  
  if (structural %in% c("TF", "both")) {
    if (lat.cov$TFcov != FALSE) {
      warning("TFcov forced to FALSE to estimate requested structural model")
    }
    lat.cov$TFcov <- FALSE
  }
  
  if (structural %in% c("TMF", "both")) {
    if (lat.cov$TMFcov != FALSE) {
      warning("TMFcov forced to FALSE to estimate requested structural model")
    }
    lat.cov$TMFcov <- FALSE
  }
  
}

######################## class definition and methods ############################

setClass(
  "mmLSTrf",               
  representation(
    restrictions = "list",        #restrictions
    number       = "list",        # number of variables    
    names        = "list",        # names of variables
    labels       = "list",        # labels of parameters
    data         = "data.frame",
    lavaansyntax = "character",
    lavaanres    = "lavaan",
    coeff.fac    = "list",        #Coefficients of Factors
    coeff.ind    = "list"))       #Coefficients of Indicators                                              

setMethod("show", "mmLSTrf",                              
          function(object){
            cat("\n" , "Model Summary", "\n \n") 
            print(object@lavaanres)
            cat("\n" ,"\n")
            
            cat("\n" , "MM-LST-RF Coefficients", "\n \n")
            fac <- as.data.frame(object@coeff.fac[c(1:2)])
            rownames(fac) <- object@labels$CoefTF
            print(fac, digits=2)
            cat ("\n")
            
            cat(" The commonality coefficient 'CommTF' quantifies the proportion", "\n",
                "of trait-like variance which is common across fixed situations", "\n",
                "(only calculated when TFcov=TRUE).", "\n",
                "CommTF is calculated as follows:", "\n",
                "CommTF = (cor(T_111, T_11s))^2", "\n \n",
                
                "The situation specificity coefficient 'SitSpeTF' quantifies", "\n",
                "the proportion of stable variance which is specific to the", "\n",
                "fixed situation (only calculated when TFcov=TRUE).", "\n",
                "SitSpeTMF is calculated as follows:", "\n",
                "SitSpeTMF = 1 - (cor(T_111, T_11s))^2", "\n \n",
                
                "The commonality coefficient 'CommTMF' quantifies the proportion", "\n",
                "of variance in trait-method effects which is shared across", "\n",
                "fixed situations (only calculated when TMFcov=TRUE).", "\n",
                "CommTMF is calculated as follows:", "\n",
                "CommTMF = (cor(TM_im1, TM_ims))^2", "\n \n",
                
                "The situation specificity coefficient 'SitSpeTMF' quantifies", "\n",
                "the proportion of variance in trait-method effects which is", "\n",
                "specific to the fixed situation (only calculated when TMFcov=TRUE).", "\n",
                "SitSpeTMF is calculated as follows:", "\n",
                "SitSpeTMF = 1 - (cor(TM_im1, TM_ims))^2", "\n \n")
            
            ind <- as.data.frame(object@coeff.ind)
            rownames(ind) <- object@names$manifest
            print(ind, digits=2)
            cat ("\n")
            
            cat("The reliability coefficient 'RelY' quantifies how well the", "\n",
                "indicators Y_imts estimate the latent variables on average.", "\n",
                "The total explainable variance includes variance attributable", "\n",
                "to fixed situations, occasions as well as methods.", "\n",
                "RelY is calculated as follows:", "\n",
                "RelY = (lam_ims^2*Var(T_11s)+del_ims^2*Var(O_11ts)+gam_ims^2*Var(OM_mts)+Var(TM_ims)) / Var(Y_imts)", "\n \n",
                
                "The consistency coefficient 'ConY' quantifies the proportion", "\n",
                "of variance of the indicators Y_imts which is determined by TF,", "\n",
                "representing trait-like effects (excluding method effects).", "\n",
                "ConY is calculated as follows:", "\n",
                "ConY = (lam_ims^2*Var(T_11s)) / Var(Y_imts)", "\n \n",
                
                "The specificity coefficient 'SpeY' quantifies the proportion", "\n",
                "of variance of the indicators Y_imts which is determined by OF,", "\n",
                "representing situation effects (excluding method effects).", "\n",
                "SpeY is calculated as follows:", "\n",
                "SpeY = (del_ims^2*Var(O_11ts)) / Var(Y_imts)", "\n \n")
            cat("\n" ,"\n")
            
            cat(" A full model summary, including fitmeasures, can be accessed", "\n",
                "via 'summary(model@lavaanres, fit.measures=TRUE)' where 'model'", "\n",  
                "must be replaced with the assigned modelname.", "\n")
           })



createmmLSTrf <- function(data, nSit, nTime, nMth, structural, includeOMF, 
                          lat.cov, meanstructure, meas.invar, equiv.ass){ 

  
  restrictions <- list(structural    = structural,
                       includeOMF    = includeOMF,
                       lat.cov       = lat.cov,
                       meanstructure = meanstructure,
                       meas.invar    = meas.invar,
                       equiv.ass     = equiv.ass)

  
  number <- list(manifest = ncol(data),
                 nInd     = ncol(data)/(nSit*nTime*nMth),
                 nTF      = nSit,
                 nOF      = nSit*nTime,
                 nTMF     = (((ncol(data)/(nSit*nTime*nMth))*nMth)-1)*nSit,
                 nOMF     = nSit*nTime*nMth,
                 nSit     = nSit,
                 nTime    = nTime,
                 nMth     = nMth)
  
  
  names <- list(manifest = names(data),
                TF  = paste0("T", 1:nSit),
                OF  = paste0("O", rep(1:nTime, nSit), rep(1:nSit, each=nTime)),
                TMF = paste0("TM", rep(c(2:number$nInd, rep(1:number$nInd, nMth-1)), nSit), 
                                   rep(c(rep(1, number$nInd-1), rep(2:nMth, each=number$nInd)), nSit),
                                   rep(1:nSit, each=number$nTMF/nSit)),
                OMF = paste0("OM", rep(1:nMth, number$nOF), 
                                   rep(1:nTime, each=nMth, times=nSit), 
                                   rep(1:nSit, each=nMth*nTime)))
  
  
  labels <- createLabels_mmLSTrf(data, number, restrictions)
  
  
  model <- new("mmLSTrf", 
               restrictions = restrictions,    
               number       = number,    
               names        = names,
               labels       = labels,                
               data         = data)
  
  return(model)
}



######################## create syntax functions ############################

createLabels_mmLSTrf <- function(data, number, restrictions){

## Factors ##
  
  TF <- paste0("T", 1:number$nTF)
  OF <- paste0("O", rep(1:number$nTime, number$nTF), 
                 rep(1:number$nTF, each=number$nTime))
  
  
  ims <- paste0(rep(c(2:number$nInd, rep(1:number$nInd, number$nMth-1)), number$nTF), 
                rep(c(rep(1, number$nInd-1), rep(2:number$nMth, each=number$nInd)), 
                    number$nTF),
                rep(1:number$nTF, each=number$nTMF/number$nTF))
  TMF <- paste0("TM", ims)
  
  if(restrictions$structural == "TMF" || restrictions$structural == "both"){
    sTMim1 <- paste0("TM", rep(ims[1:(number$nTMF/number$nTF)], number$nTF-1))
    sTMims <- paste0("TM", ims[(number$nTMF/number$nTF+1):number$nTMF])
      # Prefix "s" = Structural label adaption
  } else {
    sTMim1 <- character(0)
    sTMims <- character(0)
  }
  
  
  if(restrictions$includeOMF == TRUE){
    OMF <- paste0("OM", rep(1:number$nMth, number$nOF), 
                     rep(1:number$nTime, each=number$nMth, times=number$nTF), 
                     rep(1:number$nTF, each=number$nMth*number$nTime))
  
  } else {
    OMF <- character(0)
  }
  
  
  
  
## Difference Variables ##
  
  if(restrictions$structural == "TF" || restrictions$structural == "both"){
    DiffTF <- paste0("Dif_", tail(TF, length(TF)-1))                        
    
  } else {
    DiffTF <- character(0)
  }
  
  
  if(restrictions$structural == "TMF" || restrictions$structural == "both"){
    DiffTMF   <- paste0("Dif_", sTMims)                                   
    
  } else {
    DiffTMF <- character(0)
  }
  

  
  
## Manifest Variable Indices ##
  ind_i <- rep(1:number$nInd, number$nOMF)
  ind_m <- rep(1:number$nMth, each=number$nInd, times=number$nOF)
  ind_t <- rep(1:number$nTime, each=number$nInd*number$nMth, times=number$nTF)
  ind_s <- rep(1:number$nTF, each=number$nInd*number$nMth*number$nTime) 
  
  if(restrictions$meas.invar == "metric.m" || 
     restrictions$meas.invar == "scalar.m" ||         #time & method invariance
     restrictions$meas.invar == "residual.m"){
    ires <- paste0(ind_i, rep("m", number$manifest), ind_s)
    
  } else if(restrictions$meas.invar == "metric.s" || 
            restrictions$meas.invar == "scalar.s" ||  #time & situation invariance
            restrictions$meas.invar == "residual.s"){
    ires <- paste0(ind_i, ind_m, rep("s", number$manifest))
    
  } else if(restrictions$meas.invar == "metric.b" || 
            restrictions$meas.invar == "scalar.b" ||  #time, method & situation invariance
            restrictions$meas.invar == "residual.b"){
    ires <- paste0(ind_i, rep("m", number$manifest), rep("s", number$manifest))
    
  } else {                                            # time invariance (no t index)
    ires <- paste0(ind_i, ind_m, ind_s)
  }
  
  
  
  
## Loadings ##
  
  ref <- function(value){   #replaces specified loadings with 1
    val <- value[seq(1, number$manifest, number$manifest/number$nTF)]
    replace(value, which(value %in% val), 1)
  }
  
  
  
  if(restrictions$equiv.ass$TF != "cong"){
    lambda <- rep("1", number$manifest)
    
  } else {
    lambda <- paste0("lam_", ires)
    lambda <- ref(lambda)
  }
  
  
  
  if(restrictions$equiv.ass$OF != "cong"){
    delta <- rep("1", number$manifest)
    
  } else {
    delta <- paste0("del_", ires)
    delta <- ref(delta)
  }
  
  
  
  if(restrictions$includeOMF == TRUE){ 
    if(restrictions$equiv.ass$OMF != "cong"){
      gamma  <- rep("1", number$manifest-number$nOF)
      cgamma <- rep(c("0", rep("1", number$nInd*number$nMth-1)), number$nOF)
      
    } else {
      gv0 <- seq(1, number$manifest, number$nInd)
      gv1 <- seq(1, number$manifest, number$nInd*number$nMth)
      gv0 <- replace(gv0, which(gv0 %in% gv1), gv1+1)
      
      gamma <- paste0("gam_", ires)
      gamma[gv0] <- 1
      gv2 <- NULL
      for(i in 1:number$nOF){
        gamma_temp <- gamma[(i*(number$nInd*number$nMth)-((number$nInd*number$nMth)-2)):
                              (i*(number$nInd*number$nMth))]
        gv2 <- c(gv2, gamma_temp)
      }
      gamma <- gv2
      
      cgamma <- paste0("gam_", ires)
      cgamma[gv0] <- 1
      cgamma[gv1] <- 0
    }
    
  } else {
    gamma  <- character(0)
    cgamma <- rep("0", number$manifest)
  }
  
  
  
  
## Regression Coefficients ##

  if(restrictions$structural == "TF" || restrictions$structural == "both"){
    beta0 <- paste0("b0_T", 2:number$nTF)
    
  } else {
    beta0 <- character(0)
  }

  
  
  if(restrictions$structural == "TF" || restrictions$structural == "both"){
    TFbeta1 <- paste0("b1_T", 2:number$nTF)
    
  } else {
    TFbeta1 <- character(0)
  }

  
  
  if(restrictions$structural == "TF" || restrictions$structural == "both"){
    TFomega <- paste0("omg_T", 2:number$nTF)
    
  } else {
    TFomega <- character(0)
  }
 
  
  
  if(restrictions$structural == "TMF" || restrictions$structural == "both"){  
    TMFbeta1 <-  paste0("b1_TM", ims[(number$nTMF/number$nTF+1):number$nTMF])
    
  } else {
    TMFbeta1 <- character(0)
  }
  
  
  
  if(restrictions$structural == "TMF" || restrictions$structural == "both"){  
    TMFomega <- paste0("omg_TM", ims[(number$nTMF/number$nTF+1):number$nTMF])
    
  } else {
    TMFomega <- character(0)
  } 
  
  
  
## Intercepts & Means ##
  
  if(any(restrictions$equiv.ass$TF == c("equiv", "par"))){ 
    alpha <- rep("0", number$manifest)
    
  } else {
    alpha <- paste0("alph_", ires)
    val   <- alpha[seq(1, number$manifest, number$manifest / number$nTF)]
    alpha[alpha %in% val] <- 0
  }
  
  
  
  if(restrictions$meanstructure == TRUE){
    MeanTF  <- paste0("M_", TF)
    
  } else {
    MeanTF  <- character(0)
  }
  
  

  
## (Residual) Variances ##
  # Prefix "c" = Coefficient label adaption
  
  VarTF  <- paste0("V_", TF)
  cVarTF <- rep(VarTF, each=number$nInd*number$nMth*number$nTime) 
  
  
  
  VarOF  <- paste0("V_", OF)
  cVarOF <- rep(VarOF, each=number$nInd*number$nMth)
  
  
  
  VarTMF  <- paste0("V_", TMF)
  cVarTMF <- unlist(lapply(1:number$nTF, function(i) {
    TMF_temp <- TMF[((i-1)*(number$nTMF/number$nTF)+1):(i*(number$nTMF/number$nTF))]
    TMF_temp <- paste0("V_", TMF_temp)
    TMF_temp <- c(0, TMF_temp)
    rep(TMF_temp, number$nTime)
  }))
  
  
  if(restrictions$structural == "TMF" || restrictions$structural == "both"){
    sVarTMim1  <- paste0("V_", sTMim1)
    sVarTMims  <- paste0("V_", sTMims)
    
  } else {
    sVarTMim1  <- character(0)
    sVarTMims  <- character(0)
  }
  
  
  
  if(restrictions$includeOMF == TRUE){
    VarOMF  <- paste0("V_", OMF)
    OMF_vec <- seq(1, number$nOMF, number$nMth)
    cVarOMF <- unlist(lapply(1:number$nOMF, function(i) {
      if (i %in% OMF_vec) {
        return(c(0, rep(VarOMF[i], number$nInd - 1)))
      } else {
        return(rep(VarOMF[i], number$nInd))
      }
    }))
    
  } else {
    VarOMF  <- character(0)
    cVarOMF <- rep("0", number$manifest)
  }

  

  equiv_e <- c("ess.par", "par")
  
  if(restrictions$meas.invar == "residual.b"){  
    if(any(restrictions$equiv.ass$TF == equiv_e)){ 
      epsilon <- rep("eps_imts", number$manifest)
    } else {
      epsilon <- paste0("eps_", ind_i, "m", ind_t, "s")
    } 
    
  } else if(restrictions$meas.invar == "residual.s"){  
    if(any(restrictions$equiv.ass$TF == equiv_e)){ 
      epsilon <- rep("eps_imts", number$manifest)
    } else {
      epsilon <- paste0("eps_", ind_i, ind_m, ind_t, "s")
    }
    
  } else if(restrictions$meas.invar == "residual.m"){
    if(any(restrictions$equiv.ass$TF == equiv_e)){ 
      epsilon <- paste0("eps_", "imt", ind_s)
    } else {
      epsilon <- paste0("eps_", ind_i, "m", ind_t, ind_s)
    }
    
  } else {
    if(any(restrictions$equiv.ass$TF == equiv_e)){ 
      epsilon <- paste0("eps_", "imt", ind_s)
    } else {
      epsilon <- paste0("eps_", ind_i, ind_m, ind_t, ind_s)
    }
  } 
  
  
  
  VarY <- paste0("V_", names(data)) 
  
  
  
  
## Covariances ##
  
  if(restrictions$lat.cov$TFcov == TRUE){
    TFpairs <- combn(1:number$nTF, 2) 
    CovTF   <- paste0("Cv_T", TFpairs[1,], "x", TFpairs[2,])
  
  } else {
    CovTF <- character(0)
  }
  
  
 
  if(restrictions$lat.cov$TMFcov == TRUE){
    TMFpairs <- combn(ims, 2) 
    CovTMF   <- paste0("Cv_TM", TMFpairs[1,], "x", TMFpairs[2,])
  
  } else {
    CovTMF <- character(0)
  }
  
  

  occ_pairs <- function(vec, N) {
    len <- length(vec) / N
    result <- vector("list", N)
    
    for (i in 1:N) {
      start <- (i - 1) * len + 1
      end <- i * len
      result[[i]] <- vec[start:end]
    }
    
    pairs_mat <- matrix(ncol = 2, nrow = 0)
    
    for (pos in 1:len) {
      elements <- sapply(result, function(v) v[pos])
      pairs <- t(combn(elements, 2))
      pairs_mat <- rbind(pairs_mat, pairs)
    }
    
    return(pairs_mat)
  }
  
  
  
  if(restrictions$lat.cov$OFcov == TRUE){
    OF_ind <- paste0(rep(1:number$nTime, number$nTF),
                     rep(1:number$nTF, each=number$nTime))
    
    OFpairs <- occ_pairs(OF_ind, number$nSit)
    OFpair1 <- OFpairs[,1]
    OFpair2 <- OFpairs[,2]
    CovOF   <- paste0("Cv_O", OFpair1, "x", OFpair2)
    
  } else {
    CovOF   <- character(0)
    OFpair1 <- character(0)
    OFpair2 <- character(0)
  } 

  
  if(restrictions$includeOMF == TRUE){
    if(restrictions$lat.cov$OMFcov == TRUE){
      OMF_ind <-paste0(rep(1:number$nMth, number$nOF), 
                       rep(1:number$nTime, each=number$nMth, times=number$nTF), 
                       rep(1:number$nTF, each=number$nMth*number$nTime))
      OMFpairs <- occ_pairs(OMF_ind, number$nSit)
      OMFpair1 <- OMFpairs[,1]
      OMFpair2 <- OMFpairs[,2]
      CovOMF   <- paste0("Cv_OM", OMFpair1, "x", OMFpair2)
      
    } else {
      CovOMF   <- character(0)
      OMFpair1 <- character(0)
      OMFpair2 <- character(0)
    }
    
  } else {
    CovOMF   <- character(0)
    OMFpair1 <- character(0)
    OMFpair2 <- character(0)
  }
  
## Coefficients ##
  
  if(restrictions$lat.cov$TFcov == TRUE){  
    CommTF   <- paste0("Com_", TFpairs[2,])                                  
    SitSpeTF <- paste0("SitSp_", TFpairs[2,])                                
    CoefTF   <- paste0("T", TFpairs[1,], " x T", TFpairs[2,]) 
    
  } else if(restrictions$structural == "TF" || restrictions$structural == "both"){
    CommTF   <- paste0("Com_", tail(TF, length(TF)-1))     
    SitSpeTF <- paste0("SitSp_", tail(TF, length(TF)-1))                               
    CoefTF   <- paste0(TF[1], " x ", tail(TF, length(TF)-1))
    
  } else {
    CommTF   <- character(0)
    SitSpeTF <- character(0)
    CoefTF   <- character(0)
  }
  
  
  
  if(restrictions$lat.cov$TMFcov == TRUE){ 
    CommTMF   <- paste0("Com_TM", TMFpairs[2,])                       #TMFpairs[1,], "x", 
    SitSpeTMF <- paste0("SitSp_TM", TMFpairs[2,])                     #TMFpairs[1,], "x",
    CoefTMF   <- paste0("TM", TMFpairs[1,], " x TM", TMFpairs[2,])    
    
  } else if(restrictions$structural == "TMF" || restrictions$structural == "both"){
    CommTMF   <- paste0("Com_", sTMims)                         
    SitSpeTMF <- paste0("SitSp_", sTMims)                       
    CoefTMF   <- paste0(sTMim1, " x ", sTMims)
    
  } else {
    CommTMF   <- character(0)
    SitSpeTMF <- character(0)
    CoefTMF   <- character(0)
  }
  
  
  
  RelY <- paste0("RelY_", names(data))
  ConY <- paste0("ConY_", names(data))
  SpeY <- paste0("SpeY_", names(data))



labels <- list(TF        = TF,
               OF        = OF,
               TMF       = TMF,
               sTMim1    = sTMim1,
               sTMims    = sTMims,
               OMF       = OMF,
               DiffTF    = DiffTF,
               DiffTMF   = DiffTMF,
               lambda    = lambda,
               delta     = delta,
               gamma     = gamma,
               cgamma    = cgamma,
               beta0     = beta0,
               TFbeta1   = TFbeta1,
               TFomega   = TFomega,
               TMFbeta1  = TMFbeta1,
               TMFomega  = TMFomega,
               alpha     = alpha,
               MeanTF    = MeanTF,
               VarTF     = VarTF,
               cVarTF    = cVarTF,
               VarOF     = VarOF,
               cVarOF    = cVarOF,
               VarTMF    = VarTMF,
               cVarTMF   = cVarTMF,
               sVarTMim1 = sVarTMim1,
               sVarTMims = sVarTMims,
               VarOMF    = VarOMF,
               cVarOMF   = cVarOMF,
               epsilon   = epsilon,
               VarY      = VarY,
               CovTF     = CovTF,
               CovTMF    = CovTMF,
               CoefTMF   = CoefTMF,
               CovOF     = CovOF,
               OFpair1   = OFpair1,
               OFpair2   = OFpair2,
               CovOMF    = CovOMF,
               OMFpair1  = OMFpair1, 
               OMFpair2  = OMFpair2,
               CommTF    = CommTF,
               SitSpeTF  = SitSpeTF,
               CommTMF   = CommTMF,
               SitSpeTMF = SitSpeTMF,
               CoefTF    = CoefTF,
               RelY      = RelY,
               ConY      = ConY,
               SpeY      = SpeY)

return(labels)
}

#------------------------------

createCompleteSyntax_mmLSTrf <- function(mod){
  completesyntax <- paste0(createSyntaxLoadingsTF_mmLSTrf(mod), "\n", 
                           createSyntaxLoadingsOF_mmLSTrf(mod), "\n",
                           createSyntaxLoadingsTMF_mmLSTrf(mod), "\n",
                           createSyntaxLoadingsOMF_mmLSTrf(mod), "\n",
                           createSyntaxLoadingsDiffTF_mmLSTrf(mod), "\n",
                           createSyntaxLoadingsDiffTMF_mmLSTrf(mod), "\n",
                           createSyntaxRegTF_mmLSTrf(mod), "\n",
                           createSyntaxBeta0_mmLSTrf(mod), "\n",
                           createSyntaxTFBeta1_mmLSTrf(mod), "\n",
                           createSyntaxTFomega_mmLSTrf(mod), "\n",
                           createSyntaxRegTMF_mmLSTrf(mod), "\n",
                           createSyntaxTMFBeta1_mmLSTrf(mod), "\n",
                           createSyntaxTMFomega_mmLSTrf(mod), "\n",
                           createSyntaxIntercepts_mmLSTrf(mod), "\n",
                           createSyntaxMeanTF_mmLSTrf(mod), "\n",
                           createSyntaxMeanOF_mmLSTrf(mod), "\n",
                           createSyntaxMeanTMF_mmLSTrf(mod), "\n",
                           createSyntaxMeanOMF_mmLSTrf(mod), "\n",
                           createSyntaxVarTF_mmLSTrf(mod), "\n",
                           createSyntaxVarOF_mmLSTrf(mod), "\n",
                           createSyntaxVarTMF_mmLSTrf(mod), "\n",
                           createSyntaxVarOMF_mmLSTrf(mod), "\n",
                           createSyntaxEpsilon_mmLSTrf(mod), "\n",
                           createSyntaxCovTF_mmLSTrf(mod), "\n",
                           createSyntaxCovOF_mmLSTrf(mod), "\n",
                           createSyntaxCovTMF_mmLSTrf(mod), "\n",
                           createSyntaxCovOMF_mmLSTrf(mod), "\n",
                           collapse="")  
  
  completesyntax <- paste0(completesyntax, "\n", 
                           createSyntaxMeanTFs_mmLSTrf(mod), "\n",
                           createSyntaxVarTFs_mmLSTrf(mod), "\n",
                           createSyntaxVarTMFs_mmLSTrf(mod), "\n",
                           createSyntaxVarY_mmLSTrf(mod), "\n",
                           createCoefficientCommTF_mmLSTrf(mod), "\n",
                           createCoefficientSitSpeTF_mmLSTrf(mod), "\n",
                           createCoefficientCommTMF_mmLSTrf(mod), "\n",
                           createCoefficientSitSpeTMF_mmLSTrf(mod), "\n",
                           createCoefficientRelY_mmLSTrf(mod), "\n",
                           createCoefficientConY_mmLSTrf(mod), "\n",
                           createCoefficientSpeY_mmLSTrf(mod), "\n",
                           collapse="")  

  return(completesyntax)
}




## Latent variables as measured by indicators ##

createSyntaxLoadingsTF_mmLSTrf <- function(mod){
  lhs <- rep(mod@names$TF, each=mod@number$manifest/mod@number$nTF)  
  rhs <- paste(mod@labels$lambda, mod@names$manifest, sep="*")
  res <- paste(lhs, "=~", rhs, collapse="\n")  
  return(res)
}



createSyntaxLoadingsOF_mmLSTrf <- function(mod){
  lhs <- rep(mod@names$OF, each=mod@number$manifest/mod@number$nOF)  
  rhs <- paste(mod@labels$delta, mod@names$manifest, sep="*")
  res <- paste(lhs, "=~", rhs, collapse="\n")  
  return(res)
}



createSyntaxLoadingsTMF_mmLSTrf <- function(mod){
  lhs <- unlist(lapply(1:mod@number$nTF, function(i) {
    TMF_temp <- mod@names$TMF[(i*(mod@number$nTMF/mod@number$nTF)-
                                 ((mod@number$nTMF/mod@number$nTF)-1)):
                                (i*(mod@number$nTMF/mod@number$nTF))]
    rep(TMF_temp, mod@number$nTime)
  }))
  
  
  man_vec <- unlist(lapply(1:(mod@number$nTime * mod@number$nTF), function(i) {
    start_ind <- i*(mod@number$manifest/mod@number$nOF)-
                   ((mod@number$manifest/mod@number$nOF)-2)
    end_ind <- i*(mod@number$manifest/mod@number$nOF)
    mod@names$manifest[start_ind:end_ind]
  }))
  
  rhs <- paste("1", man_vec, sep="*")
  res <- paste(lhs, "=~", rhs, collapse="\n")  
  return(res)
}



createSyntaxLoadingsOMF_mmLSTrf <- function(mod){ 

  if(mod@restrictions$includeOMF == TRUE){
    lhs_vec <- seq(1, mod@number$nOMF, mod@number$nMth)
    lhs_reps <- ifelse(1:mod@number$nOMF %in% lhs_vec, mod@number$nInd-1, mod@number$nInd)
    lhs <- rep(mod@labels$OMF, lhs_reps)
    
    man_vec <- seq(1, mod@number$manifest, mod@number$nInd*mod@number$nMth)
    man_var <- unlist(lapply(man_vec, function(i) {
      mod@names$manifest[(i+1):(i+mod@number$nInd*mod@number$nMth-1)]
    }))
    
    rhs <- paste(mod@labels$gamma, man_var, sep="*")
    res <- paste(lhs, "=~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxLoadingsDiffTF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTF  
    rhs <- paste("1", tail(mod@labels$TF, length(mod@labels$TF)-1), sep="*")
    res <- paste(lhs, "=~", rhs, collapse="\n")  
    
  } else {
    res <- character(0)
  }
  
  return(res)
}  



createSyntaxLoadingsDiffTMF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTMF  
    rhs <- paste("1", mod@labels$sTMims, sep="*")
    res <- paste(lhs, "=~", rhs, collapse="\n")  
    
  } else {
    res <- character(0)
  }
  
  return(res)
}  




## Regression ##

createSyntaxRegTF_mmLSTrf <- function(mod){

  if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- tail(mod@labels$TF, length(mod@labels$TF)-1)
    rhs <- paste("1",  rep(mod@labels$TF[1], mod@number$nTF-1), sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
} 



createSyntaxBeta0_mmLSTrf <- function(mod){
  
  if(mod@restrictions$meanstructure == TRUE && 
    (mod@restrictions$structural == "TF" || mod@restrictions$structural == "both")){
      lhs <- mod@labels$DiffTF 
      rhs <- paste(mod@labels$beta0, "1", sep="*")
      res <- paste(lhs, "~", rhs, collapse="\n")
      
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxTFBeta1_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTF 
    rhs <- paste(mod@labels$TFbeta1, mod@names$TF[1], sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxTFomega_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTF 
    rhs <- paste(mod@labels$TFomega, mod@labels$DiffTF, sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}




createSyntaxRegTMF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$sTMims
    rhs <- paste("1", mod@labels$sTMim1, sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
} 



createSyntaxTMFBeta1_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTMF 
    rhs <- paste(mod@labels$TMFbeta1, mod@labels$sTMim1, sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxTMFomega_mmLSTrf <- function(mod){
  
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$DiffTMF 
    rhs <- paste(mod@labels$TMFomega, mod@labels$DiffTMF, sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



##Intercepts & Means##

createSyntaxIntercepts_mmLSTrf <- function(mod){
  
  if(mod@restrictions$meanstructure == TRUE){
    lhs <- mod@names$manifest
    rhs <- paste(mod@labels$alpha, "1", sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxMeanTF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$meanstructure == TRUE){
    lhs <- mod@names$TF
    
    if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
      rhs <- paste(c(mod@labels$MeanTF[1], 
                     rep(0, times=mod@number$nTF-1)), "1", sep="*")
    } else {
      rhs <- paste(mod@labels$MeanTF, "1", sep="*")
    }
    
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxMeanTFs_mmLSTrf <- function(mod){ 
  
  if(mod@restrictions$meanstructure == TRUE && 
    (mod@restrictions$structural == "TF" || mod@restrictions$structural == "both")){
    lhs <- tail(mod@labels$MeanTF, length(mod@labels$MeanTF)-1)
    rhs <- paste0(mod@labels$MeanTF[1], " + (", mod@labels$beta0, " + ", 
                  mod@labels$TFbeta1, "*", mod@labels$MeanTF[1], ")")
    res <- paste(lhs, ":=", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxMeanOF_mmLSTrf <- function(mod){  
  
  if(mod@restrictions$meanstructure == TRUE){
    lhs <- mod@names$OF
    rhs <- paste("0", "1", sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxMeanTMF_mmLSTrf <- function(mod){ 
  
  if(mod@restrictions$meanstructure == TRUE){
    lhs <- mod@names$TMF
    rhs <- paste("0", "1", sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxMeanOMF_mmLSTrf <- function(mod){  
  
  if(mod@restrictions$meanstructure == TRUE && 
     mod@restrictions$includeOMF == TRUE){
    lhs <- mod@names$OMF
    rhs <- paste("0", "1", sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
  return(res)
}




##(Residual) Variances##

createSyntaxVarTF_mmLSTrf <- function(mod){
  lhs <- mod@names$TF
  
  if(mod@restrictions$structural == "TF" || mod@restrictions$structural =="both"){
    rhs <- paste(c(mod@labels$VarTF[1], 
                   rep(0, times=mod@number$nTF-1)), mod@names$TF, sep="*")
    
  } else {
    rhs <- paste(mod@labels$VarTF, mod@names$TF, sep="*")
  }
  
  res <- paste(lhs, "~~", rhs, collapse="\n") 
  return(res)
}



createSyntaxVarTFs_mmLSTrf <- function(mod){
  if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- tail(mod@labels$VarTF, length(mod@labels$VarTF)-1)
    rhs <- paste("(1 + ", mod@labels$TFbeta1, ")^2 *", mod@labels$VarTF[1], 
                 " + ", mod@labels$TFomega)
    res <- paste(lhs, ":=", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxVarOF_mmLSTrf <- function(mod){
  lhs <- mod@names$OF
  rhs <- paste(mod@labels$VarOF, mod@names$OF, sep="*")
  res <- paste(lhs, "~~", rhs, collapse="\n")  
  return(res)
}



createSyntaxVarTMF_mmLSTrf <- function(mod){
  lhs <- mod@names$TMF
  
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    TMF_s1 <- mod@number$nTMF/mod@number$nTF
    rhs <- paste(c(mod@labels$VarTMF[1:TMF_s1], 
                   rep(0, times=TMF_s1*(mod@number$nTF-1))), mod@names$TMF, sep="*")
    
  } else {
    rhs <- paste(mod@labels$VarTMF, mod@names$TMF, sep="*")
  }
  
  res <- paste(lhs, "~~", rhs, collapse="\n")  
  return(res)
}



createSyntaxVarTMFs_mmLSTrf <- function(mod){
  if(mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$sVarTMims
    rhs <- paste("(1 + ", mod@labels$TMFbeta1, ")^2 *", mod@labels$sVarTMim1, 
                 " + ", mod@labels$TMFomega)
    res <- paste(lhs, ":=", rhs, collapse="\n")  
  
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxVarOMF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$includeOMF == TRUE){
    lhs <- mod@names$OMF
    rhs <- paste(mod@labels$VarOMF, mod@names$OMF, sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n")  
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxEpsilon_mmLSTrf <- function(mod){
  lhs <- mod@names$manifest
  rhs <- paste(mod@labels$epsilon, mod@names$manifest, sep="*")
  res <- paste(lhs, "~~", rhs, collapse="\n")  
  return(res)
}



createSyntaxVarY_mmLSTrf <- function(mod){
  lhs <- mod@labels$VarY
  rhs <- paste0("(", mod@labels$lambda,  "^2 * ", mod@labels$cVarTF,  " + ", 
                     mod@labels$delta,   "^2 * ", mod@labels$cVarOF,  " + ",
                     mod@labels$cgamma,  "^2 * ", mod@labels$cVarOMF, " + ",
                     mod@labels$cVarTMF, " + ",   mod@labels$epsilon, ")"   )
  res <- paste(lhs, ":=", rhs, collapse="\n") 
  return(res)
}




##Covariances##

createSyntaxCovTF_mmLSTrf <- function(mod){                           

    if(mod@restrictions$lat.cov$TFcov == TRUE){
    lhs <- rep(mod@names$TF, c((mod@number$nTF-1):0))
    CovTF_vec <- unlist(sapply(2:mod@number$nTF, 
                               function(i) mod@names$TF[i:mod@number$nTF]))
    rhs <- paste(mod@labels$CovTF, CovTF_vec, sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
 return(res)
}



createSyntaxCovOF_mmLSTrf <- function(mod){                           
  
  if(mod@restrictions$lat.cov$OFcov == TRUE){
    lhs <- paste0("O", mod@labels$OFpair1)
    rhs <- paste0(mod@labels$CovOF, "*O", mod@labels$OFpair2)
    res <- paste(lhs, "~~", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createSyntaxCovTMF_mmLSTrf <- function(mod){                          

  if(mod@restrictions$lat.cov$TMFcov == TRUE){
    lhs <- rep(mod@names$TMF, c((mod@number$nTMF-1):0))
    CovTMF_vec <- unlist(sapply(2:mod@number$nTMF, 
                                function(i) mod@names$TMF[i:mod@number$nTMF]))
    rhs <- paste(mod@labels$CovTMF, CovTMF_vec, sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }

  return(res)
}



createSyntaxCovOMF_mmLSTrf <- function(mod){                           
  
  if(mod@restrictions$includeOMF == TRUE &&
     mod@restrictions$lat.cov$OMFcov == TRUE){
    lhs <- paste0("OM", mod@labels$OMFpair1)
    rhs <- paste0(mod@labels$CovOMF, "*OM", mod@labels$OMFpair2)
    res <- paste(lhs, "~~", rhs, collapse="\n") 
    
  } else {
    res <- character(0)
  }
  
  return(res)
}




##Coefficients##

createCoefficientCommTF_mmLSTrf <- function(mod){

  if(mod@restrictions$lat.cov$TFcov == TRUE){
    lhs <- mod@labels$CommTF
    rhs <- paste0("(", mod@labels$CovTF, " / sqrt(" , 
                  mod@labels$VarTF[combn(mod@number$nTF, 2)[1,]], "*", 
                  mod@labels$VarTF[combn(mod@number$nTF, 2)[2,]], "))^2")
    res <- paste(lhs, ":=", rhs, collapse="\n") 
    
    
    } else if(mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
      lhs <- mod@labels$CommTF
      rhs <- paste0("(((1 + ", mod@labels$TFbeta1, ") * ", rep(mod@labels$VarTF[1], 
                    times=mod@number$nTF-1) , ") / sqrt(" , rep(mod@labels$VarTF[1], 
                    times=mod@number$nTF-1), "*", tail(mod@labels$VarTF, 
                    length(mod@labels$VarTF)-1), "))^2")
      res <- paste(lhs, ":=", rhs, collapse="\n")

  } else {
    res <- character(0)
  }
  
  return(res)
}



createCoefficientSitSpeTF_mmLSTrf <- function(mod){

  if(mod@restrictions$lat.cov$TFcov == TRUE ||
     mod@restrictions$structural == "TF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$SitSpeTF
    rhs <- paste0("1 - ", mod@labels$CommTF)
    res <- paste(lhs, ":=", rhs, collapse="\n")
  } else {
    res <- character(0)
  }
  
  return(res)
}



createCoefficientCommTMF_mmLSTrf <- function(mod){

  if(mod@restrictions$lat.cov$TMFcov == TRUE){
    lhs <- mod@labels$CommTMF
    rhs <- paste0("(", mod@labels$CovTMF, " / sqrt(" , 
                  mod@labels$VarTMF[combn(mod@number$nTMF, 2)[1,]], "*", 
                  mod@labels$VarTMF[combn(mod@number$nTMF, 2)[2,]], "))^2")
    res <- paste(lhs, ":=", rhs, collapse="\n")
    
    } else if(mod@restrictions$structural == "TMF" || 
              mod@restrictions$structural == "both"){
      lhs <- mod@labels$CommTMF
      rhs <- paste0("(((1 + ", mod@labels$TMFbeta1, ") * ", mod@labels$sVarTMim1, 
                    ") / sqrt(" , mod@labels$sVarTMim1, "*", mod@labels$sVarTMims, 
                    "))^2")
      res <- paste(lhs, ":=", rhs, collapse="\n")

  } else {
    res <- character(0)
  }
  
  return(res)
}



createCoefficientSitSpeTMF_mmLSTrf <- function(mod){
  
  if(mod@restrictions$lat.cov$TMFcov == TRUE || 
     mod@restrictions$structural == "TMF" || mod@restrictions$structural == "both"){
    lhs <- mod@labels$SitSpeTMF
    rhs <- paste0("1", " - ", mod@labels$CommTMF)
    res <- paste(lhs, ":=", rhs, collapse="\n")
    
  } else {
    res <- character(0)
  }
  
  return(res)
}



createCoefficientConY_mmLSTrf <- function(mod){
  lhs <- mod@labels$ConY
  rhs <- paste0("(",    mod@labels$lambda, "^2 * ", mod@labels$cVarTF, 
                ") / ", mod@labels$VarY)
  res <- paste(lhs, ":=", rhs, collapse="\n")      
  return(res)
}



createCoefficientSpeY_mmLSTrf <- function(mod){
  lhs <- mod@labels$SpeY
  rhs <- paste0("(",    mod@labels$delta, "^2 * ", mod@labels$cVarOMF, 
                ") / ", mod@labels$VarY)
  res <- paste(lhs, ":=", rhs, collapse="\n")      
  return(res)
}



createCoefficientRelY_mmLSTrf <- function(mod){
  lhs <- mod@labels$RelY
  rhs <- paste0("(", mod@labels$lambda,  "^2 * ", mod@labels$cVarTF,  " + ", 
                     mod@labels$delta,   "^2 * ", mod@labels$cVarOF,  " + ",
                     mod@labels$cgamma,  "^2 * ", mod@labels$cVarOMF, " + ",
                     mod@labels$cVarTMF, ") / ",  mod@labels$VarY)
  res <- paste(lhs, ":=", rhs, collapse="\n")      
  return(res)
}


