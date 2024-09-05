

## A General Framework and Software for LST-R Models for Experience Sampling Data


# Maybe do different steps as in testing measurement invariance...
# traitmodel <- c("singletrait", "day-specific", "indicator-specific", "day-and-indicator-specific")
# equiv <- c("invar", "period.invar", "free")

# Test Stan version or Jags version using blavaan
# Look at multilevel SEM and DSEM


############ class definitions and show method ####################

setClass(
  "lstmodel",               
  representation( 
    lstmodel = "list", # input
    number = "list", # number of variables    
    names = "list", # names of variables
    labels = "list", # labels of parameters
    data = "data.frame",
    lavaansyntax = "character",
    lavaanres = "lavaan",
    varcomp = "data.frame"  # results
  )  
)


setMethod ("show", "lstmodel",
           function (object){
             
             print(object@lavaanres)
             cat("\n" ,"\n")
             print(object@varcomp, digits=2)
             cat ("\n ")
           })



######################## main high-level function for users ############################

#' Estimate latent state-trait models for experience sampling data
#' 
#' This function can be used to estimate various latent state-trait models (LST models)
#' for experience sampling data. It is based on the revised version of the LST theory presented in 
#' Steyer, Mayer, Geiser & Cole (2015) and on Eid et al. (2017). The function is a wrapper for
#' lst_models_es_common_trait() and lst_models_es_indicator_specific_trait().
#' 
#' @param traitmodel character. Can be one of c("singletrait", "day-specific", "indicator-specific", "day-and-indicator-specific")
#' @param ntimepoints integer. Number of measurement occasions
#' @param data a data.frame. This data frame contains the observed variables, sorted by time t and then 
#' by indicator i, i.e., Y11, Y21, Y31, ... Y12, Y22, Y32 ... Y15, Y25, Y35 ... etc.
#' @param nperiods integer. Number of periods (trait periods, zeta periods, and epsilon periods).
#' @param equiv character. Equivalence assumption. Can be one of c("invar", "period.invar", "free")
#' @param ar logical. Should autoregressive effects be included
#' @param manifest_thetacovariates an optional vector with variable names of manifest covariates which further explain the latent traits. 
#' Must be assessed at a single occasion.
#' @param ... further arguments passed to lower level functions
#' @return object of class lstmodel
#' @examples
#' m1 <- lst_models_es(traitmodel="singletrait", ntimepoints=9, 
#' data=d_lst_es, nperiods=3, ar=FALSE, equiv="invar")
#' 
#' print(m1)
#' 
#' m2 <- lst_models_es(traitmodel="indicator-specific", ntimepoints=9,
#' data=d_lst_es, nperiods=3, ar=FALSE, equiv="invar")
#' 
#' print(m2)
#' @export
#' @import lavaan
#' @import dplyr
lst_models_es <- 
  function(traitmodel, ntimepoints, data, 
           nperiods=1, equiv="invar", ar=TRUE, 
           manifest_thetacovariates = NULL, 
           ...){
    
    .Deprecated("lsttheory_es") # the new function provides more detailed functionality
    
    res <- NULL
    
    if(ntimepoints %% nperiods != 0){
      stop("ntimepoints must be a multiple of nperiods")
    }
    if((ncol(data) - length(manifest_thetacovariates)) %% ntimepoints != 0){ 
      stop("The number of variables (excluding covariates) is not a multiple of ntimepoints. Does your dataset include variables which are not part of the model?")
    }
    
    if(equiv=="invar"){
      la_t_equiv <- "one"
      la_o_equiv <- "one"
      la_s_equiv <- "time.invar"
      vzeta_eqiv <- "time.invar"
      veps_equiv <- "invar"
      vtheta_equiv <- "indicator.invar"
      nu_equiv <- "zero"
      alpha_equiv <- "zero"
      mtheta_equiv <- "indicator.invar"
      gamma_t_equiv <- "invar"

    }else if(equiv=="period.invar"){
      la_t_equiv <- "period.invar"
      la_o_equiv <- "period.invar"
      la_s_equiv <- "period.invar"
      vzeta_eqiv <- "period.invar"
      veps_equiv <- "period.invar"
      vtheta_equiv <- "indicator.invar"
      nu_equiv <- "period.invar"
      alpha_equiv <- "period.invar"
      mtheta_equiv <- "indicator.invar"
      gamma_t_equiv <- "indicator.invar"

    }else if(equiv=="free"){
      la_t_equiv <- "free"
      la_o_equiv <- "free"
      la_s_equiv <- "free"
      vzeta_eqiv <- "free"
      veps_equiv <- "free"
      vtheta_equiv <- "free"
      nu_equiv <- "free"
      alpha_equiv <- "free"
      mtheta_equiv <- "free"
      gamma_t_equiv <- "free"
    }
    
    ntraitperiods <- nzetaperiods <- nepsperiods <- nperiods
    
    if(traitmodel %in% c("singletrait","indicator-specific")){
      ntraitperiods <- 1 ## overwrite ntraitperiods in constant-trait models
    }
    
    if(!ar){
      la_s_equiv <- "zero" ## overwrite auto-regressive parameter
    }
    
    
    if(traitmodel %in% c("singletrait","day-specific")){
      
      res <- lst_models_es_common_trait(
        ntimepoints=ntimepoints, 
        data=data, 
        ntraitperiods=ntraitperiods, 
        nzetaperiods=nzetaperiods,
        nepsperiods=nepsperiods, 
        la_t_equiv=la_t_equiv, 
        la_o_equiv=la_o_equiv,
        la_s_equiv=la_s_equiv, 
        vzeta_eqiv=vzeta_eqiv, 
        veps_equiv=veps_equiv,
        vtheta_equiv=vtheta_equiv, 
        nu_equiv=nu_equiv, 
        alpha_equiv=alpha_equiv,
        mtheta_equiv=mtheta_equiv, 
        gamma_t_equiv=gamma_t_equiv,
        manifest_thetacovariates = manifest_thetacovariates, 
        ...)
      
    }else if(traitmodel %in% c("indicator-specific","day-and-indicator-specific")){
      
      res <- lst_models_es_indicator_specific_trait(
        ntimepoints=ntimepoints, 
        data=data, 
        ntraitperiods=ntraitperiods, 
        nzetaperiods=nzetaperiods,
        nepsperiods=nepsperiods, 
        la_t_equiv=la_t_equiv, 
        la_o_equiv=la_o_equiv,
        la_s_equiv=la_s_equiv, 
        vzeta_eqiv=vzeta_eqiv, 
        veps_equiv=veps_equiv,
        vtheta_equiv=vtheta_equiv, 
        nu_equiv=nu_equiv, 
        alpha_equiv=alpha_equiv,
        mtheta_equiv=mtheta_equiv,
        gamma_t_equiv=gamma_t_equiv,
        manifest_thetacovariates = manifest_thetacovariates, 
        ...)
    }
    
    return(res)  
  }



###### common trait lower-level function for more experienced users #######

# ## Arguments
# ntimepoints
# data ##  (data format in y11 y21 y31 ... y12 y22 y32, ...)
# ntraitperiods <- 1
# la_t_equiv <- "one" ## c("one","period.invar", "free")
# la_o_equiv <- "one" ## c("one", "time.invar", "period.invar", "free")
# la_s_equiv <- "zero" ## c("zero", "time.invar", "period.invar", "free")
# vzeta_eqiv <- "time.invar" ## c("time.invar", "period.invar", "free")
# veps_equiv <- "invar" ## c("invar", "time.invar", "indicator.invar", "period.invar", "free")
# vtheta_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# nu_equiv <- "zero" ## c("zero", "period.invar", "free")
# alpha_equiv <- "zero" ## c("zero", "period.invar", "free")
# mtheta_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# gamma_t_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# ## TODO maybe cov_equiv

lst_models_es_common_trait <- 
  function(ntimepoints, data, addsyntax = "", ntraitperiods=1, nzetaperiods=1,
           nepsperiods=1, la_t_equiv="one", la_o_equiv="one",
           la_s_equiv="zero", vzeta_eqiv="time.invar", veps_equiv="invar",
           vtheta_equiv="invar", nu_equiv="zero", alpha_equiv="zero",
           mtheta_equiv="invar", gamma_t_equiv="invar",
           manifest_thetacovariates = NULL, ...){
    
    ######## important information ############
    nindicators <- (ncol(data) - length(manifest_thetacovariates) ) / ntimepoints
    nyvariables <- ntimepoints*nindicators
    nyvariables_per_traitperiod <- nyvariables/ntraitperiods
    nyvariables_per_zetaperiod <- nyvariables/nzetaperiods
    nyvariables_per_epsperiod <- nyvariables/nepsperiods
    ntvariables <- ntraitperiods
    ntimepoints_per_traitperiod <- ntimepoints/ntraitperiods
    ntimepoints_per_zetaperiod <- ntimepoints/nzetaperiods
    ntimepoints_per_epsperiod <- ntimepoints/nepsperiods
    
    ######### variable names ##############
    y_variables <- data %>% dplyr::select(!all_of(manifest_thetacovariates)) %>% names()
    eta_variables_full <- paste0("eta", rep(1:ntimepoints, each=nindicators))
    eta_variables_unique <- paste0("eta", 1:ntimepoints)
    o_variables <- paste0("O", 1:ntimepoints)
    zeta_variables <- paste0("zeta", 1:ntimepoints)
    theta_variables_full <- paste0("theta", rep(1:ntvariables, each=ntimepoints_per_traitperiod))
    theta_variables_unique <- unique(theta_variables_full)
    
    # covariates
    manifest_thetacovariates_full <- rep(manifest_thetacovariates, each=length(theta_variables_unique))
    
    ############ parameter names ###########
    
    ## la_o
    # TODO: add indicator.invar
    if(la_o_equiv == "one"){
      la_o <- rep("1", times=nyvariables)
    } else if(la_o_equiv == "time.invar" || la_o_equiv == "invar"){ # should be "invar", but we keep time.invar for backwards compatibility
      if(la_o_equiv == "time.invar"){ 
        message("time.invar is deprecated for la_o_equiv. Please use 'invar' instead.")
      }
      la_o <- rep("la_o", times=nyvariables)
    } else if(la_o_equiv == "period.invar"){
      la_o <- paste0("la_o", 1:nindicators, "_", 
                     rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    } else if(la_o_equiv == "free"){
      la_o <- paste0("la_o", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    } else{stop("the argument la_o_equiv must have one of the following options: 'one', 'invar', 'period.invar', 'free'")}
    
    
    ## la_t
    if(la_t_equiv == "one" || la_t_equiv == "indicator.invar"){
      la_t <- rep("1", times=ntimepoints)
    } else if(la_t_equiv == "period.invar"){
      la_t <- paste0("la_t", rep(1:ntimepoints_per_traitperiod, times=ntraitperiods))
      la_t <- matrix(la_t, nrow=ntimepoints_per_traitperiod)
      la_t[1,] <- "1"
      la_t <- c(la_t)
    } else if(la_t_equiv == "free"){
      la_t <- paste0("la_t", rep(1:ntimepoints_per_traitperiod, times=ntraitperiods),
                     "_", rep(1:ntraitperiods, each=ntimepoints_per_traitperiod))
      la_t <- matrix(la_t, nrow=ntimepoints_per_traitperiod)
      la_t[1,] <- "1"
      la_t <- c(la_t)
    } else{stop("the argument la_t_equiv must have one of the following options: 'one', 'period.invar', 'free'")}
    
    
    ## la_s
    if(la_s_equiv == "zero"){
      la_s <- rep("0", times=ntimepoints-1)
    } else if(la_s_equiv == "time.invar" || la_s_equiv == "invar"){
      if(la_s_equiv == "time.invar"){ 
        message("time.invar is deprecated for la_s_equiv. Please use 'invar' instead.")
      }
      la_s <- rep("la_s", times=ntimepoints-1)
    } else if(la_s_equiv == "overnight" || la_s_equiv == "betweenperiods"){
      la_s <- paste0("la_s", c(1, rep(2, ntimepoints_per_zetaperiod-1)))
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    } else if(la_s_equiv == "period.invar" || la_s_equiv == "interval.invar"){
      la_s <- paste0("la_s", 1:ntimepoints_per_zetaperiod)
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    } else if(la_s_equiv == "free"){
      la_s <- paste0("la_s", 2:ntimepoints)
    } else{stop("the argument la_s_equiv must have one of the following options: 'zero', 'invar', 'overnight', 'interval.invar', 'free'")}
    
    
    ## nu
    if(nu_equiv == "zero"){
      nu <- rep("0", times=nyvariables)
    } else if(nu_equiv == "period.invar"){
      nu <- paste0("nu", 1:nindicators, "_",
                   rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      nu[seq(from=1, to=nyvariables, by=nindicators)] <- "0"
    } else if(nu_equiv == "indicator.invar"){
      nu <- rep(paste0("nu", 1:nindicators), times=ntimepoints)
      nu[seq(from=1, to=nyvariables, by=nindicators)] <- "0"
    } else if(nu_equiv == "free"){
      nu <- paste0("nu", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      nu[seq(from=1, to=nyvariables, by=nindicators)] <- "0"
    } else{stop("the argument nu_equiv must have one of the following options: 'zero', 'period.invar', 'indicator.invar', 'free'")}
    
    
    ## alpha
    if(alpha_equiv == "zero" || alpha_equiv == "indicator.invar"){
      alpha <- rep("0", times=ntimepoints)
    } else if(alpha_equiv == "period.invar"){
      alpha <- rep(paste0("alpha", 1:ntimepoints_per_traitperiod), times=ntraitperiods)
      alpha[seq(from=1, to=ntimepoints, by=ntimepoints_per_traitperiod)] <- "0"
    } else if(alpha_equiv == "free"){
      alpha <- paste0("alpha", 1:ntimepoints_per_traitperiod, "_", 
                      rep(1:ntraitperiods, each=ntimepoints_per_traitperiod))
      alpha[seq(from=1, to=ntimepoints, by=ntimepoints_per_traitperiod)] <- "0"
    } else{stop("the argument alpha_equiv must have one of the following options: 'zero', 'indicator.invar', 'free'")}
    
    
    ## mtheta
    if(mtheta_equiv == "invar" || mtheta_equiv == "indicator.invar"){
      mtheta <- rep("mtheta", times=ntvariables)
    } else if(mtheta_equiv == "free"){
      mtheta <- paste0("mtheta", 1:ntvariables)
    } else{stop("the argument nu_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    ## vzeta
    if(vzeta_eqiv == "time.invar" || vzeta_eqiv == "invar"){
      if(vzeta_eqiv == "time.invar"){ #TODO add for other options where time.invar is replaced
        message("time.invar is deprecated for vzeta_eqiv. Please use 'invar' instead.")
      }
      vzeta <- rep("vzeta", times=ntimepoints)
    } else if(vzeta_eqiv == "period.invar"){
      vzeta <- rep(paste0("vzeta", 1:nzetaperiods), each=ntimepoints_per_zetaperiod)
    } else if(vzeta_eqiv == "free"){
      vzeta <- paste0("vzeta", 1:ntimepoints)   
    } else{stop("the argument vzeta_eqiv must have one of the following options: 'invar', 'period.invar', 'free'")}
    
    
    ## veps
    if(veps_equiv == "invar"){
      veps <- rep("veps", times=nyvariables)
    } else if(veps_equiv == "time.invar"){
      veps <- rep(paste0("veps", 1:ntimepoints), each=nindicators)
    } else if(veps_equiv == "indicator.invar"){
      veps <- rep(paste0("veps", 1:nindicators), times=ntimepoints)
    } else if(veps_equiv == "period.invar"){
      veps <- rep(paste0("veps", 1:nepsperiods), each=nyvariables_per_epsperiod)
    } else if(veps_equiv == "free"){
      veps <- paste0("veps", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
    } else{stop("the argument veps_equiv must have one of the following options: 'invar', 'time.invar', 'period.invar', 'indicator.invar', 'free'")}
    
    
    ## vtheta
    if(vtheta_equiv == "invar" || vtheta_equiv == "indicator.invar"){
      vtheta <- rep("vtheta", times=ntvariables)
    } else if(vtheta_equiv == "free"){
      vtheta <- paste0("vtheta", 1:ntvariables)
    } else{stop("the argument vtheta_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    ## gamma_t (regression coefficients for covariates explaining the latent traits)
    if(gamma_t_equiv == "invar" || gamma_t_equiv == "indicator.invar"){
      gamma_t <- paste0("gamma_t", "c", rep(1:length(manifest_thetacovariates), each=ntraitperiods))
    } else if(gamma_t_equiv == "free"){
      gamma_t <- paste0("gamma_t", rep(1:ntraitperiods, length(manifest_thetacovariates)),
                        "c", rep(1:length(manifest_thetacovariates), each=ntraitperiods))
    } else{warning("the argument gamma_t_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    ############# generate syntax ##################
    
    ## loadings
    tmp1 <- paste0(eta_variables_full, " =~ ", la_o, "*", y_variables, collapse="\n")
    tmp2 <- paste0(theta_variables_full, " =~ ", la_t, "*", eta_variables_unique, collapse="\n")
    tmp3 <- paste0(o_variables, " =~ ", "1*", eta_variables_unique, collapse="\n")
    tmp4 <- paste0(zeta_variables, " =~ ", "1*", o_variables, collapse="\n")
    
    ## intercepts and means
    tmp5 <- paste0(y_variables, " ~ ", nu, "*1", collapse="\n")
    tmp6 <- paste0(eta_variables_unique, " ~ ", alpha, "*1", collapse="\n")
    tmp7 <- paste0(theta_variables_unique, " ~ ", mtheta, "*1", collapse="\n")
    
    ## variances
    tmp8 <- paste0(y_variables, " ~~ ", veps, "*" ,y_variables, collapse="\n")
    tmp9 <- paste0(zeta_variables, " ~~ ", vzeta, "*", zeta_variables, collapse="\n")
    tmp10 <- paste0(theta_variables_unique, " ~~ ", vtheta, "*", 
                    theta_variables_unique, collapse="\n")
    
    ## covariances
    if(ntvariables == 1){
      tmp11 <- ""
    }
    if(ntvariables > 1){
      tmp11 <- apply(combn(theta_variables_unique, 2), 2, function(x){paste0(x, collapse=" ~~ ")})
      tmp11 <- paste0(tmp11, collapse="\n")
    }
    
    ## autoregressive effects
    tmp12 <- paste0(o_variables[-1], " ~ ", la_s , "*", 
                    o_variables[-length(o_variables)], collapse="\n")
    
    ## theta covariates
    if(is.null(manifest_thetacovariates)){
      tmp13 <- ""
    }else{
      tmp13 <- paste0(theta_variables_unique, " ~ ", gamma_t, "*", manifest_thetacovariates_full, collapse="\n")
    }
    
    ############# fit model ##################
    model <- paste0(tmp1, "\n", tmp2, "\n", tmp3, "\n",
                    tmp4, "\n", tmp5, "\n", tmp6, "\n",
                    tmp7, "\n", tmp8, "\n", tmp9, "\n",
                    tmp10, "\n", tmp11, "\n", tmp12, "\n",
                    tmp13, "\n", addsyntax, "\n",
                    collapse="\n")
    
    m1 <- lavaan(model, data=data, ...)
    
    ######## compute variance components ###########
    
    ## rel
    vary_fitted <- unlist(diag(lavInspect(m1, "fitted")$cov))[y_variables]
    veps_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("veps", label), select=est))
    rel_fitted <- 1 - veps_fitted / vary_fitted
    names(rel_fitted) <- paste0("rel", y_variables)
    
    ## spe
    la_o_fitted <- unlist(subset(parameterEstimates(m1), 
                                 subset=lhs %in% eta_variables_full & op=="=~", 
                                 select=est))
    vzeta_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("vzeta", label), select=est))
    vzeta_fitted <- rep(vzeta_fitted, each=nindicators)
    spe_fitted <- la_o_fitted^2 * vzeta_fitted / vary_fitted
    names(spe_fitted) <- paste0("spe", y_variables)
    
    ## con
    con_fitted <- rel_fitted - spe_fitted
    names(con_fitted) <- paste0("con", y_variables)
    
    # Pred
    la_t_fitted <- unlist(subset(parameterEstimates(m1), 
                                 subset=lhs %in% theta_variables_full & op=="=~", 
                                 select=est))
    la_t_fitted <- rep(la_t_fitted, each=nindicators)
    vtheta_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("vtheta", label), select=est))
    vtheta_fitted <- rep(vtheta_fitted, each=nyvariables_per_traitperiod)
    pred_fitted <- la_o_fitted^2 * la_t_fitted^2 * vtheta_fitted / vary_fitted
    names(pred_fitted) <- paste0("pred", y_variables)
    
    # UPred
    upred_fitted <- con_fitted - pred_fitted
    upred_fitted <- round(upred_fitted, digits=10) 
    # why round? the calculation of con and pred differ differ in the 17th or so digit, although they are by definition equal for the first value (for y_11). This may give the impression of negative values for upred. Slight rounding takes this issue away. 
    names(upred_fitted) <- paste0("upred", y_variables)
    
    ## summarize
    variance_components <- 
      data.frame(y=y_variables,
                 rel=rel_fitted,
                 spe=spe_fitted,
                 con=con_fitted,
                 pred=pred_fitted,
                 upred=upred_fitted,
                 indicator=rep(1:nindicators, times=ntimepoints),
                 timepoint=rep(1:ntimepoints, each=nindicators),
                 traitperiod=rep(1:ntraitperiods, each=nyvariables_per_traitperiod))
    row.names(variance_components) <- NULL
    
    res <- new("lstmodel",
               data = data,
               lavaansyntax = model,
               lavaanres = m1,
               varcomp = variance_components)
    
    return(res)
  }


###### indicator-specific trait lower-level function for more experienced users #######


########## TODO
## streng genommen m?sste es auch f?r la_o parameters eigene periods geben...
## aber vielleicht nehmen wir da einfach auch die zetaperiods daf?r (das macht wahrscheinlich Sinn)
## nur bei den nu nochmal ?berlegen wegen periods



# ## Arguments
# ntimepoints
# data ##  (data format in y11 y21 y31 ... y12 y22 y32, ...)
# ntraitperiods <- 1
# la_t_equiv <- "one" ## c("one","period.invar", "free")
# la_o_equiv <- "one" ## c("one", "time.invar", "period.invar", "free")
# la_s_equiv <- "zero" ## c("zero", "time.invar", "period.ivar", "free")
# vzeta_eqiv <- "time.invar" ## c("time.invar", "period.invar", "free")
# veps_equiv <- "invar" ## c("invar", "time.invar", "indicator.invar", "period.invar", "free")
# vtheta_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# nu_equiv <- "zero" ## c("zero", "period.invar", "free")
# alpha_equiv <- "zero" ## c("zero", "period.invar", "free")
# mtheta_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# gamma_t_equiv <- "invar" ## c("invar", "indicator.invar", "free")
# ## TODO maybe cov_equiv


lst_models_es_indicator_specific_trait <- 
  function(ntimepoints, data, addsyntax = "", ntraitperiods=1, nzetaperiods=1,
           nepsperiods=1, la_t_equiv="one", la_o_equiv="one",
           la_s_equiv="zero", vzeta_eqiv="time.invar", veps_equiv="invar",
           vtheta_equiv="invar", nu_equiv="zero", alpha_equiv="zero",
           mtheta_equiv="invar", gamma_t_equiv="invar", 
           manifest_thetacovariates = NULL, ...){
    
    ######## important information ############
    nindicators <- (ncol(data) - length(manifest_thetacovariates)) / ntimepoints
    nyvariables <- ntimepoints*nindicators
    nyvariables_per_traitperiod <- nyvariables/ntraitperiods
    nyvariables_per_zetaperiod <- nyvariables/nzetaperiods
    nyvariables_per_epsperiod <- nyvariables/nepsperiods
    ntvariables <- nindicators*ntraitperiods
    ntimepoints_per_traitperiod <- ntimepoints/ntraitperiods
    ntimepoints_per_zetaperiod <- ntimepoints/nzetaperiods
    ntimepoints_per_epsperiod <- ntimepoints/nepsperiods
    
    ######### variable names ##############
    y_variables <- data %>% dplyr::select(!all_of(manifest_thetacovariates)) %>% names()
    o_variables_full <- paste0("O", rep(1:ntimepoints, each=nindicators))
    o_variables_unique <- paste0("O", 1:ntimepoints)
    zeta_variables_full <- paste0("zeta", rep(1:ntimepoints, each=nindicators))
    zeta_variables_unique <- unique(zeta_variables_full)
    theta_variables_full <- paste0("theta", 1:nindicators, "_", 
                                   rep(1:ntraitperiods, each=nyvariables_per_traitperiod))
    theta_variables_unique <- unique(theta_variables_full)
    
    # covariates
    manifest_thetacovariates_full <- rep(manifest_thetacovariates, each=length(theta_variables_unique))
    
    
    ############ parameter names ###########
    
    ## la_o
    # TODO: add indicator.invar
    if(la_o_equiv == "one"){
      la_o <- rep("1", times=nyvariables)
    } else if(la_o_equiv == "time.invar" || la_o_equiv == "invar"){ # should be "invar", time.invar is kept for compatibility
      if(la_o_equiv == "time.invar"){ 
        message("time.invar is deprecated for la_o_equiv. Please use 'invar' instead.")
      }
      la_o <- rep("la_o", times=nyvariables)
    } else if(la_o_equiv == "period.invar"){
      la_o <- paste0("la_o", 1:nindicators, "_", 
                     rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    } else if(la_o_equiv == "free"){
      la_o <- paste0("la_o", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    } else{stop("the argument la_o_equiv must have one of the following options: 'one', 'invar', 'period.invar', 'free'")}
    
    
    
    ## la_t
    
    if(la_t_equiv == "one"){
      la_t <- rep("1", times=nyvariables)
    } else if(la_t_equiv == "indicator.invar"){
      la_t <- rep(paste0("la_t", 1:nindicators), times=ntimepoints)
      la_t[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    } else if(la_t_equiv == "period.invar"){
      la_t <- paste0("la_t", 1:nindicators, "_", 
                     rep(rep(1:ntimepoints_per_traitperiod, each=nindicators), times=ntraitperiods))
      la_t <- matrix(la_t, nrow=nyvariables_per_traitperiod)
      la_t[1:nindicators,] <- "1"
      la_t <- c(la_t)
    } else if(la_t_equiv == "free"){
      la_t <- paste0("la_t", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_t <- matrix(la_t, nrow=nyvariables_per_traitperiod)
      la_t[1:nindicators,] <- "1"
      la_t <- c(la_t)
    } else{stop("the argument la_t_equiv must have one of the following options: 'one', 'period.invar', 'indicator.invar', 'free'")}
    
    
    ## la_s
    if(la_s_equiv == "zero"){
      la_s <- rep("0", times=ntimepoints-1)
    } else if(la_s_equiv == "time.invar" || la_s_equiv == "invar"){ # should be "invar", time.invar is kept for compatibility
      if(la_s_equiv == "time.invar"){ 
        message("time.invar is deprecated for la_s_equiv. Please use 'invar' instead.")
      }
      la_s <- rep("la_s", times=ntimepoints-1)
    } else if(la_s_equiv == "overnight" || la_s_equiv == "betweenperiods"){
      la_s <- paste0("la_s", c(1, rep(2, ntimepoints_per_zetaperiod-1)))
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    } else if(la_s_equiv == "period.invar" || la_s_equiv == "interval.invar"){
      la_s <- paste0("la_s", 1:ntimepoints_per_zetaperiod)
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    } else if(la_s_equiv == "free"){
      la_s <- paste0("la_s", 2:ntimepoints)
    } else{stop("the argument la_s_equiv must have one of the following options: 'zero', 'invar', 'overnight', 'interval.invar', 'free'")}
    
    
    ## nu
    if(nu_equiv == "zero"){
      nu <- rep("0", times=nyvariables)
    } else if(nu_equiv == "indicator.invar"){
      nu <- rep(paste0("nu", 1:nindicators), times=ntimepoints)
      nu[1:nindicators,] <- "0"
    } else if(nu_equiv == "period.invar"){
      nu <- paste0("nu", 1:nindicators, "_", 
                   rep(rep(1:ntimepoints_per_traitperiod, each=nindicators), times=ntraitperiods))
      nu <- matrix(nu, nrow=nyvariables_per_traitperiod)
      nu[1:nindicators,] <- "0"
      nu <- c(nu)
    } else if(nu_equiv == "free"){
      nu <- paste0("nu", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      nu <- matrix(nu, nrow=nyvariables_per_traitperiod)
      nu[1:nindicators,] <- "0"
      nu <- c(nu)
    } else{stop("the argument nu_equiv must have one of the following options: 'zero', 'period.invar', 'indicator.invar', 'free'")}
    
    
    ## mtheta
    if(mtheta_equiv == "invar"){
      mtheta <- rep("mtheta", times=ntvariables)
    } else if(mtheta_equiv == "indicator.invar"){
      mtheta <- rep(paste0("mtheta", 1:nindicators), times=ntraitperiods)
    } else if(mtheta_equiv == "free"){
      mtheta <- paste0("mtheta", 1:nindicators, "_", rep(1:ntraitperiods, each=nindicators))
    } else{stop("the argument mtheta_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    ## vzeta
    if(vzeta_eqiv == "time.invar" || vzeta_eqiv == "invar"){ # time.invar is kept for compatibility
      if(vzeta_eqiv == "time.invar"){ 
        message("time.invar is deprecated for vzeta_eqiv. Please use 'invar' instead.")
      }
      vzeta <- rep("vzeta", times=ntimepoints)
    } else if(vzeta_eqiv == "period.invar"){
      vzeta <- rep(paste0("vzeta", 1:nzetaperiods), each=ntimepoints_per_zetaperiod)
    } else if(vzeta_eqiv == "free"){
      vzeta <- paste0("vzeta", 1:ntimepoints)   
    } else{stop("the argument vzeta_eqiv must have one of the following options: 'invar', 'period.invar', 'free'")}
    
    
    ## veps
    if(veps_equiv == "invar"){
      veps <- rep("veps", times=nyvariables)
    } else if(veps_equiv == "time.invar"){
      veps <- rep(paste0("veps", 1:ntimepoints), each=nindicators)
    } else if(veps_equiv == "indicator.invar"){
      veps <- rep(paste0("veps", 1:nindicators), times=ntimepoints)
    } else if(veps_equiv == "period.invar"){
      veps <- rep(paste0("veps", 1:nepsperiods), each=nyvariables_per_epsperiod)
    } else if(veps_equiv == "free"){
      veps <- paste0("veps", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
    } else{stop("the argument veps_equiv must have one of the following options: 'invar', 'time.invar', 'period.invar', 'indicator.invar', 'free'")}
    
    
    ## vtheta
    if(vtheta_equiv == "invar"){
      vtheta <- rep("vtheta", times=ntvariables)
    } else if(vtheta_equiv == "indicator.invar"){
      vtheta <- rep(paste0("vtheta", 1:nindicators), times=ntraitperiods)
    } else if(vtheta_equiv == "free"){
      vtheta <- paste0("vtheta", 1:nindicators, "_", rep(1:ntraitperiods, each=nindicators))
    } else{stop("the argument vtheta_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    ## gamma_t (regression coefficients for covariates explaining the latent traits)
    if(gamma_t_equiv == "invar"){
      gamma_t <- paste0("gamma_t", "c", rep(1:length(manifest_thetacovariates), each=ntvariables)) # okay!
    } else if(gamma_t_equiv == "indicator.invar"){
      gamma_t <- paste0("gamma_t", 1:nindicators, "c", rep(1:length(manifest_thetacovariates), each=ntvariables)) # okay i think
    } else if(gamma_t_equiv == "free"){
      gamma_t <- paste0("gamma_t", 1:nindicators, #"_", 
                        rep(1:ntraitperiods, each=nindicators),
                        "c", rep(1:length(manifest_thetacovariates), each=ntvariables)) # each=ntvariables?
    } else{warning("the argument gamma_t_equiv must have one of the following options: 'invar', 'indicator.invar', 'free'")}
    
    
    
    ############# generate syntax ##################
    
    ## loadings
    tmp1 <- paste0(o_variables_full, " =~ ", la_o, "*", y_variables, collapse="\n")
    tmp2 <- paste0(theta_variables_full, " =~ ", la_t, "*", y_variables, collapse="\n")
    tmp3 <- paste0(zeta_variables_unique, " =~ ", "1*", o_variables_unique, collapse="\n")
    
    ## intercepts
    tmp4 <- paste0(y_variables, " ~ ", nu, "*1", collapse="\n")
    tmp5 <- paste0(theta_variables_unique, " ~ ", mtheta, "*1", collapse="\n")
    
    ## variances
    tmp6 <- paste0(y_variables, " ~~ ", veps, "*" ,y_variables, collapse="\n")
    tmp7 <- paste0(zeta_variables_unique, " ~~ ", vzeta, "*", zeta_variables_unique, collapse="\n")
    tmp8 <- paste0(theta_variables_unique, " ~~ ", vtheta, "*", 
                   theta_variables_unique, collapse="\n")
    
    ## covariances
    tmp9 <- apply(combn(theta_variables_unique, 2), 2, function(x){paste0(x, collapse=" ~~ ")})
    tmp9 <- paste0(tmp9, collapse="\n")
    
    ## autoregressive effects
    tmp10 <- paste0(o_variables_unique[-1], " ~ ", la_s , "*", 
                    o_variables_unique[-length(o_variables_unique)], collapse="\n")
    
    ## theta covariates
    if(is.null(manifest_thetacovariates)){
      tmp11 <- ""
    }else{
      tmp11 <- paste0(theta_variables_unique, " ~ ", gamma_t, "*", manifest_thetacovariates_full, collapse="\n")
    }
  
    
    ############# fit model ##################
    model <- paste0(tmp1, "\n", tmp2, "\n", tmp3, "\n",
                    tmp4, "\n", tmp5, "\n", tmp6, "\n",
                    tmp7, "\n", tmp8, "\n", tmp9, "\n",
                    tmp10, "\n", tmp11, "\n", addsyntax, "\n",
                    collapse="\n")
    
    m1 <- lavaan(model, data=data, ...)
    
    
    ######## compute variance components ###########
    
    ## rel
    vary_fitted <- unlist(diag(lavInspect(m1, "fitted")$cov))[y_variables]
    veps_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("veps", label), select=est))
    rel_fitted <- 1 - veps_fitted / vary_fitted
    names(rel_fitted) <- paste0("rel", y_variables)
    
    ## spe
    la_o_fitted <- unlist(subset(parameterEstimates(m1), 
                                 subset=lhs %in% o_variables_full & op=="=~", 
                                 select=est))
    vzeta_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("vzeta", label), select=est))
    vzeta_fitted <- rep(vzeta_fitted, each=nindicators)
    spe_fitted <- la_o_fitted^2 * vzeta_fitted / vary_fitted
    names(spe_fitted) <- paste0("spe", y_variables)
    
    ## con
    con_fitted <- rel_fitted - spe_fitted
    names(con_fitted) <- paste0("con", y_variables)
    
    # Pred
    la_t_fitted <- unlist(subset(parameterEstimates(m1), 
                                 subset=lhs %in% theta_variables_full & op=="=~", 
                                 select=est))
    vtheta_fitted <- unlist(subset(parameterEstimates(m1), subset=grepl("vtheta", label), select=est))
    vtheta_per_traitperiod <- split(vtheta_fitted, ceiling(seq_along(vtheta_fitted) / nindicators))
    vtheta_per_traitperiod <- lapply(vtheta_per_traitperiod, function(x) rep(x, times=ntimepoints/ntraitperiods))
    vtheta_fitted <- unlist(vtheta_per_traitperiod)
    pred_fitted <- la_t_fitted^2 * vtheta_fitted / vary_fitted
    names(pred_fitted) <- paste0("pred", y_variables)
    
    # UPred
    upred_fitted <- con_fitted - pred_fitted
    upred_fitted <- round(upred_fitted, digits=10) 
    # why round? the calculation of con and pred differ differ in the 17th or so digit, although they are by definition equal for the first value (for y_11). This may give the impression of negative values for upred. Slight rounding takes this issue away. 
    names(upred_fitted) <- paste0("upred", y_variables)
    
    ## summarize
    variance_components <- 
      data.frame(y=y_variables,
                 rel=rel_fitted,
                 spe=spe_fitted,
                 con=con_fitted,
                 pred=pred_fitted,
                 upred=upred_fitted,
                 indicator=rep(1:nindicators, times=ntimepoints),
                 timepoint=rep(1:ntimepoints, each=nindicators),
                 traitperiod=rep(1:ntraitperiods, each=nyvariables_per_traitperiod))
    row.names(variance_components) <- NULL
    
    res <- new("lstmodel",
               data = data,
               lavaansyntax = model,
               lavaanres = m1,
               varcomp = variance_components)
    
    return(res)
  }



#' Dataset d_lst_es.
#' 
#' A simulated dataset to test experience sampling LST models. The data includes 3 indicators, assessed 3 times a day on 3 days. It is based on a model with day-specific traits. The variables are:
#' 
#' \itemize{
#'   \item y11 
#'   \item ...
#'   \item ...
#'   \item y39
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 1000 rows and 27 variables
#' @name d_lst_es
NULL



############## namespace ###########

#' @importFrom methods new is
NULL

#' @importMethodsFrom methods show 
NULL

#' @importFrom stats as.formula ftable model.frame model.matrix pnorm relevel var qnorm cov lm mahalanobis pchisq pf pt sd
NULL

#' @importFrom utils capture.output read.csv read.csv2 read.table combn
NULL

