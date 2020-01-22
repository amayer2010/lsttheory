

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

# traitmodel <- c("singletrait", "day-specific", "indicator-specific", "day-and-indicator-specific")
# equiv <- c("invar", "period.invar", "free")


#' Estimate latent state-trait models für experience sampling data
#' 
#' This function can be used to estimate various latent state-trait models (LST models)
#' for experience sampling data. It is based on the revised version of the LST theory presented in 
#' Steyer, Mayer, Geiser & Cole (2015) and on Eid et al. (2017). The function is a wrapper for
#' lst_models_es_common_trait() and lst_models_es_indicator_specific_trait().
#' 
#' @param neta integer. Number of latent state variables eta.
#' @param addsyntax character string. Will be added to generated lavaan syntax. 
#' @param ... further arguments passed to lavaan::sem().
#' @return object of class LSTModel.
#' @references 
#' @examples 
#' @export
#' @import lavaan
lst_models_es <- 
  function(traitmodel, ntimepoints, data, 
           nperiods=1, equiv="invar", ar=TRUE, ...){
    
    res <- NULL
    
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
        mtheta_equiv=mtheta_equiv, ...)
      
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
        mtheta_equiv=mtheta_equiv, ...)
      
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
# ## TODO maybe cov_equiv

lst_models_es_common_trait <- 
  function(ntimepoints, data, ntraitperiods=1, nzetaperiods=1,
           nepsperiods=1, la_t_equiv="one", la_o_equiv="one",
           la_s_equiv="zero", vzeta_eqiv="time.invar", veps_equiv="invar",
           vtheta_equiv="invar", nu_equiv="zero", alpha_equiv="zero",
           mtheta_equiv="invar", ...){
    
    ######## important information ############
    nindicators <- ncol(data)/ntimepoints
    nyvariables <- ntimepoints*nindicators
    nyvariables_per_traitperiod <- nyvariables/ntraitperiods
    nyvariables_per_zetaperiod <- nyvariables/nzetaperiods
    nyvariables_per_epsperiod <- nyvariables/nepsperiods
    ntvariables <- ntraitperiods
    ntimepoints_per_traitperiod <- ntimepoints/ntraitperiods
    ntimepoints_per_zetaperiod <- ntimepoints/nzetaperiods
    ntimepoints_per_epsperiod <- ntimepoints/nepsperiods
    
    ######### variable names ##############
    y_variables <- names(data)
    eta_variables_full <- paste0("eta", rep(1:ntimepoints, each=nindicators))
    eta_variables_unique <- paste0("eta", 1:ntimepoints)
    o_variables <- paste0("O", 1:ntimepoints)
    zeta_variables <- paste0("zeta", 1:ntimepoints)
    theta_variables_full <- paste0("theta", rep(1:ntvariables, each=ntimepoints_per_traitperiod))
    theta_variables_unique <- unique(theta_variables_full)
    
    ############ parameter names ###########
    
    ## la_o
    if(la_o_equiv == "one"){
      la_o <- rep("1", times=nyvariables)
    }
    if(la_o_equiv == "time.invar"){
      la_o <- rep("la_o", times=nyvariables)
    }
    if(la_o_equiv == "period.invar"){
      la_o <- paste0("la_o", 1:nindicators, "_", 
                     rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    }
    if(la_o_equiv == "free"){
      la_o <- paste0("la_o", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    }
    
    
    ## la_t
    if(la_t_equiv == "one"){
      la_t <- rep("1", times=ntimepoints)
    }
    if(la_t_equiv == "period.invar"){
      la_t <- paste0("la_t", rep(1:ntimepoints_per_traitperiod, times=ntraitperiods))
      la_t <- matrix(la_t, nrow=ntimepoints_per_traitperiod)
      la_t[1,] <- "1"
      la_t <- c(la_t)
    }
    if(la_t_equiv == "free"){
      la_t <- paste0("la_t", rep(1:ntimepoints_per_traitperiod, times=ntraitperiods),
                     "_", rep(1:ntraitperiods, each=ntimepoints_per_traitperiod))
      la_t <- matrix(la_t, nrow=ntimepoints_per_traitperiod)
      la_t[1,] <- "1"
      la_t <- c(la_t)
    }
    
    
    ## la_s
    if(la_s_equiv == "zero"){
      la_s <- rep("0", times=ntimepoints-1)
    }
    if(la_s_equiv == "time.invar"){
      la_s <- rep("la_s", times=ntimepoints-1)
    }
    if(la_s_equiv == "period.invar"){
      la_s <- paste0("la_s", 1:ntimepoints_per_zetaperiod)
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    }
    if(la_s_equiv == "free"){
      la_s <- paste0("la_s", 2:ntimepoints)
    }
    
    ## nu
    if(nu_equiv == "zero"){
      nu <- rep("0", times=nyvariables)
    }
    if(nu_equiv == "period.invar"){
      nu <- paste0("nu", 1:nindicators, "_",
                   rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      nu[seq(from=1, to=nyvariables, by=nindicators)] <- "0"
    }
    if(nu_equiv == "free"){
      nu <- paste0("nu", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      nu[seq(from=1, to=nyvariables, by=nindicators)] <- "0"
    }
    
    ## alpha
    if(alpha_equiv == "zero"){
      alpha <- rep("0", times=ntimepoints)
    }
    if(alpha_equiv == "period.invar"){
      alpha <- rep(paste0("alpha", 1:ntimepoints_per_traitperiod), times=ntraitperiods)
      alpha[seq(from=1, to=ntimepoints, by=ntimepoints_per_traitperiod)] <- "0"
    }
    if(alpha_equiv == "free"){
      alpha <- paste0("alpha", 1:ntimepoints_per_traitperiod, "_", 
                      rep(1:ntraitperiods, each=ntimepoints_per_traitperiod))
      alpha[seq(from=1, to=ntimepoints, by=ntimepoints_per_traitperiod)] <- "0"
    }
    
    ## mtheta
    if(mtheta_equiv == "invar" || mtheta_equiv == "indicator.invar"){
      mtheta <- rep("mtheta", times=ntvariables)
    }
    if(mtheta_equiv == "free"){
      mtheta <- paste0("mtheta", 1:ntvariables)
    }
    
    ## vzeta
    if(vzeta_eqiv == "time.invar"){
      vzeta <- rep("vzeta", times=ntimepoints)
    }
    if(vzeta_eqiv == "period.invar"){
      vzeta <- rep(paste0("vzeta", 1:nzetaperiods), each=ntimepoints_per_zetaperiod)
    }
    if(vzeta_eqiv == "free"){
      vzeta <- paste0("vzeta", 1:ntimepoints)   
    }
    
    ## veps
    if(veps_equiv == "invar"){
      veps <- rep("veps", times=nyvariables)
    }
    if(veps_equiv == "time.invar"){
      veps <- rep(paste0("veps", 1:ntimepoints), each=nindicators)
    }
    if(veps_equiv == "indicator.invar"){
      veps <- rep(paste0("veps", 1:nindicators), times=ntimepoints)
    }
    if(veps_equiv == "period.invar"){
      veps <- rep(paste0("veps", 1:nepsperiods), each=nyvariables_per_epsperiod)
    }
    if(veps_equiv == "free"){
      veps <- paste0("veps", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
    }
    
    
    ## vtheta
    if(vtheta_equiv == "invar" || vtheta_equiv == "indicator.invar"){
      vtheta <- rep("vtheta", times=ntvariables)
    }
    if(vtheta_equiv == "free"){
      vtheta <- paste0("vtheta", 1:ntvariables)
    }
    
    
    
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
    
    
    ############# fit model ##################
    model <- paste0(tmp1, "\n", tmp2, "\n", tmp3, "\n",
                    tmp4, "\n", tmp5, "\n", tmp6, "\n",
                    tmp7, "\n", tmp8, "\n", tmp9, "\n",
                    tmp10, "\n", tmp11, "\n", tmp12, "\n",
                    collapse="\n")
    
    m1 <- lavaan(model, data=data, ...)
    
    ######## compute variance components ###########
    
    ## rel
    vary_fitted <- unlist(diag(lavInspect(m1, "fitted")$cov))
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
    
    ## summarize
    variance_components <- 
      data.frame(y=y_variables,
                 rel=rel_fitted,
                 spe=spe_fitted,
                 con=con_fitted,
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
## streng genommen müsste es auch für la_o parameters eigene periods geben...
## aber vielleicht nehmen wir da einfach auch die zetaperiods dafür (das macht wahrscheinlich Sinn)
## nur bei den nu nochmal überlegen wegen periods



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
# ## TODO maybe cov_equiv

lst_models_es_indicator_specific_trait <- 
  function(ntimepoints, data, ntraitperiods=1, nzetaperiods=1,
           nepsperiods=1, la_t_equiv="one", la_o_equiv="one",
           la_s_equiv="zero", vzeta_eqiv="time.invar", veps_equiv="invar",
           vtheta_equiv="invar", nu_equiv="zero", alpha_equiv="zero",
           mtheta_equiv="invar",...){
    
    ######## important information ############
    nindicators <- ncol(data)/ntimepoints
    nyvariables <- ntimepoints*nindicators
    nyvariables_per_traitperiod <- nyvariables/ntraitperiods
    nyvariables_per_zetaperiod <- nyvariables/nzetaperiods
    nyvariables_per_epsperiod <- nyvariables/nepsperiods
    ntvariables <- nindicators*ntraitperiods
    ntimepoints_per_traitperiod <- ntimepoints/ntraitperiods
    ntimepoints_per_zetaperiod <- ntimepoints/nzetaperiods
    ntimepoints_per_epsperiod <- ntimepoints/nepsperiods
    
    ######### variable names ##############
    y_variables <- names(data)
    o_variables_full <- paste0("O", rep(1:ntimepoints, each=nindicators))
    o_variables_unique <- paste0("O", 1:ntimepoints)
    zeta_variables_full <- paste0("zeta", rep(1:ntimepoints, each=nindicators))
    zeta_variables_unique <- unique(zeta_variables_full)
    theta_variables_full <- paste0("theta", 1:nindicators, "_", 
                                   rep(1:ntraitperiods, each=nyvariables_per_traitperiod))
    theta_variables_unique <- unique(theta_variables_full)
    
    ############ parameter names ###########
    
    ## la_o
    if(la_o_equiv == "one"){
      la_o <- rep("1", times=nyvariables)
    }
    if(la_o_equiv == "time.invar"){
      la_o <- rep("la_o", times=nyvariables)
    }
    if(la_o_equiv == "period.invar"){
      la_o <- paste0("la_o", 1:nindicators, "_", 
                     rep(1:nzetaperiods, each=nyvariables_per_zetaperiod))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    }
    if(la_o_equiv == "free"){
      la_o <- paste0("la_o", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_o[seq(from=1, to=nyvariables, by=nindicators)] <- "1"
    }
    
    
    ## la_t
    if(la_t_equiv == "one"){
      la_t <- rep("1", times=nyvariables)
    }
    if(la_t_equiv == "period.invar"){
      la_t <- paste0("la_t", 1:nindicators, "_", 
                     rep(rep(1:ntimepoints_per_traitperiod, each=nindicators), times=ntraitperiods))
      la_t <- matrix(la_t, nrow=nyvariables_per_traitperiod)
      la_t[1:nindicators,] <- "1"
      la_t <- c(la_t)
    }
    if(la_t_equiv == "free"){
      la_t <- paste0("la_t", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      la_t <- matrix(la_t, nrow=nyvariables_per_traitperiod)
      la_t[1:nindicators,] <- "1"
      la_t <- c(la_t)
    }
    
    
    ## la_s
    if(la_s_equiv == "zero"){
      la_s <- rep("0", times=ntimepoints-1)
    }
    if(la_s_equiv == "time.invar"){
      la_s <- rep("la_s", times=ntimepoints-1)
    }
    if(la_s_equiv == "period.invar"){
      la_s <- paste0("la_s", 1:ntimepoints_per_zetaperiod)
      la_s <- rep(la_s, times=nzetaperiods)
      la_s <- la_s[-1]
    }
    if(la_s_equiv == "free"){
      la_s <- paste0("la_s", 2:ntimepoints)
    }
    
    ## nu
    if(nu_equiv == "zero"){
      nu <- rep("0", times=nyvariables)
    }
    if(nu_equiv == "period.invar"){
      nu <- paste0("nu", 1:nindicators, "_", 
                   rep(rep(1:ntimepoints_per_traitperiod, each=nindicators), times=ntraitperiods))
      nu <- matrix(nu, nrow=nyvariables_per_traitperiod)
      nu[1:nindicators,] <- "0"
      nu <- c(nu)
    }
    if(nu_equiv == "free"){
      nu <- paste0("nu", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
      nu <- matrix(nu, nrow=nyvariables_per_traitperiod)
      nu[1:nindicators,] <- "0"
      nu <- c(nu)
    }
    
    
    ## mtheta
    if(mtheta_equiv == "invar"){
      mtheta <- rep("mtheta", times=ntvariables)
    }
    if(mtheta_equiv == "indicator.invar"){
      mtheta <- rep(paste0("mtheta", 1:nindicators), times=ntraitperiods)
    }
    if(mtheta_equiv == "free"){
      mtheta <- paste0("mtheta", 1:nindicators, "_", rep(1:ntraitperiods, each=nindicators))
    }
    
    ## vzeta
    if(vzeta_eqiv == "time.invar"){
      vzeta <- rep("vzeta", times=ntimepoints)
    }
    if(vzeta_eqiv == "period.invar"){
      vzeta <- rep(paste0("vzeta", 1:nzetaperiods), each=ntimepoints_per_zetaperiod)
    }
    if(vzeta_eqiv == "free"){
      vzeta <- paste0("vzeta", 1:ntimepoints)   
    }
    
    ## veps
    if(veps_equiv == "invar"){
      veps <- rep("veps", times=nyvariables)
    }
    if(veps_equiv == "time.invar"){
      veps <- rep(paste0("veps", 1:ntimepoints), each=nindicators)
    }
    if(veps_equiv == "indicator.invar"){
      veps <- rep(paste0("veps", 1:nindicators), times=ntimepoints)
    }
    if(veps_equiv == "period.invar"){
      veps <- rep(paste0("veps", 1:nepsperiods), each=nyvariables_per_epsperiod)
    }
    if(veps_equiv == "free"){
      veps <- paste0("veps", 1:nindicators, "_", rep(1:ntimepoints, each=nindicators))
    }
    
    
    ## vtheta
    if(vtheta_equiv == "invar"){
      vtheta <- rep("vtheta", times=ntvariables)
    }
    if(vtheta_equiv == "indicator.invar"){
      vtheta <- rep(paste0("vtheta", 1:nindicators), times=ntraitperiods)
    }
    if(vtheta_equiv == "free"){
      vtheta <- paste0("vtheta", 1:nindicators, "_", rep(1:ntraitperiods, each=nindicators))
    }
    
    
    
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
    
    
    ############# fit model ##################
    model <- paste0(tmp1, "\n", tmp2, "\n", tmp3, "\n",
                    tmp4, "\n", tmp5, "\n", tmp6, "\n",
                    tmp7, "\n", tmp8, "\n", tmp9, "\n",
                    tmp10,
                    collapse="\n")
    
    m1 <- lavaan(model, data=data, ...)
    
    
    ######## compute variance components ###########
    
    ## rel
    vary_fitted <- unlist(diag(lavInspect(m1, "fitted")$cov))
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
    
    ## summarize
    variance_components <- 
      data.frame(y=y_variables,
                 rel=rel_fitted,
                 spe=spe_fitted,
                 con=con_fitted,
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






