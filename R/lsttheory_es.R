
#' Estimate latent state-trait models for experience sampling data
#'
#' @author Julia Norget
#' @param model integer or character.
#' @param ntimepoints integer. The total number of measurement occasions on which data was collected.
#' @param nperiods integer. The number of periods (e.g. days or weeks) on which data was collected.
#' @param data a data.frame. This data frame contains the observed variables, sorted by time t and then 
#' by indicator i, i.e., Y11, Y21, Y31, ... Y12, Y22, Y32 ... Y15, Y25, Y35 ... etc.
#' @param missing lavaan Option. Default is set to "fiml" instead of the standard default option "listwise" with lavaan.
#' See \code{\link[lavaan]{lavOptions}} for details.
#' @param la_t_equiv Character. Factor loadings of the latent trait. 
#' Can be one of \code{c("one", "period.invar", "free")}.
#' @param la_o_equiv Character. Factor loadings of the occasion factor (OCC). 
#' Can be one of \code{c("one", "time.invar", "period.invar", "free")}.
#' In models without autoregression this corresponds to the factor loadings 
#' of the state variables (models with a single trait or period-specific traits) or state residual variables (models with indicator-specific traits).
#' @param la_s_equiv Character. Autoregression between occasion factors. Can be one of \code{c("zero", "time.invar", "period.invar", "free")}.
#' @param vzeta_eqiv Character. Variances of the state residual (zeta) variables. Can be one of \code{c("time.invar", "period.invar", "free")}.
#' @param veps_equiv Character. Variances of the residual (epsilon) variables. 
#' Can be one of \code{c("invar",  "time.invar", "indicator.invar", "period.invar", "free")}.
#' @param vtheta_equiv Character. Variances of the latent trait. Can be one of \code{c("invar","indicator.invar", "free")}.
#' @param nu_equiv Character. Intercepts of the indicators. Can be one of \code{c("zero","period.invar", "free")}.
#' @param alpha_equiv Character. Intercepts of the latent states. 
#' Only relevant for models with a single trait or period-specific traits.
#' Can be one of \code{c("zero","period.invar", "free")}.
#' @param mtheta_equiv Means of the latent traits. Character. Can be one of \code{c("invar","indicator.invar", "free")}.
#' @param ... Further arguments passed to lower-level functions
#' @export
lsttheory_es <- function(model, ntimepoints, nperiods, data, missing="fiml", 
                           la_t_equiv = NULL, la_o_equiv = NULL, la_s_equiv = NULL, vzeta_eqiv = NULL, veps_equiv = NULL, 
                           vtheta_equiv = NULL, nu_equiv = NULL, alpha_equiv = NULL, mtheta_equiv = NULL, ...){
  
  userdefined <- c(la_t_equiv = la_t_equiv, la_o_equiv = la_o_equiv, la_s_equiv = la_s_equiv, vzeta_eqiv = vzeta_eqiv, veps_equiv = veps_equiv, 
                   vtheta_equiv = vtheta_equiv, nu_equiv = nu_equiv, alpha_equiv = alpha_equiv, mtheta_equiv = mtheta_equiv)
  
  if(!is.null(alpha_equiv) & model %in% c(4,5,8,9)){
    message("alpha_equiv option will be ignored. Indicator-specific LST-R models are formulated as bi-factor models and do not include intercepts for the latent states.")
  }
  
  if(model == 1 || model == "base" || model == "MSSTinvar" || model == "STinvar"){ # singletrait model, no autoregression
    ntraitperiods = 1
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "one"
    la_s_equiv = "zero"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    alpha_equiv = "zero"
    mtheta_equiv = "invar"
    
  }else if(model == 2 || model == "STARinvar"){ # singletrait model with autoregression -- STARinvar
    ntraitperiods = 1
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "one"
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    alpha_equiv = "zero"
    mtheta_equiv = "invar"
    
  }else if(model == 3 || model == "PTARinvar"){ # period-specific traits with autoregression -- PTARinvar
    ntraitperiods = nperiods
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "one"
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    alpha_equiv = "zero"
    mtheta_equiv = "invar"
  }else if(model == 4 || model == "ITARinvar"){ # indicator-specific traits with autoregression -- ITARinvar
    ntraitperiods = 1
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "one"
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    mtheta_equiv = "invar"
  }else if(model == 5 || model == "PITARinvar"){ # period- and indicator-specific traits with autoregression -- PITARinvar
    ntraitperiods = nperiods 
    nzetaperiods = nperiods 
    nepsperiods = nperiods 
    la_t_equiv = "one"
    la_o_equiv = "one"
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    mtheta_equiv = "invar"
  }else if(model == 6 || model == "STAR2"){ # model 2 with state congenericity and measurement invariance within each period (e.g. day)
    ntraitperiods = 1
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "period.invar" # changed to model 2 
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero" 
    alpha_equiv = "period.invar" # changed to model 2
    mtheta_equiv = "invar"
  }else if(model == 7 || model == "PTAR2"){ # model 3 with state congenericity and measurement invariance within each period (e.g. day)
    ntraitperiods = nperiods
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "period.invar" # changed to model 3
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    alpha_equiv = "period.invar" # changed to model 3
    mtheta_equiv = "invar"
  }else if(model == 8 || model == "ITAR2"){ # model 4 with state residual congenericity and measurement invariance within each period (e.g. day)
    ntraitperiods = 1
    nzetaperiods = nperiods
    nepsperiods = nperiods
    la_t_equiv = "one"
    la_o_equiv = "period.invar" # changed to model 4
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    mtheta_equiv = "invar"
  }else if(model == 9 || model == "PITAR2"){ # model 5 with state residual congenericity and measurement invariance within each period (e.g. day)
    ntraitperiods = nperiods 
    nzetaperiods = nperiods 
    nepsperiods = nperiods 
    la_t_equiv = "one"
    la_o_equiv = "period.invar" # changed to model 5
    la_s_equiv = "time.invar"
    vzeta_eqiv = "time.invar"
    veps_equiv = "invar"
    vtheta_equiv = "invar"
    nu_equiv = "zero"
    mtheta_equiv = "invar"
  }
  
  # overwrite the invariance options implied by models 1-9 if the user specifically defined these options
  if(!is.null(userdefined)){
    if(!is.na(userdefined["la_t_equiv"])){la_t_equiv = unname(userdefined["la_t_equiv"])}
    if(!is.na(userdefined["la_o_equiv"])){la_o_equiv = unname(userdefined["la_o_equiv"])}
    if(!is.na(userdefined["la_s_equiv"])){la_s_equiv = unname(userdefined["la_s_equiv"])}
    if(!is.na(userdefined["vzeta_eqiv"])){vzeta_eqiv = unname(userdefined["vzeta_eqiv"])}
    if(!is.na(userdefined["veps_equiv"])){veps_equiv = unname(userdefined["veps_equiv"])}
    if(!is.na(userdefined["vtheta_equiv"])){vtheta_equiv = unname(userdefined["vtheta_equiv"])}
    if(!is.na(userdefined["nu_equiv"])){nu_equiv = unname(userdefined["nu_equiv"])}
    if(!is.na(userdefined["alpha_equiv"])){alpha_equiv = unname(userdefined["alpha_equiv"])}
    if(!is.na(userdefined["mtheta_equiv"])){mtheta_equiv = unname(userdefined["mtheta_equiv"])}    
  }
  
  if(model %in% c(1,2,3,6,7)){
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
    
  }else if(model %in% c(4,5,8,9)){
    message("test")
    
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


