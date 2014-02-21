
######################## main function for users ############################

##TODO invarianz für 3 oder mehr Indikatoren anpassen...

#' Estimate latent state-trait models
#' 
#' This function is the main funtion of the package and can be used to estimate
#' various latent state-trait models (LST models). It is based on the 
#' revised version of the LST theory presented in 
#' Steyer, Mayer, Geiser & Cole (in press).
#' 
#' @param neta integer. Number of latent state variables eta.
#' @param nxi integer. Number of latent state trait variables xi.
#' @param data a data frame. 
#' @param subset picks a subset of data.
#' @param order can be one of c("time").
#' @param addsyntax character string.
#' @param equiv.assumption list of equivalence assumptions for tau variables (tau)
#' and xi variables (TODO: better names). Each can be one of c("equi","ess","cong"),
#' for equivalence ("equi"), essential equivalence ("ess"), 
#' or congenericity ("cong").
#' @param scale.invariance list of invariance assumtions for lambda and gamma
#' parameters
#' @param ... further arguments passed to lavaan::sem().
#' @return object of class LSTModel. 
#' @export
#' @import lavaan
lsttheory <- function(neta, nxi=0, data, subset=NULL, order="time", addsyntax="", equiv.assumption=list(tau="cong", xi="cong"), scale.invariance=list(lait0=FALSE, lait1=FALSE, gat0=FALSE, gat1=FALSE), ...)
{
  checkInput() # TODO
  mod <- createLSTModel(neta, nxi, data, equiv.assumption, scale.invariance)
  
  completesyntax <- createCompleteSyntax(mod)
  completesyntax <- paste0(completesyntax, addsyntax, sep="\n")
  
  lavaanres <- sem(completesyntax, data=data, ...)
  
  # save results in mod
  mod@lavaansyntax <- completesyntax
  mod@lavaanres <- lavaanres
  
  mod@lsttheory <- list(rely = coef(lavaanres, type="user")[mod@labels$rely],
                        spey = coef(lavaanres, type="user")[mod@labels$spey],
                        cony = coef(lavaanres, type="user")[mod@labels$cony]
  )
  
  return(mod)  
}


######################## class definition and methods ############################

setClass(
  "LSTModel",               
  representation( 
    lstmodel = "list", # multistate usw. + equivalence and scale assumptions
    number = "list", # number of variables    
    names = "list", # names of variables
    labels = "list", # labels of parameters
    data = "data.frame",
    lavaansyntax = "character",
    lavaanres = "lavaan",
    lsttheory = "list"  # results
  )  
)


setMethod ("show", "LSTModel",
           function (object){
             res <- as.data.frame(object@lsttheory)
             rownames(res) <- object@names$manifest
               
             cat("\n" , object@lstmodel$name, "Model \n \n")
             print(res, digits=2)
             cat ("\n ")
})


createLSTModel <- function (neta, nxi, data, equiv.assumption, scale.invariance){
  
  name=character()
  if(nxi == 0){name <- "Multistate"}
  if(nxi == 1){name <- "Singletrait-Multistate"}
  if(nxi > 1){name <- "Multitrait-Multistate"}
  
  lstmodel <- list(name = name,
                   equiv.assumption = equiv.assumption,
                   scale.invariance = scale.invariance)
  
  number <- list(manifest = length(names(data)),
                 eta = neta,
                 xi = nxi,
                 etaind = length(names(data))/neta,
                 xiind = ifelse(nxi==0, NA, neta/nxi)
  )
  
  labels <- createLabels(number, lstmodel)
  
  xi <- character()
  if(nxi > 0){xi <- paste0("xi", 1:nxi)}
  
  names <- list(manifest = names(data),
                eta = paste0("eta", 1:neta),
                xi = xi
  )
  
  
  
  model <- new("LSTModel", 
      lstmodel = lstmodel,               
      number = number,    
      names = names,
      labels = labels,                
      data = data               
  )
  
  return(model)
}


######################## create syntax functions ############################



createLabels <- function(number, lstmodel)
{
  it <- paste0(rep(1:number$etaind ,number$eta),
               rep(1:number$eta, each=number$etaind)) # index it
  
  nu <- paste0("la",it,"0")
  lambda = paste0("la",it,"1")
  theta <- paste0("eps",it)
  alpha <- paste0("ga",1:number$eta,"0")
  gamma <- paste0("ga",1:number$eta,"1")
  psi <- paste0("psi",1:number$eta)
  varxi <- character()
  mxi <- character()
  vareta <- paste0("vareta",1:number$eta)
  vary <- paste0("vary",it)
  rely <- paste0("rely",it)
  spey <- paste0("spey",it)
  cony <- paste0("cony",it)

  fixedeta <- seq(1,number$manifest,by=number$manifest/number$eta) 
  fixedxi <- seq(1,number$eta,by=number$eta/number$xi)
  
  # for all models
  if(!is.null(lstmodel$name)){
    
    if(lstmodel$equiv.assumption$tau == "equi"){
      nu <- rep("0", number$manifest)
      lambda = rep("1", number$manifest)      
    }
    if(lstmodel$equiv.assumption$tau == "ess"){
      nu[fixedeta] <- 0      
      lambda = rep("1", number$manifest)
      
      if(lstmodel$scale.invariance$lait0){
        nu <- rep(nu[1:number$etaind], number$eta)
      }      
    }
    if(lstmodel$equiv.assumption$tau == "cong"){
      nu[fixedeta] <- 0      
      lambda[fixedeta] <- 1
      
      if(lstmodel$scale.invariance$lait0){
        nu <- rep(nu[1:number$etaind], number$eta)        
      }
      if(lstmodel$scale.invariance$lait1){
        lambda <- rep(lambda[1:number$etaind], number$eta)        
      }            
    }
    
  }
  
  if(lstmodel$name != "Multistate"){
    
    if(lstmodel$equiv.assumption$xi == "equi"){
      alpha <- rep("0", number$eta)
      gamma <- rep("1", number$eta)      
    }
    if(lstmodel$equiv.assumption$xi == "ess"){
      alpha[fixedxi] <- 0
      gamma <- rep("1", number$eta)
      
      if(lstmodel$scale.invariance$gat0){
        alpha <- rep(alpha[1:number$xiind], number$xi)        
      }
    }
    if(lstmodel$equiv.assumption$xi == "cong"){
      alpha[fixedxi] <- 0
      gamma[fixedxi] <- 1
      
      if(lstmodel$scale.invariance$gat0){
        alpha <- rep(alpha[1:number$xiind], number$xi)
      }
      if(lstmodel$scale.invariance$gat1){
        gamma <- rep(gamma[1:number$xiind], number$xi)
      }
    }   
  }
  
    
  if(number$xi > 0){varxi <- paste0("varxi",1:number$xi)}
  if(number$xi > 0){mxi <- paste0("mxi",1:number$xi)}
  
  labels <- list(nu=nu,
                 lambda=lambda,
                 theta=theta,
                 alpha=alpha,
                 gamma=gamma,
                 psi=psi,
                 varxi=varxi,
                 mxi=mxi,
                 vareta=vareta,
                 vary=vary,
                 rely=rely,
                 spey=spey,
                 cony=cony
  )
  
  return(labels)
}



createCompleteSyntax <- function(mod)
{
  completesyntax <- paste0(createSyntaxLoadings(mod), "\n", 
                           createSyntaxIntercepts(mod), "\n",
                           createSyntaxMeanEta(mod), "\n",
                           createSyntaxVarEps(mod), "\n",
                           createSyntaxVarEta(mod),  "\n",
                           createSyntaxLoadingsXi(mod),  "\n",
                           createSyntaxVarXi(mod),  "\n",
                           createSyntaxMeanXi(mod),  "\n",
                           collapse=""
  )  
  
  completesyntax <- paste0(completesyntax, "\n", 
                           createConstraintVarEta(mod), "\n",                           
                           createConstraintVarY(mod), "\n",
                           createConstraintRelY(mod), "\n",
                           createConstraintSpeY(mod), "\n",
                           createConstraintConY(mod), "\n",                                                    
                           collapse=""
  )  
  
  return(completesyntax)
}




createSyntaxIntercepts <- function(mod)
{
  lhs <- mod@names$manifest
  rhs <- paste(mod@labels$nu,"1",sep="*")
  res <- paste(lhs, "~", rhs, collapse="\n")  
  
  return(res)
}


createSyntaxLoadings <- function(mod)
{
  lhs <- rep(mod@names$eta, each=mod@number$etaind)  
  rhs <- paste(mod@labels$lambda, mod@names$manifest, sep="*")      
  res <- paste(lhs, "=~", rhs, collapse="\n")  
  
  return(res) 
}


createSyntaxVarEps <- function(mod)
{
  lhs <- mod@names$manifest
  rhs <- paste(mod@labels$theta, mod@names$manifest, sep="*")
  res <- paste(lhs, "~~", rhs, collapse="\n")  
  
  return(res)
}


createSyntaxMeanEta <- function(mod)
{
  lhs <- mod@names$eta
  rhs <- paste(mod@labels$alpha,"1",sep="*")
  res <- paste(lhs, "~", rhs, collapse="\n")  
  
  return(res)
}



createSyntaxVarEta <- function(mod)
{
  lhs <- mod@names$eta
  rhs <- paste(mod@labels$psi, mod@names$eta, sep="*")
  res <- paste(lhs, "~~", rhs, collapse="\n")  
  
  return(res)
}


createSyntaxLoadingsXi <- function(mod)
{
  res <- character(0)
  
  if(mod@number$xi > 0){
    lhs <- rep(mod@names$xi, each=mod@number$xiind)  
    rhs <- paste(mod@labels$gamma, mod@names$eta, sep="*")      
    res <- paste(lhs, "=~", rhs, collapse="\n")  
  }
  
  return(res)
}




createSyntaxMeanXi <- function(mod)
{
  res <- character(0)
  
  if(mod@number$xi > 0){
    lhs <- mod@names$xi
    rhs <- paste(mod@labels$mxi,"1",sep="*")
    res <- paste(lhs, "~", rhs, collapse="\n")      
  }

  return(res)
}



createSyntaxVarXi <- function(mod)
{
  res <- character(0)
  
  if(mod@number$xi > 0){
    lhs <- mod@names$xi
    rhs <- paste(mod@labels$varxi,mod@names$xi,sep="*")
    res <- paste(lhs, "~~", rhs, collapse="\n")      
  }
  
  return(res)
}


######################## create constraints functions ############################

createConstraintVarEta <- function(mod)
{
  #TODO für mehr xis machen
  res <- character(0)
  
  if(mod@number$xi==0){
    res <- paste(mod@labels$vareta, ":=", mod@labels$psi, collapse="\n")
  }else{
    lhs <- mod@labels$vareta
    xis <- rep(mod@labels$varxi, each=mod@number$xiind)
    rhs <- paste0(mod@labels$gamma, "^2 * ",xis, " + ", mod@labels$psi)
    res <- paste(lhs, ":=", rhs, collapse="\n")  
  }
  
  return(res)
}



createConstraintVarY <- function(mod)
{
  lhs <- mod@labels$vary
  etas <- rep(mod@labels$vareta, each=mod@number$etaind)
  rhs <- paste0(mod@labels$lambda, "^2 * ",etas, " + ", mod@labels$theta)
  res <- paste(lhs, ":=", rhs, collapse="\n")  
  
  return(res)
}



createConstraintRelY <- function(mod)
{
  lhs <- mod@labels$rely
  etas <- rep(mod@labels$vareta, each=mod@number$etaind)
  rhs <- paste0(mod@labels$lambda, "^2 * ",etas, " / ", mod@labels$vary)
  res <- paste(lhs, ":=", rhs, collapse="\n")  
  
  return(res) 
}


createConstraintSpeY <- function(mod)
{
  res <- character(0)
  
  if(mod@number$xi > 0){
    lhs <- mod@labels$spey
    psis <- rep(mod@labels$psi, each=mod@number$etaind)
    rhs <- paste0(mod@labels$lambda, "^2 * ",psis, " / ", mod@labels$vary)
    res <- paste(lhs, ":=", rhs, collapse="\n")      
  }
  
  return(res)
}


createConstraintConY <- function(mod)
{
  res <- character(0)
  
  if(mod@number$xi > 0){
    lhs <- mod@labels$cony
    xis <- rep(mod@labels$varxi, each=mod@number$xiind*mod@number$etaind)
    gammas <- rep(mod@labels$gamma, each=mod@number$etaind)
    rhs <- paste0(mod@labels$lambda, "^2 * ",gammas, "^2 * ", xis)
    rhs <- paste0(rhs, " / ", mod@labels$vary)
    
    res <- paste(lhs, ":=", rhs, collapse="\n")      
  }
  
  return(res)
}



######################## check input ############################

checkInput <- function(){}




