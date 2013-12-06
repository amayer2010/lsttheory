
######################## main function for users ############################

growthcomponents <- function(neta, data, contrasts, addsyntax="", equiv.assumption=list(tau="cong"), scale.invariance=list(lait0=TRUE, lait1=TRUE), ...)
{
  mod <- createGCModel(neta, data, contrasts, equiv.assumption, scale.invariance)
  
  gcsyntax <- paste0(createSyntaxLoadings(mod), "\n", 
                     createSyntaxIntercepts(mod), "\n",
                     createSyntaxMeanEta(mod), "\n",
                     createSyntaxVarEps(mod), "\n",
                     createSyntaxVarEta(mod),  "\n",
                     createSyntaxLoadingsPi(mod),  "\n",
                     createSyntaxMeanPi(mod),  "\n",    
                     createSyntaxVarPi(mod),  "\n",                     
                     collapse=""
  )  
  
  
  lavaanres <- sem(gcsyntax, data=data, ...)
  return(lavaanres)
  
}


######################## class definition and methods ############################

setClass(
  "GrowthComponents",               
  representation( 
    gcmodel = "list", # growth components model
    number = "list", # number of variables    
    names = "list", # names of variables
    labels = "list", # labels of parameters
    data = "data.frame",
    lavaansyntax = "character",
    lavaanres = "lavaan",
    gcresults = "list"  # results
  )  
)


setMethod ("show", "GrowthComponents",
           function (object){
             print(object@lavaansyntax)
           })


createGCModel <- function (neta, data, contrasts, equiv.assumption, scale.invariance){
  
  gcmodel <- list(name = "Growth Components Model",
                  contrasts = contrasts,
                  bmatrix = solve(contrasts),
                  equiv.assumption = equiv.assumption,
                  scale.invariance = scale.invariance)
  
  
  number <- list(manifest = length(names(data)),
                 eta = neta,
                 pi = neta,
                 etaind = length(names(data))/neta
  )
  
  labels <- createGCLabels(number, gcmodel)
    
  names <- list(manifest = names(data),
                eta = paste0("eta", 1:neta),
                pi = paste0("pi", 0:(neta-1))
  )

  model <- new("GrowthComponents", 
               gcmodel = gcmodel,               
               number = number,    
               names = names,
               labels = labels,                
               data = data               
  )
  
  return(model)
}



createGCLabels <- function(number, gcmodel)
{
  it <- paste0(rep(1:number$etaind ,number$eta),
               rep(1:number$eta, each=number$etaind)) # index it
  
  nu <- paste0("la",it,"0")
  lambda = paste0("la",it,"1")
  theta <- paste0("eps",it)
  alpha <- rep("0",number$eta)
  gamma <- paste0(as.vector(gcmodel$bmatrix))
  psi <- rep("0",number$eta)
  varpi <- paste0("varpi",0:(number$eta-1))
  mpi <- paste0("mpi",0:(number$eta-1))
  vareta <- rep("0",number$eta)
  
  fixedeta <- seq(1,number$manifest,by=number$manifest/number$eta) 
  
  if(gcmodel$equiv.assumption$tau == "equi"){
    nu <- rep("0", number$manifest)
    lambda = rep("1", number$manifest)      
  }
  if(gcmodel$equiv.assumption$tau == "ess"){
    nu[fixedeta] <- 0      
    lambda = rep("1", number$manifest)
    
    if(gcmodel$scale.invariance$lait0){
      nu <- rep(nu[1:number$etaind], number$eta)
    }      
  }
  if(gcmodel$equiv.assumption$tau == "cong"){
    nu[fixedeta] <- 0      
    lambda[fixedeta] <- 1
    
    if(gcmodel$scale.invariance$lait0){
      nu <- rep(nu[1:number$etaind], number$eta)        
    }
    if(gcmodel$scale.invariance$lait1){
      lambda <- rep(lambda[1:number$etaind], number$eta)        
    }            
  }
  
  labels <- list(nu=nu,
                 lambda=lambda,
                 theta=theta,
                 alpha=alpha,
                 gamma=gamma,
                 psi=psi,
                 varpi=varpi,
                 mpi=mpi,
                 vareta=vareta
  )
  
  return(labels)
}




createSyntaxLoadingsPi <- function(mod)
{  
  lhs <- rep(mod@names$pi, each=mod@number$eta)  
  rhs <- paste(mod@labels$gamma, mod@names$eta, sep="*")      
  res <- paste(lhs, "=~", rhs, collapse="\n")  
  
  return(res)
}


createSyntaxMeanPi <- function(mod)
{  
  lhs <- mod@names$pi
  rhs <- paste(mod@labels$mpi,"1",sep="*")
  res <- paste(lhs, "~", rhs, collapse="\n")      
  
  return(res)
}



createSyntaxVarPi <- function(mod)
{  
  lhs <- mod@names$pi
  rhs <- paste(mod@labels$varpi,mod@names$pi,sep="*")
  res <- paste(lhs, "~~", rhs, collapse="\n")      
   
  return(res)
}


