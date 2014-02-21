\name{lsttheory}
\alias{lsttheory}
\title{Estimate latent state-trait models}
\usage{
  lsttheory(neta, nxi = 0, data, subset = NULL,
    order = "time", addsyntax = "",
    equiv.assumption = list(tauÂ =Â "cong",Â xiÂ =Â "cong"),
    scale.invariance = list(lait0Â =Â FALSE,Â lait1Â =Â FALSE,Â gat0Â =Â FALSE,Â gat1Â =Â FALSE),
    ...)
}
\arguments{
  \item{neta}{integer. Number of latent state variables
  eta.}

  \item{nxi}{integer. Number of latent state trait
  variables xi.}

  \item{data}{a data frame.}

  \item{subset}{picks a subset of data.}

  \item{order}{can be one of c("time").}

  \item{addsyntax}{character string.}

  \item{equiv.assumption}{list of equivalence assumptions
  for tau variables (tau) and xi variables (TODO: better
  names). Each can be one of c("equi","ess","cong"), for
  equivalence ("equi"), essential equivalence ("ess"), or
  congenericity ("cong").}

  \item{scale.invariance}{list of invariance assumtions for
  lambda and gamma parameters}

  \item{...}{further arguments passed to lavaan::sem().}
}
\value{
  object of class LSTModel.
}
\description{
  This function is the main funtion of the package and can
  be used to estimate various latent state-trait models
  (LST models). It is based on the revised version of the
  LST theory presented in Steyer, Mayer, Geiser & Cole (in
  press).
}

