\name{lsttheory}
\alias{lsttheory}
\title{Estimate latent state-trait models}
\usage{
  lsttheory(neta, ntheta = 0, data, addsyntax = "",
    equiv.assumption = list(tauÂ =Â "cong",Â thetaÂ =Â "cong"),
    scale.invariance = list(lait0Â =Â FALSE,Â lait1Â =Â FALSE,Â lat0Â =Â FALSE,Â lat1Â =Â FALSE),
    ...)
}
\arguments{
  \item{neta}{integer. Number of latent state variables
  eta.}

  \item{ntheta}{integer. Number of latent trait variables
  theta.}

  \item{data}{a data frame. This data frame only contains
  the observables, which will all be used to fit the LST-R
  model. The order of the observables Y_it should be by
  time t and then by indicator i, i.e., Y_11, Y_21, ...,
  Y_12, Y_22, ... and so forth}

  \item{addsyntax}{character string.}

  \item{equiv.assumption}{list of equivalence assumptions
  for tau variables (tau) and theta variables. Each can be
  one of c("equi","ess","cong"), for equivalence ("equi"),
  essential equivalence ("ess"), or congenericity
  ("cong").}

  \item{scale.invariance}{list of invariance assumtions for
  lambda_it and lambda_t parameters}

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

