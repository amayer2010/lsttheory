

############ shiny ##############

#' Shiny interface for lsttheory()
#'
#' This function calls a shiny interface for lsttheory().
#'
#' @export
lsttheoryGUI <- function(){
  shiny::runApp(system.file('lsttheorygui', package='lsttheory'))
}


#' Shiny interface for lst_models_es()
#'
#' This function calls a shiny interface for LST-R models with experience sampling data.
#' @author Julia Norget
#'
#' @export
lstesGUI <- function(){
  shiny::runApp(system.file('lstesgui', package='lsttheory'))
}



#' Shiny interface for mmLSTrf()
#'
#' This function calls a shiny interface for MM-LST-RF models.
#' @author Dora Tinhof
#'
#' @export
mmLSTrfGUI <- function(){
  tmp <- DT::coerceValue(100,1L) ## only needed to avoid warning in check --as cran
  tmp <- bsplus::shiny_iconlink() ## only needed to avoid warning in check --as cran
  shiny::runApp(system.file('mmlstrfgui', package='lsttheory'))
}
