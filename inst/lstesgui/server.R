#
# This is the server logic of the LST-ES Shiny web application.

#require(semPlot)

if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(lsttheory)){
    if(!require(devtools)) install.packages("devtools")
    devtools::install_github("amayer2010/lsttheory")
} 
library(lsttheory)


# Define server logic
shinyServer(function(input, output, session) {

    ######## Reactive Data Input ########
    dataInput <- reactive({
      inFile <- input$file1
      exdata <- input$exdata
      
      if(is.null(inFile) & exdata == "none"){
        return(NULL)
      }else if(is.null(inFile) & exdata == "d_lst_es (example with day-specific traits)"){
        return(d_lst_es)
      }else if(!is.null(inFile)){
        if(grepl(".csv",inFile$name)){
          return(read.csv(inFile$datapath))
        }else{
          return(spss.get(inFile$datapath))
        }
      }
      
    })
    
    
    ###### Output Data Table ######### 

    output$datatable1 <- DT::renderDT({
        dataInput()
    })
    

    ###### selectInput for nperiods ######
    
    nperiodoptions <- reactive({
        number <- input$ntimepoints
        all <- 1:number
        possibleN <- all[number %% all == 0]
        return(possibleN[-length(possibleN)]) 
    })
    
    output$selectnperiods <- renderUI({
        selectInput(inputId = "nperiods",
                    label = "How many periods (e.g. days or weeks) do you have?",
                    choices = nperiodoptions(),
                    selected = nperiodoptions()[length(nperiodoptions())]) # last option is pre-selected
    })
    
    ###### Update SelectInput for equivalences ######### 
    
    implied.equivalences <- reactive({
      if (input$lstmodel == 1) {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "zero",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar",
          gamma_t_equiv = "invar" )
      }
      else if (input$lstmodel == 2) { 
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "time.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar",
          gamma_t_equiv = "invar" )
      }
      else if (input$lstmodel == 3) {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "time.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar",
          gamma_t_equiv = "invar")
      }
      else if (input$lstmodel == 4) {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "time.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv   <- NULL,
          mtheta_equiv <- "invar",
          gamma_t_equiv = "indicator.invar")
      }
      else if(input$lstmodel == 5) {
        list(
          la_t_equiv    <- "one",
          la_o_equiv    <- "one",
          la_s_equiv    <- "time.invar",
          vzeta_eqiv    <- "time.invar",
          veps_equiv    <- "invar",
          vtheta_equiv  <- "invar",
          nu_equiv      <- "zero",
          alpha_equiv   <- NULL,
          mtheta_equiv  <- "invar",
          gamma_t_equiv <- "indicator.invar")
      }
      else if(input$lstmodel == 6) {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "period.invar" ,
          la_s_equiv <- "time.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv      <- "zero" ,
          alpha_equiv   <- "period.invar",
          mtheta_equiv  <- "invar",
          gamma_t_equiv <-  "invar"
          )
      }      
      else if(input$lstmodel == 7) {
        list(
          la_t_equiv    <- "one",
          la_o_equiv    <- "period.invar",
          la_s_equiv    <- "time.invar",
          vzeta_eqiv    <- "time.invar",
          veps_equiv    <- "invar",
          vtheta_equiv  <- "invar",
          nu_equiv      <- "zero",
          alpha_equiv   <- "period.invar" ,
          mtheta_equiv  <- "invar",
          gamma_t_equiv <-  "invar")
      }
      else if(input$lstmodel == 8) {
        list(
          la_t_equiv    <- "one",
          la_o_equiv    <- "period.invar",
          la_s_equiv    <- "time.invar",
          vzeta_eqiv    <- "time.invar",
          veps_equiv    <- "invar",
          vtheta_equiv  <- "invar",
          nu_equiv      <- "zero",
          alpha_equiv   <- NULL,
          mtheta_equiv  <- "invar",
          gamma_t_equiv <- "indicator.invar")
      } 
      else if(input$lstmodel == 9) {
        list(
          la_t_equiv    <- "one",
          la_o_equiv    <- "period.invar",
          la_s_equiv    <- "time.invar",
          vzeta_eqiv    <- "time.invar",
          veps_equiv    <- "invar",
          vtheta_equiv  <- "invar",
          nu_equiv      <- "zero",
          alpha_equiv   <- NULL,
          mtheta_equiv  <- "invar",
          gamma_t_equiv <- "indicator.invar")
      }
    })
    
    observe({
        updateSelectInput(session, "la_t_equiv",   selected = implied.equivalences()[1])
        updateSelectInput(session, "la_o_equiv",   selected = implied.equivalences()[2])
        updateSelectInput(session, "la_s_equiv",   selected = implied.equivalences()[3])
        updateSelectInput(session, "vzeta_eqiv",   selected = implied.equivalences()[4])
        updateSelectInput(session, "veps_equiv",   selected = implied.equivalences()[5])
        updateSelectInput(session, "vtheta_equiv", selected = implied.equivalences()[6])
        updateSelectInput(session, "nu_equiv",     selected = implied.equivalences()[7])
        updateSelectInput(session, "alpha_equiv",  selected = implied.equivalences()[8]) #every model must have this option!
        updateSelectInput(session, "mtheta_equiv", selected = implied.equivalences()[9])
        updateSelectInput(session, "gamma_t_equiv",selected = implied.equivalences()[10])
        
    })
    
    ###### Variable name recognition #########  
    detectvarnames <- function(data, ntimepoints, sep = ""){
        #require(dplyr)
        varnames <- vector(mode = "list", length = 2)
        names(varnames) <- c("repeated_measures", "single_measures")
        
        # repeated measures
        # select variable names that end on separator + 1 to ntimepoints, cut this end off and keep the names occurring ntimepoint times
        repnames1 <- data %>% select(ends_with(paste0(sep, 1:ntimepoints))) %>% names()
        if(ntimepoints <= 9){
            repnames2 <- gsub(pattern = paste0(sep, "[1-", ntimepoints, "]$"), replacement = "", repnames1)
        }
        if(ntimepoints > 9){
            repnames2 <- gsub(pattern = paste0(sep, "\\d+$"), replacement = "", repnames1)
        }
        repeatedmeasures <- names(table(repnames2)[table(repnames2) == ntimepoints])
        varnames[["repeated_measures"]] <- repeatedmeasures
        
        # single measures
        # variable names that do *not* start with varnames[["repeated_measures"]] + the separator + 1 to ntimepoints
        singlemeasures <- data %>% select(!(paste0(rep(repeatedmeasures, each = ntimepoints), sep, 1:ntimepoints))) %>% names()
        varnames[["single_measures"]] <- singlemeasures
        
        return(varnames)
    }
    
    varnames <- reactive({
        validate(
            need(!is.null(dataInput()), "Please select a dataset under 'Reading in data file'."),
            need(input$ntimepoints !="", "Please provide the number of measurement occasions under 'Reading in data file'.")
        )
        data <- dataInput()
        ntimepoints <- input$ntimepoints
        sep <- input$seperator
        
        return(detectvarnames(data, ntimepoints, sep))
    })
    
    output$variables <- renderPrint({
        varnames()
    })
    
    #### renderUI for indicator and covariate selection ####
    
    output$indicatorselect <- renderUI({
        selectInput(inputId="indicators",
                    label="What are your indicators for the LST model?", 
                    choices = varnames()[["repeated_measures"]],
                    multiple = TRUE)
    })
    
    # options for state covariates should not include variables which are already selected as indicators
    # choicesstatecovariates <- reactive({
    #   indicators <- input$indicators
    #   repeatedvariables <- varnames()[["repeated_measures"]]
    #   statecovariates <- setdiff(repeatedvariables, indicators)
    #   return(statecovariates)
    # })
    
    # output$statecovariates <- renderUI({
    #     selectInput(inputId="statecov",
    #                 label="What are your covariates explaining the latent states?", 
    #                 choices = choicesstatecovariates(), # choices = varnames()[["repeated_measures"]], gives all repeated variables as choices
    #                 multiple = TRUE)
    # })
    
    output$traitcovariates <- renderUI({
        selectInput(inputId="traitcov",
                    label="What are your covariates explaining the latent traits?", 
                    choices = varnames()[["single_measures"]],
                    multiple = TRUE)
    })

    #### Summary ####
    
    summarytext <- reactive({
        validate(
            need(input$ntimepoints != "", "Please provide the number of measurement occasions in the 'Reading in Data' section."),
            need(input$indicators != "", "Please select two or more indicators in the 'variable specifics' section."),
        )        
        
        ntimepoints <- input$ntimepoints
        indicators  <- input$indicators
        nindicators <- length(indicators)
        
        lstmodel    <- input$lstmodel
        nperiods    <- as.integer(input$nperiods)
        ar          <- ifelse(input$autoregression, "with", "without")
        equiv       <- ifelse(input$detailedequivalences, "individual", input$globalequivalence)
        
        paste0("Your model has ", nindicators, " indicators (", paste0(indicators, collapse = ", "), ") measured at ", ntimepoints, " occasions. ",
              "You have selected model ", lstmodel, "." #"traitmodel with ", nperiods, " periods, ", ar, " autoregression and the '", equiv, "' equivalence option."
              )
    })
    
    output$summary <- renderText({summarytext()})
    
    modelrstudio <- reactive({
      traitcov    <- input$traitcov
      # TODO check if the detailed options differ from the implied details
      code <- paste0("lsttheory_es(model = ", input$lstmodel, ", ntimepoints = ", input$ntimepoints,
                     ", nperiods = ", input$nperiods, ", data = downloadeddata", 
                     ifelse(input$includecovariates, 
                            # yes
                            ifelse(
                              length(traitcov) > 1,
                              paste0(", \n manifest_thetacovariates = c(", paste0(traitcov, collapse = ", "), ")" ), 
                              paste0(", \n manifest_thetacovariates = ", traitcov)
                            ),
                            # no
                            ""),
                     ")"
                     )
      return(code)
    })
    
    output$modelsyntax <- renderText({modelrstudio()})
    
    #### RESULTS ####
    
    #### lst_models_es() call ####

    finaldata <- reactive({
      ntimepoints <- input$ntimepoints
      indicators  <- input$indicators
      seperator   <- input$seperator
       
      vars <- as.vector(outer(indicators, 1:ntimepoints, paste, sep=seperator))

      if(!input$includecovariates){
        data <- dataInput() %>% select(all_of(vars))
      }
      else if (input$includecovariates){
        covs <- input$traitcov
        data <- dataInput() %>% select(all_of(c(vars, covs)))
        
      }
           return(data)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(finaldata(), file, row.names = FALSE)
      }
    )
    
    model <- eventReactive(input$run, {
        
        lstmodel             <- input$lstmodel
        ntimepoints          <- input$ntimepoints 
        nperiods             <- as.integer(input$nperiods)
        la_t_equiv           <- input$la_t_equiv
        la_o_equiv           <- input$la_o_equiv
        la_s_equiv           <- input$la_s_equiv
        vzeta_eqiv           <- input$vzeta_eqiv
        veps_equiv           <- input$veps_equiv
        vtheta_equiv         <- input$vtheta_equiv
        nu_equiv             <- input$nu_equiv
        alpha_equiv          <- input$alpha_equiv
        mtheta_equiv         <- input$mtheta_equiv
        
        #### TODO #####
        if(input$includecovariates){
          manifest_thetacovariates <- input$traitcov
          gamma_t_equiv            <- input$gamma_t_equiv
        }
        
        ntraitperiods <- nzetaperiods <- nepsperiods <- nperiods
        
        if(!input$includecovariates){
          res <- lsttheory::lsttheory_es(model = lstmodel, ntimepoints = ntimepoints, nperiods = nperiods, 
                                       data = finaldata(), la_t_equiv = la_t_equiv, 
                                       la_o_equiv = la_o_equiv, la_s_equiv = la_s_equiv,
                                       vzeta_eqiv = vzeta_eqiv, veps_equiv = veps_equiv,
                                       vtheta_equiv = vtheta_equiv, nu_equiv = nu_equiv,
                                       alpha_equiv = alpha_equiv, mtheta_equiv = mtheta_equiv)
        }
        if(input$includecovariates){
          res <- lsttheory::lsttheory_es(model = lstmodel, ntimepoints = ntimepoints, nperiods = nperiods, 
                                       data = finaldata(), la_t_equiv = la_t_equiv, 
                                       la_o_equiv = la_o_equiv, la_s_equiv = la_s_equiv,
                                       vzeta_eqiv = vzeta_eqiv, veps_equiv = veps_equiv,
                                       vtheta_equiv = vtheta_equiv, nu_equiv = nu_equiv,
                                       alpha_equiv = alpha_equiv, mtheta_equiv = mtheta_equiv,
                                       gamma_t_equiv = gamma_t_equiv,
                                       manifest_thetacovariates = manifest_thetacovariates)          
        }

        return(res)

    })
        

    
    ##### Output variance components #####
    
    output$varcompsummary <- DT::renderDT({
      if(input$la_s_equiv == "zero"){
        sumtable <- model()@varcomp %>% select(rel, spe, con) %>%
          psych::describe(skew = FALSE) 
      } else {
        sumtable <- model()@varcomp %>% select(rel, spe, con, pred, upred) %>% 
          psych::describe(skew = FALSE) 
      }
        class(sumtable) <- "data.frame"
        sumtable %>% select(mean, sd, min, max, range) %>% round(3)  
    })
    
    output$varcomp <- renderPrint({
        model()
        })
    
    ##### Output lav syntax #####
    output$lavaansyntax <- renderText({model()@lavaansyntax})
    
    ##### Output lav res #####
    output$lavaanresults <- renderPrint({
        summary(model()@lavaanres, fit.measures = TRUE)
        })
    
    ##### output download model #####
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste("model-", Sys.Date(), ".rds", sep="")
      },
      content = function(file) {
        saveRDS(model(), file)
      }
    )


})