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
    
    ###### SliderInput for nperiods ######
    
    # observe({
    #     updateSliderInput(session, 
    #                   "nperiods",
    #                   min = 1,
    #                   max = input$ntimepoints,
    #                   step = 1)
    # })
    # 
    # output$warnmsg <- renderPrint({
    #     number <- input$ntimepoints
    #     all <- 1:number
    #     possibleN <- all[number %% all == 0]
    # 
    #     if(!input$nperiods %in% possibleN){
    #         cat("Warning: The number of measurement ocassions cannot be divided by the number of periods. \n Please select one of the following:", possibleN)
    #     }
    # })
    
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
      if (input$lstmodel == "model1") {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "zero",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar")
      }
      else if (input$lstmodel == "model2") { 
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "period.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar")
      }
      else if (input$lstmodel == "model3") {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "period.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero",
          mtheta_equiv <- "invar")
      }
      else if (input$lstmodel == "model4") {
        list(
          la_t_equiv <- "one",
          la_o_equiv <- "one",
          la_s_equiv <- "period.invar",
          vzeta_eqiv <- "time.invar",
          veps_equiv <- "invar",
          vtheta_equiv <- "invar",
          nu_equiv <- "zero",
          alpha_equiv <- "zero", #not relevant for indicator-specific models
          mtheta_equiv <- "invar")
      }
      else {
        list(
          la_t_equiv   <- "free",
          la_o_equiv   <- "free",
          la_s_equiv   <- "free",
          vzeta_eqiv   <- "free",
          veps_equiv   <- "free",
          vtheta_equiv <- "free",
          nu_equiv     <- "free",
          alpha_equiv  <- "free",
          mtheta_equiv <- "free")
      }
    })
    
    
    # implied.equivalences <- reactive({
    #     if (input$globalequivalence == "invar") {
    #         list(
    #             la_t_equiv <- "one",
    #             la_o_equiv <- "one",
    #             la_s_equiv <- "time.invar",
    #             vzeta_eqiv <- "time.invar",
    #             veps_equiv <- "invar",
    #             vtheta_equiv <- "indicator.invar",
    #             nu_equiv <- "zero",
    #             alpha_equiv <- "zero",
    #             mtheta_equiv <- "indicator.invar")
    #     }
    #     else if (input$globalequivalence == "period.invar") {
    #         list(
    #             la_t_equiv <- "period.invar",
    #             la_o_equiv <- "period.invar",
    #             la_s_equiv <- "period.invar",
    #             vzeta_eqiv <- "period.invar",
    #             veps_equiv <- "period.invar",
    #             vtheta_equiv <- "indicator.invar",
    #             nu_equiv <- "period.invar",
    #             alpha_equiv <- "period.invar",
    #             mtheta_equiv <- "indicator.invar")
    #     }
    #     else if (input$globalequivalence == "free") {
    #         list(
    #             la_t_equiv   <- "free",
    #             la_o_equiv   <- "free",
    #             la_s_equiv   <- "free",
    #             vzeta_eqiv   <- "free",
    #             veps_equiv   <- "free",
    #             vtheta_equiv <- "free",
    #             nu_equiv     <- "free",
    #             alpha_equiv  <- "free",
    #             mtheta_equiv <- "free")
    #     }
    # })
    
    observe({
        updateSelectInput(session, "la_t_equiv",   selected = implied.equivalences()[1])
        updateSelectInput(session, "la_o_equiv",   selected = implied.equivalences()[2])
        updateSelectInput(session, "la_s_equiv",   selected = implied.equivalences()[3])
        updateSelectInput(session, "vzeta_eqiv",   selected = implied.equivalences()[4])
        updateSelectInput(session, "veps_equiv",   selected = implied.equivalences()[5])
        updateSelectInput(session, "vtheta_equiv", selected = implied.equivalences()[6])
        updateSelectInput(session, "nu_equiv",     selected = implied.equivalences()[7])
        updateSelectInput(session, "alpha_equiv",  selected = implied.equivalences()[8])
        updateSelectInput(session, "mtheta_equiv", selected = implied.equivalences()[9])
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
    choicesstatecovariates <- reactive({
      indicators <- input$indicators
      repeatedvariables <- varnames()[["repeated_measures"]]
      statecovariates <- setdiff(repeatedvariables, indicators)
      return(statecovariates)
    })
    
    output$statecovariates <- renderUI({
        selectInput(inputId="statecov",
                    label="What are your covariates explaining the latent states?", 
                    choices = choicesstatecovariates(), # choices = varnames()[["repeated_measures"]], gives all repeated variables as choices
                    multiple = TRUE)
    })
    
    output$traitcovariates <- renderUI({
        selectInput(inputId="traitcov",
                    label="What are your covariates explaining the latent traits?", 
                    choices = varnames()[["single_measures"]],
                    multiple = TRUE)
    })

    #### Summary ####
    
    summarytext <- reactive({
        #TODO implement warning if any info is missing
        validate(
            need(input$ntimepoints != "", "Please provide the number of measurement occasions in the 'Reading in Data' section."),
            need(input$indicators != "", "Please select two or more indicators in the 'variable specifics' section."),
        )        
        
        ntimepoints <- input$ntimepoints
        indicators  <- input$indicators
        nindicators <- length(indicators)
        
        # traitmodel  <- input$traitmodel #singletrait/ day-specific/ indicator-specific/ day- and indicator-specific
        lstmodel    <- input$lstmodel
        nperiods    <- as.integer(input$nperiods)
        ar          <- ifelse(input$autoregression, "with", "without")
        equiv       <- ifelse(input$detailedequivalences, "individual", input$globalequivalence)
        
        paste0("Your model has ", nindicators, " indicators (", paste0(indicators, collapse = ", "), ") measured at ", ntimepoints, " occasions. ",
              "You have selected ", lstmodel, "." #"traitmodel with ", nperiods, " periods, ", ar, " autoregression and the '", equiv, "' equivalence option."
              )
    })
    
    output$summary <- renderText({summarytext()})
    
    #### RESULTS ####
    
    #### lst_models_es() call ####

    model <- eventReactive(input$run, {
        ntimepoints <- input$ntimepoints 
        indicators  <- input$indicators
        seperator   <- input$seperator

        # if(!input$includecovariates){
             vars <- as.vector(outer(indicators, 1:ntimepoints, paste, sep=seperator))
             data <- dataInput() %>% select(vars)
        # }
        # else if (input$includecovariates){
             #TODO
        # }
        
        lstmodel             <- input$lstmodel
        # traitmodel           <- input$traitmodel
        #data                 <- dataInput()
        nperiods             <- as.integer(input$nperiods)
        # autoregression       <- input$autoregression
        # detailedequivalences <- input$detailedequivalences
        # globalequivalence    <- input$globalequivalence
        la_t_equiv           <- input$la_t_equiv
        la_o_equiv           <- input$la_o_equiv
        la_s_equiv           <- input$la_s_equiv
        vzeta_eqiv           <- input$vzeta_eqiv
        veps_equiv           <- input$veps_equiv
        vtheta_equiv         <- input$vtheta_equiv
        nu_equiv             <- input$nu_equiv
        alpha_equiv          <- input$alpha_equiv
        mtheta_equiv         <- input$mtheta_equiv
        
        ntraitperiods <- nzetaperiods <- nepsperiods <- nperiods
        # if (traitmodel %in% c("singletrait", "indicator-specific")) {
        #     ntraitperiods <- 1
        # }
        
        # if(!detailedequivalences){
            # res <- lsttheory:::lst_models_es(traitmodel = traitmodel,
            #                      ntimepoints = ntimepoints,
            #                      data = data, nperiods = nperiods,
            #                      equiv = globalequivalence, ar = autoregression,
            #                      missing = "fiml")
          # res <- lsttheory:::lst_models_es(traitmodel = "singletrait", 
          #                                  ntimepoints = 9,
          #                                  data = lsttheory::d_lst_es, nperiods = nperiods, # TODO nperiods nicht richtig erkannt
          #                                  equiv = globalequivalence, ar = autoregression,
          #                                  missing = "fiml")
        # }
        # if(detailedequivalences){
            if(lstmodel %in% c("model1", "model2", "model3")){
                res <- lsttheory:::lst_models_es_common_trait(ntimepoints = ntimepoints,
                                                  data = data, ntraitperiods = nperiods, nzetaperiods = nperiods,
                                                  nepsperiods = nperiods, la_t_equiv = la_t_equiv,
                                                  la_o_equiv = la_o_equiv, la_s_equiv = la_s_equiv,
                                                  vzeta_eqiv = vzeta_eqiv, veps_equiv = veps_equiv,
                                                  vtheta_equiv = vtheta_equiv, nu_equiv = nu_equiv,
                                                  alpha_equiv = alpha_equiv, mtheta_equiv = mtheta_equiv,
                                                  missing = "fiml")

            }
            else if (lstmodel %in% c("model4")){
                res <- lsttheory:::lst_models_es_indicator_specific_trait(ntimepoints = ntimepoints, 
                                                              data = data, ntraitperiods = nperiods, nzetaperiods = nperiods, 
                                                              nepsperiods = nperiods, la_t_equiv = la_t_equiv, 
                                                              la_o_equiv = la_o_equiv, la_s_equiv = la_s_equiv, 
                                                              vzeta_eqiv = vzeta_eqiv, veps_equiv = veps_equiv, 
                                                              vtheta_equiv = vtheta_equiv, nu_equiv = nu_equiv, 
                                                              alpha_equiv = alpha_equiv, mtheta_equiv = mtheta_equiv,
                                                              missing = "fiml")
            }
        # }
        return(res)

    })
        
    
    ##### Output variance components #####
    
    output$varcompsummary <- DT::renderDT({
        sumtable <- model()@varcomp %>% 
            select(rel, spe, con) %>% 
            psych::describe(skew = FALSE) %>% 
            round(3)
        class(sumtable) <- "data.frame"
        sumtable %>% select(mean, sd, min, max, range)
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
    


})
