###SHINY APP###

require(magrittr)
require(shiny)
require(bsplus)

# require(Hmisc)
# require(htmltools)
# require(sass)
# require(DT)



####-----------SERVER-----------####

server <- shinyServer(function(input, output, session) {
  
  
  ######## Data Input ########
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if (is.null(inFile) & exdata == "none") {
      return(NULL)
    } else if (is.null(inFile) & exdata == "mmLSTrf_SimulatedDataExample") {
      return(mmLSTrf_SimulatedDataExample)
    } else if (is.null(inFile) & exdata == "mmLSTrf_RealDataExample") {
      return(mmLSTrf_RealDataExample)
    } else if (!is.null(inFile)) {
      if (grepl(".csv", inFile$name)) {
        return(read.csv(inFile$datapath))      
      } else if (grepl(".sav", inFile$name)) {
        return(foreign::read.spss(inFile$datapath, to.data.frame=TRUE))
      }
    }
  })
  
  
  ###### Run Model #########
  model <- eventReactive(input$runModel, {
    
    id <- showNotification("Model is being estimated. This may take a few minutes.", duration = NULL)
    
    # Base arguments for mmLSTrf function
    base_args <- paste0("data=dataInput(), nSit=input$nSit, nTime=input$nTime, nMth=input$nMth,",
                        "structural=input$structural, includeOMF=input$inclOMF, lat.cov=list",
                        "(TFcov=input$TFcov, OFcov=input$OFcov, TMFcov=input$TMFcov,",
                        "OMFcov=input$OMFcov), meanstructure=input$meanstr, meas.invar=input$invar,",
                        "equiv.ass=list(TF=input$TFequiv, OF=input$OFequiv, OMF=input$OMFequiv),", 
                        "addsyntax=input$addsyntax")
    
    additional_args <- input$additionalArgs 
    
    all_args <- base_args
    if (nchar(additional_args) > 0) {
      all_args <- paste(base_args, additional_args, sep = ", ")
    }

    function_call <- paste0("mmLSTrf(", all_args, ")")
    result <- eval(parse(text = function_call))
    
    removeNotification(id)
    
    return(result)
  })
  
  
  
  ###### Output Data Table #########  
  output$table = DT::renderDT({ 
    d <- dataInput()
    if(!is.null(d)){
      d <- format(d, digits=3)
      d <- DT::datatable(d)
    }
    d
  })

    
  ###### Output lsttheory Summary #########
  output$mod_coeff <- renderPrint({        
    model()
  })
  
  ###### Output Lavaan Results #########
  output$fit_par <- renderPrint({      
    mod <- model()
    summary(mod@lavaanres, fit.measures=TRUE, standardized = TRUE)
  })
  
  ###### Output Lavaan Syntax #########
  output$syntax <- renderPrint({      
    mod <- model()
    cat(mod@lavaansyntax)  
  })
  
})
            




####-----------UI-----------####

ui <- fluidPage(

        titlePanel(
          tags$div(
            tags$h3("MM-LST-RF Model"),
            tags$h6("(Model introduced by Hintz et al., 2019, https://doi.org/10.1111/jopy.12400)", 
                    style = "color: grey;")
          )
        ),
  
        
        
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
        tabPanel("Model",        
               selectInput("exdata", "Select Example Data", 
                           c("none", "mmLSTrf_RealDataExample", "mmLSTrf_SimulatedDataExample"), selected="none")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="The real dataset consists of 2 fixed situations (s = 2), 
2 measurement occasions (t = 2) and 2 methods (m = 2). 
The simulated example dataset consists of 2 fixed situations (s = 2),
3 measurement occasions (t = 3) and 2 methods (m = 2).")),
               
               
               fileInput("file1", "Choose File (.sav or .csv)", accept=c(".sav", ".csv")),

               
               sliderInput(inputId="nSit", label="Number of Fixed Situations",
                           min=2, max=25, step=1, value=2),
               
               
               sliderInput(inputId="nTime", label="Number of Measurement Occasions",
                           min=2, max=25, step=1, value=2),
               
               
               sliderInput(inputId="nMth", label="Number of Methods",
                           min=2, max=25, step=1, value=2)
      ),
      
      
      
      
      tabPanel("Options", 
               
               selectInput("structural", "Structural Model", 
                           choices = c("Measurement model"                 = "none", 
                                       "Structural - Trait factors"        = "TF", 
                                       "Structural - Trait-Method factors" = "TMF", 
                                       "Structural - Both"                 = "both"),
                           selected = "TF") %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(
                       title = "Measurement model only: No interactions will be modeled. 
Trait factors: Person x Situation interactions will be modeled.        
Trait-Method factors: Method x Situation interactions will be modeled.
Both: Both Person x Situation & Method x Situation interactions will be modeled.")),
               
               
               
               checkboxInput(inputId="TFcov",
                             label="Trait Factor Covariances",
                             value=FALSE) %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="When ticked, covariances between all Trait Factors will be estimated.
If only specific covariances should be estimated, use the free input field for additional Syntax in the 'Additional Options' panel.
If the 'Structural Model' Option is set to 'TF' or 'both' no Trait Factor covariances will be estimated.")),
                   
               
               
               checkboxInput(inputId="OFcov",
                             label="Occasion Factor Covariances",
                             value=FALSE) %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="When ticked, covariances between all Occasion Factors will be estimated.
If only specific covariances should be estimated, use the free input field for additional Syntax in the 'Additional Options' panel.")),
               
               
               
               checkboxInput(inputId="TMFcov",
                             label="Trait-Method Factor Covariances",
                             value=FALSE) %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="When ticked, covariances between all Trait-Method Factors will be estimated.
If only specific covariances should be estimated, use the free input field for additional Syntax in the 'Additional Options' panel.
If the 'Structural Model' Option is set to 'TMF' or 'both' no Trait-Method Factor covariances will be estimated.")),
               
               
               
               checkboxInput(inputId="OMFcov",
                             label="Occasion-Method Factor Covariances",
                             value=FALSE) %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="When ticked, covariances between all Occasion-Method Factors will be estimated.
If only specific covariances should be estimated, use the free input field for additional Syntax in the 'Additional Options' panel.")),
               
               
               
               checkboxInput(inputId="inclOMF",
                             label="Inclusion of Occasion-Method Factors",
                             value=TRUE) %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="When ticked, Occasion-Method Factors OMF will be included in the model.")),
               
               
               
               checkboxInput(inputId="meanstr",
                             label="Estimate Meanstructure",
                             value=FALSE),
      
      
                       
              selectInput("invar", "Measurement Invariance (MI)", 
                          choices = c("Time Invariance"                = "time.invar", 
                                      "Metric MI - Methods"            = "metric.m", 
                                      "Metric MI - Fixed situations"   = "metric.s", 
                                      "Metric MI - Both"               = "metric.b", 
                                      "Scalar MI - Methods"            = "scalar.m", 
                                      "Scalar MI - Fixed situations"   = "scalar.s", 
                                      "Scalar MI - Both"               = "scalar.b", 
                                      "Residual MI - Methods"          = "residual.m", 
                                      "Residual MI - Fixed situations" = "residual.s", 
                                      "Residual MI - Both"             = "residual.b"),
                          selected = "time.invar") %>%
                shinyInput_label_embed(
                  shiny_iconlink() %>%
                    bs_embed_popover(title = "Time Invariance: Equality of loadings and intercepts across occasions (required).
Metric (weak) MI: Equality of loadings across conditions.
Scalar (strong) MI: Equality of loadings & intercepts across conditions.
Residual (strict) MI: Equality of loadings & intercepts & residuals across conditions.\n
Equality constraints can either be applied across methods, across fixed situations or across both.")),
      
      
              
              selectInput("TFequiv", "Trait Factor Equivalence", 
                          choices = c("Congenericity"         = "cong", 
                                      "Essential equivalence" = "ess.equiv", 
                                      "Equivalence"           = "equiv", 
                                      "Essential parallelity" = "ess.par", 
                                      "Parallelity"           = "par"),
                          selected = "par")%>%
                shinyInput_label_embed(
                  shiny_iconlink() %>%
                    bs_embed_popover(title="Congenericity: First indicator of latent variable loading = 1 & intercept = 0 (required for model identification).
Essential equivalence: Equal loadings (= 1) for all indicators of latent variable.
Equivalence: Equal loadings (= 1) & intercepts (= 0) for all indicators of latent variable (only for TF).
Essential parallelity: Equal loadings (= 1)  & error variances for all indicators of latent variable (only for TF).
Parallelity: Equal loadings (= 1), intercepts (= 0) & error variances for all indicators of latent variable(only for TF).")), 
      
              
              selectInput("OFequiv", "Occasion Factor Equivalence", 
                          choices = c("Congenericity"         = "cong", 
                                      "Essential equivalence" = "ess.equiv"),
                          selected = "ess.equiv"),
              
              selectInput("OMFequiv", "Occasion-Method Factor Equivalence", 
                          choices = c("Congenericity"         = "cong", 
                                      "Essential equivalence" = "ess.equiv"),
                          selected = "ess.equiv"),
      ),
              
              
      
      
      tabPanel("Additional Options" ,
               textAreaInput(inputId="addsyntax", label="Additional lavaan model syntax", 
                             value="", width="100%" ,height="100%", 
                             placeholder="O11 ~ O12 
TM211 ~~ TM322") %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="This field is optional.
Every individual specification needs to be written in a new line.
Check mmLSTrf() documentation from lsttheory package for variable naming convention!")),
               
               
               textAreaInput(inputId="additionalArgs", label="Additional lavaan function arguments", 
                             value="", width="100%" ,height="100%", 
                             placeholder="name=value, name2=value2, ...") %>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title="This field is optional.
Individual arguments need to be separated by a comma.
Check sem() documentation from lavaan package for additional arguments!"))
      ),
      
      
      
      
      tabPanel(actionButton("runModel", "Estimate Model", class = "btn btn-primary", 
                            style = "background-color: blue; color: white; font-weight: bold;"),
               h5("The model is being estimated. This may take a few minutes. \n
Click on one of the panels other than 'Data' to see when the estimation is finished!")
      )
      )
      ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel('Data', DT::DTOutput("table")),
      tabPanel("Model Summary & Coefficients", verbatimTextOutput("mod_coeff")), ##Output
      tabPanel("Model Fit & Parameters", verbatimTextOutput("fit_par")), ##Output
      tabPanel("lavaan Syntax", verbatimTextOutput("syntax"))
      )
    )
  )
)




####-----------SHINY-----------####

shinyApp(ui = ui, server = server)



