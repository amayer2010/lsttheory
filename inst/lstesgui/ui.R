#
# This is the user-interface definition of the LST-ES Shiny web application. 


# big TODOs
# TODO: implement model comparison (upload several .rds files)
# TODO: add help texts

# small TODOs
# should models 2-5 really be "invar" for the states? Or better "indicator.invar"?
# ! check path models
# !! make the "detailed options" clearer by using headings/categories (autoregression, covariates, traits, state (residuals)) 
# show warning message for output if the model did not converge/ there are Heywood cases

# optional TODOs
# pre-select number of measurement occasions for example data (=9)
# model output variance components: add graphical summary (x = t, y = value (0 - 1), lines for rel, con, spe, pred, upred per indicator)
# Output für Autoregressionen aufbereiten
# Output für Covariaten aufbereiten

# DONE
# adjust the implied invar-options for all 9 models
# add download option for re-structured data
# add download option for the fitted model (as .rds)
# change fitting function to new lstmodels_es() function 
# variance components summary table: add summary of pred and upred
# adjust covariate integration
#     + only trait covariates (delete everything for state covariates)
#     + include covariates in creating the dataset
#     + add gamma_t invariance to the detailed options if covariates are included
#     + adjust lsttheory_es() call to include covariates
# adjust summary (before "running" the model) for usable R-code
# added Yuan-corrected model fit to output


library(shiny)

# Define UI
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),
    
    navbarPage("LST-R Models for Experience Sampling Data",
               tabPanel("Model",

                #### Menu Panel: Data #########  
                        
                    navlistPanel(
                       "Data",
                ##### Reading in data file #########  
                       tabPanel("Reading in data file",
                                
                                tabsetPanel(
                                    tabPanel("Select data",
                                             h2("Reading in data file"),
                                             br(),
                                             fileInput("file1", "Upload data (SPSS, csv or rds file)", accept=c('.sav', '.csv', '.rds')),
                                    
                                             selectInput("exdata", "Select Example Data", 
                                                c("none","d_lst_es (example with day-specific traits)"), selected="none"),
                                             numericInput("ntimepoints", "What is the number of measurement occasions?", value=""),
                                             conditionalPanel(
                                               condition = "typeof input.ntimepoints !== 'undefined' && input.ntimepoints > 0",
                                               uiOutput("selectnperiods")
                                             )
                                    ),
                                    tabPanel("Print data", DT::DTOutput("datatable1")),
                                    tabPanel("More Info", 
                                             h2("Data Files"),
                                             tags$div("Data files are accepted in .csv or .sav format. Please provide a dataset in wide format. That means, there should be a single row in the dataset for each participants and columns for each variable measured at each occasion.
                                                                   For correct variable recognition, all repeated measures should end on a number indicating the measurement occasion. Variables which were assessed only once can be included as covariates explaining the trait(s)."),
                                             h2("Number of periods"),
                                             tags$div("If you have collected data several times a day, the number of periods may refer to the number of days. 
                                                                If you have collected data several times a week or month, indicate the number of weeks/ months etc. 
                                                                In this case, the 'day-specific' traits will in fact be week- or month-specific."))
                                )
                       ),
                ##### Variable specifics #########  
                       tabPanel("Variable specifics",
                                tabsetPanel(
                                  tabPanel("Variable specifics",
                                           h2("select variables"),
                                           textInput("seperator", 
                                                     "How is the indicator label seperated from the time label? (leave empty if there is no seperator)",
                                                     value = ""),
                                           h5("Please check if the names below correspond to the names of your variables. Consult the 'More Info' tab if the variables were not recognized correctly."),
                                           verbatimTextOutput("variables"),
                                           uiOutput("indicatorselect"),
                                           checkboxInput("includecovariates", 
                                                         "include manifest trait-covariates",
                                                         value=FALSE),
                                           conditionalPanel(
                                             condition = "input.includecovariates",
                                             uiOutput("traitcovariates")
                                           )
                                           ),
                                  tabPanel("More Info",
                                           h2("Automatic Variable Recognition"),
                                           tags$div("The names under $repeated_measures are recognized as variables assessed at each occasion. The indicators for the construct of interest have to be two or more of these variables. The names under $single_measures are recognized as variables measured only once. If there are no such variables in the dataset, the application will give the output 'character(0)'."),
                                           br(),
                                           tags$div("If your variables were not recognized correctly, you can try one of the following:",
                                             tags$ul(
                                               tags$li("make sure the number of measurement occasions (in the tab 'reading in data file') is correct"),
                                               tags$li("make sure all variable names (of repeated measures) end with the number indicating the measurement occasion"),
                                               tags$li("add a seperator between variable name and the number indicating the measurement occasion (e.g. firstvar_1, secondvar_1 etc.). This is usually relevant if the variable name ends on a number.")
                                               ),
                                           br(),
                                           h2("Covariates"),
                                           tags$div("You may include manifest covariates which were measured once during the study to further explain the trait aspect(s). 
                                                     Please focus on few covariates. With more variables in the model, there is a higher chance that your model may not converge,
                                                     i.e. that it is not possible to estimate the parameters of interest.
                                                     Latent covariates are not supported for the same reason.
                                                     If you cannot select a variable for the covariates explaining the latent traits, the dataset does not contain any variables 
                                                     which were measured only once, or the software did not recognize any such variables.")
                                             
                                           ),
                                           
                                  )
                                )
                                
                       ),
                       
                    #### Menu Panel: Model Specifics ###########  
                       
                       "Model specifics",
                       tabPanel("Model",
                    ##### Model ####            
                                tabsetPanel(
                                  tabPanel("Model",
                                           h2("Model"),
                                           br(),
                                           selectInput("lstmodel", #before: traitmodel 
                                                        label = "Please choose a model.",
                                                              #h3("radio"), 
                                                              choices = list("1: singletrait model, no autoregression"  = 1,
                                                                             "2: singletrait model with autoregression"  = 2,
                                                                             "3: period-specific traits with autoregression"  = 3,
                                                                             "4: indicator-specific traits with autoregression"  = 4,
                                                                             "5: period- and indicator-specific traits with autoregression"  = 5,
                                                                             "6: model 2 with state congenericity and measurement invariance within each period (e.g. day)"  = 6,
                                                                             "7: model 3 with state congenericity and measurement invariance within each period (e.g. day)"  = 7,
                                                                             "8: model 4 with state residual congenericity and measurement invariance within each period (e.g. day)"  = 8,
                                                                             "9: model 5 with state residual congenericity and measurement invariance within each period (e.g. day)"  = 9
                                                                             ),
                                                              selected = 1,
                                                       width = "80%"),
                                           conditionalPanel("input.lstmodel == 1",
                                                            shiny::img(src = "1_multistate-singletrait.png", height="900") # alternative: height="70%", but it weirdly adjusts between small and maximized window size
                                           ),
                                           conditionalPanel("input.lstmodel == 2",
                                                            shiny::img(src = "2_multistate-singletraitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 3",
                                                            shiny::img(src = "3_dayspecifictrait_mitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 4",
                                                            shiny::img(src = "4_indicatorspecific_mitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 5",
                                                            shiny::img(src = "5_day-and-indicatorspecific_mitAR.png", height="1000")
                                           )
                                                            
                                  ),
                                  tabPanel("More Info", 
                                           h2("LST-R Models"),
                                           tags$div("Details of each model are explained in Norget, Weiss & Mayer (202x). The supplementary material can be found on the OSF: https://osf.io/vd9br/"),
                                           tags$ul(
                                             tags$li("model 1"),
                                             tags$li("model 2"),
                                             tags$li("model 3"),
                                             tags$li("model 4"),
                                             tags$li("model 5"),
                                             tags$li("model 6"),
                                             tags$li("model 7"),
                                             tags$li("model 8"),
                                             tags$li("model 9")
                                           )
                                           # h2("Number of periods"),
                                           # tags$div("If you have collected data several times a day, the number of periods may refer to the number of days. 
                                                                # If you have collected data several times a week or month, indicate the number of weeks/ months etc. 
                                                                # In this case, the 'day-specific' traits will in fact be week- or month-specific.")
                                           # tags$div("You must specifiy the number of periods if at least one of the following applies",
                                           #          tags$ul(
                                           #            tags$li("your traitmodel is day-specific or day-and-indicator-specific"),
                                           #            tags$li("you make the assumption that your model is invariant within each period (menu tab 'Equivalence assumptions')")
                                           #          ),
                                           #          tags$span("Otherwise, the number of periods do not affect your model and you can simply keep it on 1."),
                                           #          tags$span("If you have collected data several times a day, the number of periods may refer to the number of days. 
                                           #                     If you have collected data several times a week or month, indicate the number of weeks/ months etc. 
                                           #                     In this case, the 'day-specific' traits will in fact be week- or month-specific.")
                                           # ),                                           
                                           # h2("The different traitmodels"),
                                           # tags$div(
                                           #   tags$span("You can select one of four traitmodels. The number of periods (days/weeks) have to be adjusted if you select a day-specific or day-and-indicator-specific model or if you choose period invariant equivalence assumptions in the next menu tab."),
                                           #   tags$span("Your choice can either be based on theory or you can run several models and compare them (outside of this application) to find the one that best describes the structure in your data.")
                                           # ),
                                           # 
                                           # tags$div(style = "width:400px; float:left",
                                           #   h4("The singletrait model"),
                                           #   tags$span("There is a stable influences across the entire measurement period."),
                                           #   img(src='Singletrait_mitAR_free.png', align = "left", alt="pathmodel of a singletrait model with autoregression",  width="400", height="600")
                                           # ),
                                           # tags$div(style = "width:400px; float:left",
                                           #   h4("The model with day-specific traits"),
                                           #   tags$span("there are stable influences across each day, e.g. mood, but not across the entire measurement period"),
                                           #   img(src='dayspecific_mitAR_free.png', align = "left", alt="pathmodel of a singletrait model with autoregression",  width="400", height="600")
                                           # ),
                                           # tags$div(style = "width:400px; float:left",
                                           #   h4("The model with indicator-specific traits"),
                                           #   tags$span("the indicators are not homogenous, e.g. coded positively and negatively. Their difference is captured in indicator-specific traits, which should correlate highly."),
                                           #   img(src='indicatorspecific_mitAR_free.png', align = "left", alt="pathmodel of a singletrait model with autoregression",  width="400", height="600")
                                           # ),
                                           # tags$div(style = "width:400px; float:left",
                                           #   h4("The model with  day-and-indicator-specific traits"),
                                           #   tags$div("This models combindes the model with day-specific traits and the model with indicator-specific traits."),
                                           #   img(src='day-and-indicatorspecific_mitAR_free.png', align = "left", alt="pathmodel of a singletrait model with autoregression",  width="400", height="600")
                                           # )

                                   )
                                )
                                ),
                ##### Details ####
                       tabPanel("Details", # before: Equivalence assumptions
                                tabsetPanel(
                                  tabPanel("Details",
                                           h2("Model Details"),
                                           br(),
                                           helpText("The LST-R model you selected implies the following equivalences. You can adjust each assumption individually. Click 'More Info' for details."),
                                           br(),
                                               selectInput("la_t_equiv", h5("Factor loadings of the latent trait variable(s)"),
                                                           choices = list("one", "indicator.invar", "period.invar", "free")),
                                               # la_o_equiv = "one", "time.invar", "period.invar", "free"
                                               selectInput("la_o_equiv", h5("Factor loadings of the latent state variables; for models with indicator-specific traits: factor loadings of the occasion factor (OCC)"),
                                                           choices = list("one", "time.invar", "period.invar", "free")),
                                               # la_s_equiv
                                               selectInput("la_s_equiv", h5("Autoregression between occasion factors"),
                                                          choices = list("zero", "invar", "overnight", "interval.invar", "free")), 
                                               # vzeta_eqiv = "time.invar", "period.invar", "free"
                                               selectInput("vzeta_eqiv", h5("Variances of the state residual (zeta)"),
                                                           choices = list("invar", "period.invar", "free")),
                                               # veps_equiv = "invar",  "time.invar", "indicator.invar", "period.invar", "free"
                                               selectInput("veps_equiv", h5("Variances of the residual/ error term (epsilon)"),
                                                           choices = list("invar",  "time.invar", "indicator.invar", "period.invar", "free")),
                                               # vtheta_equiv = "invar", "indicator.invar", "free"
                                               selectInput("vtheta_equiv", h5("Variances of the latent trait"),
                                                           choices = list("invar","indicator.invar", "free")),
                                               # nu_equiv = "zero", "period.invar", "free"
                                               selectInput("nu_equiv", h5("Intercepts of the indicators"),
                                                           choices = list("zero","period.invar", "free")),
                                               # alpha_equiv = "zero", "period.invar", "free"
                                               selectInput("alpha_equiv", h5("Intercepts of the latent states (only for second-order models, i.e. models without indicator-specific traits)"), #TODO: nur das singletrait und day-specific sind als Higher-Order modell formuliert und haben daher intercepts fuer die latent states. Die indikatorspezifischen (first-order/ Bifaktor) Modelle haben kein alpha. Muss dann also auch nicht angezeigt werden
                                                           choices = list("zero","period.invar", "free")),
                                               # mtheta_equiv = "invar", "free", "
                                               # TODO only if model =/= singletrait
                                               selectInput("mtheta_equiv", h5("Means of the latent traits"),
                                                           choices = list("invar","indicator.invar", "free")),
                                               conditionalPanel(
                                                 condition = "input.includecovariates",
                                                 selectInput("gamma_t_equiv", h5("Regression of covariates on the latent traits"),
                                                             choices = list("invar","indicator.invar", "free")),
                                               )
                                            
                                  ),
                                  tabPanel("More Info",
                                           h2("Detailed Equivalences"),
                                           tags$span("More Info will follow soon - stay tuned!"),
                                            tags$div("text here and some more"),
                                            br(),
                                           tags$div("here's some text"),
                                                    tags$ul(
                                                      tags$li("a bullet point"),
                                                      tags$li("another bullet point"),
                                                      tags$li("a third bullet point")
                                                    ),
                                           # h2("Equivalence assumptions"),
                                           # tags$div(
                                           #   h4("invariant"),
                                           #   tags$span("This is the most restrictive option. There is state- and trait-equivalence, meaning that all factor loadings are set to 1 and intercepts to 0. Residual variances are equal for all indicators and state residual variances are equal for all occasions. The autoregressive path is equal between all occasions. There is strong factorial invariance (= scalar invariance).")
                                           # ),
                                           # tags$div(
                                           #   h4("invariant within each period"),
                                           #   tags$span("There is state- and trait-congenericity, but strong factorial invariance (= scalar invariance) within each period. You can specify periods in the traitmodel menu tab. Each period should contain several measurement occasions.")
                                           # ),
                                           # tags$div(
                                           #   h4("free"),
                                           #   tags$span("There is congenericity on the state- and trait-side of the model, meaning that all factor loadings and intercepts are freely estimated. There is no measurement invariance.")
                                           # )
                                           )
                                )
                                ),
                       # tabPanel("Autoregression",
                       #          tabsetPanel(
                       #            tabPanel("autoregression",
                       #                     h2("Autoregression"),
                       #                     br(),
                       #                     checkboxInput("autoregression", 
                       #                                   "include autoregression (recommended with experience sampling data)",
                       #                                   value=TRUE),
                       #                     conditionalPanel(
                       #                       condition = "input.autoregression && input.detailedequivalences",
                       #                       # radioButtons("la_s_equiv",
                       #                       #              h5("Your global equivalence selection implies the following equivalence for your autoregressive paths. You can adjust this assumption below."),
                       #                       #              choices = list("time invariant" = "time.invar", 
                       #                       #                             "period invariant" = "period.invar",
                       #                       #                             "free" = "free"),
                       #                       #              selected = "time.invar")
                       #                       selectInput("la_s_equiv", h5("Your global equivalence selection implies the following equivalence for your autoregressive paths. You can adjust this assumption below."),
                       #                                   choices = list("time invariant" = "time.invar", 
                       #                                                  "period invariant" = "period.invar",
                       #                                                  "free" = "free"),
                       #                                   selected = "period.invar")
                       #                      )#,
                       #                     # conditionalPanel(
                       #                     #   condition = "input.includecovariates", # && state covariates ausgewählt
                       #                     #   checkboxInput("ar_zetacovariates", 
                       #                     #                 "include autoregression between state covariates",
                       #                     #                 value=TRUE),
                       #                     #   conditionalPanel(
                       #                     #     condition = "input.detailedequivalences",
                       #                     #     selectInput("gamma_ar_equiv", h5("Your global equivalence selection implies the following equivalence for autoregressive paths between the state covariates. You can adjust this assumption below."),
                       #                     #                 choices = list("time invariant" = "time.invar", 
                       #                     #                                "period invariant" = "period.invar",
                       #                     #                                "free" = "free"),
                       #                     #                 selected = "period.invar")
                       #                     #   )
                       #                     # )
                       #            ),
                       #            tabPanel("More Info",
                       #                     tags$div(
                       #                       h2("Autoregression"),
                       #                       tags$span("In experience sampling (and other intensive longitudinal methods), the time lag between measurements is very short. Therefore, each measurement is likely influenced by the previous one. We can account for this influence by including autoregressive effects between the situational influences in the model.")
                       #                     ))
                       #          )
                       #          ),
                       "-----",
                #### Summary ####
                       tabPanel("Summary",
                                tabsetPanel(
                                  tabPanel("Summary",
                                           h2("Summary of your model"),
                                           br(),
                                           textOutput("summary"),
                                           br(),
                                           helpText("You can also run this model directly in R/ RStudio with the syntax and data file provided below."),
                                           verbatimTextOutput("modelsyntax"),
                                           downloadButton("downloadData", "Download Data"),
                                           br(),
                                           helpText("Please be patient. Running the model may take several minutes, depending on your model and data."),
                                           #TODO implement a warning if only one indicator was selected
                                           ),
                                           
                                           
                                           
                                  tabPanel("More Info")
                                )
                    ),
                       tabPanel(actionButton("run", label="RUN", width = "100%"),
                    #### Model Output ####
                                tabsetPanel(
                                  id = "outputpanel",
                                  tabPanel("Variance components",
                                           DT::DTOutput("varcompsummary"),
                                           verbatimTextOutput("varcomp")
                                           ),
                                  tabPanel("lavaan syntax",
                                           verbatimTextOutput("lavaansyntax")),
                                  tabPanel("lavaan results",
                                           verbatimTextOutput("lavaanresults")),
                                  tabPanel("model fit",
                                           helpText("The fit measures below are corrected according to the procedure by Yuan et al. (2015) for SEM with many manifest variables. Uncorrected fit indices are provided with the lavaan results."),
                                           verbatimTextOutput("correctedfit")),
                                  
                                 # tabPanel("trait covariates"),
                                  tabPanel("download", 
                                           helpText("The fitted model can be downloaded below. The .rds file can be used for model comparisons in R/RStudio. Read the file into R with the function readRDS(). Compare with anova(model1@lavaanres, model2@lavaanres)."),
                                           br(),
                                           downloadButton("downloadModel", "Download fitted model")
                                           ),
                                  tabPanel("More Info")
                                ),
                                )
                    ),
                   
             ),
               # tabPanel("Model comparison", helpText("TODO: let users download their fitted model in the Model tab. Then here in this tab let them upload several models to compare them with anova(). Make the model comparison informative, i.e. provide some information on the interpretation.")),
               tabPanel("Info", helpText("This shiny app is a tool for fitting LST-R models, especially with experience sampling or other intensive longitudinal data. It is part of the R-package lsttheory. Details of the software are explained in Norget, Weiss & Mayer (202x). The supplementary material can be found on the OSF: https://osf.io/vd9br"))
    )

)
)
