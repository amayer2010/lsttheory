#
# This is the user-interface definition of the LST-ES Shiny web application. 

# TODO: implement model comparison
# TODO: show warning message for output if the model did not converge/ there are Heywood cases

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    navbarPage("LST-R Models for Experience Sampling Data",
               tabPanel("Model",

                ###### Menu Panel: Data #########  
                        
                    navlistPanel(
                       "Data",
                       tabPanel("Reading in data file",
                                
                                tabsetPanel(
                                    tabPanel("Select data",
                                             h2("Reading in data file"),
                                             br(),
                                             fileInput("file1", "Upload data (SPSS or csv file)", accept=c('.sav', '.csv')),
                                    
                                             selectInput("exdata", "Select Example Data", 
                                                c("none","d_lst_es (example with day-specific traits)"), selected="none"),
                                             numericInput("ntimepoints", "What is the number of measurement occasions?", value=""),
                                             conditionalPanel(
                                               condition = "typeof input.ntimepoints !== 'undefined' && input.ntimepoints > 0",
                                               uiOutput("selectnperiods")
                                             )
                                    ),
                                    tabPanel("Print data", DT::DTOutput("datatable1")),
                                    tabPanel("More Info", helpText("Data files are accepted in .csv or .sav format. Please provide a dataset in wide format. That means, there should be a seperate row in the dataset for each variable measured at each occasion."))
                                )
                       ),
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
                                                         "include manifest covariates",
                                                         value=FALSE),
                                           conditionalPanel(
                                             condition = "input.includecovariates",
                                             helpText("Warning: Covariate inclusion is not yet implemented in the model estimation. For now, your model will run without covariates."),
                                             uiOutput("statecovariates"),
                                             uiOutput("traitcovariates")
                                           )
                                           ),
                                  tabPanel("More Info",
                                           h2("automatic variable recognition"),
                                           tags$div("The names under $repeated_measures are recognized as variables assessed at each occasion. The indicators for the construct of interest have to be two or more of these variables. The names under $single_measures are recognized as variables measured only once. If there are no such variables in the dataset, the application will give the output 'character(0)'."),
                                           br(),
                                           tags$div("If your variables were not recognized correctly, you can try one of the following:",
                                             tags$ul(
                                               tags$li("make sure the number of measurement occasions (in the tab 'reading in data file') is correct"),
                                               tags$li("add a seperator between variable name and the number indicating the measurement occasion (e.g. firstvar_1, secondvar_1 etc.)")
                                               ),
                                           br(),
                                           h2("covariates"),
                                           tags$div("You may include manifest covariates which were measured one time during the study to further explain the trait aspect(s), 
                                                     or covariates which were measured at each occasion to further explain the state residuals. 
                                                     Please focus on few covariates. With more variables in the model, there is a higher chance that your model may not converge,
                                                     i.e. that it is not possible to estimate the parameters of interest.
                                                     Latent covariates are not supported for the same reason.")
                                             
                                           ),
                                           
                                  )
                                )
                                
                       ),
                       
                    ###### Menu Panel: Model Specifics ###########  
                       
                       "Model specifics",
                       tabPanel("Model",
                                tabsetPanel(
                                  tabPanel("Model",
                                           h2("Model"),
                                           br(),
                                           selectInput("lstmodel", #before: traitmodel 
                                                        label = "Please choose a model.",
                                                              #h3("radio"), 
                                                              choices = list("1: singletrait model, no autoregression"  = "model1",
                                                                             "2: singletrait model with autoregression"  = "model2",
                                                                             "3: period-specific traits with autoregression"  = "model3",
                                                                             "4: indicator-specific traits with autoregression"  = "model4",
                                                                             "5: period- and indicator-specific traits with autoregression"  = "model5",
                                                                             "6: model 2 with state congenericity and measurement invariance within each period (e.g. day)"  = "model6",
                                                                             "7: model 3 with state congenericity and measurement invariance within each period (e.g. day)"  = "model7",
                                                                             "8: model 4 with state residual congenericity and measurement invariance within each period (e.g. day)"  = "model8",
                                                                             "9: model 5 with state residual congenericity and measurement invariance within each period (e.g. day)"  = "model9"#,
                                                                             # "10: model 2 with intervall-invariant autoregression (less restrictive)" = "model10",
                                                                             # "11: model 3 with intervall-invariant autoregression (less restrictive)" = "model11",
                                                                             # "12: model 4 with intervall-invariant autoregression (less restrictive)" = "model12",
                                                                             # "13: model 5 with intervall-invariant autoregression (less restrictive)" = "model13",
                                                                             # "14: model 6 with intervall-invariant autoregression (less restrictive)" = "model14",
                                                                             # "15: model 7 with intervall-invariant autoregression (less restrictive)" = "model15",
                                                                             # "16: model 8 with intervall-invariant autoregression (less restrictive)" = "model16",
                                                                             # "17: model 9 with intervall-invariant autoregression (less restrictive)" = "model17"
                                                                             ),
                                                              selected = "model1",
                                                       width = "80%"),
                                           conditionalPanel("input.lstmodel == 'model1'",
                                                            shiny::img(src = "1_multistate-singletrait.png", height="900") # alternative: height="70%", but it weirdly adjusts between small and maximized window size
                                           ),
                                           conditionalPanel("input.lstmodel == 'model2'",
                                                            shiny::img(src = "2_multistate-singletraitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 'model3'",
                                                            shiny::img(src = "3_dayspecifictrait_mitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 'model4'",
                                                            shiny::img(src = "4_indicatorspecific_mitAR.png", height="1000")
                                           ),
                                           conditionalPanel("input.lstmodel == 'model5'",
                                                            shiny::img(src = "5_day-and-indicatorspecific_mitAR.png", height="1000")
                                           )
                                                            
                                  ),
                                  tabPanel("More Info", 
                                           h2("LST-R Models"),
                                           tags$div("Details of each model are explained in Norget & Mayer (202x). The supplementary material will be linked here, summarized details about each model will be added."),
                                           h2("Number of periods"),
                                           tags$div("If you have collected data several times a day, the number of periods may refer to the number of days. 
                                                                If you have collected data several times a week or month, indicate the number of weeks/ months etc. 
                                                                In this case, the 'day-specific' traits will in fact be week- or month-specific.")
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
                       tabPanel("Details", # before: Equivalence assumptions
                                tabsetPanel(
                                  tabPanel("Details",
                                           h2("Model Details"),
                                           br(),
                                           helpText("The LST-R model you selected implies the following equivalences. You can adjust each assumption individually. Click 'More Info' for details."),
                                           # radioButtons("globalequivalence",
                                           #              "global equivalence option",
                                           #              #choices = list("invar", "period.invar", "free"),
                                           #              choiceNames = list("invariant (most restrictive)", "invariant within each period", "free (no invariance)"),
                                           #              choiceValues = list("invar", "period.invar", "free"),
                                           #              selected = "invar"
                                           # ),
                                           br(),
                                           # checkboxInput("detailedequivalences", "Set more detailed equivalence assumptions", value = FALSE),
                                           
                                           # conditionalPanel("input.detailedequivalences",
                                                            
                                               # la_t_equiv = "one", "period.invar", "free"
                                               #TODO why can these not be time.invar??
                                               selectInput("la_t_equiv", h5("Factor loadings of the latent trait"),
                                                           choices = list("one", "period.invar", "free")),
                                               # la_o_equiv = "one", "time.invar", "period.invar", "free"
                                               selectInput("la_o_equiv", h5("Factor loadings of the occasion factor (OCC)"),
                                                           choices = list("one", "time.invar", "period.invar", "free")),
                                               # la_s_equiv
                                               selectInput("la_s_equiv", h5("Autoregression between occasion factors"),
                                                          choices = list("zero", "time.invar", "period.invar", "free")), # TODO period.invar should be renamed e.g. interval.invar, add overnight.invar
                                               # vzeta_eqiv = "time.invar", "period.invar", "free"
                                               selectInput("vzeta_eqiv", h5("Variances of the state residual (zeta)"),
                                                           choices = list("time.invar", "period.invar", "free")),
                                               # veps_equiv = "invar",  "time.invar", "indicator.invar", "period.invar", "free"
                                               selectInput("veps_equiv", h5("Variances of the residual/ error term (epsilon)"),
                                                           choices = list("invar",  "time.invar", "indicator.invar", "period.invar", "free")),
                                               # vtheta_equiv = "invar", "indicator.invar", "free"
                                               selectInput("vtheta_equiv", h5("Variances of the latent trait"),
                                                           choices = list("invar","indicator.invar", "free"),
                                                           selected = "indicator.invar"),
                                               # nu_equiv = "zero", "period.invar", "free"
                                               selectInput("nu_equiv", h5("Intercepts of the indicators"),
                                                           choices = list("zero","period.invar", "free")),
                                               # alpha_equiv = "zero", "period.invar", "free"
                                               selectInput("alpha_equiv", h5("Intercepts of the latent states"), #TODO: nur das singletrait und day-specific sind als Higher-Order modell formuliert und haben daher intercepts fuer die latent states. Die indikatorspezifischen (Bifaktor) Modelle haben kein alpha. Muss dann also auch nicht angezeigt werden
                                                           choices = list("zero","period.invar", "free")),
                                               # mtheta_equiv = "invar", "free", "
                                               # TODO only if model =/= singletrait
                                               selectInput("mtheta_equiv", h5("Means of the latent traits"),
                                                           choices = list("invar","indicator.invar", "free"),
                                                           selected = "indicator.invar")
                                            # )
                                  ),
                                  tabPanel("More Info",
                                           h2("Detailed Equivalences"),
                                           tags$span("More Info will follow soon - stay tuned!")
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
                       tabPanel("Summary",
                                tabsetPanel(
                                  tabPanel("Summary",
                                           h2("Summary of your model"),
                                           br(),
                                           textOutput("summary"),
                                           #TODO implement a warning if only one indicator was selected
                                           # conditionalPanel("!input.detailedequivalences && !input.indicators=='' ",
                                           #    h4("Simplified Path model"),
                                           #    helpText("The path model below shows a simplified version of your model, with 2 indicators and 6 measurement occasions (2 periods)."),
                                           #    
                                           #    # singletrait 
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "Singletrait_mitAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "Singletrait_mitAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "Singletrait_mitAR_free.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && !input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "Singletrait_ohneAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && !input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "Singletrait_ohneAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'singletrait' && !input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "Singletrait_ohneAR_free.png", width="600", height="800")
                                           #    ),
                                           #    
                                           #    #daypecific
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "dayspecific_mitAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "dayspecific_mitAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "dayspecific_mitAR_free.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && !input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "dayspecific_ohneAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && !input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "dayspecific_ohneAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-specific' && !input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "dayspecific_ohneAR_free.png", width="600", height="800")
                                           #    ),
                                           #    
                                           #    #indicator-specific
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "indicatorspecific_mitAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "indicatorspecific_mitAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "indicatorspecific_mitAR_free.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && !input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "indicatorspecific_ohneAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && !input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "indicatorspecific_ohneAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'indicator-specific' && !input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "indicatorspecific_ohneAR_free.png", width="600", height="800")
                                           #    ),
                                           #    
                                           #    #day-and-indicator-specific
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_mitAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_mitAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_mitAR_free.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && !input.autoregression && input.globalequivalence == 'invar'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_ohneAR_invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && !input.autoregression && input.globalequivalence == 'period.invar'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_ohneAR_period.invariant.png", width="600", height="800")
                                           #    ),
                                           #    conditionalPanel("input.traitmodel == 'day-and-indicator-specific' && !input.autoregression && input.globalequivalence == 'free'",
                                           #                     shiny::img(src = "day-and-indicatorspecific_ohneAR_free.png", width="600", height="800")
                                           #    )
                                           #    
                                           #    #TODO include the lst_models_es() call
                                           # 
                                           # )
                                           
                                  ),
                                           
                                           
                                           
                                  tabPanel("More Info")
                                )
                    ),
                       tabPanel(actionButton("run", label="RUN", width = "100%"),
                                tabsetPanel(
                                  tabPanel("Variance components",
                                           DT::DTOutput("varcompsummary"),
                                           verbatimTextOutput("varcomp")
                                           ),
                                  tabPanel("lavaan syntax",
                                           verbatimTextOutput("lavaansyntax")),
                                  tabPanel("lavaan results",
                                           verbatimTextOutput("lavaanresults")),
                                  tabPanel("download", helpText("TODO: let users download (1) the fitted lsttheory object for model comparisons and (2) model output, knitted with RMarkdown")),
                                  tabPanel("More Info")
                                ),
                                )
                    ),
                   
             ),
               tabPanel("Model comparison", helpText("TODO: let users download their fitted model in the Model tab. Then here in this tab let them upload several models to compare them with anova(). Make the model comparison informative, i.e. provide some information on the interpretation.")),
               tabPanel("Info", helpText("This shiny app has two parts: one for fitting an LST-R model and one for comparing fitted models."))
    )

)
)
