
shinyUI(pageWithSidebar(
  headerPanel("lsttheory"),
  
  sidebarPanel(
    tabsetPanel(
      tabPanel('Data',        
        selectInput("exdata", "Select Example Data", 
            c("none","multistate","multitraitmultistate"),selected="none"),
        
        fileInput('file1', 'Choose SPSS File', accept=c('.sav')),
        
        sliderInput(inputId = "neta", label = "Number of eta variables",
          min = 0, max = 10, step = 1, value = 2),
        
        sliderInput(inputId = "ntheta", label = "Number of theta variables",
                  min = 0, max = 10, step = 1, value = 0)
      ),
    
      tabPanel('Options',        
        selectInput("tau", "Tau-Equivalence Assumption", 
            c("equi","ess","cong"),selected="equi"),
        selectInput("theta", "Theta-Equivalence Assumption", 
            c("equi","ess","cong"),selected="equi"),      
        checkboxInput(inputId = "lait0",
            label = "Invariance of lait0",
            value = FALSE),
        checkboxInput(inputId = "lait1",
            label = "Invariance of lait1",
            value = FALSE),
        checkboxInput(inputId = "lat0",
            label = "Invariance of lat0",
            value = FALSE),
        checkboxInput(inputId = "lat1",
            label = "Invariance of lat1",
            value = FALSE)      
    )
  )),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Data', dataTableOutput("mytable1")),
      tabPanel("lsttheory", verbatimTextOutput("summary")),
      tabPanel("lavaan Syntax", verbatimTextOutput("lavsyntax")),
      tabPanel("lavaan Results", verbatimTextOutput("lavresults")),
      tabPanel("Plot", plotOutput("plot1"))
    )
  )
))