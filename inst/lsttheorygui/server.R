
require(lsttheory)
require(semPlot)
require(Hmisc)

shinyServer(function(input, output, session) {
  
  ######## Reactive Data Input ########
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile) & exdata=="none"){
      return(NULL)
    }else if(is.null(inFile) & exdata=="multistate"){
      return(multistate)
    }else if(is.null(inFile) & exdata=="multitraitmultistate"){
      return(multitraitmultistate)
    }else if(!is.null(inFile))
      if(grepl(".csv",inFile)){
        return(read.csv(inFile$datapath))      
      }else{
        return(spss.get(inFile$datapath))      
      }      
  })
  
  ###### Reactive Run Model #########
  model <- reactive({
    
    d <- dataInput()
    
    lsttheory(neta=input$neta, 
              ntheta=input$ntheta, 
              data=d,
              equiv.assumption=list(tau=input$tau, 
                                    theta=input$theta),
              scale.invariance=list(lait0=input$lait0,
                                    lait1=input$lait1,
                                    lat0=input$lat0,
                                    lat1=input$lat1))    
  })
  
  
  
  
  ###### Output Data Table #########  
  output$mytable1 = renderDataTable({ 
    d <- dataInput()
    d
  })

  ###### Output Plot 1 #########
  output$plot1 <- renderPlot({    

    m1 <- model()
    semPaths(m1@lavaanres, style="lisrel", intercepts=F, 
             layout="tree2", rotation=4, nCharNodes=4, nCharEdges=4,
             optimizeLatRes=F, residScale=10)        
  })

  
  ###### Output lsttheory Summary #########
  output$summary <- renderPrint({        
    m1 <- model()
    m1    
  })
    
  ###### Output Lavaan Syntax #########
  output$lavsyntax <- renderPrint({      
    m1 <- model()
    cat(m1@lavaansyntax)  
  })

  ###### Output Lavaan Results #########
  output$lavresults <- renderPrint({      
    m1 <- model()
    summary(m1@lavaanres)
  })
  
})