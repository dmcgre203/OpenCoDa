library(shiny)

server <- function(input, output) {
  
  require(ggtern)  
  require(robCompositions)
  require(zCompositions)
  
  # Setup reactive data
  dat <- reactive({
    if(is.null(input$file1)) return()
    read.csv(input$file1$datapath,
             header = input$header,
             sep    = input$sep,
             quote  = input$quote,
             dec    = input$deci)
  })
  
  output$rawData_Header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Raw Data"
  })
  
  output$rawDataSummary_Header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Raw Data Summary Table"
  })
  
  # Display data
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    df <- dat()
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  # Raw data summary
  output$rawSummary <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    df   <- dat()
    out1 <- do.call(cbind, lapply(df, summary))
    out  <- data.frame(rownames(out1),out1)
    colnames(out)[1] <- "Statistic"
    out
    
  })
  
  # Setup UI for selecting response variable 
  output$choose_response <- renderUI({ 
    
    # this is the code taken from the column subsetting example ...to subset columns
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    
    radioButtons("model_response","Identify response",choices=col_names,selected = col_names[1])
    
  })
  
  # Setup UI for selecting continuous explanatory variables
  output$choose_covariates <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_covars", "Choose continuous covariates",
                       choices = col_names,
                       selected = NULL)
  })
  
  # Setup UI for selecting categorical explanatory variables
  output$choose_cofactors <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_cofactors", "Choose categorical covariates",
                       choices = col_names,
                       selected = NULL)
  })
  
  # Print aov model formula used
  output$modelText <- renderText({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    #    if(is.null(input$model_covars)) return()
    if(is.null(input$model_response)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")",sep="") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    paste(input$model_response," ~ ",paste(covars2,collapse=" + "))
    
    if(is.null(input$model_cofactors)) {
      paste(input$model_response," ~ ",covars1)
    } else if(is.null(input$model_covars)) {
      paste(input$model_response," ~ ",cofacs2)
    } else {
      paste(input$model_response," ~ ",paste(covars2,collapse=" + "))
    }
    
    
  })
  
  # Setup aov model for use in other routines
  runRegression <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    #    if(is.null(input$model_covars)) return()
    if(is.null(input$model_response)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    if(is.null(input$model_cofactors)) {
      aov(as.formula(paste(input$model_response," ~ ",covars1)),data=dat())
    } else if(is.null(input$model_covars)) {
      aov(as.formula(paste(input$model_response," ~ ",cofacs1)),data=dat())
    } else {
      aov(as.formula(paste(input$model_response," ~ ",covars2)),data=dat())
    }
    
    
  })
  
  # Show drop1 table for selecting covariates
  output$regTab <- renderTable({
    if(!is.null(input$model_covars)){
      model.drop       <- drop1(runRegression(),test="Chisq")
      model.drop.table <- cbind(rownames(model.drop),as.data.frame(model.drop))
      colnames(model.drop.table)[1] <- "Eliminated Covariate"
      model.drop.table
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
  output$cofactorSummary <- renderPrint({
    
    if(is.null(dat())) return()
    if(is.null(input$model_cofactors)) return()
    
    df   <- dat()
    dat1 <- data.frame(df[,input$model_cofactors])
    zfac <- lapply(dat1,as.factor)
    
    out<-lapply(zfac, function(x) {
      if (is.numeric(x)) return(summary(x))
      if (is.factor(x))  return( rbind(levels(x),paste0(prop.table(table(x))*100,"%")) )
    })
    out
    
  })
  
  # Setup UI for selecting compositional variables
  output$choose_CODA <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_CODAvars", "Choose physical activity compositional variables",
                       choices = col_names,
                       selected = NULL)
  })
  
  scale_CODAdat <- reactive({
    
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    
    df      <- dat()    
    df.CODA <- data.frame(df[,input$model_CODAvars])
    totals  <- rowSums(df.CODA)
    df.CODA <- df.CODA/totals
    
    if(input$useImpute=="Yes") {
      if((any(df.CODA==0))|(any(is.na(df.CODA))) ) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEMplus(df.CODA,dl=dl1,ini.cov="multRepl")
      }
    }
    
    df.CODA
    
  })
  
  #UI to confirm whether to impute zero values
  output$choose_useImpute <- renderUI({ 
    
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()   
    
    col_names <- c("Yes","No")
    
    radioButtons("useImpute","Do you require to impute zero and/or n/a values?",choices=col_names,
                 selected = col_names[2])
    
  })
  
  #UI to set detection limits for imputation
  output$choose_detectLimits <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
    
    D  <- length(input$model_CODAvars)
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    CODA.min <- min(df.CODA[df.CODA>0])
    
    lapply(seq(D),function(i){
      numericInput(paste0("detectLimit_",i), input$model_CODAvars[i], val=CODA.min)
    })
  })
  
  output$choose_detectLimits_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
    
    "Specify detection limits by component"
    
  })
  
  output$zPatterns <- renderPlot({
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])    
    
    if(length(which(df.CODA==0))>0) {
      zPatterns(df.CODA,label=0, plot=T)
      output <- recordPlot()
      return(output)
    } else{
      return()
    }
    
  })
  
  output$zPatterns_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat)) return()  
    if(is.null(input$model_CODAvars)) return()      
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars]) 
    
    if(length(which(df.CODA==0))>0) {
      "Patterns of Zeroes in Data"
    } else {
      "No Zeroes in Data"
    }
    
  })
  
  # ilr transform of compositional variables 
  CODAdat <- reactive({
    
    if(is.null(dat())) return()  
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    if(length(df.CODA)<3) return()  
    
    if(input$useImpute=="Yes") {
      if((any(df.CODA==0))|(any(is.na(df.CODA))) ) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEMplus(df.CODA,dl=dl1,ini.cov="multRepl")
      }
    }
    
    zz <- get_ilrCoord(df.CODA)
    zz
  })
  
  # display ilr coordinates
  output$ilrCoords <- renderTable({
    
    if(is.null(dat())) return()     
    if(is.null(CODAdat())) return()
    
    CODAdat.display <- data.frame(dat()[,1],CODAdat())
    colnames(CODAdat.display)[1] <- colnames(dat())[1]
    
    if(input$disp == "head") {
      return(head(CODAdat.display))
    }
    else {
      return(CODAdat.display)
    }
    
  }) 
  
  # Setup UI for selecting compositional variables
  output$choose_ilr <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(CODAdat())) return()
    
    # Get the data set with the appropriate name
    col_names2 <- colnames(CODAdat())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_ilr_coords", "Choose ilr coordinates",
                       choices = col_names2,
                       selected = NULL)
  })  
  
  # Display sumary of Imputation
  output$imputeTable <- renderTable({
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    D <- length(df.CODA)
    
    if(input$useImpute=="Yes") {
      if((any(df.CODA==0))|(any(is.na(df.CODA))) ) {
        dl1      <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1      <- as.numeric(dl1)
        df.Fixed <- lrEMplus(df.CODA,dl=dl1,ini.cov="multRepl")
        t1       <- which(!apply(df.Fixed == df.CODA,1,all))
        output   <- data.frame(df.CODA[t1,],"TO________"="",df.Fixed[t1,])
        return(output)
      } else{
        return()
      }
    } else{
      return()
    }
  })
  
  output$imputeTable_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()   
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars]) 
    
    if(length(which(df.CODA==0))>0) {
      "Individuals where zero values were imputed"
    } else {
      "No Zeroes in Data"
    }
    
  })
  
  avgCODA <- reactive({
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()  
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    D <- length(df.CODA)
    
    if(input$useImpute=="Yes") {
      if((any(df.CODA==0))|(any(is.na(df.CODA))) ) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEMplus(df.CODA,dl=dl1,ini.cov="multRepl")
      } else{
        return()
      }
    }
    
    gmall  <- apply(df.CODA,2,gm)
    gmall  <- gmall/sum(gmall)
    
    gmall  <- as.data.frame(t(gmall))
    gmall
    
  })
  
  #Display CoDa average
  output$avgCODA_Table <- renderTable({
    if(is.null(avgCODA())) return()
    
    return(avgCODA())
  })
  
  output$avgCODA_Table_Header <- renderText({
    if(is.null(avgCODA())) return()
    
    "CoDa (geometric) Average Composition"
  })
  
  # setup combined table
  Fulldat <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(CODAdat())) return()
    
    data.frame(dat(),CODAdat())
    
  })  
  
  # fit linear model
  runFullRegression <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_response)) return()
    if(is.null(input$model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(input$model_ilr_coords,collapse="+") 
    
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) {
      coda2   <- coda1
    } else if(is.null(input$model_cofactors)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(input$model_covars)) {
      coda2   <- paste(c(cofacs1,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    aov(as.formula(paste(input$model_response,
                         " ~ ",coda2)),data=Fulldat())
  }) 
  
  # fit linear model
  runFullRegression2 <- reactive({
    
    if(is.null(dat())) return()  
    #    if(is.null(input$model_cofactors)) return()
    #    if(is.null(input$model_covars)) return()
    #    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    if(is.null(input$model_response)) return()
    if(is.null(input$model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(input$model_ilr_coords,collapse="+") 
    
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) {
      coda2   <- coda1
    } else if(is.null(input$model_cofactors)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(input$model_covars)) {
      coda2   <- paste(c(cofacs1,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    lm(as.formula(paste(input$model_response,
                        " ~ ",coda2)),data=Fulldat())
  })
  
  # Show drop1 table for selecting covariates
  output$fullRegTab <- renderTable({
    if(!is.null(runFullRegression())){
      model.drop       <- drop1(runFullRegression(),test="Chisq")
      model.drop.table <- cbind(rownames(model.drop),as.data.frame(model.drop))
      colnames(model.drop.table)[1] <- "Eliminated Covariate"
      model.drop.table
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })  
  
  # Show coefficients for selected covariates
  output$fullRegCoef <- renderTable({
    if(!is.null(runFullRegression())){
      model.coef       <- round(coef(runFullRegression())[-1],4)
      model.coef.table <- data.frame(cbind(names(model.coef),model.coef))
      colnames(model.coef.table)[1] <- "Covariate"
      colnames(model.coef.table)[2] <- "Coefficient"
      model.coef.table
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  }) 
  
  # Show coefficient of determination = Rsquared
  output$fullRegRsquared <- renderTable({
    if(!is.null(runFullRegression2())){
      R_Squared <- data.frame(summary(runFullRegression2())$r.squared)
      colnames(R_Squared) <- "Coefficient of Determination (R^2)"
      R_Squared
    } 
  }) 
  
  
  output$plot1 <- renderPlot({
    if(is.null(runFullRegression())) return ()
    plot(runFullRegression(),which=1)
  })
  
  output$plot2 <- renderPlot({
    if(is.null(runFullRegression())) return ()
    plot(runFullRegression(),which=2)
  })
  
  # Setup UI for selecting continuous explanatory variables
  output$choose_covariates_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_covars_H0", "Choose continuous covariates",
                       choices = col_names,
                       selected = NULL)
  })
  
  # Setup UI for selecting categorical explanatory variables
  output$choose_cofactors_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_cofactors_H0", "Choose categorical covariates",
                       choices = col_names,
                       selected = NULL)
  })
  
  # Print aov model formula used
  output$modelText_H0 <- renderText({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors_H0) && is.null(input$model_covars_H0)) return()
    #    if(is.null(input$model_covars)) return()
    if(is.null(input$model_response_H0)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")",sep="") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    paste(input$model_response," ~ ",paste(covars2,collapse=" + "))
    
    if(is.null(input$model_cofactors)) {
      paste(input$model_response," ~ ",covars1)
    } else if(is.null(input$model_covars)) {
      paste(input$model_response," ~ ",cofacs2)
    } else {
      paste(input$model_response," ~ ",paste(covars2,collapse=" + "))
    }
    
  })
  
  # Setup UI for selecting compositional variables
  output$choose_ilr_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(CODAdat())) return()
    
    # Get the data set with the appropriate name
    col_names2 <- colnames(CODAdat())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_ilr_coords_H0", "Choose ilr coordinates",
                       choices = col_names2,
                       selected = NULL)
  })  
  
  # Setup aov model for use in other routines
  runRegression_H0 <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors_H0) && is.null(input$model_covars_H0)) return()
    if(is.null(input$model_response)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    if(is.null(input$model_cofactors_H0)) {
      aov(as.formula(paste(input$model_response," ~ ",covars1)),data=dat())
    } else if(is.null(input$model_covars_H0)) {
      aov(as.formula(paste(input$model_response," ~ ",cofacs1)),data=dat())
    } else {
      aov(as.formula(paste(input$model_response," ~ ",covars2)),data=dat())
    }
    
  })
  
  # fit linear model
  runFullRegression_H0 <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_response)) return()
    if(is.null(input$model_ilr_coords_H0)) return(runRegression_H0())
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(input$model_ilr_coords_H0,collapse="+") 
    
    if(is.null(input$model_cofactors_H0) && is.null(input$model_covars_H0)) {
      coda2   <- coda1
    } else if(is.null(input$model_cofactors_H0)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(input$model_covars_H0)) {
      coda2   <- paste(c(cofacs1,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    aov(as.formula(paste(input$model_response,
                         " ~ ",coda2)),data=Fulldat())
  }) 
  
  # Show cox regression summary for selecting covariates
  output$fullRegSummary_H0 <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    
    summary(runFullRegression_H0())
    
  })
  
  # Show anova comparison between H0 and H1
  output$fullRegComparison <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(runFullRegression())) return()
    if(is.null(runFullRegression_H0())) return()
    
    anova(runFullRegression(),runFullRegression_H0(),test="Chisq")
    
  })
  
  
  # Setup Title for Data Table
  output$titleData <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(input$file1)) return()
    
    # Render text
    "Uploaded Data"
    
  })
  
  # Setup Title for Data Table
  output$titleConfounders <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    #    if(is.null(input$model_covars)) return()
    if(is.null(input$model_response)) return()
    
    # Render text
    "Confounding Variable Selection"
    
  })
  
  # Setup Title for Data Table
  output$titleConfounderModel <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    if(is.null(input$model_response)) return()
    
    # Render text
    "   Trial Model: "
    
  })
  
  # Setup Title for ILR Coordinates Data Table
  output$titleILRData <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(dat())) return()     
    if(is.null(CODAdat())) return()
    
    # Render text
    "Uploaded Data - derived ilr coordinates"
    
  })  
  
  output$titleCODA <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(runFullRegression())) return()     
    
    # Render text
    "CODA Model Selection"
    
  })  
  
  output$titleCODAModel <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(runFullRegression())) return()     
    
    # Render text
    "   Trial model"
    
  })  
  
  output$titleCODACoef <- renderText({ 
    
    # If missing input, return so title doesn't display
    if(is.null(runFullRegression())) return()     
    
    # Render text
    "CODA Regression Coefficients"
    
  })  
  
  
  
  # Setup UI for selecting cofactors for prediction
  output$illustrationCofactors <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model.cofactors        <- input$model_cofactors
    model.cofactors.seq    <- seq(length(model.cofactors))
    df                     <- data.frame(dat())
    
    model.cofactors.vals   <- data.frame(df[,model.cofactors])
    model.cofactors.vals   <- data.frame(apply(model.cofactors.vals,2,as.factor))
    model.cofactors.levels <- lapply(model.cofactors.vals,levels)
    
    # Get the data set value for variable name
    lapply(model.cofactors.seq,function(i){
      radioButtons(paste0("factor_",i), model.cofactors[i], model.cofactors.levels[[i]])
    }) 
  })
  
  
  # Setup UI for selecting cofactors for prediction
  output$illustrationCovariates <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$model_covars)) return()     
    
    model.covars           <- input$model_covars
    model.covars.seq       <- seq(length(model.covars))
    df                     <- data.frame(dat())
    
    model.covars.vals   <- data.frame(df[,model.covars])
    model.covars.mins   <- lapply(model.covars.vals,quantile,0.25)
    model.covars.maxs   <- lapply(model.covars.vals,quantile,0.75)
    model.covars.mean   <- lapply(model.covars.vals,mean)
    
    # Get the data set value for variable name
    lapply(model.covars.seq,function(i){
      numericInput(paste0("covariate_",i), model.covars[i], min=model.covars.mins[[i]],
                   max=model.covars.maxs[[i]], val=model.covars.mean[[i]])
    }) 
  })
  
  # Setup UI for identifying plot variable x
  output$choose_illustrationX <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()   
    if(is.null(input$model_CODAvars)) return()  
    
    # Get the data set with the appropriate name
    col_names <- input$model_CODAvars
    
    # Create the checkboxes and select them all by default
    
    radioButtons("VarX","Choose variable x",choices=col_names,selected = col_names[1])
    
  })  
  
  # Setup UI for identifying plot variable y
  output$choose_illustrationY <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()    
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$VarX)) return() 
    
    # Get the data set with the appropriate name
    col_names1 <- input$model_CODAvars
    col_names  <- col_names1[!col_names1 %in% input$VarX]
    
    # Create the checkboxes and select them all by default
    
    radioButtons("VarY","Choose variable y",choices=col_names,selected = col_names[1])
    
  })  
  
  # Setup UI for identifying plot variable z
  output$choose_illustrationZ <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()    
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    
    # Get the data set with the appropriate name
    col_names1 <- input$model_CODAvars
    col_names  <- col_names1[!col_names1 %in% c(input$VarX,input$VarY)]
    
    # Create the checkboxes and select them all by default
    
    radioButtons("VarZ","Choose variable z",choices=col_names,selected = col_names[1])
    
  })   
  
  # Setup UI for selecting fixed compositional variables for 3d plot
  output$illustrationComp <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    if(is.null(input$VarZ)) return() 
    
    col_names_all <- input$model_CODAvars
    D             <- length(col_names_all)    
    
    if(D<4) return() 
    
    xnam  <- input$VarX
    ynam  <- input$VarY
    znam  <- input$VarZ
    
    col_names  <- col_names_all[!col_names_all %in% c(xnam,ynam,znam)]
    df.CODA    <- scale_CODAdat()
    offvars    <- df.CODA[,col_names]
    
    model.covars        <- col_names
    model.covars.seq    <- seq(length(col_names))
    
    model.covars.vals   <- offvars
    model.covars.mins   <- lapply(model.covars.vals,min)   # lapply to offvars
    model.covars.maxs   <- lapply(model.covars.vals,max)   # lapply to offvars
    model.covars.mean   <- lapply(model.covars.vals,mean)  # lapply to offvars
    
    # Get the data set value for variable name
    lapply(model.covars.seq,function(i){
      numericInput(paste0("fixedCODA_",i), model.covars[i], min=model.covars.mins[[i]],
                   max=model.covars.maxs[[i]], val=model.covars.mean[[i]])
    }) 
  })
  
  #=====================#
  # Illustration output #
  #=====================#
  
  genCODA <- function(CODA.data,sims) {
    D      <- dim(CODA.data)[2]
    z1     <- pivotCoord(CODA.data,1)
    minis  <- apply(z1,2,min)
    maxis  <- apply(z1,2,max)
    ranges <- lapply(seq(D-1), function(i){seq(minis[i],maxis[i],length.out=sims)})
    grid1  <- expand.grid(ranges)   
    gridX  <- pivotCoordInv(grid1)
    
    colnames(gridX) <- colnames(CODA.data)
    
    return(gridX)
  }
  
  
  # ilr transform of compositional variables 
  illCODADat <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return()  
    
    not_loaded_cofactors  <- (!is.null(input$model_cofactors)) && (is.null(input$factor_1))
    not_loaded_covariates <- (!is.null(input$model_covars)) && (is.null(input$covariate_1))
    
    if(not_loaded_cofactors)  return()
    if(not_loaded_covariates) return()
    
    # Generate grid of compositional data
    # ===================================
    
    # Compute parameters
    D       <- length(input$model_CODAvars)
    if(D<3) return()                                    # Stop if insufficient CoDa dimensions
    
    sims    <- 50
    df      <- dat()
    df.CODA <- scale_CODAdat()
    
    grid1 <- genCODA(df.CODA,sims)
    gridX <- grid1[,input$model_CODAvars]
    df1   <- get_ilrCoord(gridX)
    
    # Covariates
    # ==========
    
    cvt <- "input$covariate_"
    cvf <- "input$factor_"
    
    no_cofactors  <- (is.null(input$model_cofactors)) | (is.null(input$factor_1))
    no_covariates <- (is.null(input$model_covars)) | (is.null(input$covariate_1))
    
    if(!no_covariates) {
      cvt_seq <- seq(length(input$model_covars))
      covars1 <- lapply(cvt_seq, function(i) {eval(parse(text=paste0(cvt,i)))})    
    } 
    
    if(!no_cofactors) {
      cvf_seq <- seq(length(input$model_cofactors))
      cofacs1 <- lapply(cvf_seq, function(i) {eval(parse(text=paste0(cvf,i)))}) 
    } 
    
    if( no_cofactors && no_covariates) {
      dfOut <- as.data.frame(cbind(grid1,df1))
    } else if(no_cofactors) {
      df_cov  <- data.frame(covars1)
      colnames(df_cov) <- c(input$model_covars)
      dfOut <- as.data.frame(cbind(grid1,df1,df_cov))
    } else if(no_covariates) {
      df_cov  <- data.frame(cofacs1)
      colnames(df_cov) <- c(input$model_cofactors)
      dfOut <- as.data.frame(cbind(grid1,df1,df_cov))
    } else {
      df_cov  <- data.frame(covars1,cofacs1)
      colnames(df_cov) <- c(input$model_covars,input$model_cofactors)
      dfOut <- as.data.frame(cbind(grid1,df1,df_cov))
    }
    
    dfOut
    
  })
  
  # ilr transform of compositional variables 
  illustration <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(illCODADat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return() 
    
    model <- runFullRegression()
    data  <- illCODADat()
    
    Prediction <- predict(model,data)
    Prediction
    
  })
  
  # ilr transform of compositional variables 
  illCODADat2 <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return()  
    
    not_loaded_cofactors  <- (!is.null(input$model_cofactors)) && (is.null(input$factor_1))
    not_loaded_covariates <- (!is.null(input$model_covars)) && (is.null(input$covariate_1))
    
    if(not_loaded_cofactors)  return()
    if(not_loaded_covariates) return()
    
    Dall       <- length(input$model_CODAvars)
    if(Dall<3) return()                                    # Stop if insufficient CoDa dimensions
    if(is.null(input$VarX)) return()
    if(is.null(input$VarY)) return()
    if(is.null(input$VarZ)) return()
    #    if(is.null(input$model_CODAvars)) return()
    
    not_loaded_fixedCODA <- ( (Dall>3) && (is.null(input$fixedCODA_1)) )
    
    if(not_loaded_fixedCODA) return()
    
    col_names_all <- input$model_CODAvars    
    D             <- length(col_names_all)
    
    sims  <- 50
    xnam  <- input$VarX
    ynam  <- input$VarY
    znam  <- input$VarZ
    cvc   <- "input$fixedCODA_"    
    cvt   <- "input$covariate_"
    cvf   <- "input$factor_"    
    mcv   <- input$model_covars
    mcf   <- input$model_cofactors
    
    if (D>3) {
      cvc_seq  <- seq(D-3)
      fixcomp1 <- lapply(cvc_seq, function(i) {eval(parse(text=paste0(cvc,i)))})       
    } 
    
    cvt_seq  <- seq(length(mcv))
    cvf_seq  <- seq(length(mcf))
    covars1  <- lapply(cvt_seq, function(i) {eval(parse(text=paste0(cvt,i)))}) 
    cofacs1  <- lapply(cvf_seq, function(i) {eval(parse(text=paste0(cvf,i)))})
    
    #makeCODADat(sims,D,col_names_all,xnam,ynam,znam,cvc,cvt,cvf,mcv,mcf)
    
    # Construct compositional data
    col_names_xyz  <- col_names_all[col_names_all %in% c(xnam,ynam,znam)]
    if (D>3) {col_names_off  <- col_names_all[!col_names_all %in% c(xnam,ynam,znam)]  } 
    
    df      <- dat()
    df.CODA <- scale_CODAdat()
    df.xyz  <- data.frame(df.CODA[,col_names_xyz])
    if (D>3) {df.off  <- data.frame(df.CODA[,col_names_off])}
    
    grid1i  <- genCODA(df.xyz,sims)
    
    if (D>3) {
      grid2           <- data.frame(do.call("cbind", fixcomp1))
      colnames(grid2) <- col_names_off
      grid1           <- as.matrix(grid1i) * (1-rowSums(grid2))
      grid1           <- as.data.frame(grid1) 
      colnames(grid1) <- col_names_xyz
      gridX           <- data.frame(grid1,grid2)
    }else {
      grid1           <- as.data.frame(grid1i)
      colnames(grid1) <- col_names_xyz
      gridX           <- data.frame(grid1)
    }
    
    gridX <- gridX[,input$model_CODAvars]
    df1   <- get_ilrCoord(gridX)
    
    # Construct covariates
    df_cov           <- data.frame(covars1,cofacs1)
    colnames(df_cov) <- c(mcv,mcf) 
    
    # Bind it all together
    dfOut <- as.data.frame(cbind(gridX,df1,df_cov))
    
    #Return
    dfOut
    
  })
  
  # ilr transform of compositional variables 
  illustration2 <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model <- runFullRegression()
    data  <- illCODADat2()
    
    Prediction <- predict(model,data)
    Prediction
    
  })
  
  # Generate data frame containing predicted value and illustrative data
  output$illustrativeData <- renderTable({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(illCODADat2())) return()
    if(is.null(illustration2())) return()
    
    df1        <- illCODADat2()
    prediction <- illustration2()
    df         <- cbind(prediction,df1)
    df
  })  
  
  
  output$illustrativePlot <- renderPlot({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(illCODADat2())) return() 
    if(is.null(illustration2())) return() 
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    if(is.null(input$VarZ)) return() 
    
    df            <- illCODADat2()
    df.CODA       <- scale_CODAdat()
    
    dat.x         <- df.CODA[,input$VarX]
    dat.y         <- df.CODA[,input$VarY]
    dat.z         <- df.CODA[,input$VarZ]
    
    dat.R         <- rep(100,length(dat.x))
    
    actual.data.graph <- data.frame(cbind("x"=dat.x,"y"=dat.y,"z"=dat.z,"response"=dat.R))    
    
    x        <- df[,input$VarX] 
    y        <- df[,input$VarY]
    z        <- df[,input$VarZ]
    response <- illustration2()
    
    predict.data.graph <- data.frame(cbind(x,y,z,response))
    
    if(input$dispDat){
      plot1<-ggtern(data=predict.data.graph,aes(x=x,y=y,z=z,value=response))+
        #tern_limits(T=0.6,L=0.6,R=0.2) +
        geom_point(aes(colour=response)) +
        geom_point(data=actual.data.graph,alpha=0.25) +
        scale_colour_gradient(low="yellow",high="red") +
        theme_clockwise()+ 
        labs(x = input$VarX,y=input$VarY,z=input$VarZ)
    } else {
      plot1<-ggtern(data=predict.data.graph,aes(x=x,y=y,z=z,value=response))+
        #tern_limits(T=0.6,L=0.6,R=0.2) +
        geom_point(aes(colour=response)) +
        scale_colour_gradient(low="yellow",high="red") +
        theme_clockwise()+ 
        labs(x = input$VarX,y=input$VarY,z=input$VarZ)
    }
    
    print(plot1)
    NULL
    
  })
  
  catvar <- function(xvec,qvec=c(0.333333,0.666667)) {
    
    qs  <- quantile(xvec,qvec,na.rm=T)
    
    catvar <- 0 * xvec
    catvar <- catvar + 1*(xvec  < qs[1])
    catvar <- catvar + (length(qs)+1)*(xvec > qs[length(qs)])
    
    for (i in 2:(length(qs))) {
      catvar <- catvar + i *( (xvec >= qs[i-1])&(xvec < qs[i]) ) 
    }
    
    catvar[which(is.na(xvec))] <- 0
    catvar <- as.factor(catvar)
    
    return(catvar)
    
  }
  
  get_ilrCoord <- function(df.CODA) {
    D <- dim(df.CODA)[2]
    
    if(D==3) {
      z1      <- pivotCoord(df.CODA,pivotvar=1)
      z2      <- pivotCoord(df.CODA,pivotvar=2)
      z3      <- pivotCoord(df.CODA,pivotvar=3)
      
      zz      <- cbind(z1,z2,z3)
      df1      <- data.frame(zz)
      
    } else if(D==4) {
      z1      <- pivotCoord(df.CODA,pivotvar=1)
      colnames(z1) <- gsub("-", ".", colnames(z1))
      z11     <- pivotCoord(df.CODA[,-1],pivotvar=1)
      z12     <- pivotCoord(df.CODA[,-1],pivotvar=2)
      z13     <- pivotCoord(df.CODA[,-1],pivotvar=3)
      z2      <- pivotCoord(df.CODA,pivotvar=2)
      colnames(z2) <- gsub("-", ".", colnames(z2))
      z21     <- pivotCoord(df.CODA[,-2],pivotvar=1)
      z22     <- pivotCoord(df.CODA[,-2],pivotvar=2)
      z23     <- pivotCoord(df.CODA[,-2],pivotvar=3)
      z3      <- pivotCoord(df.CODA,pivotvar=3)
      colnames(z3) <- gsub("-", ".", colnames(z3))
      z31     <- pivotCoord(df.CODA[,-3],pivotvar=1)
      z32     <- pivotCoord(df.CODA[,-3],pivotvar=2)
      z33     <- pivotCoord(df.CODA[,-3],pivotvar=3)
      z4      <- pivotCoord(df.CODA,pivotvar=4)
      colnames(z4) <- gsub("-", ".", colnames(z4))
      z41     <- pivotCoord(df.CODA[,-4],pivotvar=1)
      z42     <- pivotCoord(df.CODA[,-4],pivotvar=2)
      z43     <- pivotCoord(df.CODA[,-4],pivotvar=3)
      
      zzA      <- cbind(z1[,1],z11,z12,z13,z2[,1],z21,z22,z23,
                        z3[,1],z31,z32,z33,z4[,1],z41,z42,z43)
      zzB      <- data.frame(zzA)
      colnames(zzB)[c(1,8,15,22)] <- c(colnames(z1)[1],colnames(z2)[1],
                                       colnames(z3)[1],colnames(z4)[1])
      df1 <- zzB
      
    } else {
      
      zList <- vector("list",D)
      
      for (i in 1:D) {
        zList[[i]] <- pivotCoord(df.CODA,pivotvar=i)
      }
      
      df1 <- do.call("cbind", zList)
      colnames(df1) <- gsub("-", ".", colnames(df1))
    }
    return(df1)
  }
  
  
  # End server  
}


