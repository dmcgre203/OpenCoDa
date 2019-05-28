library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Physical Activity CODA Regression Model"),
  
  shinyUI(navbarPage("My Application",
                     tabPanel("Read_Data",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                  
                                  # Input: Select a file ----
                                  fileInput("file1", "Choose CSV File",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  # Input: Checkbox if file has header ----
                                  checkboxInput("header", "Header", TRUE),
                                  
                                  # Input: Select separator ----
                                  radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("quote", "Quote",
                                               choices = c(None = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"'),
                                  
                                  # Input: Select quotes ----
                                  radioButtons("deci", "Decimal separator",
                                               choices = c(Period = ".",
                                                           Comma =  ","),
                                               selected = '.'),
                                  
                                  # Input: Select number of rows to display ----
                                  radioButtons("disp", "Display",
                                               choices = c(Head = "head",
                                                           All = "all"),
                                               selected = "head"),
                                  
                                  # Input: Select response variables ----
                                  uiOutput("choose_response")
                                  
                                ),
                                
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  tags$h3(textOutput("rawData_Header")), 
                                  tableOutput("contents"),
                                  
                                  # Horizontal line ----
                                  tags$hr(),        
                                  
                                  tags$h3(textOutput("rawDataSummary_Header")), 
                                  tableOutput("rawSummary")
                                )
                                
                              )),
                     
                     tabPanel("Covariates",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Additional parameters
                                  uiOutput("choose_covariates"),
                                  uiOutput("choose_cofactors")
                                  
                                ),
                                
                                mainPanel(
                                  # Output: Model spec ----
                                  textOutput("modelText"),
                                  
                                  tags$br(),
                                  
                                  # Output: Drop1 Table ----
                                  tableOutput("regTab"), 
                                  
                                  # Output: Model spec ----
                                  textOutput("regSummaryHeader"),                 
                                  
                                  # Output: Model summary ----
                                  verbatimTextOutput("regSummary"),
                                  
                                  # Output: Factor summary ----
                                  verbatimTextOutput("cofactorSummary")
                                  
                                )
                              )),
                     
                     tabPanel("ILR Coordinates",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Additional parameters
                                  uiOutput("choose_CODA"),
                                  uiOutput("choose_useImpute"),
                                  textOutput("choose_detectLimits_Header"),
                                  uiOutput("choose_detectLimits"),
                                  uiOutput("choose_ilr")
                                  
                                  
                                ),
                                
                                mainPanel(
                                  
                                  tags$h3(textOutput("zPatterns_Header")),
                                  plotOutput("zPatterns"),
                                  
                                  # Output: Imputation report
                                  tags$h3(textOutput("imputeTable_Header")),
                                  tableOutput("imputeTable"),  
                                  
                                  # Output: CODA Average
                                  tags$h3(textOutput("avgCODA_Table_Header")),
                                  tableOutput("avgCODA_Table"), 
                                  
                                  # Output: CODA ----
                                  tags$h3(textOutput("titleILRData")),
                                  tableOutput("ilrCoords"),  
                                  
                                  # Output: Model LR Test ----
                                  tags$h3(textOutput("titleCODA")),
                                  tableOutput("fullRegTab"),
                                  
                                  # Output: Model coefficients ----
                                  tags$h3(textOutput("titleCODACoef")),
                                  tableOutput("fullRegCoef")

                                  
                                )
                              )
                     ),
                     
                     tabPanel("Diagnostics",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Which parameter
                                  uiOutput("choose_DiagVar")
                                  
                                ),
                                
                                mainPanel(
                                  
                                  tableOutput("fullRegRsquared"),
                                  
                                  plotOutput("plot1"),
                                  plotOutput("plot2")
                                  
                                ) # end MainPanel
                              )  #end SidebarLayout
                     ),  #end tabPanel
                     
                     tabPanel("Comparator / Null Hypothesis",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Additional parameters
                                  uiOutput("choose_covariates_H0"),
                                  uiOutput("choose_cofactors_H0"),
                                  uiOutput("choose_ilr_H0")
                                  
                                ),
                                
                                mainPanel(
                                  # Output: Model spec ----
                                  verbatimTextOutput("fullRegSummary_H0"),
                                  verbatimTextOutput("fullRegComparison")
                                  
                                )
                              )
                     ),
                     
                     tabPanel("Forecasting",
                              
                              # Sidebar layout with input and output definitions ----
                              sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(   
                                  
                                  # Input: Illustration cofactors
                                  uiOutput("illustrationCofactors"),
                                  
                                  # Input: Illustration covariates
                                  uiOutput("illustrationCovariates"),
                                  
                                  # Input: Illustration compositional
                                  uiOutput("choose_illustrationX"),
                                  
                                  # Input: Illustration compositional
                                  uiOutput("choose_illustrationY"),
                                  
                                  # Input: Illustration compositional
                                  uiOutput("choose_illustrationZ"),
                                  
                                  # Input: Illustration fixed compositional
                                  uiOutput("illustrationComp"),
                                  
                                  # Input: Select number of rows to display ----
                                  radioButtons("dispDat", "Display actual data points",
                                               choices = c(Yes = TRUE,
                                                           No = FALSE),
                                               selected = TRUE)
                                  
                                ),
                                
                                mainPanel(
                                  
                                  plotOutput("illustrativePlot"),
                                  tableOutput("illustrativeData")
                                  
                                ) # end MainPanel
                              )  #end SidebarLayout
                     )  #end tabPanel
                     
  )
  )
)
